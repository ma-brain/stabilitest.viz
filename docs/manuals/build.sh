#!/usr/bin/env bash
# Renders the three manuals to _output/ and checks them.
#
#   docs/manuals/build.sh
#
# Needs the installed stabilitest.viz (the tables are read from it), Quarto,
# the R packages knitr, rmarkdown and yaml, and pdfinfo and pdftotext
# (poppler). Screenshots are taken separately with dev/screenshots.R.
set -euo pipefail
cd "$(dirname "$0")"

version=$(Rscript -e 'cat(as.character(utils::packageVersion("stabilitest.viz")))')
stabilitest=$(Rscript -e 'cat(as.character(utils::packageVersion("stabilitest")))')
repo_version=$(sed -n 's/^Version: //p' ../../DESCRIPTION)
if [[ "$version" != "$repo_version" ]]; then
  echo "Installed stabilitest.viz $version is not the checked-out $repo_version; install it first." >&2
  exit 1
fi

quarto render -M "version:$version" -M "stabilitest:$stabilitest" -M "date:$(date +%Y-%m-%d)"

fail=0
pages() { pdfinfo "_output/$1.pdf" | awk '/^Pages:/ {print $2}'; }

# The executive summary is two pages
if [[ "$(pages executive-summary)" != 2 ]]; then
  echo "executive-summary.pdf has $(pages executive-summary) pages, not 2" >&2
  fail=1
fi

# Wording: the resampling component is never called "reproducibility". The
# technical documentation names stabilitest's own metric and the rule itself,
# so only those uses are allowed there.
for doc in executive-summary user-guide technical-documentation; do
  text=$(pdftotext -layout "_output/$doc.pdf" -)
  if [[ "$doc" == technical-documentation ]]; then
    text=$(grep -v -e 'bootstrap_reproducibility' -e '["“]reproducib' <<<"$text" || true)
  fi
  if grep -qi 'reproducib' <<<"$text"; then
    echo "$doc.pdf uses \"reproducib...\":" >&2
    grep -i 'reproducib' <<<"$text" >&2
    fail=1
  fi
  if grep -q 'not generated: run dev/screenshots.R' <<<"$text"; then
    echo "$doc.pdf is missing screenshots" >&2
    fail=1
  fi
done

for doc in executive-summary user-guide technical-documentation; do
  echo "$doc.pdf: $(pages "$doc") pages"
done
exit $fail
