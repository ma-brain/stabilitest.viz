#show: doc => stv-manual(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(doctype)$
  doctype: [$doctype$],
$endif$
$if(version)$
  version: [$version$],
$endif$
$if(stabilitest)$
  stabilitest: [$stabilitest$],
$endif$
$if(date)$
  date: [$date$],
$endif$
$if(compact)$
  cover: false,
$endif$
$if(toc)$
  toc: $toc$,
$endif$
  toc_depth: $toc-depth$,
  doc,
)
