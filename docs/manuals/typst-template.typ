// Branded template for the stabilitest.viz manuals ("Clinical calm",
// docs/brand/BRAND.md). Quarto calls stv-manual() from typst-show.typ.

#let stv-ink = rgb("#14323A")
#let stv-teal = rgb("#1F6F78")
#let stv-mist = rgb("#E3EEEC")
#let stv-border = rgb("#D9CFBF")
#let stv-muted = rgb("#5A6B70")
#let stv-paper = rgb("#F7F5F0")
#let stv-accent = rgb("#9A5E22")

#let stv-manual(
  title: none,
  subtitle: none,
  doctype: none,
  version: none,
  stabilitest: none,
  date: none,
  cover: true,
  toc: false,
  toc_title: "Contents",
  toc_depth: 2,
  doc,
) = {
  set document(title: title)
  set text(font: ("Inter",), size: 10pt, fill: stv-ink, lang: "en")
  set par(justify: false, leading: 0.68em, spacing: 1.05em)

  let footer-text = [stabilitest.viz #version #h(1fr) #doctype]
  set page(
    paper: "a4",
    margin: (x: 2.2cm, top: 2.4cm, bottom: 2.2cm),
    header: context {
      if counter(page).get().first() > 1 or not cover {
        set text(size: 8pt, fill: stv-muted)
        grid(
          columns: (auto, 1fr),
          align: (left + horizon, right + horizon),
          image("assets/mark-light.svg", height: 0.42cm),
          [#title],
        )
        v(-0.25cm)
        line(length: 100%, stroke: 0.4pt + stv-border)
      }
    },
    footer: context {
      if counter(page).get().first() > 1 or not cover {
        set text(size: 8pt, fill: stv-muted)
        line(length: 100%, stroke: 0.4pt + stv-border)
        v(-0.2cm)
        grid(
          columns: (1fr, auto),
          [stabilitest.viz #version · #doctype],
          [#counter(page).display() / #counter(page).final().first()],
        )
      }
    },
  )

  // Headings
  set heading(numbering: none)
  show heading: set text(font: ("Inter",), weight: 600, fill: stv-ink)
  show heading.where(level: 1): it => {
    v(0.6cm)
    block(below: 0.45cm)[
      #set text(size: 17pt)
      #it.body
      #v(-0.25cm)
      #line(length: 2.2cm, stroke: 2pt + stv-teal)
    ]
  }
  show heading.where(level: 2): it => block(above: 0.75cm, below: 0.35cm, text(size: 12.5pt, fill: stv-teal, it.body))
  show heading.where(level: 3): it => block(above: 0.55cm, below: 0.25cm, text(size: 10.5pt, it.body))

  // Links, code, tables
  show link: set text(fill: stv-teal)
  show raw: set text(font: ("JetBrains Mono",), size: 8.6pt)
  show raw.where(block: true): it => block(
    fill: stv-paper, stroke: (left: 2pt + stv-teal), inset: (x: 9pt, y: 7pt), radius: 2pt, width: 100%, it,
  )
  show raw.where(block: false): it => box(fill: stv-mist, inset: (x: 2.5pt), outset: (y: 2.5pt), radius: 2pt, it)
  set table(
    stroke: (x, y) => (
      top: if y == 0 { 0pt } else if y == 1 { 0.8pt + stv-ink } else { 0.4pt + stv-border },
      bottom: 0.4pt + stv-border,
    ),
    inset: (x: 6pt, y: 5pt),
  )
  show table.cell.where(y: 0): set text(weight: 600, fill: stv-muted, size: 8.6pt)
  show table: set text(size: 9pt)
  show figure.caption: set text(size: 8.6pt, fill: stv-muted)
  show figure: set block(breakable: false)

  // Cover
  if cover {
    page(header: none, footer: none, margin: (x: 2.2cm, y: 2.6cm))[
      #image("assets/lockup-light.svg", height: 1.1cm)
      #v(1fr)
      #text(size: 9pt, weight: 600, fill: stv-teal, tracking: 0.08em, upper(doctype))
      #v(0.15cm)
      #text(size: 28pt, weight: 600, fill: stv-ink, title)
      #if subtitle != none {
        v(0.2cm)
        text(size: 13pt, fill: stv-muted, subtitle)
      }
      #v(0.6cm)
      #line(length: 3cm, stroke: 2pt + stv-teal)
      #v(1fr)
      #set text(size: 9pt, fill: stv-muted)
      #grid(
        columns: (auto, auto), column-gutter: 0.6cm, row-gutter: 0.2cm,
        [Version], [stabilitest.viz #version],
        [Statistics], [stabilitest #stabilitest],
        [Date], [#date],
      )
      #v(0.4cm)
      Companion app for the stabilitest R package. Every number shown by the app comes from stabilitest.
    ]
  }

  // Compact documents (the executive summary) open with a title block
  if not cover {
    grid(
      columns: (1fr, auto),
      align: (left + bottom, right + bottom),
      [
        #text(size: 8.5pt, weight: 600, fill: stv-teal, tracking: 0.08em, upper(doctype))
        #v(0.05cm)
        #text(size: 20pt, weight: 600, title)
        #if subtitle != none { linebreak(); text(size: 11pt, fill: stv-muted, subtitle) }
      ],
      text(size: 8pt, fill: stv-muted)[stabilitest.viz #version \ stabilitest #stabilitest \ #date],
    )
    v(0.1cm)
    line(length: 100%, stroke: 2pt + stv-teal)
    v(0.2cm)
  }

  if toc {
    block(below: 0.6cm, text(size: 17pt, weight: 600, toc_title))
    outline(title: none, depth: toc_depth, indent: 1em)
    pagebreak()
  }

  doc
}
