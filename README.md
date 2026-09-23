# roc-markdown

A Markdown (CommonMark + GFM) parser in pure Roc.

This parser is LLM written. The primary consumer is my interactive app platform
so it was made with some consideration to performance and has gone through two
rounds of profiling and does the minimal amount of copying. My apps consume
`Block`s directly but the package has HTML output for testing spec compliance
and more general use.

```roc
app [main!] {
    pf: platform "…",
    md: "./package/main.roc",   # a path, a submodule, or a bundle URL
}

import md.Markdown
import md.Block exposing [Block]

blocks : List(Block)
blocks = Markdown.parse("# Title\n\nSome **bold** text.")

html = Markdown.to_html(blocks)
# <h1>Title</h1>
# <p>Some <strong>bold</strong> text.</p>
```

## Example

`examples/md2html.roc` is an end to end example on the
[basic-cli](https://github.com/roc-lang/basic-cli) platform. Markdown files in,
 `.html` files out, one per argument, beside the input with the extension swapped.

```bash
just md2html notes/*.md
```

Aside from the basic HTML serialization it demonstrates walking the `Block`
list to extract the first heading for the page title.

## Spec Conformance

The parser passes 672 / 676 combined test cases between CommonMark and GFM.

Three are extension divergences. CommonMark has no autolink extension, so it
asserts a bare URL as text while the GFM extension says it is a link.

The fourth is GFM's disallowed-raw-HTML filter, and its absence is deliberate.
Implmenting it picks up its own case but escaping `<pre`, `<style` and `<textarea`
breaks conformance in other tests. A true sanitizer is non-trivial and outside
the scope of a parser.

## Safety

`to_html` is safe by default by opting out. Raw HTML in the source becomes
`<!-- raw HTML omitted -->` rather than passing through; `to_html_unsafe` is the
opt-in.

Aside from inline HTML, hrefs are filtered by `Link.safe_href`, which refuses
`javascript:`, `vbscript:`, `data:` and `file:` and allows everything else.

## Build size: The entity table is the one large constant

As a fully compliant native parser, this package isn't particularly small
as a WASM module but there is an easy size win. Markdown allows for the
use of the full set of 2125 HTML5 named entities, which most people do
not use. Swapping the entity table over to the five named XML entities
(`&amp;` `&lt;` `&gt;` `&quot;` `&apos;`) saves 55k.

Every lookup in the package reads `Ents.table`, which is one line of
`package/Ents.roc`:

```roc
table : List(Ents.Ent)
table = Ents.full        # the 2,125 HTML5 names — edit to Ents.minimal
```

| | total | code | data |
|---|---:|---:|---:|
| `table = Ents.full` | 840,932 | 643,820 | 111,583 |
| `table = Ents.minimal` | 786,682 | 645,180 | 55,973 |

**−54,250 B**, essentially all of it data.

Roc does not currently have any facilities for configuring this at build
time so it's a source edit.

## API

| | |
|---|---|
| `parse : Str -> List(Block)` | Markdown in, blocks out. |
| `to_html : List(Block) -> Str` | Semantic HTML. Raw HTML becomes a placeholder comment. |
| `to_html_unsafe : List(Block) -> Str` | Semantic HTML, raw HTML verbatim. |

Four exposed types:

- **`Block`** — `Para`, `Heading`, `Code`, `Quote`, `Items`, `Table`, `Html`,
  `Rule`. The unit a renderer walks. Paragraphs arrive already merged across
  soft-wrapped source lines, and a list's `tight` flag is decided by the parser
  because by render time the blank lines are gone.
- **`Inl`** — the inline TREE: `Text`, `Code`, `Emph`, `Strong`, `Strike`,
  `Link`, `Image`, `SoftBreak`, `HardBreak`, `Raw`, `Task`. Nesting is
  represented rather than approximated, because `**a *b* c**` is one `<strong>`
  containing an `<em>` and the spec compares output exactly.
- **`Span`** — that tree lowered to flat styled runs (`text`, `bold`, `italic`,
  `code`, `link`), via `Span.flatten`. Lossy for structure, lossless for
  rendering, and the shape a text shaper actually wants.
- **`Ents`** — the entity tables, and the `table` line that picks one. See above.

## Layout

```
package/
  main.roc      package header
  Markdown.roc  the front door
  Stack.roc     phase 1 — block structure, a line at a time against a stack
  Lower.roc     phase 2 — lift the link definitions, then parse the inlines
  Scan.roc      the inline scanner: one left-to-right pass over the bytes
  Emph.roc      delimiter flanking, and the pass that pairs emphasis
  Link.roc      link and image parts: brackets, destinations, labels, hrefs
  Autolink.roc  `<scheme:…>`, bare URLs and email addresses
  Tag.roc       raw inline HTML tags
  Bytes.roc     bounds-safe byte reads and searches
  Lex.roc       block-level lexical predicates
  Table.roc     the table sub-parser
  Defs.roc      link reference definitions
  Ents.roc      the HTML5 named character references
  Html.roc      blocks to a semantic HTML fragment
  Block.roc  Inl.roc  Span.roc  Doc.roc  Ref.roc   the types
  Test.roc      the gate — hand-written expects
  LocTest.roc   the gate — source-location invariants the spec suite cannot see
  SpecTest.roc  the conformance suite — GENERATED, do not hand-edit
examples/
  md2html.roc   markdown files in, HTML files out, on basic-cli
spec/           the vendored specs and the generator
```

Two phases, which is the spec's own parsing strategy. The second walk is why the
first can ignore link definitions entirely: a reference may be written before the
definition that resolves it, so inlines cannot be parsed as each block closes.
