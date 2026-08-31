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

The parser passes 572 / 576 combined test cases between CommonMark and GFM.

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

Aside from inline HTML, hrefs are filtered by `Scan.safe_href`, which refuses
`javascript:`, `data:` and `vbscript:` and allows everything else.

## Build size: the entity table is chosen by the caller

As a fully compliant native parser, this package isn't particularly small
as a WASM module but there is an easy size win. Markdown allows for the
use of the full set of 2125 HTML5 named entities, which most people do
not use. Asking for the five predefined XML entities (`&amp;` `&lt;`
`&gt;` `&quot;` `&apos;`) instead saves 45k.

The choice is a parameter — `Markdown.setup({ entity_table: Xml })` — and the
API it returns carries the table to every lookup. **The compiler drops the table
you did not ask for**, provided the choice is a literal at the call:

| | total | code | data |
|---|---:|---:|---:|
| `setup(default)` — Html5 | 830,875 | 633,687 | 111,649 |
| `setup({ entity_table: Xml })` | 775,265 | 633,685 | 56,042 |
| choice taken from a runtime value | 831,037 | 633,717 | 111,781 |

**−55,610 B** for asking for `Xml`, all of it data. The third row is the
control: when the table is picked from a value the compiler cannot see, both
tables are retained (its data is the other two rows' tables plus the base), so
the win in row two is genuine dead-code elimination and not a measurement
artefact.

### What the parameter costs

This used to be a source edit — a `table = Ents.full` constant in `Ents.roc`
that a build changed by hand. Against that baseline, measured with a
structurally identical harness:

| | total | code | data |
|---|---:|---:|---:|
| `table = Ents.full` (was) | 820,934 | 623,740 | 111,654 |
| `table = Ents.minimal` (was) | 766,742 | 625,155 | 56,047 |

Threading the table costs **about 10 KB of code** — 633,687 against 623,740 for
the HTML5 build. Dropping the table saves 55 KB, so the parameter is a net win
of roughly 45 KB over shipping the default, and lands within 8.5 KB of what the
hand edit achieved while requiring nobody to edit this repo to get it.

The earlier reasoning that "the code cost most of what dropping the table saved"
was directionally right and wrong on magnitude: the cost is real, and it is a
fifth of the saving.

Reproduce with `just wasm-size main`, `just wasm-size xml`, `just wasm-size dyn`.
The harness lives in `wasm/` — the smallest platform that will link, so what the
module weighs is this package plus a rounding error. **It needs a compiler older
than `release-fast-c32a5bac`**; the wasm backend regressed after
`release-fast-cb55b063` and newer builds emit an empty 26-byte module while
reporting success. See the `wasm_roc` note in the `Justfile`.

## API

`Markdown.setup` takes the options and returns the API closed over them:

```roc
md = Markdown.setup(Markdown.default)          # or { ..Markdown.default, entity_table: Xml }
parse = md.parse
blocks = parse(source)
```

| | |
|---|---|
| `setup : Options -> Api` | The API, closed over an entity table. |
| `default : Options` | `{ entity_table: Html5 }`. |
| `Api.parse : Str -> List(Block)` | Markdown in, blocks out. |
| `Api.to_html : List(Block) -> Str` | Semantic HTML. Raw HTML becomes a placeholder comment. |
| `Api.to_html_unsafe : List(Block) -> Str` | Semantic HTML, raw HTML verbatim. |

**Bind a field before calling it.** `md.parse(source)` reads as a method call and
the compiler reports that the record has no such method; `parse = md.parse`
followed by `parse(source)` is the working form.

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
- **`Ents`** — the two entity tables, `full` and `minimal`, that `setup` picks between. See above.

## Layout

```
package/
  main.roc      package header
  Markdown.roc  the front door
  Stack.roc     phase 1 — block structure, a line at a time against a stack
  Lower.roc     phase 2 — lift the link definitions, then parse the inlines
  Scan.roc      the inline scanner: one left-to-right pass, then emphasis
  Lex.roc       block-level lexical predicates
  Table.roc     the table sub-parser
  Defs.roc      link reference definitions
  Ents.roc      the HTML5 named character references
  Html.roc      blocks to a semantic HTML fragment
  Block.roc  Inl.roc  Span.roc  Doc.roc  Ref.roc   the types
  Test.roc      the gate — 51 hand-written expects
  SpecTest.roc  the conformance suite — GENERATED, do not hand-edit
examples/
  md2html.roc   markdown files in, HTML files out, on basic-cli
spec/           the vendored specs and the generator
```

Two phases, which is the spec's own parsing strategy. The second walk is why the
first can ignore link definitions entirely: a reference may be written before the
definition that resolves it, so inlines cannot be parsed as each block closes.
