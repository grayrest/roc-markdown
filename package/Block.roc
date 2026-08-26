## One block of a document — the unit a renderer walks.
##
## This markdown implementation was written to support a native application
## that does its own block rendering and this is the output format for that.
## The package also includes an HTML lowering (in `Html.roc`) for conformance
## testing and browser output.
##
## Every block carries `loc`, its SOURCE location (`plans/2026-08-26-rocsidian.md`
## D9): `loc.outer` is the byte interval the block spans, markers and all, and it
## is what the editor uses to map the caret's position back to a block so it can
## reveal that block's raw syntax and decorate its source range. A block's
## `node` is the block itself; matching a `Block` means matching `b.node`.
import Inl exposing [Inl]
import Loc exposing [Loc]

Block := { loc : Loc.T, node : Block.Node }.{
    Node : [
        ## A paragraph, already merged across soft-wrapped source lines.
        ##
        ## Merging happens in the PARSER, not the renderer. LLM output tends
        ## to be hard wrapped at a fairly short line length and that's handled
        ## here in the parser instead of honoring the line breaks.
        Para(List(Inl)),
        ## `Heading(level, spans)` — level 1..6.
        Heading(U8, List(Inl)),
        ## A fenced block: its language tag and its contents, raw and unparsed.
        ##
        ## `lang` is `""` for a bare fence and only present when written
        ## explicitly in the source document
        Code({ lang : Str, text : Str }),
        ## A blockquote. Blockquotes can contain paragraphs, lists, headings, etc.
        Quote(List(Block)),
        ## A list: its entries, each of which is a list of BLOCKS.
        ##
        ## `tight` is a rendering property the PARSER decides: a list whose entries
        ## are separated by blank lines wraps each entry's text in `<p>`, and a
        ## tight one does not. It cannot be recovered downstream, because by then
        ## the blank lines are gone.
        Items({ ordered : Bool, start : U64, tight : Bool, entries : List(List(Block)) }),
        ## A table, row-major and rectangular.
        ##
        ## `cells` is FLAT with an explicit `ncols` — one list of cells in
        ## row-major order, not a list of rows. Row 0 is the header; markdown
        ## tables always have one.
        ##
        ## **The parser guarantees `cells.len() == nrows * ncols`** by padding short
        ## rows and truncating long ones.
        ##
        ## `align` is one entry per column. `None` is a column whose delimiter
        ## carried no colon, and it is a real state rather than a default: it means
        ## "emit no alignment", which is not the same as any of the three.
        Table({ ncols : U64, align : List(Block.Align), cells : List(Block.Cell) }),
        ## Raw HTML, verbatim, as a block.
        ##
        ## Carried rather than escaped or dropped, because the two lowerings want
        ## different things and neither wants a decision made for it here: `to_html`
        ## passes it through (or omits it, when safe), and a native renderer — which
        ## has no HTML engine — shows it as source. The one exception is a comment,
        ## which is invisible in a browser and so is equivalently invisible natively.
        Html(Str),
        ## Horizontal rule.
        Rule,
    ]

    ## A column's alignment, from the delimiter row's colons: a leading `:` is
    ## left, a trailing one right, both are centre, neither is `None`.
    ##
    ## Four cases, not three. A plain `---` column has no alignment and must
    ## emit no attribute at all, which no combination of the other three says.
    Align : [None, Left, Center, Right]

    ## One cell's contents: `| **a** b |` is ONE cell holding two inline nodes,
    ## `[Strong([Text("a")]), Text(" b")]`.
    ##
    ## Named because `List(List(Inl))` cannot say which list is which, and the
    ## answer is not guessable from the type — the outer one is cells and the
    ## inner one is a cell's content, so the column dimension is `ncols` and
    ## appears in neither.
    Cell : List(Inl)

    ## Build a `Block` from its source location and node.
    mk : Loc.T, Block.Node -> Block
    mk = |loc, node| Block.{ loc, node }

    ## Build a `Block` spanning `[start, end)`.
    at : U64, U64, Block.Node -> Block
    at = |start, end, node| Block.{ loc: Loc.at(start, end), node }
}
