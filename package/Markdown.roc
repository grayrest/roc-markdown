## Markdown to plain data. The package's front door, and nothing else.
##
## Two phases, which is the spec's own parsing strategy:
##
##   `Stack.parse`  a line at a time against a stack of open blocks, producing
##                  a `Doc` whose leaves are still raw source text.
##   `Lower.run`    walks that tree twice — once to lift out the link reference
##                  definitions, once to parse the inlines with the complete
##                  table in hand.
##
## The second walk is why the first can ignore definitions entirely. A reference
## may be written before the definition that resolves it, so inlines cannot be
## parsed as each block closes.
import Block exposing [Block]
import Stack
import Lower
import Html

Markdown := [].{
    ## Parse a whole document.
    ##
    ## Which named entities are understood is `Ents.table`, a build-time
    ## constant rather than an argument — see that module for the one line to
    ## edit, and why it is not a parameter.
    parse : Str -> List(Block)
    parse = |source| Lower.run(Stack.parse(source))

    ## Lower parsed blocks to a semantic HTML fragment.Raw HTML is replaced
    ## by a placeholder to avoid javascript injection via markdown HTML support.
    to_html : List(Block) -> Str
    to_html = |blocks| Html.render(blocks, True)

    ## Verbatim raw HTML.
    to_html_unsafe : List(Block) -> Str
    to_html_unsafe = |blocks| Html.render(blocks, False)
}
