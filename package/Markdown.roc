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
##
## ## The entity table is chosen by the CALLER
##
## `Markdown.setup` takes the choice and returns the API closed over it. That
## replaces the `Ents.table` constant a build used to edit by hand, and it is
## the whole subject of this branch: whether a caller that asks for `Xml` gets a
## build with no HTML5 table in it depends on the compiler folding the choice
## through `setup` and dropping the branch not taken. The README records what it
## actually does.
import Block exposing [Block]
import Ents
import Stack
import Lower
import Html

Markdown := [].{
    ## Which named character references the parser understands.
    ##
    ## `Html5` is the full 2,125-name set; `Xml` is the five predefined XML
    ## references (`&amp;` `&lt;` `&gt;` `&quot;` `&apos;`). Numeric references
    ## (`&#35;`, `&#x22;`) are code rather than table and work under both.
    Table : [Html5, Xml]

    Options : { entity_table : Markdown.Table }

    ## `Html5`, the CommonMark-conformant choice. Spread it to change one field:
    ## `Markdown.setup({ ..Markdown.default, entity_table: Xml })`.
    default : Markdown.Options
    default = { entity_table: Html5 }

    ## The package's functions, closed over an entity table.
    ##
    ## A record of closures cannot be called as `api.parse(src)` — that parses
    ## as a method call and the compiler reports no such method. Bind the field
    ## first: `parse = api.parse` then `parse(src)`.
    Api : {
        parse : Str -> List(Block),
        to_html : List(Block) -> Str,
        to_html_unsafe : List(Block) -> Str,
    }

    ## Build the API against an entity table.
    ##
    ## Which named entities are understood is fixed here, once, and the returned
    ## closures carry it to every lookup — including the ones inside the parser
    ## that no public parameter could otherwise reach.
    setup : Markdown.Options -> Markdown.Api
    setup = |opts| {
        tbl = match opts.entity_table {
            Html5 => Ents.full
            Xml => Ents.minimal
        }
        {
            parse: |source| Lower.run(tbl, Stack.parse(tbl, source)),
            ## Lower parsed blocks to a semantic HTML fragment. Raw HTML is
            ## replaced by a placeholder to avoid javascript injection via
            ## markdown HTML support.
            to_html: |blocks| Html.render(blocks, True),
            ## Verbatim raw HTML.
            to_html_unsafe: |blocks| Html.render(blocks, False),
        }
    }
}
