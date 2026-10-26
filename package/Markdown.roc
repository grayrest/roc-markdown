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
import Block
import Stack
import Lower
import Html

Markdown := [].{
    ## Parse a whole document.
    ##
    ## Which named entities are understood is `Ents.table`, a build-time
    ## constant rather than an argument — see that module for the one line to
    ## edit, and why it is not a parameter.
    ##
    ## Links are filtered by a denylist: `javascript:`, `vbscript:`, `data:`
    ## and `file:` hrefs become plain text and every other scheme links. Image
    ## sources are not filtered. This is `parse_with({ schemes: Any }, source)`.
    parse : Str -> List(Block)
    parse = |source| Markdown.parse_with({ schemes: Any }, source)

    ## Parse a whole document under an href policy.
    ##
    ## `schemes: Any` is `parse`'s denylist. `schemes: Only(list)` keeps the
    ## denylist and adds an allowlist: a link or autolink whose destination
    ## names a scheme links only when that scheme is in `list`, compared
    ## ignoring ASCII case; one with no scheme (`/path`, `#frag`, `rel/x`,
    ## `?q`) always links. A refused href becomes plain text, except that a
    ## refused email autolink written `<!…>` or `<?…>` is read as the raw HTML
    ## it also is, when it is some (a declaration, or an instruction ending in
    ## `?>`), which `to_html` omits. Listing `javascript` does not allow it.
    ##
    ## Under `Only`, an image's source passes the same rule or the image
    ## becomes its alt text. Under `Any` images are not filtered: a browser
    ## does not execute `javascript:` from `<img src>`, and trusted documents
    ## embed `data:` images.
    ##
    ## The scheme is everything before a `:` that comes before any `/`, `?` or
    ## `#` — so `localhost:5001/x` has the scheme `localhost`. Untrusted
    ## content should use `Only`, e.g. `Only(["http", "https", "mailto"])`.
    parse_with : { schemes : [Any, Only(List(Str))] }, Str -> List(Block)
    parse_with = |config, source| Lower.run(Stack.parse(source), config.schemes)

    ## Lower parsed blocks to a semantic HTML fragment.Raw HTML is replaced
    ## by a placeholder to avoid javascript injection via markdown HTML support.
    to_html : List(Block) -> Str
    to_html = |blocks| Html.render(blocks, True)

    ## Verbatim raw HTML.
    to_html_unsafe : List(Block) -> Str
    to_html_unsafe = |blocks| Html.render(blocks, False)
}
