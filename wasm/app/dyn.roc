app [main!] { pf: platform "../platform/main.roc", md: "../../package/main.roc" }

import pf.Host
import md.Markdown
import Sample

## The control: the table is chosen from a HOST value, so no amount of constant
## folding can decide it. Both tables must be reachable, and the module should
## weigh about the same as the HTML5 one plus the XML one. If a literal-`Xml`
## module weighs the same as this, nothing was folded.
main! = |{}| {
    seed = Host.seed!({})
    opts = if seed > 0 {
        { entity_table: Html5 }
    } else {
        { entity_table: Xml }
    }
    Sample.run(Markdown.setup(opts), seed)
}
