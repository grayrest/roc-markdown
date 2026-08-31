app [main!] { pf: platform "../platform/main.roc", md: "../../package/main.roc" }

import pf.Host
import md.Markdown
import Sample

## The same document, asking for the five XML entities. `Xml` is a literal here,
## so if the compiler can ever fold the choice through `setup` and drop the
## HTML5 table, it can do it on this module.
main! = |{}| Sample.run(Markdown.setup({ ..Markdown.default, entity_table: Xml }), Host.seed!({}))
