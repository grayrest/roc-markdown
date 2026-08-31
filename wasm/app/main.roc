app [main!] { pf: platform "../platform/main.roc", md: "../../package/main.roc" }

import pf.Host
import md.Markdown
import Sample

## The size harness's payload: one document through the whole package, with the
## entity table asked for by NAME at the call — the case a tree shaker has the
## best possible chance on, since `Html5` is a literal `setup` could be folded
## through.
##
## The source is built from `Host.seed!`, a value the compiler cannot see, so
## `parse` cannot be const-folded into static data — the measurement has to link
## the parser, not a precomputed answer. `&amp;` and `&copy;` are what reach the
## entity table, which is the constant this harness exists to weigh.
main! = |{}| Sample.run(Markdown.setup(Markdown.default), Host.seed!({}))
