app [main!] { pf: platform "../platform/main.roc", md: "../../package/main.roc" }

import pf.Host
import Sample

## The source is built from `Host.seed!`, a value the compiler cannot see, so
## `parse` cannot be const-folded into static data — the measurement has to
## link the parser, not a precomputed answer.
main! = |{}| Sample.run(Host.seed!({}))
