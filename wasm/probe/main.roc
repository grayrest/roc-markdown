app [main!] { pf: platform "../platform/main.roc" }
import pf.Host
main! = |{}| {
    message: "probe seed=${Host.seed!({}).to_str()}",
    n: Host.seed!({}) * 2,
}
