Host := [].{
    ## A host-supplied runtime value. Exists so the gate-zero app can build a
    ## result the compiler provably cannot const-evaluate into static data
    ## (G0 learning #5), forcing genuine runtime allocation through roc_alloc
    ## and proving the hosted-call direction (Roc -> Rust) on wasm32.
    seed! : {} => I64
}
