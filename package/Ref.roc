## A link reference definition, as a shared TYPE and nothing else.
##
## `[foo]: /url "title"` is collected by the block parser and consulted by the
## inline scanner, so its shape appears in about twenty signatures across four
## modules.
## 
## **Alias-only, importing nothing, so it can be a leaf of the import graph.**
## The parsing lives in `Defs.roc`, which needs `Scan`; `Scan` needs this type.
## Keeping the two apart is what stops that being a cycle.
Ref := [].{
    Def : { label : Str, dest : Str, title : Str }
}
