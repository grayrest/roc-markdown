## ATX headings: whether a line is one, its level, and its text.
##
## Pure functions of one line, split out of `Stack`. A SETEXT heading is not
## here: it rewrites the paragraph already buffered above its underline, which
## is stack work (`Stack.setext`).
import Lex

Headings := [].{
    ## Both of these are asked of nearly every line, so both start with the
    ## cheapest possible rejection. Without it they each ran a `split_on` and a
    ## `join_with` — two allocations and a substring search — to normalise tabs
    ## in a line that is not a heading and usually has no tab either.
    atx_level : Str -> U8
    atx_level = |t|
        if !t.starts_with("#") {
            0
        } else {
            Headings.atx_at(t)
        }

    ## `######` is the deepest heading; seven or more hashes are prose.
    max_atx_level : U64
    max_atx_level = 6

    atx_at : Str -> U8
    atx_at = |t| {
        detabbed = Headings.detab(t)
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        rest = Str.drop_first_bytes(detabbed, lvl).ok_or("")
        if lvl == 0 or lvl > Headings.max_atx_level {
            0
        } else if Str.is_empty(rest) or rest.starts_with(" ") {
            ## In range by the test above, so the narrowing cannot wrap.
            lvl.to_u8_wrap()
        } else {
            ## No space after the run — `#hashtag` is prose.
            0
        }
    }

    detab : Str -> Str
    detab = |t|
        if t.contains("\t") {
            Str.join_with(t.split_on("\t"), " ")
        } else {
            t
        }

    ## The heading's text plus everything phase 1 dropped after it — the closing
    ## `##` sequence and any trailing spaces — so `Loc.seg` can measure back from
    ## the line's end. Taken from the UNTRIMMED line for that reason: `atx_text`
    ## works on `rest.trim()`, and a trimmed tail would place the text as many
    ## bytes late as the line has trailing whitespace.
    atx_tail : Str -> Str
    atx_tail = |rest| {
        detabbed = Headings.detab(Str.trim_start(rest))
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        Str.trim_start(Str.drop_first_bytes(detabbed, lvl).ok_or(""))
    }

    atx_text : Str -> Str
    atx_text = |t| {
        detabbed = Headings.detab(t)
        lvl = Lex.leading_hashes(Str.to_utf8(detabbed), 0)
        rest = Str.drop_first_bytes(detabbed, lvl).ok_or("")
        if Str.is_empty(rest) {
            ""
        } else {
            Lex.drop_closing(rest.trim())
        }
    }
}
