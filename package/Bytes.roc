## Byte-level helpers: bounds-safe reads, slices and searches over `List(U8)`.
##
## Shared by the inline scanner, the block lexer and the definition parser.
## They take a plain `List(U8)` rather than the scanner's context because they
## run on labels, destinations and titles as well as on the scan buffer, and
## `Lex` calls them on lines that have no context at all.

Bytes := [].{
    at : List(U8), U64 -> U8
    at = |b, i|
        match List.get(b, i) {
            Ok(c) => c
            ## Out of range reads as a space, which is what makes the flanking
            ## rules fall out at the ends of a line without a special case:
            ## start-of-text behaves like preceding whitespace, exactly as the
            ## spec says it should.
            Err(_) => 32
        }

    ## `to - from` is a U64 subtraction, so an empty or inverted range has to be
    ## answered before it is computed. Every caller that got this wrong produced
    ## an "Integer subtraction overflowed" crash a long way from the cause.
    slice : List(U8), U64, U64 -> Str
    slice = |b, from, to|
        if to <= from {
            ""
        } else {
            Str.from_utf8_lossy(List.sublist(b, { start: from, len: to - from }))
        }

    ## Does `c` start a code point, rather than continue one? UTF-8
    ## continuation bytes are `0x80` to `0xBF`.
    is_utf8_lead : U8 -> Bool
    is_utf8_lead = |c| c < 0x80 or c >= 0xC0

    ## How many of byte `c` start at `i`.
    run_len : List(U8), U64, U8, U64 -> U64
    run_len = |b, i, c, acc|
        if Bytes.at(b, i + acc) == c and i + acc < List.len(b) {
            Bytes.run_len(b, i, c, acc + 1)
        } else {
            acc
        }

    find_byte : List(U8), U64, U8 -> Try(U64, [NotFound])
    find_byte = |b, i, c|
        if i >= List.len(b) {
            Err(NotFound)
        } else if Bytes.at(b, i) == c {
            Ok(i)
        } else {
            Bytes.find_byte(b, i + 1, c)
        }

    ## The first run of exactly `n` of byte `c` at or after `from`.
    find_run : List(U8), U64, U8, U64 -> Try(U64, [NotFound])
    find_run = |b, from, c, n|
        if from >= List.len(b) {
            Err(NotFound)
        } else if Bytes.at(b, from) == c {
            len = Bytes.run_len(b, from, c, 0)
            if len == n {
                Ok(from)
            } else {
                Bytes.find_run(b, from + len, c, n)
            }
        } else {
            Bytes.find_run(b, from + 1, c, n)
        }

    find_unescaped : List(U8), U64, U8 -> Try(U64, [NotFound])
    find_unescaped = |b, i, c|
        if i >= List.len(b) {
            Err(NotFound)
        } else if Bytes.at(b, i) == 92 {
            Bytes.find_unescaped(b, i + 2, c)
        } else if Bytes.at(b, i) == c {
            Ok(i)
        } else {
            Bytes.find_unescaped(b, i + 1, c)
        }

    ## The first occurrence of `seq` starting at or after `i`.
    find_seq : List(U8), U64, List(U8) -> Try(U64, [NotFound])
    find_seq = |b, i, seq|
        if i + List.len(seq) > List.len(b) {
            Err(NotFound)
        } else if List.sublist(b, { start: i, len: List.len(seq) }) == seq {
            Ok(i)
        } else {
            Bytes.find_seq(b, i + 1, seq)
        }

    ## One past where the last occurrence of `seq` starting before `i` starts,
    ## or 0 when none does.
    seq_last : List(U8), U64, List(U8) -> U64
    seq_last = |b, i, seq|
        if i == 0 {
            0
        } else if List.sublist(b, { start: i - 1, len: List.len(seq) }) == seq {
            i
        } else {
            Bytes.seq_last(b, i - 1, seq)
        }

    starts : List(U8), U64, Str -> Bool
    starts = |b, i, p| Bytes.slice(b, i, i + Str.count_utf8_bytes(p)) == p

    ## `is_space` and `is_punct` are the ASCII half. The spec's flanking rules
    ## are defined over Unicode whitespace and punctuation, so the callers pair
    ## them with `Emph.is_uni_punct`, which classifies by codepoint RANGE
    ## rather than by table — `*£*` and `*€*` flank like punctuation, and a
    ## codepoint outside the ranges is treated as a letter. That is the safe
    ## direction: it flanks like text rather than suppressing emphasis that
    ## should exist.
    is_space : U8 -> Bool
    is_space = |c| c == 32 or (c >= 9 and c <= 13)

    is_punct : U8 -> Bool
    is_punct = |c|
        (c >= 33 and c <= 47) or (c >= 58 and c <= 64) or (c >= 91 and c <= 96) or (c >= 123 and c <= 126)

    is_alpha : U8 -> Bool
    is_alpha = |c| (c >= 65 and c <= 90) or (c >= 97 and c <= 122)

    ## Is there a run of exactly `n` backticks starting at or after `from`?
    ## `tick_last` is the scanner's table (`Scan.Ctx`): for each run length
    ## `n`, one past where the last run of exactly `n` starts, at index
    ## `n - 1`, and 0 for none. A run can start at or after `from` exactly
    ## when the last one does.
    tick_run_after : List(U64), U64, U64 -> Bool
    tick_run_after = |tick_last, from, n|
        match List.get(tick_last, n - 1) {
            Ok(last) => last > from
            Err(_) => False
        }
}
