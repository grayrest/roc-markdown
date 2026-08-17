## The inline scanner: one left-to-right pass over bytes, then emphasis.
##
## Two stages, in the order the spec defines them:
##
##  1. `tokens` — a single pass emitting finished nodes for everything whose
##     extent is decidable on sight (escapes, code spans, autolinks, raw HTML,
##     breaks, links) and DELIMITER RUNS for `*`, `_` and `~`, which are not.
##  2. `emphasis` — the delimiter-matching pass. Whether a `*` opens, closes,
##     both or neither depends on the characters either side of it, so it cannot
##     be decided during (1).
import Ref
import Inl exposing [Inl]
import Ents

Scan := [].{
    ## A token: either a finished node, or a run of emphasis characters whose
    ## role is still undecided.
    Tk : [
        Node(Inl),
        Delim({ ch : U8, n : U64, opener : Bool, closer : Bool, both : Bool }),
    ]

    ## The generic byte helpers below — `at`, `slice`, `run_len`, `find_byte` —
    ## deliberately keep taking a plain `List(U8)`. They are called on labels,
    ## destinations and titles as well as on the scan buffer, and by `Lex` on
    ## lines that have no context at all.
    Ctx : { b : List(U8), defs : List(Ref.Def) }

    run : Str, List(Ref.Def) -> List(Inl)
    run = |s, defs| Scan.emphasis(Scan.tokens({ b: Str.to_utf8(s), defs: defs }, 0, [], []))

    ## --- character classes ---------------------------------------------------
    ##
    ## `is_space` and `is_punct` are the ASCII half. The spec's flanking rules
    ## are defined over Unicode whitespace and punctuation, so the callers pair
    ## them with `is_uni_punct` below, which classifies by codepoint RANGE
    ## rather than by table — `*£*` and `*€*` flank like punctuation, and a
    ## codepoint outside the ranges is treated as a letter. That is the safe
    ## direction: it flanks like text rather than suppressing emphasis that
    ## should exist.
    is_space : U8 -> Bool
    is_space = |c| c == 32 or (c >= 9 and c <= 13)

    is_punct : U8 -> Bool
    is_punct = |c|
        (c >= 33 and c <= 47) or (c >= 58 and c <= 64) or (c >= 91 and c <= 96) or (c >= 123 and c <= 126)

    ## The codepoint starting at `i`, or 0 if there is none.
    cp_after : List(U8), U64 -> U64
    cp_after = |b, i| {
        c = Scan.at(b, i)
        if i >= List.len(b) or c < 128 {
            0
        } else if c < 224 {
            (c - 192).to_u64() * 64 + (Scan.at(b, i + 1) - 128).to_u64()
        } else if c < 240 {
            (c - 224).to_u64() * 4096
            + (Scan.at(b, i + 1) - 128).to_u64() * 64
            + (Scan.at(b, i + 2) - 128).to_u64()
        } else {
            (c - 240).to_u64() * 262144
            + (Scan.at(b, i + 1) - 128).to_u64() * 4096
            + (Scan.at(b, i + 2) - 128).to_u64() * 64
            + (Scan.at(b, i + 3) - 128).to_u64()
        }
    }

    ## The codepoint ENDING at `i - 1`: walk back over continuation bytes to
    ## the lead, then decode forward from it.
    cp_before : List(U8), U64 -> U64
    cp_before = |b, i|
        if i == 0 {
            0
        } else {
            Scan.cp_after(b, Scan.lead_of(b, i - 1))
        }

    lead_of : List(U8), U64 -> U64
    lead_of = |b, i| {
        c = Scan.at(b, i)
        if i > 0 and c >= 128 and c < 192 {
            Scan.lead_of(b, i - 1)
        } else {
            i
        }
    }

    ## Unicode P* and S* by RANGE rather than by table: Latin-1 punctuation and
    ## symbols, general punctuation, currency, letterlike through the symbol
    ## blocks, and CJK punctuation. A codepoint outside them is treated as a
    ## letter, which is the safe direction — it flanks like text rather than
    ## suppressing emphasis that should exist.
    is_uni_punct : U64 -> Bool
    is_uni_punct = |cp|
        (cp >= 161 and cp <= 191)
        or cp == 215
        or cp == 247
        or (cp >= 8192 and cp <= 8303)
        or (cp >= 8352 and cp <= 8399)
        or (cp >= 8448 and cp <= 11263)
        or (cp >= 12288 and cp <= 12351)

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
    ## The same range as `slice`, without the conversion. `List.sublist` is a
    ## seamless slice — it shares the source allocation and copies nothing —
    ## which is what makes an inline leaf free to build.
    bytes : List(U8), U64, U64 -> List(U8)
    bytes = |b, from, to|
        if to <= from {
            []
        } else {
            List.sublist(b, { start: from, len: to - from })
        }

    slice : List(U8), U64, U64 -> Str
    slice = |b, from, to|
        if to <= from {
            ""
        } else {
            Str.from_utf8_lossy(List.sublist(b, { start: from, len: to - from }))
        }

    ## How many of byte `c` start at `i`.
    run_len : List(U8), U64, U8, U64 -> U64
    run_len = |b, i, c, acc|
        if Scan.at(b, i + acc) == c and i + acc < List.len(b) {
            Scan.run_len(b, i, c, acc + 1)
        } else {
            acc
        }

    ## --- stage 1: tokens -----------------------------------------------------
    ##
    ## `buf` is the pending literal text, flushed whenever a token interrupts it.
    ## Accumulating bytes and flushing once is what keeps this O(n): appending to
    ## the output per character would rebuild the node list on every byte.
    tokens : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    tokens = |ctx, i, buf, out|
        if i >= List.len(ctx.b) {
            Scan.flush(buf, out)
        } else {
            c = Scan.at(ctx.b, i)
            if c == 92 {
                Scan.backslash(ctx, i, buf, out)
            } else if c == 96 {
                Scan.code_span(ctx, i, buf, out)
            } else if c == 10 {
                Scan.newline(ctx, i, buf, out)
            } else if (c == 104 or c == 119 or c == 102) and Scan.at_boundary(ctx.b, i) and Scan.bare_len(ctx.b, i) > 0 {
                Scan.bare_link(ctx, i, buf, out)
            } else if Scan.at_boundary(ctx.b, i) and Scan.email_len(ctx.b, i) > 0 {
                ## Found at its START, like a bare URL. Backing up from the `@`
                ## looked simpler and could not work: an `_` in the local part
                ## is an emphasis delimiter, so by the time the `@` arrives the
                ## buffer has already been flushed and the local part is gone.
                n = Scan.email_len(ctx.b, i)
                addr = Scan.slice(ctx.b, i, i + n)
                node = Node(Link({ href: "mailto:${addr}", title: "", kids: [Text(addr)] }))
                Scan.tokens(ctx, i + n, [], List.append(Scan.flush(buf, out), node))
            } else if c == 38 {
                Scan.entity(ctx, i, buf, out)
            } else if c == 60 {
                Scan.angle(ctx, i, buf, out)
            } else if c == 91 {
                Scan.bracket(ctx, i, 1, buf, out, False)
            } else if c == 33 and Scan.at(ctx.b, i + 1) == 91 {
                Scan.bracket(ctx, i, 2, buf, out, True)
            } else if c == 42 or c == 95 or c == 126 {
                n = Scan.run_len(ctx.b, i, c, 0)
                flags = Scan.flanking(ctx.b, i, n, c)
                Scan.tokens(ctx, i + n, [], List.append(Scan.flush(buf, out), Delim({ ch: c, n, opener: flags.opener, closer: flags.closer, both: flags.opener and flags.closer })))
            } else {
                Scan.tokens(ctx, i + 1, List.append(buf, c), out)
            }
        }

    flush : List(U8), List(Scan.Tk) -> List(Scan.Tk)
    flush = |buf, out|
        if List.is_empty(buf) {
            out
        } else {
            List.append(out, Node(Text(Str.from_utf8_lossy(buf))))
        }

    ## `\` before ASCII punctuation is that character, literally. `\` before a
    ## newline is a hard break. `\` before anything else is a literal backslash —
    ## which is why this cannot be a blanket "skip the next byte".
    backslash : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    backslash = |ctx, i, buf, out| {
        nxt = Scan.at(ctx.b, i + 1)
        if nxt == 10 and i + 1 < List.len(ctx.b) {
            Scan.tokens(ctx, i + 2, [], List.append(Scan.flush(buf, out), Node(HardBreak)))
        } else if Scan.is_punct(nxt) and i + 1 < List.len(ctx.b) {
            Scan.tokens(ctx, i + 2, List.append(buf, nxt), out)
        } else {
            Scan.tokens(ctx, i + 1, List.append(buf, 92), out)
        }
    }

    ## A code span is opened by a run of N backticks and closed by the next run
    ## of EXACTLY N — not N or more. That is why this counts runs rather than
    ## searching for the next backtick: `` `` ` `` `` is one code span containing
    ## a backtick, and a first-match search finds the wrong end.
    ##
    ## An opener with no matching closer is literal text, so the backticks are
    ## emitted and scanning resumes after them.
    code_span : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    code_span = |ctx, i, buf, out| {
        n = Scan.run_len(ctx.b, i, 96, 0)
        match Scan.find_run(ctx.b, i + n, 96, n) {
            Ok(close) => {
                raw = Scan.slice(ctx.b, i + n, close)
                Scan.tokens(ctx, close + n, [], List.append(Scan.flush(buf, out), Node(Code(Scan.strip_code(raw)))))
            }
            Err(_) => Scan.tokens(ctx, i + n, List.concat(buf, List.repeat(96, n)), out)
        }
    }

    ## The first run of exactly `n` of byte `c` at or after `from`.
    find_run : List(U8), U64, U8, U64 -> Try(U64, [NotFound])
    find_run = |b, from, c, n|
        if from >= List.len(b) {
            Err(NotFound)
        } else if Scan.at(b, from) == c {
            len = Scan.run_len(b, from, c, 0)
            if len == n {
                Ok(from)
            } else {
                Scan.find_run(b, from + len, c, n)
            }
        } else {
            Scan.find_run(b, from + 1, c, n)
        }

    ## Line endings inside a code span become spaces, and one leading AND
    ## trailing space is stripped when the content has both and is not all
    ## spaces — the rule that lets `` ` `` `` hold a literal backtick.
    strip_code : Str -> Str
    strip_code = |raw| {
        spaced = Str.join_with(raw.split_on("\n"), " ")
        b = Str.to_utf8(spaced)
        n = List.len(b)
        if n >= 2 and Scan.at(b, 0) == 32 and Scan.at(b, n - 1) == 32 and !Scan.all_spaces(b) {
            Scan.slice(b, 1, n - 1)
        } else {
            spaced
        }
    }

    all_spaces : List(U8) -> Bool
    all_spaces = |b| List.len(List.keep_if(b, |c| c == 32)) == List.len(b)

    ## Two or more trailing spaces before a newline is a hard break; otherwise
    ## the newline is soft. Trailing spaces are dropped either way, so a line
    ## ending never contributes text.
    newline : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    newline = |ctx, i, buf, out| {
        trimmed = Scan.drop_trailing_spaces(buf)
        hard = List.len(buf) - List.len(trimmed) >= 2
        node = if hard {
            Node(HardBreak)
        } else {
            Node(SoftBreak)
        }
        Scan.tokens(ctx, i + 1, [], List.append(Scan.flush(trimmed, out), node))
    }

    drop_trailing_spaces : List(U8) -> List(U8)
    drop_trailing_spaces = |buf|
        match List.last(buf) {
            Ok(32) => Scan.drop_trailing_spaces(List.drop_last(buf, 1))
            _ => buf
        }

    ## Whether a delimiter run can open, close, both or neither.
    ##
    ## Left-flanking means "not followed by whitespace, and either not followed
    ## by punctuation or preceded by whitespace or punctuation". `_` additionally
    ## may not open inside a word, which is the rule that keeps `snake_case`
    ## intact — the reason the previous parser declined underscore emphasis
    ## outright.
    flanking : List(U8), U64, U64, U8 -> { opener : Bool, closer : Bool }
    flanking = |b, i, n, ch| {
        ## `i - 1` is a U64 subtraction, so start-of-text has to be answered
        ## before it is computed rather than after — the reason this is not a
        ## single expression.
        before = if i == 0 {
            32
        } else {
            Scan.at(b, i - 1)
        }
        after = Scan.at(b, i + n)
        ## U+00A0 is whitespace for flanking, and it is two bytes — 0xC2 0xA0 —
        ## so a byte-wise `is_space` cannot see it. The spec's rules are defined
        ## over Unicode categories; this covers the one that appears in prose.
        before_nb = before == 160 and Scan.at(b, i - 2) == 194 and i >= 2
        after_nb = Scan.at(b, i + n) == 194 and Scan.at(b, i + n + 1) == 160
        before_ws = Scan.is_space(before) or i == 0 or before_nb
        after_ws = Scan.is_space(after) or i + n >= List.len(b) or after_nb
        ## "Punctuation" here is the spec's, which since 0.31 means Unicode
        ## categories P* AND S* — so `£` and `€` flank like `$` does, and
        ## `*£*bravo` is not emphasis.
        before_p = Scan.is_punct(before) or Scan.is_uni_punct(Scan.cp_before(b, i))
        after_p = Scan.is_punct(after) or Scan.is_uni_punct(Scan.cp_after(b, i + n))
        left = !after_ws and (!after_p or before_ws or before_p)
        right = !before_ws and (!before_p or after_ws or after_p)
        if ch == 95 {
            { opener: left and (!right or before_p), closer: right and (!left or after_p) }
        } else {
            { opener: left, closer: right }
        }
    }

    ## --- stage 2: emphasis ---------------------------------------------------
    ##
    ## Find the first closer, then the NEAREST opener before it. Nearest rather
    ## than first is what makes `*a **b** c*` nest correctly; taking the first
    ## opener would pair the outer `*` with the inner `**`.
    ##
    ## Any delimiter left unmatched when the scan runs out is literal text, which
    ## is why `leftover` exists rather than a silent drop: a stray `*` is a `*`.
    emphasis : List(Scan.Tk) -> List(Inl)
    emphasis = |tks|
        match Scan.first_closer(tks, 0) {
            Ok(ci) =>
                match Scan.opener_before(tks, ci) {
                    Ok(oi) => Scan.emphasis(Scan.join(tks, oi, ci))
                    Err(_) => Scan.emphasis(Scan.demote(tks, ci))
                }
            Err(_) => List.join(List.map(tks, Scan.leftover))
        }

    first_closer : List(Scan.Tk), U64 -> Try(U64, [NotFound])
    first_closer = |tks, i|
        match List.get(tks, i) {
            Ok(Delim(d)) =>
                if d.closer {
                    Ok(i)
                } else {
                    Scan.first_closer(tks, i + 1)
                }
            Ok(_) => Scan.first_closer(tks, i + 1)
            Err(_) => Err(NotFound)
        }

    ## The nearest opener before `ci` of the same character. The "rule of three"
    ## is applied: when either side can both open and close, a pairing whose
    ## combined length is a multiple of 3 is rejected unless both lengths are.
    ## Without it `**foo*bar**` mis-nests.
    opener_before : List(Scan.Tk), U64 -> Try(U64, [NotFound])
    opener_before = |tks, ci| Scan.opener_walk(tks, ci, ci)

    opener_walk : List(Scan.Tk), U64, U64 -> Try(U64, [NotFound])
    opener_walk = |tks, ci, j|
        if j == 0 {
            Err(NotFound)
        } else {
            k = j - 1
            match List.get(tks, k) {
                Ok(Delim(o)) =>
                    match List.get(tks, ci) {
                        Ok(Delim(c)) =>
                            if o.opener and o.ch == c.ch and Scan.rule_of_three(o, c) {
                                Ok(k)
                            } else {
                                Scan.opener_walk(tks, ci, k)
                            }
                        _ => Scan.opener_walk(tks, ci, k)
                    }
                _ => Scan.opener_walk(tks, ci, k)
            }
        }

    rule_of_three : { ch : U8, n : U64, opener : Bool, closer : Bool, both : Bool }, { ch : U8, n : U64, opener : Bool, closer : Bool, both : Bool } -> Bool
    ## `both` is the ORIGINAL flanking, captured at tokenize time. Reading the
    ## live `closer`/`opener` flags here was a real bug: demoting an unmatched
    ## closer also made it rule-of-three-eligible, so `*foo**bar*` paired the
    ## inner run instead of the outer one.
    rule_of_three = |o, c|
        if (o.both or c.both) and (o.n + c.n) % 3 == 0 and !(o.n % 3 == 0 and c.n % 3 == 0) {
            False
        } else {
            True
        }

    ## Consume two delimiters from each side for a `<strong>`, one for an `<em>`,
    ## and wrap everything between. A run longer than what is consumed STAYS, so
    ## `***a***` becomes strong-inside-em across two passes rather than needing a
    ## special case.
    join : List(Scan.Tk), U64, U64 -> List(Scan.Tk)
    join = |tks, oi, ci|
        match List.get(tks, oi) {
            Ok(Delim(o)) =>
                match List.get(tks, ci) {
                    Ok(Delim(c)) => {
                        take = if o.n >= 2 and c.n >= 2 {
                            2
                        } else {
                            1
                        }
                        inner = Scan.emphasis(List.sublist(tks, { start: oi + 1, len: ci - oi - 1 }))
                        node = if c.ch == 126 {
                            Strike(inner)
                        } else if take == 2 {
                            Strong(inner)
                        } else {
                            Emph(inner)
                        }
                        before = List.sublist(tks, { start: 0, len: oi })
                        after = List.sublist(tks, { start: ci + 1, len: List.len(tks) - ci - 1 })
                        mid = List.concat(Scan.keep(o, o.n - take), [Node(node)])
                        tail = List.concat(Scan.keep(c, c.n - take), after)
                        List.concat(before, List.concat(mid, tail))
                    }
                    _ => tks
                }
            _ => tks
        }

    ## What is left of a partially consumed run.
    keep : { ch : U8, n : U64, opener : Bool, closer : Bool, both : Bool }, U64 -> List(Scan.Tk)
    keep = |d, n|
        if n == 0 {
            []
        } else {
            [Delim({ ..d, n })]
        }

    ## A closer with no opener cannot stay a closer, or the scan loops on it.
    demote : List(Scan.Tk), U64 -> List(Scan.Tk)
    demote = |tks, i|
        match List.get(tks, i) {
            Ok(Delim(d)) =>
                match List.set(tks, i, Delim({ ..d, closer: False })) {
                    Ok(t) => t
                    Err(_) => tks
                }
            _ => tks
        }

    ## --- links, images, autolinks --------------------------------------------

    ## `[label](dest "title")` and its `!` image form.
    ##
    ## The label is found by BALANCED bracket counting, not by the first `]`, so
    ## `[a [ctx.b] c](u)` takes the whole label. Anything not well-formed falls back
    ## to a literal `[`, which is what makes an unmatched bracket ordinary text
    ## instead of swallowing the rest of the line.
    ##
    ## `skip` is 1 for a link and 2 for an image, so the two share every rule
    ## below except which node they build and whether the label survives as
    ## markup or as alt text.
    bracket : Scan.Ctx, U64, U64, List(U8), List(Scan.Tk), Bool -> List(Scan.Tk)
    bracket = |ctx, i, skip, buf, out, image|
        match Scan.match_bracket(ctx.b, i + skip, 1) {
            Ok(close) =>
                if Scan.at(ctx.b, close + 1) == 91 {
                    ## `[text][label]` and `[label][]` — a REFERENCE. An empty
                    ## second bracket means "use the first as the label", which
                    ## is why this cannot just read the second one.
                    match Scan.match_bracket(ctx.b, close + 2, 1) {
                        Ok(rclose) => {
                            written = Scan.slice(ctx.b, close + 2, rclose)
                            key = if Str.is_empty(written) {
                                Scan.slice(ctx.b, i + skip, close)
                            } else {
                                written
                            }
                            Scan.resolve(ctx, i, skip, close, rclose + 1, key, buf, out, image)
                        }
                        Err(_) => Scan.literal(ctx, i, skip, buf, out)
                    }
                } else if Scan.at(ctx.b, close + 1) != 40 {
                    ## `[label]` alone — a SHORTCUT reference. Falls back to
                    ## literal text when no definition matches, which is what
                    ## keeps `[not a link]` readable.
                    Scan.resolve(ctx, i, skip, close, close + 1, Scan.slice(ctx.b, i + skip, close), buf, out, image)
                } else if Scan.at(ctx.b, close + 1) == 40 {
                    match Scan.match_paren(ctx.b, close + 2, 1) {
                        Ok(pclose) => {
                            label = Scan.run(Scan.slice(ctx.b, i + skip, close), ctx.defs)
                            parts = Scan.dest_title(Scan.slice(ctx.b, close + 2, pclose))
                            ## A link may not contain a link, so the outer
                            ## bracket re-scans as literal text — which keeps
                            ## the brackets AND the inner link, where emitting
                            ## the label alone dropped both.
                            if parts.ok and (image or !Scan.has_link(label)) {
                                nodes = Scan.link_node(label, parts, image)
                                Scan.tokens(ctx, pclose + 1, [], List.concat(Scan.flush(buf, out), nodes))
                            } else {
                                ## `[foo](not a link)` is a shortcut reference
                                ## followed by literal text, not text throughout
                                ## — so a bad inline destination retries as one
                                ## rather than giving up.
                                Scan.resolve(ctx, i, skip, close, close + 1, Scan.slice(ctx.b, i + skip, close), buf, out, image)
                            }
                        }
                        Err(_) => Scan.literal(ctx, i, skip, buf, out)
                    }
                } else {
                    Scan.literal(ctx, i, skip, buf, out)
                }
            Err(_) => Scan.literal(ctx, i, skip, buf, out)
        }

    ## Look the label up and build the node, or fall back to literal text.
    ##
    ## `resume` is where scanning continues — past the second bracket for a full
    ## reference, past the first for a shortcut — so one function serves both.
    resolve : Scan.Ctx, U64, U64, U64, U64, Str, List(U8), List(Scan.Tk), Bool -> List(Scan.Tk)
    resolve = |ctx, i, skip, close, resume, key, buf, out, image|
        match Scan.lookup(ctx.defs, Scan.fold_label(key)) {
            Ok(d) => {
                label = Scan.run(Scan.slice(ctx.b, i + skip, close), ctx.defs)
                if !image and Scan.has_link(label) {
                    Scan.literal(ctx, i, skip, buf, out)
                } else {
                    nodes = Scan.link_node(label, { dest: d.dest, title: d.title, ok: True }, image)
                    Scan.tokens(ctx, resume, [], List.concat(Scan.flush(buf, out), nodes))
                }
            }
            Err(_) => Scan.literal(ctx, i, skip, buf, out)
        }

    lookup : List(Ref.Def), Str -> Try(Ref.Def, [NotFound])
    lookup = |defs, key|
        match List.find_first(defs, |d| d.label == key) {
            Ok(d) => Ok(d)
            Err(_) => Err(NotFound)
        }

    ## Labels match case-insensitively with runs of whitespace collapsed, so
    ## `[Foo  Bar]` and `[foo bar]` are one label.
    fold_label : Str -> Str
    ## NOT unescaped: a label matches by its literal text, so `[foo\\!]` and
    ## `[foo!]` are different labels. Unescaping here made them the same.
    fold_label = |k| {
        ## Every kind of whitespace collapses, not just spaces — a label may
        ## span lines, so `[Foo\n  bar]` and `[Foo bar]` are one label.
        flat = Str.from_utf8_lossy(
            List.map(Str.to_utf8(k), |c| if Scan.is_space(c) { 32 } else { c }),
        )
        collapsed = Str.join_with(List.keep_if(flat.trim().split_on(" "), |x| !Str.is_empty(x)), " ")
        Scan.fold_case(Str.with_ascii_lowercased(collapsed))
    }

    ## Case-folds the two-byte scripts that appear in labels: Latin-1
    ## supplement, Greek and Cyrillic. ASCII is already folded by the caller.
    ##
    ## Not the full Unicode table. This is the shape of the problem — a
    ## codepoint range mapped by a fixed offset — and these three ranges cover
    ## what a document actually writes. A label in a script outside them folds
    ## to itself, which fails to match rather than matching wrongly.
    fold_case : Str -> Str
    fold_case = |t|
        if Scan.is_ascii(Str.to_utf8(t), 0) {
            t
        } else {
            Str.from_utf8_lossy(Scan.fold_bytes(Str.to_utf8(t), 0, []))
        }

    is_ascii : List(U8), U64 -> Bool
    is_ascii = |b, i|
        if i >= List.len(b) {
            True
        } else if Scan.at(b, i) >= 128 {
            False
        } else {
            Scan.is_ascii(b, i + 1)
        }

    ## Two-byte sequences only: `0xC3 0x80..0x9E` (Latin-1 capitals),
    ## `0xCE 0x91..0xA9` (Greek), `0xD0 0x90..0xAF` (Cyrillic А-Я).
    fold_bytes : List(U8), U64, List(U8) -> List(U8)
    fold_bytes = |b, i, out|
        ## `ẞ` folds to TWO characters. One-to-many foldings are the reason a
        ## fold is not a per-character map; this is the one that appears in
        ## practice, and the rest of the table is left alone.
        if Scan.at(b, i) == 225 and Scan.at(b, i + 1) == 186 and Scan.at(b, i + 2) == 158 {
            Scan.fold_bytes(b, i + 3, List.concat(out, [115, 115]))
        } else
        match List.get(b, i) {
            Ok(195) => {
                c = Scan.at(b, i + 1)
                if c >= 128 and c <= 158 and c != 151 {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [195, c + 32]))
                } else {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [195, c]))
                }
            }
            Ok(206) => {
                c = Scan.at(b, i + 1)
                if c >= 145 and c <= 169 {
                    ## Greek capitals run past the two-byte lead: Α-Ο fold
                    ## within 0xCE, Π-Ω carry into 0xCF.
                    if c <= 159 {
                        Scan.fold_bytes(b, i + 2, List.concat(out, [206, c + 32]))
                    } else {
                        Scan.fold_bytes(b, i + 2, List.concat(out, [207, c - 32]))
                    }
                } else {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [206, c]))
                }
            }
            Ok(208) => {
                c = Scan.at(b, i + 1)
                if c >= 144 and c <= 159 {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [208, c + 32]))
                } else if c >= 160 and c <= 175 {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [209, c - 32]))
                } else {
                    Scan.fold_bytes(b, i + 2, List.concat(out, [208, c]))
                }
            }
            Ok(c) => Scan.fold_bytes(b, i + 1, List.append(out, c))
            Err(_) => out
        }

    ## A link may not contain another link at any depth, so an outer bracket
    ## whose label already produced one is not a link — it is literal text with
    ## the inner link intact. Images are exempt: their label is alt text.
    has_link : List(Inl) -> Bool
    has_link = |ns| List.any(ns, Scan.one_has_link)

    one_has_link : Inl -> Bool
    one_has_link = |n|
        match n {
            Link(_) => True
            Emph(ks) => Scan.has_link(ks)
            Strong(ks) => Scan.has_link(ks)
            Strike(ks) => Scan.has_link(ks)
            Image(m) => Scan.has_link(m.kids)
            _ => False
        }

    link_node : List(Inl), { dest : Str, title : Str, ok : Bool }, Bool -> List(Scan.Tk)
    link_node = |label, parts, image|
        if image {
            [Node(Image({ src: parts.dest, title: parts.title, kids: label }))]
        } else if Scan.safe_href(parts.dest) {
            [Node(Link({ href: parts.dest, title: parts.title, kids: label }))]
        } else {
            ## The label survives as ordinary content — spliced, not wrapped, so
            ## a rejected href leaves no trace in the tree.
            List.map(label, |n| Node(n))
        }

    literal : Scan.Ctx, U64, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    literal = |ctx, i, skip, buf, out|
        Scan.tokens(ctx, i + skip, List.concat(buf, List.sublist(ctx.b, { start: i, len: skip })), out)

    match_bracket : List(U8), U64, U64 -> Try(U64, [NotFound])
    match_bracket = |b, i, depth|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Scan.at(b, i)
            if c == 92 {
                Scan.match_bracket(b, i + 2, depth)
            } else if c == 96 {
                ## A code span outranks a bracket, so its contents are opaque —
                ## `[foo`](/uri)`` is a bracket followed by code, not a link.
                n = Scan.run_len(b, i, 96, 0)
                match Scan.find_run(b, i + n, 96, n) {
                    Ok(close) => Scan.match_bracket(b, close + n, depth)
                    Err(_) => Scan.match_bracket(b, i + n, depth)
                }
            } else if c == 60 {
                ## A raw tag or an autolink is opaque to bracket matching, so
                ## `[foo <bar attr="](baz)">` does not close at the `]` sitting
                ## inside an attribute value.
                match Scan.find_byte(b, i + 1, 62) {
                    Ok(gt) => {
                        inner = Scan.slice(b, i + 1, gt)
                        if Scan.is_tag(inner) or Scan.is_autolink(inner) {
                            Scan.match_bracket(b, gt + 1, depth)
                        } else {
                            Scan.match_bracket(b, i + 1, depth)
                        }
                    }
                    Err(_) => Scan.match_bracket(b, i + 1, depth)
                }
            } else if c == 91 {
                Scan.match_bracket(b, i + 1, depth + 1)
            } else if c == 93 {
                if depth == 1 {
                    Ok(i)
                } else {
                    Scan.match_bracket(b, i + 1, depth - 1)
                }
            } else {
                Scan.match_bracket(b, i + 1, depth)
            }
        }

    match_paren : List(U8), U64, U64 -> Try(U64, [NotFound])
    match_paren = |b, i, depth|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Scan.at(b, i)
            if c == 92 {
                Scan.match_paren(b, i + 2, depth)
            } else if c == 60 {
                ## An angle-bracketed destination is opaque: `(<b)c>)` closes at
                ## the LAST paren, not the one inside the brackets.
                match Scan.find_byte(b, i + 1, 62) {
                    Ok(gt) => Scan.match_paren(b, gt + 1, depth)
                    Err(_) => Scan.match_paren(b, i + 1, depth)
                }
            } else if c == 40 {
                Scan.match_paren(b, i + 1, depth + 1)
            } else if c == 41 {
                if depth == 1 {
                    Ok(i)
                } else {
                    Scan.match_paren(b, i + 1, depth - 1)
                }
            } else {
                Scan.match_paren(b, i + 1, depth)
            }
        }

    ## `dest "title"` — the title is optional and quoted; everything before the
    ## first space is the destination. `<dest>` sheds its angle brackets.
    ## `dest "title"`, with `ok` saying whether it is a destination at all.
    ##
    ## A BARE destination may not contain a space — `[link](/my uri)` is not a
    ## link, it is text — while an angle-bracketed one may, and may be empty.
    ## Splitting on the first space before deciding got both of those wrong.
    dest_title : Str -> { dest : Str, title : Str, ok : Bool }
    dest_title = |raw| {
        t = raw.trim()
        if t.starts_with("<") {
            tb = Str.to_utf8(t)
            ## The closing `>` must be UNESCAPED — `<foo\\>` never closes, so
            ## `[link](<foo\\>)` is text rather than a link.
            match Scan.find_unescaped(tb, 1, 62) {
                Ok(gt) => {
                    inner = Scan.slice(tb, 1, gt)
                    {
                        dest: Scan.unescape(Ents.decode(inner)),
                        title: Scan.unescape(Ents.decode(Scan.unquote(Scan.slice(tb, gt + 1, List.len(tb)).trim()))),
                        ## Whatever follows the closing `>` must be a title or
                        ## nothing — `[a](<b>c)` is text, not a link to `b`
                        ## titled `c`.
                        ok: !inner.contains("<")
                        and !inner.contains("\n")
                        and (
                            Str.is_empty(Scan.slice(tb, gt + 1, List.len(tb)).trim())
                            or Scan.is_quoted(Scan.slice(tb, gt + 1, List.len(tb)).trim())
                        ),
                    }
                }
                Err(_) => { dest: "", title: "", ok: False }
            }
        } else {
            match Scan.split_ws(t) {
                Ok(p) => {
                    rest = p.after.trim()
                    {
                        dest: Scan.unescape(Ents.decode(p.before)),
                        title: Scan.unescape(Ents.decode(Scan.unquote(rest))),
                        ## Whatever follows the destination must be a quoted
                        ## title; anything else means the space was inside a
                        ## destination that is not allowed to hold one.
                        ok: Scan.is_quoted(rest),
                    }
                }
                Err(_) => { dest: Scan.unescape(Ents.decode(t)), title: "", ok: !t.contains("\n") }
            }
        }
    }

    ## `\(` in a destination is a literal paren. The inline scanner does this
    ## for ordinary text, but a destination is sliced out whole and never
    ## passes through it.
    unescape : Str -> Str
    unescape = |t|
        if t.contains("\\") {
            Str.from_utf8_lossy(Scan.unesc(Str.to_utf8(t), 0, []))
        } else {
            t
        }

    unesc : List(U8), U64, List(U8) -> List(U8)
    unesc = |b, i, out|
        match List.get(b, i) {
            Ok(92) =>
                if Scan.is_punct(Scan.at(b, i + 1)) and i + 1 < List.len(b) {
                    Scan.unesc(b, i + 2, List.append(out, Scan.at(b, i + 1)))
                } else {
                    Scan.unesc(b, i + 1, List.append(out, 92))
                }
            Ok(c) => Scan.unesc(b, i + 1, List.append(out, c))
            Err(_) => out
        }

    ## A title is `"…"`, `'…'` or `(…)`, and the closing delimiter may not
    ## appear inside it — `"title "and" title"` is not a title, which is what
    ## makes that whole line ordinary text rather than a link.
    ## An ESCAPED delimiter does not close a title, so the interior test cannot
    ## be a plain `contains`.
    has_bare : List(U8), U64, U64, U8 -> Bool
    has_bare = |b, i, stop, c|
        if i >= stop {
            False
        } else if Scan.at(b, i) == 92 {
            Scan.has_bare(b, i + 2, stop, c)
        } else if Scan.at(b, i) == c {
            True
        } else {
            Scan.has_bare(b, i + 1, stop, c)
        }

    find_unescaped : List(U8), U64, U8 -> Try(U64, [NotFound])
    find_unescaped = |b, i, c|
        if i >= List.len(b) {
            Err(NotFound)
        } else if Scan.at(b, i) == 92 {
            Scan.find_unescaped(b, i + 2, c)
        } else if Scan.at(b, i) == c {
            Ok(i)
        } else {
            Scan.find_unescaped(b, i + 1, c)
        }

    ## Split at the first WHITESPACE, not the first space. A destination may be
    ## followed by a newline before its title.
    split_ws : Str -> Try({ before : Str, after : Str }, [NotFound])
    split_ws = |t| {
        b = Str.to_utf8(t)
        i = Scan.ws_at(b, 0)
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            Ok({ before: Scan.slice(b, 0, i), after: Scan.slice(b, i + 1, List.len(b)) })
        }
    }

    ws_at : List(U8), U64 -> U64
    ws_at = |b, i|
        if i >= List.len(b) or Scan.is_space(Scan.at(b, i)) {
            i
        } else {
            Scan.ws_at(b, i + 1)
        }

    is_quoted : Str -> Bool
    is_quoted = |t| {
        b = Str.to_utf8(t)
        n = List.len(b)
        if n < 2 {
            False
        } else {
            open = Scan.at(b, 0)
            close = Scan.at(b, n - 1)
            pair =
                (open == 34 and close == 34)
                or (open == 39 and close == 39)
                or (open == 40 and close == 41)
            ## An ESCAPED delimiter does not close the title, so this cannot
            ## be a plain `contains` — `"title \\"…"` is one title.
            pair and !Scan.has_bare(b, 1, n - 1, close)
        }
    }

    unangle : Str -> Str
    unangle = |d|
        if d.starts_with("<") and d.ends_with(">") {
            Str.drop_suffix(d.drop_prefix("<"), ">")
        } else {
            d
        }

    ## The length test is not redundant: a lone `"` both starts and ends with a
    ## quote, and stripping one character off each end of a one-character string
    ## underflows.
    unquote : Str -> Str
    unquote = |t|
        if Scan.is_quoted(t) {
            Scan.slice(Str.to_utf8(t), 1, List.len(Str.to_utf8(t)) - 1)
        } else {
            t
        }

    ## `<https://x>` and `<a@ctx.b.c>` become links to themselves. Anything else in
    ## angle brackets is left literal — raw HTML is a separate decision and this
    ## package does not pass it through.
    angle : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    angle = |ctx, i, buf, out|
        if Scan.starts(ctx.b, i, "<![CDATA[") {
            ## CDATA ends at `]]>`, and its body may contain `>` — searching for
            ## the first one cut it in half.
            match Scan.find_cdata(ctx.b, i + 9) {
                Ok(close) =>
                    Scan.tokens(ctx, close + 3, [], List.append(Scan.flush(buf, out), Node(Raw(Scan.slice(ctx.b, i, close + 3)))))
                Err(_) => Scan.tokens(ctx, i + 1, List.append(buf, 60), out)
            }
        } else
        match Scan.find_tag_end(ctx.b, i + 1) {
            Ok(close) => {
                inner = Scan.slice(ctx.b, i + 1, close)
                if Scan.is_autolink(inner) and Scan.safe_href(inner) {
                    Scan.tokens(ctx, close + 1, [], List.append(Scan.flush(buf, out), Node(Link({ href: inner, title: "", kids: [Text(inner)] }))))
                } else if Scan.is_email(inner) {
                    Scan.tokens(ctx, close + 1, [], List.append(Scan.flush(buf, out), Node(Link({ href: "mailto:${inner}", title: "", kids: [Text(inner)] }))))
                } else if Scan.is_tag(inner) {
                    ## Raw inline HTML. It is carried, not dropped and not
                    ## escaped here — what happens to it is the LOWERING's
                    ## decision, and the two lowerings disagree: `to_html`
                    ## passes it through (or omits it, when safe), while a
                    ## native renderer has no HTML engine and shows it as text.
                    Scan.tokens(ctx, close + 1, [], List.append(Scan.flush(buf, out), Node(Raw("<${inner}>"))))
                } else {
                    Scan.tokens(ctx, i + 1, List.append(buf, 60), out)
                }
            }
            Err(_) => Scan.tokens(ctx, i + 1, List.append(buf, 60), out)
        }

    ## --- GFM extended autolinks -------------------------------------------
    ##
    ## `www.x.com` and `https://x.com` become links without angle brackets. Only
    ## at a WORD BOUNDARY, or the `www` inside `dowww.x` would start one.
    at_boundary : List(U8), U64 -> Bool
    at_boundary = |b, i|
        if i == 0 {
            True
        } else {
            p = Scan.at(b, i - 1)
            Scan.is_space(p) or p == 40 or p == 42 or p == 95 or p == 126
        }

    ## The length of the link starting here, or 0. Trailing punctuation is
    ## excluded — a sentence ending "see https://x.com." does not link the stop
    ## — and so is an unbalanced `)`, so "(see https://x.com)" keeps its paren.
    bare_len : List(U8), U64 -> U64
    bare_len = |b, i| {
        pre = if Scan.starts(b, i, "https://") {
            8
        } else if Scan.starts(b, i, "http://") {
            7
        } else if Scan.starts(b, i, "ftp://") {
            6
        } else if Scan.starts(b, i, "www.") {
            4
        } else {
            0
        }
        if pre == 0 {
            0
        } else {
            raw = Scan.run_until_space(b, i + pre, 0)
            if raw == 0 {
                0
            } else {
                Scan.trim_tail(b, i, pre + raw)
            }
        }
    }

    starts : List(U8), U64, Str -> Bool
    starts = |b, i, p| Scan.slice(b, i, i + Str.count_utf8_bytes(p)) == p

    run_until_space : List(U8), U64, U64 -> U64
    run_until_space = |b, i, acc| {
        c = Scan.at(b, i + acc)
        if i + acc >= List.len(b) or Scan.is_space(c) or c == 60 {
            acc
        } else {
            Scan.run_until_space(b, i, acc + 1)
        }
    }

    trim_tail : List(U8), U64, U64 -> U64
    trim_tail = |b, i, n|
        if n == 0 {
            0
        } else {
            last = Scan.at(b, i + n - 1)
            ## The entity test comes FIRST: `;` is also trailing punctuation, so
            ## stripping it as punctuation destroyed the `&name;` this is
            ## looking for.
            if last == 59 and Scan.entity_tail(b, i, n) > 0 {
                Scan.trim_tail(b, i, n - Scan.entity_tail(b, i, n))
            } else if last == 46 or last == 44 or last == 58 or last == 59 or last == 33 or last == 63 or last == 34 or last == 39 {
                Scan.trim_tail(b, i, n - 1)
            } else if last == 41 and Scan.count_byte(b, i, n, 41) > Scan.count_byte(b, i, n, 40) {
                Scan.trim_tail(b, i, n - 1)

            } else {
                n
            }
        }

    ## The length of a trailing `&name;`, or 0.
    entity_tail : List(U8), U64, U64 -> U64
    entity_tail = |b, i, n| Scan.entity_back(b, i, n, 1)

    entity_back : List(U8), U64, U64, U64 -> U64
    entity_back = |b, i, n, acc|
        if acc >= n {
            0
        } else {
            c = Scan.at(b, i + n - acc - 1)
            if c == 38 {
                acc + 1
            } else if Scan.is_alpha(c) or (c >= 48 and c <= 57) {
                Scan.entity_back(b, i, n, acc + 1)
            } else {
                0
            }
        }

    count_byte : List(U8), U64, U64, U8 -> U64
    count_byte = |b, i, n, c|
        List.len(List.keep_if(List.sublist(b, { start: i, len: n }), |x| x == c))

    bare_link : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    bare_link = |ctx, i, buf, out| {
        n = Scan.bare_len(ctx.b, i)
        text = Scan.slice(ctx.b, i, i + n)
        href = if Scan.starts(ctx.b, i, "www.") {
            "http://${text}"
        } else {
            text
        }
        node = Node(Link({ href: href, title: "", kids: [Text(text)] }))
        Scan.tokens(ctx, i + n, [], List.append(Scan.flush(buf, out), node))
    }

    ## The length of an email starting here, or 0.
    ##
    ## Local part, `@`, then a domain carrying a dot. The domain may not end on
    ## a hyphen or underscore, which is what keeps the trailing punctuation of
    ## `a.b-c_d@a.b-` out of the link.
    email_len : List(U8), U64 -> U64
    email_len = |b, i| {
        local = Scan.local_len(b, i, 0)
        if local == 0 or Scan.at(b, i + local) != 64 {
            0
        } else {
            dom = Scan.domain_len(b, i + local + 1, 0)
            last = Scan.at(b, i + local + dom)
            if dom == 0
            or !Scan.has_alnum(b, i, i + local)
            or !Scan.slice(b, i + local + 1, i + local + 1 + dom).contains(".")
            or last == 45
            or last == 95 {
                0
            } else {
                local + 1 + dom
            }
        }
    }

    has_alnum : List(U8), U64, U64 -> Bool
    has_alnum = |b, from, to|
        if from >= to {
            False
        } else {
            c = Scan.at(b, from)
            if Scan.is_alpha(c) or (c >= 48 and c <= 57) {
                True
            } else {
                Scan.has_alnum(b, from + 1, to)
            }
        }

    local_len : List(U8), U64, U64 -> U64
    local_len = |b, i, acc| {
        c = Scan.at(b, i + acc)
        if Scan.is_alpha(c) or (c >= 48 and c <= 57) or c == 46 or c == 45 or c == 95 or c == 43 {
            Scan.local_len(b, i, acc + 1)
        } else {
            acc
        }
    }

    domain_len : List(U8), U64, U64 -> U64
    domain_len = |b, i, acc| {
        c = Scan.at(b, i + acc)
        if i + acc < List.len(b) and (Scan.is_alpha(c) or (c >= 48 and c <= 57) or c == 45 or c == 46 or c == 95) {
            Scan.domain_len(b, i, acc + 1)
        } else if acc > 0 and Scan.at(b, i + acc - 1) == 46 {
            acc - 1
        } else {
            acc
        }
    }

    ## `&name;`, `&#35;`, `&#x22;`. An unknown name is a literal `&`, which is
    ## what keeps `AT&T` and `a &lt ctx.b` readable.
    ##
    ## The decoded text joins `buf` rather than becoming its own node, so
    ## `a&amp;ctx.b` stays one text run.
    entity : Scan.Ctx, U64, List(U8), List(Scan.Tk) -> List(Scan.Tk)
    entity = |ctx, i, buf, out|
        match Scan.find_byte(ctx.b, i + 1, 59) {
            Ok(close) =>
                match Ents.lookup(Scan.slice(ctx.b, i + 1, close)) {
                    Ok(ch) => Scan.tokens(ctx, close + 1, List.concat(buf, Str.to_utf8(ch)), out)
                    Err(_) => Scan.tokens(ctx, i + 1, List.append(buf, 38), out)
                }
            Err(_) => Scan.tokens(ctx, i + 1, List.append(buf, 38), out)
        }

    ## An autolink is an ABSOLUTE URI — a scheme, then a colon — never a
    ## relative one. `safe_href` alone is not the test: it admits scheme-less
    ## relatives so that `[x](train.jpg)` works, which made `<a>` an autolink to
    ## a file called `a` instead of a raw HTML tag.
    is_autolink : Str -> Bool
    is_autolink = |inner| {
        b = Str.to_utf8(inner)
        n = Scan.scheme_len(b, 0)
        n >= 2 and Scan.at(b, n) == 58 and !inner.contains(" ") and !inner.contains("<")
    }

    scheme_len : List(U8), U64 -> U64
    scheme_len = |b, i| {
        c = Scan.at(b, i)
        if Scan.is_alpha(c) or (i > 0 and ((c >= 48 and c <= 57) or c == 43 or c == 45 or c == 46)) {
            Scan.scheme_len(b, i + 1)
        } else {
            i
        }
    }

    ## An open tag, a closing tag, or a declaration/instruction. Deliberately
    ## shape-based rather than a tag-name allowlist: the spec's rule is about
    ## syntax, and an unknown element is still raw HTML.
    ##
    ## `a < b` is not a tag because `is_tag` requires a letter or `/` straight
    ## after the `<` — which is also the rule that keeps arithmetic readable.
    ## A tag's ATTRIBUTES have a grammar, and without checking it
    ## `<a h*#ref="hi">` and `<a href="hi'>` were raw HTML rather than the text
    ## they are — and `<http://foo.bar/baz bim>` was a tag named `http`.
    attrs_ok : List(U8), U64 -> Bool
    attrs_ok = |b, i|
        if i >= List.len(b) {
            True
        } else {
            c = Scan.at(b, i)
            if c == 47 or c == 62 {
                i + 1 >= List.len(b) or Scan.at(b, i + 1) == 62
            } else if Scan.is_space(c) {
                Scan.attrs_ok(b, i + 1)
            } else if Scan.is_alpha(c) or c == 95 or c == 58 {
                n = Scan.attr_name_len(b, i, 0)
                ## Whitespace is allowed on BOTH sides of `=`, so the name is
                ## followed by an optional gap before it — `bam = 'x'` is one
                ## attribute, not a boolean `bam` followed by junk.
                eq = Scan.skip_spaces(b, i + n)
                if Scan.at(b, eq) == 61 {
                    Scan.attr_value(b, Scan.skip_spaces(b, eq + 1))
                } else {
                    Scan.attrs_ok(b, i + n)
                }
            } else {
                False
            }
        }

    tag_name_len : List(U8), U64 -> U64
    tag_name_len = |b, i| {
        c = Scan.at(b, i)
        if Scan.is_alpha(c) or (i > 0 and ((c >= 48 and c <= 57) or c == 45)) {
            Scan.tag_name_len(b, i + 1)
        } else {
            i
        }
    }

    skip_spaces : List(U8), U64 -> U64
    skip_spaces = |b, i|
        if i < List.len(b) and Scan.is_space(Scan.at(b, i)) {
            Scan.skip_spaces(b, i + 1)
        } else {
            i
        }

    attr_name_len : List(U8), U64, U64 -> U64
    attr_name_len = |b, i, acc| {
        c = Scan.at(b, i + acc)
        if Scan.is_alpha(c) or (c >= 48 and c <= 57) or c == 95 or c == 58 or c == 45 or c == 46 {
            Scan.attr_name_len(b, i, acc + 1)
        } else {
            acc
        }
    }

    attr_value : List(U8), U64 -> Bool
    attr_value = |b, i| {
        c = Scan.at(b, i)
        if c == 34 or c == 39 {
            match Scan.find_byte(b, i + 1, c) {
                Ok(close) => {
                    ## Attributes are separated by WHITESPACE — without that,
                    ## `<a href='bar'title=title>` looked well-formed.
                    nxt = Scan.at(b, close + 1)
                    (close + 1 >= List.len(b) or Scan.is_space(nxt) or nxt == 62 or nxt == 47)
                    and Scan.attrs_ok(b, close + 1)
                }
                Err(_) => False
            }
        } else {
            n = Scan.unquoted_len(b, i, 0)
            n > 0 and Scan.attrs_ok(b, i + n)
        }
    }

    unquoted_len : List(U8), U64, U64 -> U64
    unquoted_len = |b, i, acc| {
        c = Scan.at(b, i + acc)
        if i + acc >= List.len(b) or Scan.is_space(c) or c == 62 or c == 34 or c == 39 or c == 61 or c == 60 or c == 96 {
            acc
        } else {
            Scan.unquoted_len(b, i, acc + 1)
        }
    }

    is_tag : Str -> Bool
    is_tag = |inner| {
        c = Scan.at(Str.to_utf8(inner), 0)
        n = List.len(Str.to_utf8(inner))
        ## No blanket `contains("<")` check: a QUOTED attribute value may hold
        ## one, as `bam = 'baz <em>"</em>'` does. `attrs_ok` walks the grammar
        ## and knows where a `<` is legal.
        if n == 0 {
            False
        } else if Scan.is_alpha(c) {
            b = Str.to_utf8(inner)
            ## A TAG name is narrower than an attribute name — letters, digits
            ## and hyphens only. Reusing the attribute charset let `m:abc` be a
            ## tag named `m:abc` instead of the text it is.
            name_len = Scan.tag_name_len(b, 0)
            after = Scan.at(b, name_len)
            ## A tag NAME ends at whitespace, `/` or `>` — nothing else may
            ## follow it directly. Without this `<m:abc>` was a tag named `m`
            ## rather than the text it is.
            (name_len >= List.len(b) or Scan.is_space(after) or after == 47 or after == 62)
            and Scan.attrs_ok(b, name_len)
        } else if c == 47 {
            ## A CLOSING tag takes no attributes — `</a href="foo">` is text.
            cb = Str.to_utf8(inner)
            close_len = Scan.tag_name_len(cb, 1)
            Scan.is_alpha(Scan.at(cb, 1))
            and Str.is_empty(Scan.slice(cb, close_len, List.len(cb)).trim())
        } else {
            c == 33 or c == 63
        }
    }

    is_alpha : U8 -> Bool
    is_alpha = |c| (c >= 65 and c <= 90) or (c >= 97 and c <= 122)

    ## The `>` that closes a tag, skipping quoted attribute values — one inside
    ## `bam = 'baz <em>"</em>'` closes nothing, and stopping at it cut the tag
    ## in half.
    find_tag_end : List(U8), U64 -> Try(U64, [NotFound])
    find_tag_end = |b, i|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Scan.at(b, i)
            if c == 34 or c == 39 {
                match Scan.find_byte(b, i + 1, c) {
                    Ok(close) => Scan.find_tag_end(b, close + 1)
                    Err(_) => Err(NotFound)
                }
            } else if c == 62 {
                Ok(i)
            } else {
                Scan.find_tag_end(b, i + 1)
            }
        }

    find_cdata : List(U8), U64 -> Try(U64, [NotFound])
    find_cdata = |b, i|
        if i + 2 >= List.len(b) {
            Err(NotFound)
        } else if Scan.at(b, i) == 93 and Scan.at(b, i + 1) == 93 and Scan.at(b, i + 2) == 62 {
            Ok(i)
        } else {
            Scan.find_cdata(b, i + 1)
        }

    find_byte : List(U8), U64, U8 -> Try(U64, [NotFound])
    find_byte = |b, i, c|
        if i >= List.len(b) {
            Err(NotFound)
        } else if Scan.at(b, i) == c {
            Ok(i)
        } else {
            Scan.find_byte(b, i + 1, c)
        }

    ## Deliberately crude: one `@`, a dot after it, no spaces. The spec gives a
    ## full grammar; this rejects nothing a document actually contains and keeps
    ## `a < b` from becoming a mail link.
    is_email : Str -> Bool
    is_email = |s|
        match s.split_first("@") {
            Ok(p) =>
                !Str.is_empty(p.before)
                and p.after.contains(".")
                and !s.contains(" ")
                and !s.contains("<")
                ## A backslash is not a legal address character, and admitting
                ## one turned `<foo\+@bar.example.com>` into a mail link.
                and !s.contains("\\")
            Err(_) => False
        }

    ## An allowlist, not a denylist: absolute http(s), site-relative, in-page
    ## fragment, and mailto. Everything else — `javascript:`, `data:`,
    ## `vbscript:`, and any scheme invented after this line was written —
    ## degrades to text without needing to be enumerated.
    ## The dangerous schemes, refused; everything else allowed.
    ##
    ## This was an allowlist, which is the safer instinct and the wrong rule
    ## here: it also refused `irc:`, `localhost:5001/foo` and every scheme
    ## invented after it was written, so real autolinks silently became text.
    ## cmark's safe mode names the four that matter, and that is the whole
    ## risk surface — a scheme cannot execute unless a browser executes it.
    ##
    ## `data:` is refused outright rather than allowing image subtypes: nothing
    ## here renders a data URL, so the exception would buy nothing.
    safe_href : Str -> Bool
    safe_href = |href| {
        low = Str.with_ascii_lowercased(href.trim())
        !low.starts_with("javascript:")
        and !low.starts_with("vbscript:")
        and !low.starts_with("data:")
        and !low.starts_with("file:")
    }

    leftover : Scan.Tk -> List(Inl)
    leftover = |tk|
        match tk {
            Node(n) => [n]
            Delim(d) => [Text(Str.from_utf8_lossy(List.repeat(d.ch, d.n)))]
        }
}
