## Emphasis: the delimiter runs `Scan.tokens` leaves undecided, and the pass
## that pairs them.
##
## Whether a `*`, `_` or `~` run can open, close, both or neither depends on
## the characters either side of it (`flanking`), which `Scan.tokens` records
## as it meets each run. Which runs pair up can only be decided once every run
## is known, so that is a second pass over the token list (`emphasis`), in the
## order the spec defines.
import Inl exposing [Inl]
import Bytes

Emph := [].{
    ## A token: either a finished node, or a run of emphasis characters whose
    ## role is still undecided.
    Tk : [
        Node(Inl),
        Delim(Emph.Run),
    ]

    ## A delimiter run, as `Tk.Delim` carries it.
    Run : { ch : U8, n : U64, opener : Bool, closer : Bool, both : Bool, at : U64 }

    ## What decides which openers a closer can take: its character, and the
    ## two inputs `rule_of_three` reads from it.
    Kind : { ch : U8, both : Bool, rem : U64 }

    Floor : { kind : Emph.Kind, at : U64 }

    ## An entry of the pairing stack: a token, and how many emphasis nodes deep
    ## it is — 0 for a delimiter or a scanned node, one more than its deepest
    ## child for a node `join` built.
    Slot : { tk : Emph.Tk, depth : U64 }

    ## How deeply emphasis may nest. A pairing that would go deeper leaves its
    ## closer as text, like one with no opener.
    ##
    ## Every walk of the inline tree recurses once per level, and a run of
    ## 20,000 `*` either side of a word nests 10,000 `Strong`s in linear time,
    ## which overflowed the stack as a SIGSEGV. Real text nests two or three.
    max_depth : U64
    max_depth = 100

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
            Bytes.at(b, i - 1)
        }
        after = Bytes.at(b, i + n)
        ## U+00A0 is whitespace for flanking, and it is two bytes — 0xC2 0xA0 —
        ## so a byte-wise `is_space` cannot see it. The spec's rules are defined
        ## over Unicode categories; this covers the one that appears in prose.
        before_nb = before == 160 and Bytes.at(b, i - 2) == 194 and i >= 2
        after_nb = Bytes.at(b, i + n) == 194 and Bytes.at(b, i + n + 1) == 160
        before_ws = Bytes.is_space(before) or i == 0 or before_nb
        after_ws = Bytes.is_space(after) or i + n >= List.len(b) or after_nb
        ## "Punctuation" here is the spec's, which since 0.31 means Unicode
        ## categories P* AND S* — so `£` and `€` flank like `$` does, and
        ## `*£*bravo` is not emphasis.
        before_p = Bytes.is_punct(before) or Emph.is_uni_punct(Emph.cp_before(b, i))
        after_p = Bytes.is_punct(after) or Emph.is_uni_punct(Emph.cp_after(b, i + n))
        left = !after_ws and (!after_p or before_ws or before_p)
        right = !before_ws and (!before_p or after_ws or after_p)
        if ch == 95 {
            { opener: left and (!right or before_p), closer: right and (!left or after_p) }
        } else {
            { opener: left, closer: right }
        }
    }

    ## The codepoint starting at `i`, or 0 if there is none.
    cp_after : List(U8), U64 -> U64
    cp_after = |b, i| {
        c = Bytes.at(b, i)
        if i >= List.len(b) or c < 128 {
            0
        } else if c < 224 {
            (c - 192).to_u64() * 64 + (Bytes.at(b, i + 1) - 128).to_u64()
        } else if c < 240 {
            (c - 224).to_u64() * 4096
            + (Bytes.at(b, i + 1) - 128).to_u64() * 64
            + (Bytes.at(b, i + 2) - 128).to_u64()
        } else {
            (c - 240).to_u64() * 262144
            + (Bytes.at(b, i + 1) - 128).to_u64() * 4096
            + (Bytes.at(b, i + 2) - 128).to_u64() * 64
            + (Bytes.at(b, i + 3) - 128).to_u64()
        }
    }

    ## The codepoint ENDING at `i - 1`: walk back over continuation bytes to
    ## the lead, then decode forward from it.
    cp_before : List(U8), U64 -> U64
    cp_before = |b, i|
        if i == 0 {
            0
        } else {
            Emph.cp_after(b, Emph.lead_of(b, i - 1))
        }

    lead_of : List(U8), U64 -> U64
    lead_of = |b, i| {
        c = Bytes.at(b, i)
        if i > 0 and c >= 128 and c < 192 {
            Emph.lead_of(b, i - 1)
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

    ## Take closers left to right; for each, the NEAREST opener before it.
    ## Nearest rather than first is what makes `*a **b** c*` nest correctly;
    ## taking the first opener would pair the outer `*` with the inner `**`.
    ##
    ## Any delimiter left unmatched when the scan runs out is literal text, which
    ## is why `leftover` exists rather than a silent drop: a stray `*` is a `*`.
    ##
    ## The tokens already passed sit on `stack`, and nothing on it is a closer:
    ## a closer either pairs, and the pair collapses into one node on the stack,
    ## or it is demoted where it stands. That is what keeps this linear. It used
    ## to find the first closer from token 0 after every pairing and rebuild the
    ## whole token list around each one, so `*a* *b* …` and a paragraph of
    ## unmatched `a* a* …` were both quadratic.
    emphasis : List(Emph.Tk) -> List(Inl)
    emphasis = |tks| Emph.emph_walk(tks, 0, [], [])

    slot : Emph.Tk -> Emph.Slot
    slot = |tk| { tk, depth: 0 }

    ## `floors` is cmark's openers-bottom: for each kind of closer
    ## (`Emph.Kind`), no stack entry below its floor is an opener that kind
    ## could take, because an earlier closer of the kind walked down there and
    ## found none. Whether an opener suits a closer depends only on the
    ## closer's kind, so the next closer of that kind stops at the floor
    ## instead of walking to the bottom again.
    emph_walk : List(Emph.Tk), U64, List(Emph.Slot), List(Emph.Floor) -> List(Inl)
    emph_walk = |tks, i, stack, floors|
        match List.get(tks, i) {
            Ok(Delim(d)) =>
                if d.closer {
                    Emph.close_with(tks, i, d, stack, floors)
                } else {
                    Emph.emph_walk(tks, i + 1, List.append(stack, Emph.slot(Delim(d))), floors)
                }
            Ok(tk) => Emph.emph_walk(tks, i + 1, List.append(stack, Emph.slot(tk)), floors)
            Err(_) => List.join(List.map(stack, |s| Emph.leftover(s.tk)))
        }

    ## Pair closer `c` (token `i`) with the nearest opener it can take, or
    ## demote it. A closer longer than what one pairing consumes is paired
    ## again with what is left, so `***a***` becomes strong-inside-em.
    ##
    ## A pairing that would nest past `max_depth` is refused the same way, and
    ## the floor it sets is as true: every opener below `oi` would wrap the
    ## same too-deep node, and joins above `oi` only ever deepen it.
    close_with : List(Emph.Tk), U64, Emph.Run, List(Emph.Slot), List(Emph.Floor) -> List(Inl)
    close_with = |tks, i, c, stack, floors| {
        kind = Emph.kind_of(c)
        match Emph.opener_walk(stack, c, List.len(stack), Emph.floor_of(floors, kind)) {
            Ok(oi) => {
                depth = Emph.depth_above(stack, oi) + 1
                if depth > Emph.max_depth {
                    Emph.demote(tks, i, c, stack, floors)
                } else {
                    joined = Emph.join(stack, oi, c, depth)
                    ## The stack from `oi` up was rebuilt, so a floor above it no
                    ## longer indexes the entry it did. Clamping keeps each claim
                    ## true: nothing below `oi` moved.
                    clamped = Emph.clamp_floors(floors, oi)
                    if joined.rest.n > 0 {
                        Emph.close_with(tks, i, joined.rest, joined.stack, clamped)
                    } else {
                        Emph.emph_walk(tks, i + 1, joined.stack, clamped)
                    }
                }
            }
            Err(_) => Emph.demote(tks, i, c, stack, floors)
        }
    }

    ## A closer with no opener it may take cannot stay a closer. It keeps its
    ## opener flag: a run that can both open and close still opens.
    demote : List(Emph.Tk), U64, Emph.Run, List(Emph.Slot), List(Emph.Floor) -> List(Inl)
    demote = |tks, i, c, stack, floors|
        Emph.emph_walk(
            tks,
            i + 1,
            List.append(stack, Emph.slot(Delim({ ..c, closer: False }))),
            Emph.set_floor(floors, Emph.kind_of(c), List.len(stack)),
        )

    ## The deepest nesting among the stack entries above `oi` — what a node
    ## wrapping them would sit on.
    depth_above : List(Emph.Slot), U64 -> U64
    depth_above = |stack, oi| List.fold(List.drop_first(stack, oi + 1), 0, |m, s| if s.depth > m { s.depth } else { m })

    kind_of : Emph.Run -> Emph.Kind
    kind_of = |d| { ch: d.ch, both: d.both, rem: d.n % 3 }

    floor_of : List(Emph.Floor), Emph.Kind -> U64
    floor_of = |floors, kind|
        match List.find_first(floors, |f| f.kind == kind) {
            Ok(f) => f.at
            Err(_) => 0
        }

    set_floor : List(Emph.Floor), Emph.Kind, U64 -> List(Emph.Floor)
    set_floor = |floors, kind, pos| List.append(List.drop_if(floors, |f| f.kind == kind), { kind, at: pos })

    clamp_floors : List(Emph.Floor), U64 -> List(Emph.Floor)
    clamp_floors = |floors, top| List.map(floors, |f| if f.at > top { { ..f, at: top } } else { f })

    ## The nearest opener below `j` and at or above `floor` of the same
    ## character. The "rule of three" is applied: when either side can both
    ## open and close, a pairing whose combined length is a multiple of 3 is
    ## rejected unless both lengths are. Without it `**foo*bar**` mis-nests.
    opener_walk : List(Emph.Slot), Emph.Run, U64, U64 -> Try(U64, [NotFound])
    opener_walk = |stack, c, j, floor|
        if j <= floor {
            Err(NotFound)
        } else {
            k = j - 1
            match List.get(stack, k).map_ok(|s| s.tk) {
                Ok(Delim(o)) =>
                    if o.opener and o.ch == c.ch and Emph.rule_of_three(o, c) {
                        Ok(k)
                    } else {
                        Emph.opener_walk(stack, c, k, floor)
                    }
                _ => Emph.opener_walk(stack, c, k, floor)
            }
        }

    rule_of_three : Emph.Run, Emph.Run -> Bool
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
    ## and wrap everything above the opener. What is left of the opener stays
    ## on the stack beneath the new node; what is left of the closer (`rest`,
    ## `n` 0 when nothing is) goes back to `close_with`.
    ##
    ## The stack is cut with `drop_last` after the inner nodes are copied out,
    ## so a unique stack is shortened in place rather than rebuilt. `depth` is
    ## the new node's, which `close_with` has already checked.
    join : List(Emph.Slot), U64, Emph.Run, U64 -> { stack : List(Emph.Slot), rest : Emph.Run }
    join = |stack, oi, c, depth|
        match List.get(stack, oi).map_ok(|s| s.tk) {
            Ok(Delim(o)) => {
                take = if o.n >= 2 and c.n >= 2 {
                    2
                } else {
                    1
                }
                inner = List.join(List.map(List.drop_first(stack, oi + 1), |s| Emph.leftover(s.tk)))
                node = if c.ch == 126 {
                    Strike(inner)
                } else if take == 2 {
                    Strong(inner)
                } else {
                    Emph(inner)
                }
                base = List.drop_last(stack, List.len(stack) - oi)
                kept = List.concat(base, Emph.keep(o, o.n - take, o.at))
                emph = { tk: Node(Inl.at(o.at + (o.n - take), c.at + take, node)), depth }
                { stack: List.append(kept, emph), rest: { ..c, n: c.n - take, at: c.at + take } }
            }
            ## `opener_walk` only answers with a delimiter, so this is not
            ## reached; the closer is left unconsumed rather than looping.
            _ => { stack: List.append(stack, Emph.slot(Delim({ ..c, closer: False }))), rest: { ..c, n: 0 } }
        }

    ## What is left of a partially consumed run.
    keep : Emph.Run, U64, U64 -> List(Emph.Slot)
    keep = |d, n, pos|
        if n == 0 {
            []
        } else {
            [Emph.slot(Delim({ ..d, n, at: pos }))]
        }

    leftover : Emph.Tk -> List(Inl)
    leftover = |tk|
        match tk {
            Node(n) => [n]
            Delim(d) => [Inl.at(d.at, d.at + d.n, Text(Str.from_utf8_lossy(List.repeat(d.ch, d.n))))]
        }
}
