## The inline scanner: one left-to-right pass over bytes, then emphasis.
##
## Two stages, in the order the spec defines them:
##
##  1. `tokens` — a single pass emitting finished nodes for everything whose
##     extent is decidable on sight (escapes, code spans, autolinks, raw HTML,
##     breaks, links) and DELIMITER RUNS for `*`, `_` and `~`, which are not.
##  2. `Emph.emphasis` — the delimiter-matching pass. Whether a `*` opens, closes,
##     both or neither depends on the characters either side of it, so it cannot
##     be decided during (1).
import Ref
import Inl exposing [Inl]
import Ents
import Bytes
import Emph
import Tag
import Autolink
import Link

Scan := [].{
    ## The byte helpers (`Bytes`) deliberately take a plain `List(U8)` instead;
    ## see that module.
    ##
    ## Where a scan stops and how many labels deep it is are not here: they
    ## are `tokens`' own arguments, so that a label scan can be handed this
    ## context whole (see `bracket_at`).
    ##
    ## `schemes` is the href policy (`Link.allows`). It is asked where a link
    ## is BUILT, so a refused href becomes text here, in the tree, and no
    ## lowering has to remember to filter it.
    ##
    ## The rest is what makes an UNMATCHED opener cheap. Each search below runs
    ## to the end of the text when it fails, so a paragraph of `<<<<…` or
    ## `[[[[…` cost a search per opener — quadratic. The facts that answer those
    ## searches in advance are computed once per scan, as cmark does:
    ##
    ##  - `gt_end` / `rb_end`: one past the last `>` / `]`, or 0 when there is
    ##    none. A tag or a bracket opened at or beyond it cannot close.
    ##  - `tick_last`: for each backtick run length `n`, one past where the last
    ##    run of exactly `n` starts (index `n - 1`, 0 for none). A code span
    ##    opened at or beyond it cannot close.
    ##  - `gts`: every `>`, ascending, for the `<` jump in an inline link's
    ##    destination (`Link.match_paren`), which finds its `>` by binary
    ##    search. Built only when the text has both a `(` and a `<`, since a
    ##    destination needs the one and the jump the other; empty otherwise.
    ##  - `lasts`: where the last `-->`, `?>` and `]]>` start (`Tag.Lasts`). A
    ##    comment, instruction or CDATA section opened at or beyond one cannot
    ##    close.
    ##
    ## A closer that DOES exist is no help to those, so the rest are learned
    ## during the scan, from searches that have already run:
    ##
    ##  - `email_dead`: a position below which every word boundary is known not
    ##    to start an email — see the email branch of `tokens`.
    ##  - `brackets`: the `]` closing each bracket a walk has passed, or that
    ##    none does (`Link.match_bracket`).
    ##  - `tall_end`: the `]` of the last label refused for its height
    ##    (`settle`), or 0 for none. A label that ends after it and starts
    ##    before it holds the refused one.
    ##
    ## `tokens` learns `email_dead` itself; `bracket` learns `brackets` and
    ## `settle` learns `tall_end`, and each hands them back (`Learned`), as
    ## does a label scan.
    Ctx : {
        b : List(U8),
        defs : Ref.Index,
        schemes : Link.Schemes,
        gt_end : U64,
        rb_end : U64,
        tick_last : List(U64),
        email_dead : U64,
        brackets : Link.Known,
        gts : List(U64),
        lasts : Tag.Lasts,
        tall_end : U64,
    }

    context : List(U8), Ref.Index, Link.Schemes -> Scan.Ctx
    context = |b, defs, schemes| {
        b,
        defs,
        schemes,
        gt_end: Scan.end_of_last(b, List.len(b), 62),
        rb_end: Scan.end_of_last(b, List.len(b), 93),
        tick_last: if List.contains(b, 96) {
            Scan.tick_table(b, 0, [])
        } else {
            []
        },
        email_dead: 0,
        brackets: Dict.empty(),
        gts: if List.contains(b, 40) and List.contains(b, 60) {
            Scan.positions(b, 62, 0, [])
        } else {
            []
        },
        lasts: Tag.lasts_of(b),
        tall_end: 0,
    }

    ## Every index of `c` in `b` from `i` on, appended to `acc` in order.
    positions : List(U8), U8, U64, List(U64) -> List(U64)
    positions = |b, c, i, acc|
        if i >= List.len(b) {
            acc
        } else if Bytes.at(b, i) == c {
            Scan.positions(b, c, i + 1, List.append(acc, i))
        } else {
            Scan.positions(b, c, i + 1, acc)
        }

    ## One past the last `c` before `i`, or 0 when there is none.
    end_of_last : List(U8), U64, U8 -> U64
    end_of_last = |b, i, c|
        if i == 0 {
            0
        } else if Bytes.at(b, i - 1) == c {
            i
        } else {
            Scan.end_of_last(b, i - 1, c)
        }

    ## `tick_last`, built in one pass over the maximal backtick runs.
    tick_table : List(U8), U64, List(U64) -> List(U64)
    tick_table = |b, i, table|
        if i >= List.len(b) {
            table
        } else if Bytes.at(b, i) == 96 {
            n = Bytes.run_len(b, i, 96, 0)
            grown = if List.len(table) < n {
                List.concat(table, List.repeat(0, n - List.len(table)))
            } else {
                table
            }
            Scan.tick_table(b, i + n, List.set(grown, n - 1, i + 1).ok_or(grown))
        } else {
            Scan.tick_table(b, i + 1, table)
        }

    ## Is there a run of exactly `n` backticks starting at or after `from`?
    tick_run_after : Scan.Ctx, U64, U64 -> Bool
    tick_run_after = |ctx, from, n| Bytes.tick_run_after(ctx.tick_last, from, n)

    ## The pending literal text, carrying the SOURCE offset it began at (T0b).
    ## The offset rides WITH the buffer so a finished `Text` node knows its
    ## source span without threading a separate cursor through every scanner
    ## function. `bytes` is DECODED (entities, escapes), so it is shorter than
    ## `[start, upto)` — which is correct: `loc` spans source, the payload does
    ## not (D11).
    Buf : { start : U64, bytes : List(U8) }

    buf0 : U64 -> Scan.Buf
    buf0 = |start| { start, bytes: [] }

    push : Scan.Buf, U8 -> Scan.Buf
    push = |buf, c| { ..buf, bytes: List.append(buf.bytes, c) }

    push_all : Scan.Buf, List(U8) -> Scan.Buf
    push_all = |buf, xs| { ..buf, bytes: List.concat(buf.bytes, xs) }

    run : Str, Ref.Index, Link.Schemes -> List(Inl)
    run = |s, defs, schemes| {
        b = Str.to_utf8(s)
        Emph.emphasis(Scan.tokens(Scan.context(b, defs, schemes), List.len(b), 0, 0, Scan.buf0(0), []).out)
    }

    ## --- stage 1: tokens -----------------------------------------------------
    ##
    ## `buf` is the pending literal text, flushed whenever a token interrupts it.
    ## Accumulating bytes and flushing once is what keeps this O(n): appending to
    ## the output per character would rebuild the node list on every byte.
    ##
    ## The helpers each return a `Step` — where to carry on — and every branch
    ## here ends in a call to `tokens` itself, which is a loop. They used to
    ## call `tokens` themselves: mutual recursion is not a loop, so each token
    ## they handled cost a stack frame, and a paragraph of about 6k lines, links
    ## or escapes overflowed the 8 MB stack.
    Step : { i : U64, buf : Scan.Buf, out : List(Emph.Tk) }

    ## A step taken with what it learned about the text on the way.
    Learned : { i : U64, buf : Scan.Buf, out : List(Emph.Tk), ctx : Scan.Ctx }

    learned : Scan.Ctx, Scan.Step -> Scan.Learned
    learned = |ctx, st| { i: st.i, buf: st.buf, out: st.out, ctx }

    ## `end` is where this scan stops: the end of the text, or the `]` of a
    ## link label being scanned in place (`settle`). `nest` is how many link
    ## labels this scan is inside; at `Emph.max_depth` a bracket is text, which
    ## bounds the tree and the scanner's own recursion into labels. The scan
    ## hands back its tokens and the context it ends with, which holds what it
    ## learned on the way.
    tokens : Scan.Ctx, U64, U64, U64, Scan.Buf, List(Emph.Tk) -> { out : List(Emph.Tk), ctx : Scan.Ctx }
    tokens = |ctx, end, nest, i, buf, out|
        if i >= end {
            { out: Scan.flush(buf, i, out), ctx }
        } else {
            c = Bytes.at(ctx.b, i)
            if c == 92 {
                st = Scan.backslash(ctx, i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 96 {
                st = Scan.code_span(ctx, i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 10 {
                st = Scan.newline(i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if (c == 104 or c == 119 or c == 102) and Autolink.at_boundary(ctx.b, i) and Autolink.bare_len(ctx.b, i) > 0 {
                st = Scan.bare_link(ctx, i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if i >= ctx.email_dead and Autolink.at_boundary(ctx.b, i) {
                ## An email is found at its START, like a bare URL. Backing up
                ## from the `@` looked simpler and could not work: an `_` in the
                ## local part is an emphasis delimiter, so by the time the `@`
                ## arrives the buffer has already been flushed and the local
                ## part is gone.
                e = Autolink.email_len(ctx.b, i)
                if e.len > 0 {
                    st = Scan.bare_autolink(ctx, i, i + e.len, "mailto:", buf, out)
                    Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
                } else {
                    ## A miss marks the whole local-part run dead, and the byte
                    ## is dispatched again past this test. Every later boundary
                    ## inside the run (after an `_`) reaches the same run end,
                    ## the same `@` or lack of one and the same domain, with a
                    ## local part that is a suffix of this one — so it fails
                    ## too. Without the mark, `a_a_a_…` re-scanned the rest of
                    ## the run at every underscore.
                    Scan.tokens({ ..ctx, email_dead: i + Scan.max_u64(e.local, 1) }, end, nest, i, buf, out)
                }
            } else if c == 38 {
                st = Scan.entity(ctx, i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 60 {
                st = Scan.angle(ctx, i, buf, out)
                Scan.tokens(ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 91 {
                st = Scan.bracket(ctx, nest, i, 1, buf, out, False)
                Scan.tokens(st.ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 33 and Bytes.at(ctx.b, i + 1) == 91 {
                st = Scan.bracket(ctx, nest, i, 2, buf, out, True)
                Scan.tokens(st.ctx, end, nest, st.i, st.buf, st.out)
            } else if c == 42 or c == 95 or c == 126 {
                n = Bytes.run_len(ctx.b, i, c, 0)
                flags = Emph.flanking(ctx.b, i, n, c)
                Scan.tokens(ctx, end, nest, i + n, Scan.buf0(i + n), List.append(Scan.flush(buf, i, out), Delim({ ch: c, n, opener: flags.opener, closer: flags.closer, both: flags.opener and flags.closer, at: i })))
            } else {
                Scan.tokens(ctx, end, nest, i + 1, Scan.push(buf, c), out)
            }
        }

    max_u64 : U64, U64 -> U64
    max_u64 = |a, b| if a > b { a } else { b }

    flush : Scan.Buf, U64, List(Emph.Tk) -> List(Emph.Tk)
    flush = |buf, upto, out|
        if List.is_empty(buf.bytes) {
            out
        } else {
            List.append(out, Node(Inl.at(buf.start, upto, Text(Str.from_utf8_lossy(buf.bytes)))))
        }

    ## `\` before ASCII punctuation is that character, literally. `\` before a
    ## newline is a hard break. `\` before anything else is a literal backslash —
    ## which is why this cannot be a blanket "skip the next byte".
    backslash : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    backslash = |ctx, i, buf, out| {
        nxt = Bytes.at(ctx.b, i + 1)
        if nxt == 10 and i + 1 < List.len(ctx.b) {
            { i: i + 2, buf: Scan.buf0(i + 2), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, i + 2, HardBreak))) }
        } else if Bytes.is_punct(nxt) and i + 1 < List.len(ctx.b) {
            { i: i + 2, buf: Scan.push(buf, nxt), out: out }
        } else {
            { i: i + 1, buf: Scan.push(buf, 92), out: out }
        }
    }

    ## A code span is opened by a run of N backticks and closed by the next run
    ## of EXACTLY N — not N or more. That is why this counts runs rather than
    ## searching for the next backtick: `` `` ` `` `` is one code span containing
    ## a backtick, and a first-match search finds the wrong end.
    ##
    ## An opener with no matching closer is literal text, so the backticks are
    ## emitted and scanning resumes after them.
    code_span : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    code_span = |ctx, i, buf, out| {
        n = Bytes.run_len(ctx.b, i, 96, 0)
        found = if Scan.tick_run_after(ctx, i + n, n) {
            Bytes.find_run(ctx.b, i + n, 96, n)
        } else {
            Err(NotFound)
        }
        match found {
            Ok(close) => {
                raw = Bytes.slice(ctx.b, i + n, close)
                { i: close + n, buf: Scan.buf0(close + n), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + n, Code(Scan.strip_code(raw))))) }
            }
            Err(_) => { i: i + n, buf: Scan.push_all(buf, List.repeat(96, n)), out: out }
        }
    }

    ## Line endings inside a code span become spaces, and one leading AND
    ## trailing space is stripped when the content has both and is not all
    ## spaces — the rule that lets `` ` `` `` hold a literal backtick.
    strip_code : Str -> Str
    strip_code = |raw| {
        spaced = Str.join_with(raw.split_on("\n"), " ")
        b = Str.to_utf8(spaced)
        n = List.len(b)
        if n >= 2 and Bytes.at(b, 0) == 32 and Bytes.at(b, n - 1) == 32 and !Scan.all_spaces(b) {
            Bytes.slice(b, 1, n - 1)
        } else {
            spaced
        }
    }

    all_spaces : List(U8) -> Bool
    all_spaces = |b| List.len(List.keep_if(b, |c| c == 32)) == List.len(b)

    ## Two or more trailing spaces before a newline is a hard break; otherwise
    ## the newline is soft. Trailing spaces are dropped either way, so a line
    ## ending never contributes text.
    newline : U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    newline = |i, buf, out| {
        trimmed_bytes = Scan.drop_trailing_spaces(buf.bytes)
        dropped = List.len(buf.bytes) - List.len(trimmed_bytes)
        text_end = i - dropped
        trimmed = { start: buf.start, bytes: trimmed_bytes }
        hard = dropped >= 2
        # The break covers the dropped trailing spaces and the newline byte.
        node = if hard {
            Node(Inl.at(text_end, i + 1, HardBreak))
        } else {
            Node(Inl.at(text_end, i + 1, SoftBreak))
        }
        { i: i + 1, buf: Scan.buf0(i + 1), out: List.append(Scan.flush(trimmed, text_end, out), node) }
    }

    drop_trailing_spaces : List(U8) -> List(U8)
    drop_trailing_spaces = |buf|
        match List.last(buf) {
            Ok(32) => Scan.drop_trailing_spaces(List.drop_last(buf, 1))
            _ => buf
        }

    ## --- links and images -----------------------------------------------------

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
    ##
    ## The label is scanned only once a destination or a definition has made a
    ## link possible, and then only once: `settle` either wraps its tokens or
    ## splices them back as text. Scanning it up front and again on the way to
    ## a literal made nested brackets exponential — each level scanned its label
    ## twice, so `[[[…a](u)…](u)](u)` doubled in cost per level.
    bracket : Scan.Ctx, U64, U64, U64, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    bracket = |ctx, nest, i, skip, buf, out, image|
        if nest >= Emph.max_depth {
            ## As many labels deep as the tree may nest: this label is not
            ## scanned, so neither the tree nor this recursion grows further.
            Scan.learned(ctx, Scan.literal(ctx, i, skip, buf, out))
        } else {
            Scan.bracket_at(ctx, nest, i, skip, buf, out, image)
        }

    ## Nothing here reads `ctx` once `close_of` has it, and nothing reads the
    ## first walk's context once the second has it: everything comes from the
    ## context the last walk handed back. A context still held after it went
    ## into a walk shares its `brackets` with the walk, so every answer the
    ## walk recorded copied the whole dictionary — 40,000 `[a][b]` in one
    ## paragraph took eleven seconds under `roc test`.
    ##
    ## The same holds for a label scan, and a spread is enough to break it:
    ## `{ ..ctx, end: close }` passed to a scan from inside a branch keeps the
    ## caller's record, and with it `brackets`, alive until the scan returns.
    ## So nothing may spread a `Ctx` into a scan. `end` and `nest` are
    ## `tokens`' arguments, a label scan gets the context whole, and every
    ## step from here on hands back the context the last scan or walk ended
    ## with.
    bracket_at : Scan.Ctx, U64, U64, U64, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    bracket_at = |ctx, nest, i, skip, buf, out, image| {
        first = Scan.close_of(ctx, i + skip)
        fctx = first.ctx
        match first.close {
            Ok(close) =>
                if Bytes.at(fctx.b, close + 1) == 91 {
                    ## `[text][label]` and `[label][]` — a REFERENCE. An empty
                    ## second bracket means "use the first as the label", which
                    ## is why this cannot just read the second one.
                    second = Scan.close_of(fctx, close + 2)
                    sctx = second.ctx
                    match second.close {
                        Ok(rclose) => {
                            key = if rclose == close + 2 {
                                { from: i + skip, to: close }
                            } else {
                                { from: close + 2, to: rclose }
                            }
                            Scan.resolve(sctx, nest, i, skip, close, rclose + 1, key, buf, out, image)
                        }
                        Err(_) => Scan.learned(sctx, Scan.literal(sctx, i, skip, buf, out))
                    }
                } else {
                    Scan.inline_or_shortcut(fctx, nest, i, skip, close, buf, out, image)
                }
            Err(_) => {
                step = Scan.literal(fctx, i, skip, buf, out)
                Scan.learned(fctx, step)
            }
        }
    }

    ## The bracket closed at `close` and no second bracket follows: an inline
    ## link if `(` does, a shortcut reference otherwise.
    inline_or_shortcut : Scan.Ctx, U64, U64, U64, U64, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    inline_or_shortcut = |ctx, nest, i, skip, close, buf, out, image|
        if Bytes.at(ctx.b, close + 1) != 40 {
            ## `[label]` alone — a SHORTCUT reference. Falls back to
            ## literal text when no definition matches, which is what
            ## keeps `[not a link]` readable.
            Scan.resolve(ctx, nest, i, skip, close, close + 1, { from: i + skip, to: close }, buf, out, image)
        } else {
            match Link.match_paren(ctx.b, close + 2, 1, ctx.gts) {
                Ok(pclose) => {
                    parts = Link.dest_title(Bytes.slice(ctx.b, close + 2, pclose))
                    if parts.ok {
                        Scan.settle(ctx, nest, i, skip, close, pclose + 1, parts, buf, out, image)
                    } else {
                        ## `[foo](not a link)` is a shortcut reference
                        ## followed by literal text, not text throughout
                        ## — so a bad inline destination retries as one
                        ## rather than giving up.
                        Scan.resolve(ctx, nest, i, skip, close, close + 1, { from: i + skip, to: close }, buf, out, image)
                    }
                }
                ## No `)` closes the destination, or its parens nest too
                ## deep: no destination at all is a bad one too, so `[foo](`
                ## retries as a shortcut reference, as cmark and
                ## commonmark.js read it.
                Err(_) => Scan.resolve(ctx, nest, i, skip, close, close + 1, { from: i + skip, to: close }, buf, out, image)
            }
        }

    ## The `]` closing a bracket whose content starts at `from` — answered
    ## without a search when no `]` is left in the text, or when a walk has
    ## already passed this bracket (`ctx.brackets`).
    close_of : Scan.Ctx, U64 -> { close : Try(U64, [NotFound]), ctx : Scan.Ctx }
    close_of = |ctx, from|
        if from >= ctx.rb_end {
            { close: Err(NotFound), ctx }
        } else
        match Dict.get(ctx.brackets, from) {
            Ok(known) => { close: known, ctx }
            Err(_) => {
                walk = Link.match_bracket(ctx.b, from, { gt_end: ctx.gt_end, tick_last: ctx.tick_last, lasts: ctx.lasts, schemes: ctx.schemes }, ctx.brackets)
                { close: walk.close, ctx: { ..ctx, brackets: walk.known } }
            }
        }

    ## Look the label up and build the node, or fall back to literal text.
    ##
    ## `resume` is where scanning continues — past the second bracket for a full
    ## reference, past the first for a shortcut — so one function serves both.
    ##
    ## `key` is where the label's text lies. A label longer than a label may
    ## be (`Link.label_fits`) is text before it is sliced or folded.
    resolve : Scan.Ctx, U64, U64, U64, U64, U64, { from : U64, to : U64 }, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    resolve = |ctx, nest, i, skip, close, resume, key, buf, out, image|
        if !Link.label_fits(ctx.b, key.from, key.to) {
            Scan.learned(ctx, Scan.literal(ctx, i, skip, buf, out))
        } else
        match Link.lookup(ctx.defs, Link.fold_label(Bytes.slice(ctx.b, key.from, key.to))) {
            Ok(d) => Scan.settle(ctx, nest, i, skip, close, resume, { dest: d.dest, title: d.title, ok: True }, buf, out, image)
            Err(_) => Scan.learned(ctx, Scan.literal(ctx, i, skip, buf, out))
        }

    ## A destination is in hand; scan the label once and decide.
    ##
    ## The label is scanned IN PLACE — the same bytes, stopping at the `]` —
    ## so its tokens are exactly what scanning on past the `[` would produce:
    ## a word boundary, a delimiter's flanking and a bare URL all see the text
    ## around the label, not the edge of a copy.
    ##
    ## A link may not contain a link, so a label that produced one leaves the
    ## outer bracket literal with the inner link intact. Its tokens are then
    ## spliced back in after the `[`, and scanning carries on where they end —
    ## the result re-scanning the label as text gave, without scanning it a
    ## second time. The same holds when a token ran past the `]` (an inner
    ## link whose destination does): the label was not a label after all. The
    ## tokens are still DELIMITERS, not finished emphasis, so a `*` inside the
    ## label can still pair with one outside it.
    ##
    ## What the label scan learned stays learned: a walk's answer depends only
    ## on the text and the position it started from, not on where the scan
    ## that asked stops, and so does `email_dead`.
    settle : Scan.Ctx, U64, U64, U64, U64, U64, { dest : Str, title : Str, ok : Bool }, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    settle = |ctx, nest, i, skip, close, resume, parts, buf, out, image| {
        { out: label, ctx: lctx } = Scan.tokens(ctx, close, nest + 1, i + skip, Scan.buf0(i + skip), [])
        stop = Scan.tks_end(label, i + skip)
        if stop > close or (!image and Link.tks_have_link(label)) {
            Scan.learned(lctx, Scan.splice(lctx, i, skip, stop, label, buf, out))
        } else if lctx.tall_end > i + skip {
            ## A label inside this one was refused for its height, and its
            ## tokens are this label's too, so this one cannot stand shorter
            ## in practice: a closer inside the refused label that found no
            ## opener there could pair with one out here instead, but no input
            ## tried, fuzzed ones at the cap included, has come out shorter.
            ## It is refused without pairing them again, which every
            ## enclosing label did: 99 images around one tall label paid for
            ## it 99 times.
            Scan.too_tall(lctx, i, skip, close, stop, label, buf, out)
        } else {
            inner = Emph.emphasis(label)
            ## The node would stand one level above its label's tallest, and
            ## past the cap it is refused the way a link in a link is.
            if Emph.height_all(inner) >= Emph.max_depth {
                Scan.too_tall(lctx, i, skip, close, stop, label, buf, out)
            } else {
                nodes = Link.link_node(i, resume, inner, parts, image, lctx.schemes)
                Scan.learned(lctx, { i: resume, buf: Scan.buf0(resume), out: List.concat(Scan.flush(buf, i, out), nodes) })
            }
        }
    }

    ## The label closed at `close` is too tall to be a link: it is text, and
    ## the enclosing labels learn so (`Ctx.tall_end`).
    too_tall : Scan.Ctx, U64, U64, U64, U64, List(Emph.Tk), Scan.Buf, List(Emph.Tk) -> Scan.Learned
    too_tall = |ctx, i, skip, close, stop, label, buf, out| {
        step = Scan.splice(ctx, i, skip, stop, label, buf, out)
        Scan.learned({ ..ctx, tall_end: close }, step)
    }

    ## The bracket is text after all: its opener joins the pending text and
    ## the label's tokens go back in, scanning on from where they end.
    splice : Scan.Ctx, U64, U64, U64, List(Emph.Tk), Scan.Buf, List(Emph.Tk) -> Scan.Step
    splice = |ctx, i, skip, stop, label, buf, out| {
        opener = Scan.push_all(buf, List.sublist(ctx.b, { start: i, len: skip }))
        { i: stop, buf: Scan.buf0(stop), out: List.concat(Scan.flush(opener, i + skip, out), label) }
    }

    ## Where a token list ends: every token ends where the next begins, so the
    ## last one's end is where the scan that made them stopped.
    tks_end : List(Emph.Tk), U64 -> U64
    tks_end = |tks, from|
        match List.last(tks) {
            Ok(Node(n)) => n.loc.outer.end
            Ok(Delim(d)) => d.at + d.n
            Err(_) => from
        }

    literal : Scan.Ctx, U64, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    literal = |ctx, i, skip, buf, out|
        { i: i + skip, buf: Scan.push_all(buf, List.sublist(ctx.b, { start: i, len: skip })), out: out }

    ## `<https://x>` and `<a@b.c>` become links to themselves, and a
    ## well-formed tag, comment, instruction, CDATA section or declaration
    ## becomes a `Raw` node. Anything else in angle brackets is left literal.
    ## Whether raw HTML reaches the output is the LOWERING's decision (see the
    ## tag branch below), not the scanner's.
    angle : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    angle = |ctx, i, buf, out|
        if i + 1 >= ctx.gt_end {
            ## No `>` anywhere after this, so nothing here can close.
            Scan.lt(i, buf, out)
        } else if Bytes.at(ctx.b, i + 1) == 33 or Bytes.at(ctx.b, i + 1) == 63 {
            Scan.special(ctx, i, buf, out)
        } else {
            Scan.tag(ctx, i, buf, out)
        }

    ## The `<` at `i` as text.
    lt : U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    lt = |i, buf, out| { i: i + 1, buf: Scan.push(buf, 60), out: out }

    ## `<!` or `<?`: an email autolink first, since `!` and `?` may start an
    ## address and an autolink outranks raw HTML, so `<!a@b.co>` is a mail
    ## link; else a comment, instruction, CDATA section or declaration
    ## (`Tag.special_end`). The address search stops at the next `<` or
    ## whitespace, so trying it first costs no more than the text to there.
    ##
    ## An address the href policy refuses is not a link, and is then read as
    ## raw HTML like any other `<!`, the way `tag` falls through.
    special : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    special = |ctx, i, buf, out|
        match Scan.mail_close(ctx, i) {
            Ok(close) => {
                inner = Bytes.slice(ctx.b, i + 1, close)
                Scan.angle_link(i, close, "mailto:${inner}", inner, buf, out)
            }
            Err(_) =>
                match Tag.special_end(ctx.b, i, ctx.lasts) {
                    Ok(end) => Scan.raw(i, end, Bytes.slice(ctx.b, i, end), buf, out)
                    Err(_) => Scan.lt(i, buf, out)
                }
        }

    ## The `>` closing an email autolink opened by the `<` at `i`, when the
    ## href policy lets it link.
    mail_close : Scan.Ctx, U64 -> Try(U64, [NotFound])
    mail_close = |ctx, i|
        match Autolink.email_close(ctx.b, i + 1) {
            Ok(close) =>
                if Link.allows(ctx.schemes, "mailto:${Bytes.slice(ctx.b, i + 1, close)}") {
                    Ok(close)
                } else {
                    Err(NotFound)
                }
            Err(_) => Err(NotFound)
        }

    tag : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    tag = |ctx, i, buf, out|
        match Tag.find_tag_end(ctx.b, i + 1) {
            Ok(close) => {
                inner = Bytes.slice(ctx.b, i + 1, close)
                if Autolink.is_autolink(inner) and Link.allows(ctx.schemes, inner) {
                    Scan.angle_link(i, close, inner, inner, buf, out)
                } else if Autolink.is_email(inner) and Link.allows(ctx.schemes, "mailto:${inner}") {
                    Scan.angle_link(i, close, "mailto:${inner}", inner, buf, out)
                } else if Tag.is_tag(inner) {
                    ## Raw inline HTML. It is carried, not dropped and not
                    ## escaped here — what happens to it is the LOWERING's
                    ## decision, and the two lowerings disagree: `to_html`
                    ## passes it through (or omits it, when safe), while a
                    ## native renderer has no HTML engine and shows it as text.
                    Scan.raw(i, close + 1, "<${inner}>", buf, out)
                } else {
                    Scan.lt(i, buf, out)
                }
            }
            Err(_) => Scan.lt(i, buf, out)
        }

    ## An autolink from the `<` at `i` to the `>` at `close`, showing `text`.
    angle_link : U64, U64, Str, Str, Scan.Buf, List(Emph.Tk) -> Scan.Step
    angle_link = |i, close, href, text, buf, out|
        { i: close + 1, buf: Scan.buf0(close + 1), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + 1, Link({ href, title: "", kids: [Inl.at(i + 1, close, Text(text))] })))) }

    ## Raw HTML spanning `[i, end)`.
    raw : U64, U64, Str, Scan.Buf, List(Emph.Tk) -> Scan.Step
    raw = |i, end, html, buf, out|
        { i: end, buf: Scan.buf0(end), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, end, Raw(html)))) }

    bare_link : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    bare_link = |ctx, i, buf, out| {
        prefix = if Bytes.starts(ctx.b, i, "www.") {
            "http://"
        } else {
            ""
        }
        Scan.bare_autolink(ctx, i, i + Autolink.bare_len(ctx.b, i), prefix, buf, out)
    }

    ## A bare URL or email address spanning `[start, end)`, linked to itself
    ## behind `prefix` — or, when the href policy refuses that, the same bytes
    ## as text, which is what a refused link's label becomes too.
    bare_autolink : Scan.Ctx, U64, U64, Str, Scan.Buf, List(Emph.Tk) -> Scan.Step
    bare_autolink = |ctx, start, end, prefix, buf, out| {
        text = Bytes.slice(ctx.b, start, end)
        href = "${prefix}${text}"
        if Link.allows(ctx.schemes, href) {
            node = Node(Inl.at(start, end, Link({ href: href, title: "", kids: [Inl.at(start, end, Text(text))] })))
            { i: end, buf: Scan.buf0(end), out: List.append(Scan.flush(buf, start, out), node) }
        } else {
            { i: end, buf: Scan.push_all(buf, List.sublist(ctx.b, { start, len: end - start })), out: out }
        }
    }

    ## `&name;`, `&#35;`, `&#x22;`. An unknown name is a literal `&`, which is
    ## what keeps `AT&T` and `a &lt ctx.b` readable.
    ##
    ## The decoded text joins `buf` rather than becoming its own node, so
    ## `a&amp;ctx.b` stays one text run.
    ##
    ## The `;` is looked for no further than the longest name could reach. An
    ## unbounded search ran to the end of the text for every `&` without one,
    ## so a paragraph of `&`s was quadratic.
    entity : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    entity = |ctx, i, buf, out|
        match Ents.semi_within(ctx.b, i + 1) {
            Ok(close) =>
                match Ents.lookup(Bytes.slice(ctx.b, i + 1, close)) {
                    Ok(ch) => { i: close + 1, buf: Scan.push_all(buf, Str.to_utf8(ch)), out: out }
                    Err(_) => { i: i + 1, buf: Scan.push(buf, 38), out: out }
                }
            Err(_) => { i: i + 1, buf: Scan.push(buf, 38), out: out }
        }
}
