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
    ## `end` is where this scan stops: the end of `b`, or the `]` of a link
    ## label being scanned in place (`settle`).
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
    ##
    ## A closer that DOES exist is no help to those, so the rest are learned
    ## during the scan, from searches that have already run:
    ##
    ##  - `email_dead`: a position below which every word boundary is known not
    ##    to start an email — see the email branch of `tokens`.
    ##  - `cdata_end`: a CDATA body starting at or past it has no `]]>` after
    ##    it. It starts at the end of the text and moves down to where a search
    ##    failed, since one that finds nothing from there finds nothing later.
    ##  - `brackets`: the `]` closing each bracket a walk has passed, or that
    ##    none does (`Link.match_bracket`).
    ##
    ## `tokens` learns `email_dead` itself; `angle` and `bracket` learn the
    ## other two and hand them back (`Learned`).
    Ctx : {
        b : List(U8),
        defs : Ref.Index,
        end : U64,
        gt_end : U64,
        rb_end : U64,
        tick_last : List(U64),
        email_dead : U64,
        cdata_end : U64,
        brackets : Link.Known,
    }

    context : List(U8), Ref.Index -> Scan.Ctx
    context = |b, defs| {
        b,
        defs,
        end: List.len(b),
        gt_end: Scan.end_of_last(b, List.len(b), 62),
        rb_end: Scan.end_of_last(b, List.len(b), 93),
        tick_last: if List.contains(b, 96) {
            Scan.tick_table(b, 0, [])
        } else {
            []
        },
        email_dead: 0,
        cdata_end: List.len(b),
        brackets: Dict.empty(),
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
    tick_run_after = |ctx, from, n|
        match List.get(ctx.tick_last, n - 1) {
            Ok(last) => last > from
            Err(_) => False
        }

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

    run : Str, Ref.Index -> List(Inl)
    run = |s, defs| Emph.emphasis(Scan.tokens(Scan.context(Str.to_utf8(s), defs), 0, Scan.buf0(0), []))

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

    tokens : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> List(Emph.Tk)
    tokens = |ctx, i, buf, out|
        if i >= ctx.end {
            Scan.flush(buf, i, out)
        } else {
            c = Bytes.at(ctx.b, i)
            if c == 92 {
                st = Scan.backslash(ctx, i, buf, out)
                Scan.tokens(ctx, st.i, st.buf, st.out)
            } else if c == 96 {
                st = Scan.code_span(ctx, i, buf, out)
                Scan.tokens(ctx, st.i, st.buf, st.out)
            } else if c == 10 {
                st = Scan.newline(i, buf, out)
                Scan.tokens(ctx, st.i, st.buf, st.out)
            } else if (c == 104 or c == 119 or c == 102) and Autolink.at_boundary(ctx.b, i) and Autolink.bare_len(ctx.b, i) > 0 {
                st = Scan.bare_link(ctx, i, buf, out)
                Scan.tokens(ctx, st.i, st.buf, st.out)
            } else if i >= ctx.email_dead and Autolink.at_boundary(ctx.b, i) {
                ## An email is found at its START, like a bare URL. Backing up
                ## from the `@` looked simpler and could not work: an `_` in the
                ## local part is an emphasis delimiter, so by the time the `@`
                ## arrives the buffer has already been flushed and the local
                ## part is gone.
                e = Autolink.email_len(ctx.b, i)
                if e.len > 0 {
                    n = e.len
                    addr = Bytes.slice(ctx.b, i, i + n)
                    node = Node(Inl.at(i, i + n, Link({ href: "mailto:${addr}", title: "", kids: [Inl.at(i, i + n, Text(addr))] })))
                    Scan.tokens(ctx, i + n, Scan.buf0(i + n), List.append(Scan.flush(buf, i, out), node))
                } else {
                    ## A miss marks the whole local-part run dead, and the byte
                    ## is dispatched again past this test. Every later boundary
                    ## inside the run (after an `_`) reaches the same run end,
                    ## the same `@` or lack of one and the same domain, with a
                    ## local part that is a suffix of this one — so it fails
                    ## too. Without the mark, `a_a_a_…` re-scanned the rest of
                    ## the run at every underscore.
                    Scan.tokens({ ..ctx, email_dead: i + Scan.max_u64(e.local, 1) }, i, buf, out)
                }
            } else if c == 38 {
                st = Scan.entity(ctx, i, buf, out)
                Scan.tokens(ctx, st.i, st.buf, st.out)
            } else if c == 60 {
                st = Scan.angle(ctx, i, buf, out)
                Scan.tokens(st.ctx, st.i, st.buf, st.out)
            } else if c == 91 {
                st = Scan.bracket(ctx, i, 1, buf, out, False)
                Scan.tokens(st.ctx, st.i, st.buf, st.out)
            } else if c == 33 and Bytes.at(ctx.b, i + 1) == 91 {
                st = Scan.bracket(ctx, i, 2, buf, out, True)
                Scan.tokens(st.ctx, st.i, st.buf, st.out)
            } else if c == 42 or c == 95 or c == 126 {
                n = Bytes.run_len(ctx.b, i, c, 0)
                flags = Emph.flanking(ctx.b, i, n, c)
                Scan.tokens(ctx, i + n, Scan.buf0(i + n), List.append(Scan.flush(buf, i, out), Delim({ ch: c, n, opener: flags.opener, closer: flags.closer, both: flags.opener and flags.closer, at: i })))
            } else {
                Scan.tokens(ctx, i + 1, Scan.push(buf, c), out)
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
    bracket : Scan.Ctx, U64, U64, Scan.Buf, List(Emph.Tk), Bool -> Scan.Learned
    bracket = |ctx, i, skip, buf, out, image| {
        first = Scan.close_of(ctx, i + skip)
        match first.close {
            Ok(close) =>
                if Bytes.at(ctx.b, close + 1) == 91 {
                    ## `[text][label]` and `[label][]` — a REFERENCE. An empty
                    ## second bracket means "use the first as the label", which
                    ## is why this cannot just read the second one.
                    second = Scan.close_of(first.ctx, close + 2)
                    Scan.learned(
                        second.ctx,
                        match second.close {
                            Ok(rclose) => {
                                written = Bytes.slice(ctx.b, close + 2, rclose)
                                key = if Str.is_empty(written) {
                                    Bytes.slice(ctx.b, i + skip, close)
                                } else {
                                    written
                                }
                                Scan.resolve(second.ctx, i, skip, close, rclose + 1, key, buf, out, image)
                            }
                            Err(_) => Scan.literal(ctx, i, skip, buf, out)
                        },
                    )
                } else {
                    Scan.learned(first.ctx, Scan.inline_or_shortcut(first.ctx, i, skip, close, buf, out, image))
                }
            Err(_) => Scan.learned(first.ctx, Scan.literal(ctx, i, skip, buf, out))
        }
    }

    ## The bracket closed at `close` and no second bracket follows: an inline
    ## link if `(` does, a shortcut reference otherwise.
    inline_or_shortcut : Scan.Ctx, U64, U64, U64, Scan.Buf, List(Emph.Tk), Bool -> Scan.Step
    inline_or_shortcut = |ctx, i, skip, close, buf, out, image|
        if Bytes.at(ctx.b, close + 1) != 40 {
            ## `[label]` alone — a SHORTCUT reference. Falls back to
            ## literal text when no definition matches, which is what
            ## keeps `[not a link]` readable.
            Scan.resolve(ctx, i, skip, close, close + 1, Bytes.slice(ctx.b, i + skip, close), buf, out, image)
        } else {
            match Link.match_paren(ctx.b, close + 2, 1) {
                Ok(pclose) => {
                    parts = Link.dest_title(Bytes.slice(ctx.b, close + 2, pclose))
                    if parts.ok {
                        Scan.settle(ctx, i, skip, close, pclose + 1, parts, buf, out, image)
                    } else {
                        ## `[foo](not a link)` is a shortcut reference
                        ## followed by literal text, not text throughout
                        ## — so a bad inline destination retries as one
                        ## rather than giving up.
                        Scan.resolve(ctx, i, skip, close, close + 1, Bytes.slice(ctx.b, i + skip, close), buf, out, image)
                    }
                }
                Err(_) => Scan.literal(ctx, i, skip, buf, out)
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
                walk = Link.match_bracket(ctx.b, from, ctx.gt_end, ctx.brackets)
                { close: walk.close, ctx: { ..ctx, brackets: walk.known } }
            }
        }

    ## Look the label up and build the node, or fall back to literal text.
    ##
    ## `resume` is where scanning continues — past the second bracket for a full
    ## reference, past the first for a shortcut — so one function serves both.
    resolve : Scan.Ctx, U64, U64, U64, U64, Str, Scan.Buf, List(Emph.Tk), Bool -> Scan.Step
    resolve = |ctx, i, skip, close, resume, key, buf, out, image|
        match Link.lookup(ctx.defs, Link.fold_label(key)) {
            Ok(d) => Scan.settle(ctx, i, skip, close, resume, { dest: d.dest, title: d.title, ok: True }, buf, out, image)
            Err(_) => Scan.literal(ctx, i, skip, buf, out)
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
    settle : Scan.Ctx, U64, U64, U64, U64, { dest : Str, title : Str, ok : Bool }, Scan.Buf, List(Emph.Tk), Bool -> Scan.Step
    settle = |ctx, i, skip, close, resume, parts, buf, out, image| {
        label = Scan.tokens({ ..ctx, end: close }, i + skip, Scan.buf0(i + skip), [])
        stop = Scan.tks_end(label, i + skip)
        if stop > close or (!image and Link.tks_have_link(label)) {
            opener = Scan.push_all(buf, List.sublist(ctx.b, { start: i, len: skip }))
            { i: stop, buf: Scan.buf0(stop), out: List.concat(Scan.flush(opener, i + skip, out), label) }
        } else {
            nodes = Link.link_node(i, resume, Emph.emphasis(label), parts, image)
            { i: resume, buf: Scan.buf0(resume), out: List.concat(Scan.flush(buf, i, out), nodes) }
        }
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
    ## well-formed tag becomes a `Raw` node. Anything else in angle brackets is
    ## left literal. Whether raw HTML reaches the output is the LOWERING's
    ## decision (see the tag branch below), not the scanner's.
    angle : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Learned
    angle = |ctx, i, buf, out|
        if i + 1 >= ctx.gt_end {
            ## No `>` anywhere after this, so nothing here can close.
            Scan.learned(ctx, { i: i + 1, buf: Scan.push(buf, 60), out: out })
        } else if Bytes.starts(ctx.b, i, "<![CDATA[") {
            Scan.cdata(ctx, i, buf, out)
        } else {
            Scan.learned(ctx, Scan.tag(ctx, i, buf, out))
        }

    ## CDATA ends at `]]>`, and its body may contain `>` — searching for the
    ## first one cut it in half.
    cdata : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Learned
    cdata = |ctx, i, buf, out| {
        body = i + 9
        lt = { i: i + 1, buf: Scan.push(buf, 60), out: out }
        if body >= ctx.cdata_end {
            Scan.learned(ctx, lt)
        } else
        match Bytes.find_cdata(ctx.b, body) {
            Ok(close) =>
                Scan.learned(ctx, { i: close + 3, buf: Scan.buf0(close + 3), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + 3, Raw(Bytes.slice(ctx.b, i, close + 3))))) })
            Err(_) => Scan.learned({ ..ctx, cdata_end: body }, lt)
        }
    }

    tag : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    tag = |ctx, i, buf, out|
        match Tag.find_tag_end(ctx.b, i + 1) {
            Ok(close) => {
                inner = Bytes.slice(ctx.b, i + 1, close)
                if Autolink.is_autolink(inner) and Link.safe_href(inner) {
                    { i: close + 1, buf: Scan.buf0(close + 1), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + 1, Link({ href: inner, title: "", kids: [Inl.at(i + 1, close, Text(inner))] })))) }
                } else if Autolink.is_email(inner) {
                    { i: close + 1, buf: Scan.buf0(close + 1), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + 1, Link({ href: "mailto:${inner}", title: "", kids: [Inl.at(i + 1, close, Text(inner))] })))) }
                } else if Tag.is_tag(inner) {
                    ## Raw inline HTML. It is carried, not dropped and not
                    ## escaped here — what happens to it is the LOWERING's
                    ## decision, and the two lowerings disagree: `to_html`
                    ## passes it through (or omits it, when safe), while a
                    ## native renderer has no HTML engine and shows it as text.
                    { i: close + 1, buf: Scan.buf0(close + 1), out: List.append(Scan.flush(buf, i, out), Node(Inl.at(i, close + 1, Raw("<${inner}>")))) }
                } else {
                    { i: i + 1, buf: Scan.push(buf, 60), out: out }
                }
            }
            Err(_) => { i: i + 1, buf: Scan.push(buf, 60), out: out }
        }

    bare_link : Scan.Ctx, U64, Scan.Buf, List(Emph.Tk) -> Scan.Step
    bare_link = |ctx, i, buf, out| {
        n = Autolink.bare_len(ctx.b, i)
        text = Bytes.slice(ctx.b, i, i + n)
        href = if Bytes.starts(ctx.b, i, "www.") {
            "http://${text}"
        } else {
            text
        }
        node = Node(Inl.at(i, i + n, Link({ href: href, title: "", kids: [Inl.at(i, i + n, Text(text))] })))
        { i: i + n, buf: Scan.buf0(i + n), out: List.append(Scan.flush(buf, i, out), node) }
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
