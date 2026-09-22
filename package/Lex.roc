## Block-level lexical predicates: what does this LINE look like?
##
## Every function here answers a question about one line — or a few bytes of one
## — with no reference to what is open, what came before, or what the document
## is. That is the entire membership rule: nothing here touches parser state.
import Scan

Lex := [].{
    ## `drop_prefix`, not `drop_first_bytes`: the latter is the CHECKED form and
    ## returns `Try(Str, [BadUtf8])` because it validates that the byte offset
    ## lands on a character boundary. Dropping a literal `" "` cannot land
    ## mid-character, so it needs no `Try` and this stays a plain fold.
    indent : Str, U64 -> U64
    indent = |s, acc|
        if s.starts_with(" ") {
            Lex.indent(s.drop_prefix(" "), acc + 1)
        } else {
            acc
        }

    ## Drop up to `n` leading spaces — never more, so content indented past the
    ## fence keeps the difference.
    unindent : Str, U64 -> Str
    unindent = |src, n|
        if n == 0 or !src.starts_with(" ") {
            src
        } else {
            Lex.unindent(src.drop_prefix(" "), n - 1)
        }

    ## A tab advances to the next multiple of four, which is what makes
    ## indentation comparable at all: a tab is not "one space" and is not "four
    ## spaces" either — `a\tb` puts `b` at column 4, but `ab\tc` puts `c` there
    ## too. Every indent test downstream counts spaces, so this runs first.
    ##
    ## **Only the LEADING whitespace is expanded.** Tabs decide block structure;
    ## inside content they are ordinary characters, and expanding them there
    ## rewrote the contents of every code block containing one.
    expand_tabs : Str -> Str
    expand_tabs = |src|
        if src.contains("\t") {
            Str.from_utf8_lossy(Lex.tabs(Str.to_utf8(src), 0, []))
        } else {
            src
        }

    ## Expands tabs in the line's PREFIX — its indentation and any container
    ## markers — and stops at the first content byte.
    ##
    ## The prefix is where a tab means a column, which is why `-\tfoo` needs it:
    ## the marker moves the column, so the tab after it lands on a different
    ## stop than a tab at the start would. Past the prefix a tab is an ordinary
    ## character, and expanding there rewrote the contents of code blocks.
    tabs : List(U8), U64, List(U8) -> List(U8)
    tabs = |b, i, out|
        match List.get(b, i) {
            Ok(9) => Lex.tabs(b, i + 1, List.concat(out, List.repeat(32, 4 - (List.len(out) % 4))))
            Ok(32) => Lex.tabs(b, i + 1, List.append(out, 32))
            ## Still prefix: a quote marker, a bullet, or an ordered marker's
            ## digits and delimiter.
            Ok(c) =>
                if c == 62 or c == 45 or c == 42 or c == 43 or c == 46 or c == 41 or (c >= 48 and c <= 57) {
                    Lex.tabs(b, i + 1, List.append(out, c))
                } else {
                    List.concat(out, List.drop_first(b, i))
                }
            Err(_) => out
        }

    ## Three or more of `-`, `*` or `_`, all the SAME character, with spaces
    ## allowed between them. Only `-` was recognised before, so `***` and `___`
    ## were prose, and `- - -` was a list.
    ## One pass and no intermediate lists.
    ##
    ## The four `List.keep_if`s this replaces allocated a list each, every time,
    ## and `is_rule` is asked of nearly every line — from `starts_block`, from
    ## `marker_ahead`, and from the dispatch itself.
    is_rule : Str -> Bool
    is_rule = |t| {
        ## Annotated, or `n` has nothing to fix its type and defaults to `Dec`
        ## — decimal arithmetic, in the counting loop of a per-line predicate.
        st : { n : U64, ch : U8, ok : Bool }
        st = List.fold(
            Str.to_utf8(t),
            { n: 0, ch: 0, ok: True },
            |acc, x|
                if x == 32 or x == 9 {
                    acc
                } else if x != 45 and x != 42 and x != 95 {
                    { ..acc, ok: False }
                } else if acc.n == 0 {
                    { n: 1, ch: x, ok: acc.ok }
                } else if x == acc.ch {
                    { ..acc, n: acc.n + 1 }
                } else {
                    { ..acc, ok: False }
                },
        )
        st.ok and st.n >= 3
    }

    ## A `===` or `---` line turns the paragraph ABOVE it into a heading. The
    ## underline is not a block of its own, which is why this cannot live in the
    ## dispatch chain beside the others: it reaches back and rewrites what is
    ## already buffered.
    setext_level : Str -> U8
    setext_level = |t| {
        b = Str.to_utf8(t)
        if !List.is_empty(b) and List.len(List.keep_if(b, |x| x == 61)) == List.len(b) {
            1
        } else if !List.is_empty(b) and List.len(List.keep_if(b, |x| x == 45)) == List.len(b) {
            2
        } else {
            0
        }
    }

    is_digits : Str -> Bool
    is_digits = |s| {
        b = Str.to_utf8(s)
        !List.is_empty(b) and List.len(List.keep_if(b, |x| x >= 48 and x <= 57)) == List.len(b)
    }

    ## The value of a run of ASCII digits. `marker_at` bounds the run to nine
    ## digits first, so it cannot overflow.
    digits_value : List(U8), U64 -> U64
    digits_value = |b, acc|
        match List.first(b) {
            Ok(c) => Lex.digits_value(List.drop_first(b, 1), acc * 10 + (c - 48).to_u64())
            Err(_) => acc
        }

    digit_run : List(U8), U64 -> U64
    digit_run = |b, i| {
        c = Scan.at(b, i)
        if c >= 48 and c <= 57 {
            Lex.digit_run(b, i + 1)
        } else {
            i
        }
    }

    ## A fence opens on three or more backticks or tildes. A backtick fence's
    ## info string may not contain a backtick, which is what keeps `` `foo` ``
    ## in a paragraph from opening one.
    opens_fence : Str -> Bool
    opens_fence = |t| {
        b = Str.to_utf8(t)
        ch = Scan.at(b, 0)
        if ch != 96 and ch != 126 {
            False
        } else {
            n = Lex.leading_run(b, ch, 0)
            rest = Str.drop_first_bytes(t, n).ok_or("")
            n >= 3 and (ch == 126 or !rest.contains("`"))
        }
    }

    closes_fence : Str, U8, U64 -> Bool
    closes_fence = |t, ch, len| {
        b = Str.to_utf8(t)
        n = Lex.leading_run(b, ch, 0)
        n >= len and n == List.len(b)
    }

    leading_run : List(U8), U8, U64 -> U64
    leading_run = |b, ch, acc|
        if List.get(b, acc) == Ok(ch) {
            Lex.leading_run(b, ch, acc + 1)
        } else {
            acc
        }

    first_word : Str -> Str
    first_word = |info|
        match info.split_first(" ") {
            Ok(p) => p.before
            Err(_) => info
        }

    ## A marker starts with a bullet character or a digit, and ONE byte settles
    ## that. Everything in `marker_at` — two conditional rewrites, three
    ## `starts_with`s, a `split_first` and a digit check — is skipped for every
    ## line of prose, which is most lines of most documents. `Str.to_utf8` is
    ## free, so the guard costs a load and three comparisons.
    marker : Str -> [Bullet(U64, Str), Numbered(U64, U64, Str), NotList]
    marker = |raw| {
        c0 = Scan.at(Str.to_utf8(raw.trim()), 0)
        if c0 == 45 or c0 == 42 or c0 == 43 or (c0 >= 48 and c0 <= 57) {
            Lex.marker_at(raw)
        } else {
            NotList
        }
    }

    ## Which list marker a raw line carries, if any.
    ##
    ## The ordered case splits at the first `.` and then checks the text before
    ## it is all digits, so `e.g. thing` and `Note 1. thing` both stay prose.
    marker_at : Str -> [Bullet(U64, Str), Numbered(U64, U64, Str), NotList]
    marker_at = |raw| {
        ind = Lex.indent(raw, 0)
        t = raw.trim()
        ## Guarded, because `marker` is asked of nearly every line and both of
        ## these rewrites are no-ops on nearly every line. Unguarded they were
        ## the parser's single hottest allocation: `Str.split_on` plus
        ## `join_with` is two allocations and a substring search, and a profile
        ## put `str_split` and its `mem.findPos` at the top of the flat counts.
        ## `contains` scans without allocating.
        tabbed = if t.contains("\t") {
            Str.join_with(t.split_on("\t"), " ")
        } else {
            t
        }
        if t == "-" or t == "*" or t == "+" {
            ## A marker alone on its line opens an EMPTY entry, whose content
            ## arrives on the lines below it.
            Bullet(ind + 2, "")
        } else if tabbed.starts_with("- ") or tabbed.starts_with("* ") or tabbed.starts_with("+ ") {
            after = Str.drop_first_bytes(tabbed, 1).ok_or("")
            Bullet(ind + 1 + Lex.content_gap(after), Str.trim_start(after))
        } else {
            dotted = if tabbed.contains(")") {
                Str.join_with(tabbed.split_on(")"), ".")
            } else {
                tabbed
            }
            match dotted.split_first(".") {
                Ok(sp) =>
                    ## Nine digits at most — `1234567890.` is prose, per the
                    ## spec, and without the bound it became a list starting at
                    ## a billion.
                    if Lex.is_digits(sp.before)
                    and Str.count_utf8_bytes(sp.before) <= 9
                    and (sp.after.starts_with(" ") or Str.is_empty(sp.after)) {
                        n = Str.count_utf8_bytes(sp.before) + 1
                        ## `2.` alone is an EMPTY entry; the gap is 1 so the
                        ## content column lands just past the marker.
                        Numbered(
                            ind + n + Lex.content_gap(sp.after),
                            Lex.digits_value(Str.to_utf8(sp.before), 0),
                            Str.trim_start(sp.after),
                        )
                    } else {
                        NotList
                    }
                Err(_) => NotList
            }
        }
    }

    ## Reports the CONTENT COLUMN, not a depth bucket: the column the entry's
    ## text starts at is what decides whether a later line belongs to this entry,
    ## and bucketing indents into depths lost exactly that.
    ## An EMPTY entry may not interrupt a paragraph — `foo\n*` is prose with a
    ## star in it, not a one-item list. Only the caller knows whether a
    ## paragraph is open, so the rule lives here rather than in `marker`.
    marker_here : Str, Bool -> [Bullet(U64, Str), Numbered(U64, U64, Str), NotList]
    marker_here = |raw, no_para|
        match Lex.marker(raw) {
            Bullet(col, rest) =>
                if Str.is_empty(rest) and !no_para {
                    NotList
                } else {
                    Bullet(col, rest)
                }
            Numbered(col, num, rest) =>
                if (Str.is_empty(rest) or num != 1) and !no_para {
                    ## Only a `1.` may interrupt a paragraph — otherwise a line
                    ## ending "…is\n14. The number…" becomes a list at 14.
                    NotList
                } else {
                    Numbered(col, num, rest)
                }
            NotList => NotList
        }

    ## The character that ends the marker: the bullet itself, or the `.`/`)`
    ## after an ordered marker's digits.
    marker_char : Str -> Str
    marker_char = |t| {
        b = Str.to_utf8(t)
        c0 = Scan.at(b, 0)
        if c0 == 45 or c0 == 42 or c0 == 43 {
            Str.from_utf8_lossy([c0])
        } else {
            Str.from_utf8_lossy([Scan.at(b, Lex.digit_run(b, 0))])
        }
    }

    ## How far past the marker the content starts. A marker may be followed by
    ## up to four spaces and the content column moves with them — `-    one`
    ## puts its content at column 5, and assuming exactly one space put every
    ## continuation line of such an entry at the wrong indent.
    ##
    ## Five or more spaces means the content is an indented code block, and the
    ## entry's column is the marker plus ONE — the spec's rule, and the reason
    ## this is not simply "count the spaces".
    content_gap : Str -> U64
    content_gap = |after| {
        n = Lex.indent(after, 0)
        if n >= 1 and n <= 4 {
            n
        } else {
            1
        }
    }

    ## The first line of an entry, de-indented to its content column.
    ##
    ## A line no longer than the column has NO content — a marker alone. It has
    ## to answer "" rather than the line itself, or a bare `-` re-parses as a
    ## marker and the recursion never bottoms out.
    entry_head : Str, U64 -> Str
    entry_head = |raw, col|
        if Str.count_utf8_bytes(raw) <= col {
            ""
        } else {
            ## Drops `col` CHARACTERS — indent, marker and gap — not `col`
            ## spaces. `unindent` strips whitespace only, so it left the marker
            ## in place and the entry re-parsed as itself forever.
            Str.drop_first_bytes(raw, col).ok_or("")
        }

    task_of : Str -> { state : [NoTask, Todo, Done], rest : Str }
    task_of = |rest| {
        low = Str.with_ascii_lowercased(rest)
        if low.starts_with("[ ] ") or low == "[ ]" {
            { state: Todo, rest: Str.drop_first_bytes(rest, 4).ok_or("") }
        } else if low.starts_with("[x] ") or low == "[x]" {
            { state: Done, rest: Str.drop_first_bytes(rest, 4).ok_or("") }
        } else {
            { state: NoTask, rest: rest }
        }
    }

    ## The seven start conditions, as the closer each one implies. `""` means
    ## "ends at a blank line"; `"\n"` means "this one line only". A non-start
    ## returns `""` too, so callers test `html_start(t) != ""` first — which is
    ## why the blank-line closers report `"BLANK"` rather than empty.
    html_start : Str -> Str
    html_start = |t|
        if !t.starts_with("<") {
            ""
        } else if Lex.starts_any(t, ["<script", "<pre", "<style", "<textarea"]) {
            "</"
        } else if t.starts_with("<!--") {
            "-->"
        } else if t.starts_with("<?") {
            "?>"
        } else if t.starts_with("<![CDATA[") {
            "]]>"
        } else if t.starts_with("<!") {
            ">"
        } else if Lex.is_block_tag(t) {
            ## Conditions 6 and 7: any block-level tag, closed by a blank line.
            "BLANK"
        } else {
            ""
        }

    starts_any : Str, List(Str) -> Bool
    starts_any = |t, ps| List.any(ps, |p| t.starts_with(p))

    ## Condition 6 is a NAMED list of block elements; condition 7 is any other
    ## complete tag alone on its line. Both close at a blank line, so they are
    ## one branch here — the distinction only matters for whether the tag may
    ## interrupt a paragraph, which the caller already checks.
    ## Conditions 6 and 7, which are NOT the same rule and were collapsed here
    ## for too long.
    ##
    ## Six is a NAMED block element — `<div id="foo"` opens a block even though
    ## the tag never finishes, because the name alone is the signal.
    ##
    ## Seven is any other complete tag with nothing else on the line. Both parts
    ## matter: `<del>*foo*</del>` has a complete tag and trailing content, so it
    ## is a paragraph, and `<http://foo.bar>` is not a tag at all — after the
    ## name must come `>`, `/` or whitespace, which is what stops every
    ## angle-bracket autolink being swallowed as raw HTML.
    is_block_tag : Str -> Bool
    is_block_tag = |t| {
        name = Str.with_ascii_lowercased(Lex.tag_name(t))
        if Str.is_empty(name) {
            False
        } else if List.contains(Lex.block_tags, name) {
            True
        } else {
            after = Scan.at(
                Str.to_utf8(t),
                Str.count_utf8_bytes(name) + (if t.starts_with("</") { 2 } else { 1 }),
            )
            ## Condition 7 needs a WELL-FORMED tag, attributes and all — the
            ## same test the inline scanner applies, so `<a h*#ref="hi">` is
            ## text in both places rather than a block in one.
            t.ends_with(">")
            and (after == 62 or after == 47 or Scan.is_space(after))
            and Lex.tag_is_whole_line(t)
            and Scan.is_tag(Str.drop_suffix(t.drop_prefix("<"), ">"))
        }
    }

    ## Condition 7 needs the tag to be the ONLY thing on its line.
    tag_is_whole_line : Str -> Bool
    tag_is_whole_line = |t|
        match t.split_first(">") {
            Ok(p) => Str.is_empty(p.after.trim())
            Err(_) => False
        }

    block_tags : List(Str)
    block_tags = [
        "address", "article", "aside", "base", "basefont", "blockquote", "body",
        "caption", "center", "col", "colgroup", "dd", "details", "dialog", "dir",
        "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer", "form",
        "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
        "hr", "html", "iframe", "legend", "li", "link", "main", "menu", "menuitem",
        "nav", "noframes", "ol", "optgroup", "option", "p", "param", "search",
        "section", "summary", "table", "tbody", "td", "tfoot", "th", "thead",
        "title", "tr", "track", "ul",
    ]

    tag_name : Str -> Str
    tag_name = |t| {
        body = t.drop_prefix("<").drop_prefix("/")
        Lex.take_alpha(Str.to_utf8(body), 0)
    }

    take_alpha : List(U8), U64 -> Str
    take_alpha = |b, i| {
        c = Scan.at(b, i)
        if Scan.is_alpha(c) or (i > 0 and c >= 48 and c <= 57) {
            Lex.take_alpha(b, i + 1)
        } else {
            Str.from_utf8_lossy(List.sublist(b, { start: 0, len: i }))
        }
    }

    ## A blank-line-closed block is closed by the caller's blank-line branch, so
    ## this only answers for the token-closed conditions.
    html_closes : Str, Str -> Bool
    html_closes = |t, ending|
        if ending == "BLANK" or Str.is_empty(ending) {
            False
        } else {
            t.contains(ending)
        }

    ## Would this line open a block of its own? Used only to decide whether a
    ## quote's lazy continuation ends.
    ## Takes the RAW line: four spaces of indent is code, so a marker there
    ## starts nothing and cannot end a quote's lazy continuation.
    starts_block : Str -> Bool
    starts_block = |raw| {
        t = raw.trim()
        if Lex.indent(raw, 0) >= 4 {
            False
        } else
        Lex.is_rule(t)
        or t.starts_with("#")
        or t.starts_with(">")
        or t.starts_with("|")
        or Lex.opens_fence(t)
        or Lex.html_start(t) != ""
        or (
            match Lex.marker(t) {
                NotList => False
                _ => True
            }
        )
    }

    ## Strip a trailing all-`#` run, which must be preceded by a space (or be
    ## the whole content) to count. `foo#` keeps its hash.
    drop_closing : Str -> Str
    drop_closing = |t| {
        b = Str.to_utf8(t)
        n = Lex.trailing_hashes(b, List.len(b), 0)
        if n == 0 {
            t
        } else if n == List.len(b) {
            ""
        } else if List.get(b, List.len(b) - n - 1) == Ok(32) {
            Str.from_utf8_lossy(List.sublist(b, { start: 0, len: List.len(b) - n })).trim()
        } else {
            t
        }
    }

    ## The opening run's length. The old helper compared the WHOLE string against
    ## "#", "##", … because it was handed the first word; counting bytes is what
    ## lets the closing sequence be stripped without splitting on a space first.
    ##
    ## Counted in U64, not in the U8 a level fits in: the run is whatever the
    ## line holds, and a line of 256 `#` overflowed a U8 counter and crashed.
    leading_hashes : List(U8), U64 -> U64
    leading_hashes = |b, acc|
        if List.get(b, acc) == Ok(35) {
            Lex.leading_hashes(b, acc + 1)
        } else {
            acc
        }

    trailing_hashes : List(U8), U64, U64 -> U64
    trailing_hashes = |b, len, acc|
        if acc >= len {
            acc
        } else if List.get(b, len - acc - 1) == Ok(35) {
            Lex.trailing_hashes(b, len, acc + 1)
        } else {
            acc
        }
}
