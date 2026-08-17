## Reading a link reference definition — `[foo]: /url "title"`.
##
## Recognising one, and reporting how many lines it ate. Nothing here searches
## for definitions or decides where they may appear
##
## Every part of a definition may span lines — the label (`[Foo\n  bar]:`), the
## destination, the title — so these work over a run joined with newlines rather
## than line by line.
import Ref
import Lex
import Scan
import Ents

Defs := [].{
    ## How many lines, from the front, are link reference definitions.
    lead_lines : List(Str), U64 -> U64
    lead_lines = |lines, n|
        match Defs.def_at(lines) {
            Ok(d) => Defs.lead_lines(List.drop_first(lines, d.used), n + d.used)
            Err(_) => n
        }

    ## A definition starting at the head of `lines`, and how many lines it ate.
    ##
    ## Works on the RUN joined with newlines rather than line by line, because
    ## every part of a definition may span lines: the label (`[Foo\n  bar]:`),
    ## the destination, and the title. A bounded lookahead could read the common
    ## shapes and no others.
    ##
    ## The run stops at the first blank line — a definition cannot contain one —
    ## which is what keeps this from scanning the whole document per candidate.
    def_at : List(Str) -> Try({ def : Ref.Def, used : U64 }, [NotDef])
    def_at = |lines| {
        run = Defs.until_blank(lines, [])
        joined = Str.join_with(run, "\n")
        b = Str.to_utf8(joined)
        if !joined.trim().starts_with("[") {
            Err(NotDef)
        } else {
            start = Lex.indent(joined, 0)
            match Defs.find_label_end(b, start + 1) {
                Ok(close) => {
                    label = Scan.slice(b, start + 1, close)
                    after = Defs.skip_ws(b, close + 2)
                    parts = Defs.dest_at(b, after)
                    ## A label of nothing but whitespace is not a label.
                    if Str.is_empty(Scan.fold_label(label)) or !parts.ok {
                        Err(NotDef)
                    } else {
                        ## A title must be SEPARATED from the destination, or
                        ## `[foo]: <bar>(baz)` is a definition titled `baz`.
                        tstart = Defs.skip_ws(b, parts.next)
                        spaced = tstart > parts.next or tstart >= List.len(b)
                        title = if spaced {
                            Defs.title_at(b, tstart)
                        } else {
                            { text: "", next: parts.next, ok: False }
                        }
                        ## A quote that opens and never closes invalidates the
                        ## whole definition — it is not a definition with a
                        ## ragged title.
                        dangling = spaced and !title.ok and Defs.opens_title(Scan.at(b, tstart))
                        ## Only the rest of the LINE has to be clear. Anything
                        ## after that is the next definition, or prose, and
                        ## belongs to whoever comes next.
                        after_title = Defs.rest_of_line_clear(b, title.next)
                        after_dest = Defs.rest_of_line_clear(b, parts.next)
                        keep_title = title.ok and after_title
                        stop = if keep_title {
                            title.next
                        } else {
                            parts.next
                        }
                        if dangling or !(keep_title or after_dest) {
                            Err(NotDef)
                        } else {
                            Ok({
                                def: {
                                    label: Scan.fold_label(label),
                                    dest: Scan.unescape(Ents.decode(parts.dest)),
                                    title: if keep_title {
                                        Scan.unescape(Ents.decode(title.text))
                                    } else {
                                        ""
                                    },
                                },
                                used: Defs.line_of(b, stop) + 1,
                            })
                        }
                    }
                }
                Err(_) => Err(NotDef)
            }
        }
    }

    ## The lines up to (not including) the first blank one.
    until_blank : List(Str), List(Str) -> List(Str)
    until_blank = |lines, acc|
        match List.first(lines) {
            Ok(l) =>
                if Str.is_empty(l.trim()) {
                    acc
                } else {
                    Defs.until_blank(List.drop_first(lines, 1), List.append(acc, l))
                }
            Err(_) => acc
        }

    ## The `]` that closes a label, i.e. one followed by `:` and not escaped.
    find_label_end : List(U8), U64 -> Try(U64, [NotDef])
    find_label_end = |b, i|
        if i >= List.len(b) {
            Err(NotDef)
        } else if Scan.at(b, i) == 92 {
            Defs.find_label_end(b, i + 2)
        } else if Scan.at(b, i) == 91 {
            Err(NotDef)
        } else if Scan.at(b, i) == 93 and Scan.at(b, i + 1) == 58 {
            Ok(i)
        } else {
            Defs.find_label_end(b, i + 1)
        }

    ## A destination at `i`: angle-bracketed, or a run with no whitespace.
    dest_at : List(U8), U64 -> { dest : Str, next : U64, ok : Bool }
    dest_at = |b, i|
        if Scan.at(b, i) == 60 {
            match Scan.find_unescaped(b, i + 1, 62) {
                Ok(gt) => { dest: Scan.slice(b, i + 1, gt), next: gt + 1, ok: True }
                Err(_) => { dest: "", next: i, ok: False }
            }
        } else {
            e = Defs.until_ws(b, i)
            { dest: Scan.slice(b, i, e), next: e, ok: e > i }
        }

    ## A quoted or parenthesised title at `i`, which may span lines.
    title_at : List(U8), U64 -> { text : Str, next : U64, ok : Bool }
    title_at = |b, i| {
        open = Scan.at(b, i)
        close = if open == 40 {
            41
        } else {
            open
        }
        if i >= List.len(b) or !(open == 34 or open == 39 or open == 40) {
            { text: "", next: i, ok: False }
        } else {
            match Scan.find_unescaped(b, i + 1, close) {
                Ok(e) => { text: Scan.slice(b, i + 1, e), next: e + 1, ok: True }
                Err(_) => { text: "", next: i, ok: False }
            }
        }
    }

    skip_ws : List(U8), U64 -> U64
    skip_ws = |b, i|
        if i < List.len(b) and Scan.is_space(Scan.at(b, i)) {
            Defs.skip_ws(b, i + 1)
        } else {
            i
        }

    until_ws : List(U8), U64 -> U64
    until_ws = |b, i|
        if i < List.len(b) and !Scan.is_space(Scan.at(b, i)) {
            Defs.until_ws(b, i + 1)
        } else {
            i
        }

    ## How many newlines precede `i` — the count of lines the definition ate.
    line_of : List(U8), U64 -> U64
    line_of = |b, i| List.len(List.keep_if(List.sublist(b, { start: 0, len: i }), |c| c == 10))

    opens_title : U8 -> Bool
    opens_title = |c| c == 34 or c == 39 or c == 40

    ## Is everything from `i` to the end of that line blank?
    rest_of_line_clear : List(U8), U64 -> Bool
    rest_of_line_clear = |b, i|
        if i >= List.len(b) or Scan.at(b, i) == 10 {
            True
        } else if Scan.is_space(Scan.at(b, i)) {
            Defs.rest_of_line_clear(b, i + 1)
        } else {
            False
        }
}
