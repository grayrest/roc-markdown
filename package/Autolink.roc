## Autolinks: the `<scheme:…>` and `<a@b.c>` forms, and GFM's bare
## `www.`/`https://` links and email addresses, found at a word boundary.
import Bytes

Autolink := [].{
    ## `www.x.com` and `https://x.com` become links without angle brackets. Only
    ## at a WORD BOUNDARY, or the `www` inside `dowww.x` would start one.
    at_boundary : List(U8), U64 -> Bool
    at_boundary = |b, i|
        if i == 0 {
            True
        } else {
            p = Bytes.at(b, i - 1)
            Bytes.is_space(p) or p == 40 or p == 42 or p == 95 or p == 126
        }

    ## The length of the link starting here, or 0. Trailing punctuation is
    ## excluded — a sentence ending "see https://x.com." does not link the stop
    ## — and so is an unbalanced `)`, so "(see https://x.com)" keeps its paren.
    bare_len : List(U8), U64 -> U64
    bare_len = |b, i| {
        pre = if Bytes.starts(b, i, "https://") {
            8
        } else if Bytes.starts(b, i, "http://") {
            7
        } else if Bytes.starts(b, i, "ftp://") {
            6
        } else if Bytes.starts(b, i, "www.") {
            4
        } else {
            0
        }
        if pre == 0 {
            0
        } else {
            raw = Autolink.run_until_space(b, i + pre, 0)
            if raw == 0 {
                0
            } else {
                Autolink.trim_tail(b, i, pre + raw)
            }
        }
    }

    run_until_space : List(U8), U64, U64 -> U64
    run_until_space = |b, i, acc| {
        c = Bytes.at(b, i + acc)
        if i + acc >= List.len(b) or Bytes.is_space(c) or c == 60 {
            acc
        } else {
            Autolink.run_until_space(b, i, acc + 1)
        }
    }

    ## The parens are counted ONCE, and each `)` trimmed off takes one from the
    ## count. Recounting the candidate per `)` made `https://a` followed by
    ## 50,000 of them quadratic. Nothing else trimmed here is a paren, so the
    ## counts stay true as the tail shrinks.
    trim_tail : List(U8), U64, U64 -> U64
    trim_tail = |b, i, n| Autolink.trim_from(b, i, n, Autolink.count_byte(b, i, n, 40), Autolink.count_byte(b, i, n, 41))

    trim_from : List(U8), U64, U64, U64, U64 -> U64
    trim_from = |b, i, n, opens, closes|
        if n == 0 {
            0
        } else {
            last = Bytes.at(b, i + n - 1)
            ## The entity test comes FIRST: `;` is also trailing punctuation, so
            ## stripping it as punctuation destroyed the `&name;` this is
            ## looking for.
            if last == 59 and Autolink.entity_tail(b, i, n) > 0 {
                Autolink.trim_from(b, i, n - Autolink.entity_tail(b, i, n), opens, closes)
            } else if last == 46 or last == 44 or last == 58 or last == 59 or last == 33 or last == 63 or last == 34 or last == 39 {
                Autolink.trim_from(b, i, n - 1, opens, closes)
            } else if last == 41 and closes > opens {
                Autolink.trim_from(b, i, n - 1, opens, closes - 1)
            } else {
                n
            }
        }

    ## The length of a trailing `&name;`, or 0.
    entity_tail : List(U8), U64, U64 -> U64
    entity_tail = |b, i, n| Autolink.entity_back(b, i, n, 1)

    entity_back : List(U8), U64, U64, U64 -> U64
    entity_back = |b, i, n, acc|
        if acc >= n {
            0
        } else {
            c = Bytes.at(b, i + n - acc - 1)
            if c == 38 {
                acc + 1
            } else if Bytes.is_alpha(c) or (c >= 48 and c <= 57) {
                Autolink.entity_back(b, i, n, acc + 1)
            } else {
                0
            }
        }

    count_byte : List(U8), U64, U64, U8 -> U64
    count_byte = |b, i, n, c|
        List.len(List.keep_if(List.sublist(b, { start: i, len: n }), |x| x == c))

    ## The length of an email starting here (`len`, 0 for none), and how far
    ## its local-part run reached either way (`local`) — the second is what
    ## lets `Scan.tokens` skip the boundaries inside a run that failed.
    ##
    ## Local part, `@`, then a domain carrying a dot. The domain may not end on
    ## a hyphen or underscore, which is what keeps the trailing punctuation of
    ## `a.b-c_d@a.b-` out of the link.
    email_len : List(U8), U64 -> { len : U64, local : U64 }
    email_len = |b, i| {
        local = Autolink.local_len(b, i, 0)
        { len: Autolink.email_rest(b, i, local), local }
    }

    email_rest : List(U8), U64, U64 -> U64
    email_rest = |b, i, local|
        if local == 0 or Bytes.at(b, i + local) != 64 {
            0
        } else {
            dom = Autolink.domain_len(b, i + local + 1, 0)
            last = Bytes.at(b, i + local + dom)
            if dom == 0
            or !Autolink.has_alnum(b, i, i + local)
            or !Bytes.slice(b, i + local + 1, i + local + 1 + dom).contains(".")
            or last == 45
            or last == 95 {
                0
            } else {
                local + 1 + dom
            }
        }

    has_alnum : List(U8), U64, U64 -> Bool
    has_alnum = |b, from, to|
        if from >= to {
            False
        } else {
            c = Bytes.at(b, from)
            if Bytes.is_alpha(c) or (c >= 48 and c <= 57) {
                True
            } else {
                Autolink.has_alnum(b, from + 1, to)
            }
        }

    local_len : List(U8), U64, U64 -> U64
    local_len = |b, i, acc| {
        c = Bytes.at(b, i + acc)
        if Bytes.is_alpha(c) or (c >= 48 and c <= 57) or c == 46 or c == 45 or c == 95 or c == 43 {
            Autolink.local_len(b, i, acc + 1)
        } else {
            acc
        }
    }

    domain_len : List(U8), U64, U64 -> U64
    domain_len = |b, i, acc| {
        c = Bytes.at(b, i + acc)
        if i + acc < List.len(b) and (Bytes.is_alpha(c) or (c >= 48 and c <= 57) or c == 45 or c == 46 or c == 95) {
            Autolink.domain_len(b, i, acc + 1)
        } else if acc > 0 and Bytes.at(b, i + acc - 1) == 46 {
            acc - 1
        } else {
            acc
        }
    }

    ## An autolink is an ABSOLUTE URI — a scheme, then a colon — never a
    ## relative one. The href policy (`Link.allows`) alone is not the test: it
    ## admits scheme-less relatives so that `[x](train.jpg)` works, which made
    ## `<a>` an autolink to a file called `a` instead of a raw HTML tag. This
    ## says what IS an autolink; the policy then says whether it may link.
    is_autolink : Str -> Bool
    is_autolink = |inner| {
        b = Str.to_utf8(inner)
        n = Autolink.scheme_len(b, 0)
        n >= 2 and Bytes.at(b, n) == 58 and !inner.contains(" ") and !inner.contains("<")
    }

    scheme_len : List(U8), U64 -> U64
    scheme_len = |b, i| {
        c = Bytes.at(b, i)
        if Bytes.is_alpha(c) or (i > 0 and ((c >= 48 and c <= 57) or c == 43 or c == 45 or c == 46)) {
            Autolink.scheme_len(b, i + 1)
        } else {
            i
        }
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

    ## The `>` closing an email autolink whose address starts at `from`.
    ## The search gives up at a `<` or whitespace, which no address may hold,
    ## so it costs at most the distance to the next of those.
    email_close : List(U8), U64 -> Try(U64, [NotFound])
    email_close = |b, from|
        match Autolink.address_gt(b, from) {
            Ok(gt) =>
                if Autolink.is_email(Bytes.slice(b, from, gt)) {
                    Ok(gt)
                } else {
                    Err(NotFound)
                }
            Err(_) => Err(NotFound)
        }

    address_gt : List(U8), U64 -> Try(U64, [NotFound])
    address_gt = |b, i|
        if i >= List.len(b) {
            Err(NotFound)
        } else {
            c = Bytes.at(b, i)
            if c == 62 {
                Ok(i)
            } else if c == 60 or Bytes.is_space(c) {
                Err(NotFound)
            } else {
                Autolink.address_gt(b, i + 1)
            }
        }
}
