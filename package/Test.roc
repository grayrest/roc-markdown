## The package's gate. `just test` runs this.
##
## **No platform, no host, no glue, no window.** The parser depends on nothing
## but the stdlib, so this is the first test in this repo that runs without
## building anything — which is also the property that makes the package
## publishable rather than merely extracted.
##
## Each block below states what it is defending. A test whose failure would not
## tell you which decision broke is not worth the line it costs.
import Span exposing [Span]
import Inl exposing [Inl]
import Block exposing [Block]
import Markdown
import Scan
import Html
import Loc

Test := [].{
    ## Blocks rendered as a compact shape string, so an assertion reads as the
    ## structure it is checking rather than as a wall of constructors.
    shape : Str -> Str
    shape = |src| Str.join_with(List.map(Markdown.parse(src), Test.tag), "|")

    tag : Block -> Str
    tag = |b|
        match b.node {
            Para(sp) => "P(${Test.text(sp)})"
            Heading(l, sp) => "H${l.to_str()}(${Test.text(sp)})"
            Code(c) => "C[${c.lang}](${c.text})"
            Quote(bs) => "Q(${Str.join_with(List.map(bs, Test.tag), "|")})"
            Items(l) => {
                ## `Lo` for ordered, `L` for bulleted — the entries no longer
                ## carry it, so the LIST has to say it or the distinction stops
                ## being asserted anywhere.
                k = if l.ordered {
                    "Lo"
                } else {
                    "L"
                }
                "${k}(${Str.join_with(List.map(l.entries, Test.entry), ",")})"
            }
            Table(t) => "T${t.ncols.to_str()}(${Str.join_with(List.map(t.cells, Test.text), ",")})"
            Html(h) => "X(${h})"
            Rule => "R"
        }

    ## Each block's kind only, with a table's width and height: for inputs too
    ## big to spell out in a shape string.
    outline : Str -> Str
    outline = |src| Str.join_with(List.map(Markdown.parse(src), Test.kind), "|")

    kind : Block -> Str
    kind = |b|
        match b.node {
            Table(t) => "T${t.ncols.to_str()}x${(List.len(t.cells) // Test.at_least_one(t.ncols)).to_str()}"
            Para(_) => "P"
            Quote(bs) => "Q(${Str.join_with(List.map(bs, Test.kind), "|")})"
            Items(l) => "L(${Str.join_with(List.map(l.entries, |e| Str.join_with(List.map(e, Test.kind), "+")), ",")})"
            _ => "B"
        }

    at_least_one : U64 -> U64
    at_least_one = |n| if n == 0 { 1 } else { n }

    ## An entry is a list of BLOCKS now, so it renders as its blocks — which is
    ## what makes a nested list visible in the shape string instead of a depth
    ## number that had to be read against its neighbours.
    entry : List(Block) -> Str
    entry = |bs| Str.join_with(List.map(bs, Test.tag), "+")

    ## Through `Span.flatten`, deliberately: these assertions are about what a
    ## NATIVE renderer receives, and flattening is that lowering. A soft break
    ## becomes a space here for the same reason it does on screen.
    text : List(Inl) -> Str
    text = |sp| Str.join_with(List.map(Span.flatten(sp), |s| s.text), "")

    html : Str -> Str
    html = |src| Markdown.to_html(Markdown.parse(src))

    ## With raw HTML passed through, so an assertion sees where a raw span
    ## ends, which `html` hides behind one placeholder.
    unsafe_html : Str -> Str
    unsafe_html = |src| Markdown.to_html_unsafe(Markdown.parse(src))

    ## The inline TREE of the first block, with each container marked by its
    ## kind: `a **b** c` is `a S[b] c`. `text` flattens that structure away, so
    ## a test through it passes whether the parser built a `Strong` or not.
    tree : Str -> Str
    tree = |src|
        match List.first(Markdown.parse(src)) {
            Ok(b) =>
                match b.node {
                    Para(ns) => Test.nodes(ns)
                    _ => ""
                }
            Err(_) => ""
        }

    nodes : List(Inl) -> Str
    nodes = |ns| Str.join_with(List.map(ns, Test.node), "")

    node : Inl -> Str
    node = |n|
        match n.node {
            Text(t) => t
            Code(t) => "C[${t}]"
            Emph(ks) => "E[${Test.nodes(ks)}]"
            Strong(ks) => "S[${Test.nodes(ks)}]"
            Strike(ks) => "X[${Test.nodes(ks)}]"
            Link(l) => "A[${Test.nodes(l.kids)}](${l.href})"
            Image(m) => "I[${Test.nodes(m.kids)}](${m.src})"
            SoftBreak => " "
            HardBreak => "\n"
            Raw(r) => r
            Task(_) => "T"
        }
}

## --- paragraph merging -------------------------------------------------------
##
## The corpus is hard-wrapped throughout. Without merging, every paragraph
## renders as a ragged stack of one-line blocks.
expect Test.shape("one two\nthree four") == "P(one two three four)"
expect Test.shape("a\n\nb") == "P(a)|P(b)"

## --- headings ----------------------------------------------------------------
expect Test.shape("# Title") == "H1(Title)"
expect Test.shape("###### Deep") == "H6(Deep)"
## Not a heading: no space after the marker, and seven is not a level.
expect Test.shape("#hashtag") == "P(#hashtag)"
expect Test.shape("####### seven") == "P(####### seven)"

## --- rules -------------------------------------------------------------------
expect Test.shape("---") == "R"
expect Test.shape("-----") == "R"

## --- fences ------------------------------------------------------------------
##
## The language tag is carried (99 of 169 fences in the corpus name one), and
## the body is raw: `**` inside a fence is literal, which is the entire point.
expect Test.shape("```roc\nx = 1\n```") == "C[roc](x = 1)"
expect Test.shape("```\n**not bold**\n```") == "C[](**not bold**)"
## An unterminated fence still emits its code rather than swallowing it.
expect Test.shape("```zig\nleft open") == "C[zig](left open)"

## --- lists -------------------------------------------------------------------
expect Test.shape("- a\n- b") == "L(P(a),P(b))"
expect Test.shape("1. a\n2. b") == "Lo(P(a),P(b))"
## Nesting is retained, not flattened — the corpus nests to depth 4.
## Nesting is STRUCTURAL now, which the shape string shows directly instead of
## as depth numbers that had to be read against their neighbours.
expect Test.shape("- a\n  - b\n    - c") == "L(P(a)+L(P(b)+L(P(c))))"
## A wrapped bullet is ONE entry. Treated naively the continuation line becomes
## a paragraph, which splits the bullet AND closes the list.
expect Test.shape("- first line\n  second line\n- next") == "L(P(first line second line),P(next))"
## `e.g.` and a sentence containing a number are not ordered items.
expect Test.shape("e.g. thing") == "P(e.g. thing)"
expect Test.shape("Note 1. thing") == "P(Note 1. thing)"

## --- tables ------------------------------------------------------------------
expect Test.shape("| a | b |\n|---|---|\n| c | d |") == "T2(a,b,c,d)"
## Ragged rows are padded to the header's width, so `cells.len() == nrows*ncols`
## holds and every renderer is a plain loop.
expect Test.shape("| a | b |\n|---|---|\n| c |") == "T2(a,b,c,)"
expect Test.shape("| a | b |\n|---|---|\n| c | d | e |") == "T2(a,b,c,d)"
## Padding is the one cost a table does not pay for in source bytes: a header
## of 2000 cells over 2000 one-cell rows would be four million cells. The rows
## that fit the padding budget stay the table and the rest become prose.
## should_end_a_table_where_its_padding_passes_the_budget
expect
    Test.outline("|${Str.repeat("a|", 2000)}\n|${Str.repeat("-|", 2000)}\n${Str.repeat("a\n", 2000)}")
    == "T2000x33|P"

## --- quotes ------------------------------------------------------------------
expect Test.shape("> quoted") == "Q(P(quoted))"

## --- inline ------------------------------------------------------------------
expect Test.tree("a **b** c") == "a S[b] c"
expect Test.tree("a *b* c") == "a E[b] c"

## --- negative cases ----------------------------------------------------------
##
## An unsafe href keeps its LABEL and loses its href, becoming ordinary text.
## This is the only defence against a `javascript:` URL reaching a browser
## through `to_html`, so it is asserted on both sides of the package.
## The label comes back as TEXT. A count alone cannot say so: an accepted
## link is one node too.
expect Test.nodes(Scan.run("[x](javascript:void)", Dict.empty(), Any)) == "x"
expect Test.html("[x](javascript:void)") == "<p>x</p>\n"
expect Test.html("[x](data:text/html,y)") == "<p>x</p>\n"
expect Test.html("[x](vbscript:y)") == "<p>x</p>\n"
## Parentheses inside a destination are matched by BALANCE, so the whole of
## `javascript:alert(1)` reaches `safe_href` and is rejected as one thing. The
## label survives and no stray `)` leaks — the cosmetic wart the split-on-`)`
## parser left behind, and which its own note predicted would need this rewrite.
expect Test.html("[x](javascript:alert(1))") == "<p>x</p>\n"
expect Test.html("[x](https://e.com)") == "<p><a href=\"https://e.com\">x</a></p>\n"
## An image is an `<img>`, with the label's TEXT as its alt. It used to degrade
## to bare alt text because a flat `Span` could not represent a box; the inline
## tree can, and the spec requires it. `Span.flatten` still lowers an image to
## its alt text, so native renderers see what they always did.
expect Test.html("![alt](https://e.com/i.png)")
    == "<p><img src=\"https://e.com/i.png\" alt=\"alt\" /></p>\n"

## --- html escaping -----------------------------------------------------------
##
## 77 of 93 corpus files contain `<`, `>` or `&`. `&` must be replaced first or
## the replacements re-escape themselves.
expect Test.html("a < b & c") == "<p>a &lt; b &amp; c</p>\n"
## The block's content ends with a newline, as a last line does.
expect Test.html("```rust\nVec<&str>\n```")
    == "<pre><code class=\"language-rust\">Vec&lt;&amp;str&gt;\n</code></pre>\n"
expect Test.html("plain `x` end") == "<p>plain <code>x</code> end</p>\n"

## --- html structure ----------------------------------------------------------
expect Test.html("# T") == "<h1>T</h1>\n"
expect Test.html("---") == "<hr />\n"
expect Test.html("**b** and *i*") == "<p><strong>b</strong> and <em>i</em></p>\n"
expect Test.html("| a |\n|---|\n| b |")
    == "<table>\n<thead>\n<tr>\n<th>a</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>b</td>\n</tr>\n</tbody>\n</table>\n"
## Nesting becomes a sub-list INSIDE the preceding entry, not a sibling of it —
## the outline a browser derives has to match the one the source wrote.
expect Test.html("- a\n  - b\n- c")
    == "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n<li>c</li>\n</ul>\n"
expect Test.html("1. a\n2. b") == "<ol>\n<li>a</li>\n<li>b</li>\n</ol>\n"

## --- blockquotes span lines -------------------------------------------------
##
## `> a` / `> b` is ONE quote. A block per line renders as a stack of separate
## quote boxes, which is what the corpus showed.
expect Test.shape("> one\n> two") == "Q(P(one two))"
expect Test.shape("> one\n\n> two") == "Q(P(one))|Q(P(two))"
## LAZY CONTINUATION: a plain line under an open quote belongs to the quote's
## paragraph. This used to end the quote and start a new one, which is the
## reading a line-at-a-time parser falls into and the spec explicitly rejects.
expect Test.shape("> quoted\nplain") == "Q(P(quoted plain))"
## The soft break SURVIVES into HTML as a newline, which is what the spec
## expects; `Span.flatten` is what turns it back into a space for a reflowing
## native renderer. The merge moved from the parser to the lowering.
expect Test.html("> a\n> b") == "<blockquote>\n<p>a\nb</p>\n</blockquote>\n"

## --- escaped pipes in table cells --------------------------------------------
##
## `\|` is a literal pipe, not a cell delimiter (GFM 4.10). Found by a document
## in the corpus whose cell was `` `\|x\| x` ``: the row split into four cells,
## `fit` truncated it back to two, and the LAST cell of the row was silently
## dropped. A wrong cell count does not fail loudly — it renders a plausible
## table with the wrong contents, which is why these are pinned by cell text and
## not just by column count.
expect Test.shape("| a |\n|---|\n| x \\| y |") == "T1(a,x | y)"
## The reported case: escaped inside a code span. The unescape has to happen
## before the inline phase, or the backslashes survive into the `<code>`.
expect Test.shape("| a | b |\n|---|---|\n| `\\|x\\| x` | q |") == "T2(a,b,|x| x,q)"
expect Test.html("| a |\n|---|\n| `\\|` |")
    == "<table>\n<thead>\n<tr>\n<th>a</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td><code>|</code></td>\n</tr>\n</tbody>\n</table>\n"
## An escaped BACKSLASH does not escape the pipe that follows it: `\\|` is a
## literal `\` and then a delimiter. Without this the scanner would swallow
## every pipe after any `\\` in the row, merging the cells.
##
## The `\\` now COLLAPSES to one backslash: the inline scanner implements
## general backslash escapes, so the cell splitter's `\|` rule and the inline
## `\\` rule compose the way the spec defines them.
expect Test.shape("| a | b |\n|---|---|\n| x \\\\| y |") == "T2(a,b,x \\,y)"
## A trailing `\` is emitted as itself rather than escaping the row's end.
expect Test.shape("| a |\n|---|\n| x \\ |") == "T1(a,x \\)"

## --- table edge cells --------------------------------------------------------
##
## Only an EMPTY cell at either end is a delimiter artefact, so an intentionally
## blank first cell survives. Stripping the outer `|` characters instead cannot
## tell these two rows apart.
expect Test.shape("| a | b |\n|---|---|\n|| y |") == "T2(a,b,,y)"
expect Test.shape("| a | b |\n|---|---|\n| x ||") == "T2(a,b,x,)"
## A row with no outer pipes IS part of the run: once a table is open, any line
## with a pipe continues it. This used to be a paragraph after a one-column
## table, which was the documented divergence from GFM — the cell is fitted to
## the header's width like any other row.
expect Test.shape("| a |\n|---|\nx | y") == "T1(a,x)"

## --- numeric references out of range -----------------------------------------
##
## The spec caps a numeric reference at 7 decimal or 6 hex digits, and anything
## longer is literal text. The digits used to be accumulated in a U64 before the
## range test, so twenty of them overflowed and crashed the parse.
## should_render_an_overlong_decimal_reference_as_text
expect Test.html("&#99999999999999999999;") == "<p>&amp;#99999999999999999999;</p>\n"
## should_render_an_overlong_hex_reference_as_text
expect Test.html("&#x123456789ABCDEF01;") == "<p>&amp;#x123456789ABCDEF01;</p>\n"

## --- a long run of hashes ------------------------------------------------------
##
## A heading marker is at most six hashes, but the run was counted in a U8, so
## a line of 256 or more overflowed the counter instead of being prose.
## should_treat_256_hashes_as_a_paragraph
expect Test.shape(Str.repeat("#", 256)) == "P(${Str.repeat("#", 256)})"
## should_treat_300_hashes_as_a_paragraph
expect Test.shape(Str.repeat("#", 300)) == "P(${Str.repeat("#", 300)})"

## --- nested links in linear time ---------------------------------------------
##
## A link may not contain a link, so every outer bracket around `[a](u)` is
## literal. Deciding that used to scan each label twice — once to find the inner
## link, once again as literal text — so the time doubled per level and thirty
## levels never finished.
## should_parse_thirty_nested_links_as_one_link_in_brackets
expect
    Test.html("${Str.repeat("[", 30)}a${Str.repeat("](u)", 30)}")
    == "<p>${Str.repeat("[", 29)}<a href=\"u\">a</a>${Str.repeat("](u)", 29)}</p>\n"

## --- a label that starts with a URL -------------------------------------------
##
## GFM's bare-URL and email autolinks apply to text, not to a link's own label,
## so `[https://e.com](u)` is a link to `u`. The label used to be scanned as a
## string of its own, where its first byte sat at a word boundary; the URL
## became a link inside the label, and a link may not contain one, so the whole
## thing fell back to text.
## should_link_a_label_that_is_itself_a_url
expect Test.html("[https://e.com](https://e.com)") == "<p><a href=\"https://e.com\">https://e.com</a></p>\n"
## should_link_a_label_that_is_an_email_address
expect Test.html("[a@b.com](https://x.org)") == "<p><a href=\"https://x.org\">a@b.com</a></p>\n"

## --- unmatched openers --------------------------------------------------------
##
## Each of these used to search to the end of the paragraph for a closer that
## was not there, once per opener. They now answer from facts gathered once per
## scan (`Scan.Ctx`); these pin that the shortcut gives the answer the search
## did, including where a closer does exist.
## should_decode_the_longest_named_reference
expect Test.html("&CounterClockwiseContourIntegral;") == "<p>∳</p>\n"
## should_leave_angle_brackets_with_no_closer_literal
expect Test.html("a <<< b") == "<p>a &lt;&lt;&lt; b</p>\n"
## should_leave_brackets_with_no_closer_literal
expect Test.html("a [[[ b") == "<p>a [[[ b</p>\n"
## should_find_a_code_span_past_an_unmatched_longer_run
expect Test.html("``a`b`") == "<p>``a<code>b</code></p>\n"
## should_find_an_email_after_a_failed_one_in_the_same_line
expect Test.html("a_b c_d@y.com") == "<p>a_b <a href=\"mailto:c_d@y.com\">c_d@y.com</a></p>\n"
## should_leave_an_email_with_no_dotted_domain_literal
expect Test.html("a_b_c@x") == "<p>a_b_c@x</p>\n"
## should_pair_emphasis_after_unmatched_closers
expect Test.tree("a* a* *b*") == "a* a* E[b]"
## should_pair_each_of_many_sequential_emphases
expect Test.tree("*a* *b* **c**") == "E[a] E[b] S[c]"

## --- a long paragraph ----------------------------------------------------------
##
## Every token but plain text and delimiters went through a helper that called
## back into the scanner, so the stack grew by a frame per token and a
## paragraph of about 6k escapes (or lines, or links) overflowed it.
## should_scan_a_paragraph_of_twenty_thousand_escapes
expect Test.html(Str.repeat("\\*", 20000)) == "<p>${Str.repeat("*", 20000)}</p>\n"
## The emphasis pass had the same shape: pairing or demoting a closer called
## back into the walk, a frame per closer, and 20,000 `*a*` on one line (80 KB)
## overflowed the stack.
## should_pair_twenty_thousand_emphases_in_one_paragraph
expect Test.html(Str.repeat("*a* ", 20000)) == "<p>${Str.repeat("<em>a</em> ", 19999)}<em>a</em></p>\n"

## --- nesting past the cap -------------------------------------------------------
##
## Every walk of the finished tree recurses once per level, and nothing bounded
## the levels: a line of 2000 `> ` or `- `, or 20,000 `*` either side of a
## word, overflowed the stack as a SIGSEGV. Past the cap a marker is text.
## should_stop_nesting_quotes_at_the_cap
expect Test.outline(Str.repeat("> ", 2000)) == "${Str.repeat("Q(", 100)}P${Str.repeat(")", 100)}"
## should_stop_nesting_lists_at_the_cap
expect Test.outline("${Str.repeat("- ", 2000)}a") == "${Str.repeat("L(", 50)}P${Str.repeat(")", 50)}"
## should_stop_nesting_emphasis_at_the_cap
expect
    Test.html("${Str.repeat("*", 20000)}a${Str.repeat("*", 20000)}")
    == "<p>${Str.repeat("*", 19800)}${Str.repeat("<strong>", 100)}a${Str.repeat("</strong>", 100)}${Str.repeat("*", 19800)}</p>\n"
## Separate runs nest the same way, one level per pair; past the cap each
## closer is text, and the floor it sets keeps the rest from searching.
## should_stop_nesting_separate_emphasis_runs_at_the_cap
expect
    Test.html("${Str.repeat("*a ", 150)}${Str.repeat(" a*", 150)}")
    == "<p>${Str.repeat("*a ", 50)}${Str.repeat("<em>a ", 100)}${Str.repeat(" a</em>", 100)}${Str.repeat(" a*", 50)}</p>\n"

## --- trimming a bare URL's tail ------------------------------------------------
##
## An unbalanced `)` comes off a bare URL's end one at a time, and each step
## recounted the parens over the whole candidate: `https://a` and 20,000 `)`
## took about forty seconds.
## should_trim_twenty_thousand_closing_parens_off_a_bare_url
expect
    Test.html("https://a${Str.repeat(")", 20000)}")
    == "<p><a href=\"https://a\">https://a</a>${Str.repeat(")", 20000)}</p>\n"

## --- many reference definitions ------------------------------------------------
##
## A footer of consecutive `[n]: url` lines re-joined the rest of its run for
## every definition it stripped, and every `[n]` then searched the definitions
## in order: ten thousand of each took minutes.
## should_resolve_ten_thousand_definitions_in_one_run
expect {
    labels = List.map_with_index(List.repeat(0, 10000), |_, i| i.to_str())
    defs = Str.join_with(List.map(labels, |k| "[${k}]: /${k}"), "\n")
    refs = Str.join_with(List.map(labels, |k| "[${k}]"), " ")
    links = Str.join_with(List.map(labels, |k| "<a href=\"/${k}\">${k}</a>"), " ")
    Test.html("${defs}\n\n${refs}") == "<p>${links}</p>\n"
}

## --- openers before one closer -------------------------------------------------
##
## Knowing that no `>` or `]` is left answers an unmatched opener only when
## there is no closer at all. One closer at the end of the paragraph put every
## opener back to searching up to it, and each `<` then sliced and checked
## everything in between.
## should_scan_twenty_thousand_tag_openers_before_one_closer
expect Test.html("${Str.repeat("<a", 20000)}>") == "<p>${Str.repeat("&lt;a", 19999)}<!-- raw HTML omitted --></p>\n"
## should_scan_thirty_thousand_bracket_openers_before_one_closer
expect Test.html("${Str.repeat("[", 30000)}]") == "<p>${Str.repeat("[", 30000)}]</p>\n"
## A walk popped its stack of open brackets to a slice, and the next `[`
## pushed onto the slice copied the whole stack, so a walk through nested
## full references was quadratic: 100,000 took five seconds. Marking the
## openers left unclosed was quadratic too, but only in an optimized build,
## which this cannot pin.
## should_walk_a_hundred_thousand_nested_full_references
expect
    Test.html("${Str.repeat("[", 100000)}a${Str.repeat("][x]", 100000)}")
    == "<p>${Str.repeat("[", 100000)}a${Str.repeat("][x]", 100000)}</p>\n"
## should_scan_a_label_of_twenty_thousand_tag_openers
expect Test.html("[${Str.repeat("<a", 20000)}>]") == "<p>[${Str.repeat("&lt;a", 19999)}<!-- raw HTML omitted -->]</p>\n"
## should_scan_ten_thousand_cdata_openers_before_one_closer
expect Test.html("a ${Str.repeat("<![CDATA[", 10000)}>") == "<p>a ${Str.repeat("&lt;![CDATA[", 10000)}&gt;</p>\n"

## --- many brackets in one paragraph ----------------------------------------------
##
## The scanner kept its context after handing it to a bracket walk, so the
## walk's memo of where each bracket closes was shared, and every answer it
## recorded copied the whole memo: 40,000 `[a][b]` took eleven seconds.
## should_resolve_forty_thousand_full_references_in_one_paragraph
expect Test.html("${Str.repeat("[a][b] ", 40000)}\n\n[b]: /u") == "<p>${Str.repeat("<a href=\"/u\">a</a> ", 39999)}<a href=\"/u\">a</a></p>\n"
## A label scan got the context as a copy with its own `end`, which left the
## caller's copy sharing the memo, so the label's first walk copied all of
## it. A label walks where the scanner and the walk disagree, as they do
## about a refused autolink: 80,000 `[<javascript:[y]>](u)` took fourteen
## seconds.
## should_link_eighty_thousand_labels_holding_a_refused_autolink
expect
    Test.html(Str.repeat("[<javascript:[y]>](u) ", 80000))
    == "<p>${Str.repeat("<a href=\"u\">&lt;javascript:[y]&gt;</a> ", 79999)}<a href=\"u\">&lt;javascript:[y]&gt;</a></p>\n"

## --- how long a link label may be ----------------------------------------------
##
## A label holds at most 999 characters. Without the limit every `[` of
## nested brackets was a shortcut candidate whose whole label was sliced and
## folded, so 15,000 of them took ten seconds.
## should_scan_fifteen_thousand_nested_brackets
expect
    Test.html("${Str.repeat("[", 15000)}a${Str.repeat("]", 15000)}")
    == "<p>${Str.repeat("[", 15000)}a${Str.repeat("]", 15000)}</p>\n"
## One character over, neither the reference nor the definition is one.
## should_leave_a_thousand_character_shortcut_reference_as_text
expect
    Test.html("[${Str.repeat("a", 1000)}]\n\n[${Str.repeat("a", 1000)}]: /u")
    == "<p>[${Str.repeat("a", 1000)}]</p>\n<p>[${Str.repeat("a", 1000)}]: /u</p>\n"
## should_leave_a_full_reference_with_a_thousand_character_label_as_text
expect
    Test.html("[x][${Str.repeat("a", 1000)}]\n\n[${Str.repeat("a", 1000)}]: /u")
    == "<p>[x][${Str.repeat("a", 1000)}]</p>\n<p>[${Str.repeat("a", 1000)}]: /u</p>\n"
## should_link_a_label_of_nine_hundred_ninety_nine_characters
expect
    Test.html("[${Str.repeat("a", 999)}]\n\n[${Str.repeat("a", 999)}]: /u")
    == "<p><a href=\"/u\">${Str.repeat("a", 999)}</a></p>\n"
## Characters, not bytes: 999 `é` are 1998 bytes and still a label.
## should_count_a_label_in_characters_not_bytes
expect
    Test.html("[${Str.repeat("é", 999)}]\n\n[${Str.repeat("é", 999)}]: /u")
    == "<p><a href=\"/u\">${Str.repeat("é", 999)}</a></p>\n"

## --- backtick runs inside a bracket --------------------------------------------
##
## A bracket walk steps over a code span, and it searched to the end of the text
## for the closing run of every opening one, even where the scan's own table
## already said no run of that length follows: a label holding runs of every
## length up to 1400 (about 1 MB) took seventeen seconds.
## should_walk_a_label_of_fourteen_hundred_unclosed_backtick_runs
expect {
    runs = Str.join_with(List.map_with_index(List.repeat(0, 1400), |_, k| "${Str.repeat("`", k + 1)}a"), "")
    Test.html("[${runs}]") == "<p>[${runs}]</p>\n"
}

## --- inline link destinations --------------------------------------------------
##
## The `)` closing a destination is found by counting parens, and each `<` jumps
## to the `>` that ends an angle-bracketed destination. The jump searched to the
## end of the text when no `>` followed, and to the same far `>` for every `<`
## when one did; and with no cap on the count, every `[a](` in a paragraph of
## them walked to its end one level deeper. Each took seventeen to thirty
## seconds.
## should_scan_sixty_thousand_angle_openers_in_a_destination
expect Test.html("[a](${Str.repeat("<", 60000)}") == "<p>[a](${Str.repeat("&lt;", 60000)}</p>\n"
## should_scan_twenty_thousand_unclosed_inline_links
expect Test.html(Str.repeat("[a](", 20000)) == "<p>${Str.repeat("[a](", 20000)}</p>\n"
## should_scan_twenty_thousand_unclosed_angle_destinations_before_one_closer
expect Test.html("${Str.repeat("[a](<", 20000)}>") == "<p>${Str.repeat("[a](&lt;", 20000)}&gt;</p>\n"
## Parens nest 32 deep inside a destination, as in cmark; the spec asks for at
## least three. One more and the parens never balance, so it is not a link.
## should_link_thirty_two_nested_parens_but_not_thirty_three
expect {
    nested = |n| "${Str.repeat("(", n)}u${Str.repeat(")", n)}"
    Test.html("[a](${nested(32)})") == "<p><a href=\"${nested(32)}\">a</a></p>\n"
    and Test.html("[a](${nested(33)})") == "<p>[a](${nested(33)})</p>\n"
}

## --- one frame per repetition ----------------------------------------------------
##
## Three walks recursed once per item without being loops: attribute checking
## and its value reader called each other once per attribute, the bracket
## walk's `>` finder and its quote reader once per quote pair, and table
## rendering put each row in front of the rest. Each overflowed the stack.
## should_check_a_tag_of_two_hundred_thousand_attributes
expect Test.html("a <a${Str.repeat(" b=\"c\"", 200000)}>") == "<p>a <!-- raw HTML omitted --></p>\n"
## should_walk_a_label_holding_half_a_million_quote_pairs
expect Test.html("[<a ${Str.repeat("\"x\"", 500000)}>]") == "<p>[&lt;a ${Str.repeat("&quot;x&quot;", 500000)}&gt;]</p>\n"
## Built by hand: parsing a table this long is slow under `roc test`, and
## rendering is what overflowed.
## should_render_a_table_of_two_hundred_thousand_rows
expect {
    cell = [Inl.mk(Loc.none(0), Text("a"))]
    table = Block.mk(Loc.none(0), Table({ ncols: 1, align: [None], cells: List.repeat(cell, 200001) }))
    Html.render([table], True) == "<table>\n<thead>\n<tr>\n<th>a</th>\n</tr>\n</thead>\n<tbody>\n${Str.repeat("<tr>\n<td>a</td>\n</tr>\n", 200000)}</tbody>\n</table>\n"
}

## --- links and images past the nesting cap -------------------------------------
##
## A link or an image counted as a leaf toward the emphasis cap, and scanning
## a label recursed once per bracket, so 5000 nested images, or 5000 images
## nested inside emphasis, overflowed the stack. Past the cap a bracket is text.
## should_stop_nesting_images_at_the_cap
expect
    Test.html("${Str.repeat("![", 5000)}a${Str.repeat("](u)", 5000)}")
    == "<p><img src=\"u\" alt=\"${Str.repeat("![", 4900)}a${Str.repeat("](u)", 4900)}\" /></p>\n"
## An image is one level, so emphasis around it stops one level sooner.
## should_count_an_image_toward_the_emphasis_cap
expect
    Test.html("${Str.repeat("*", 250)}![a](u)${Str.repeat("*", 250)}")
    == "<p>${Str.repeat("*", 52)}${Str.repeat("<strong>", 99)}<img src=\"u\" alt=\"a\" />${Str.repeat("</strong>", 99)}${Str.repeat("*", 52)}</p>\n"
## Images and emphasis alternate here. The scan stops 100 labels deep, so the
## 4900 brackets inside that are text; the 50 images and 50 `<em>` around them
## fill the cap, so the 50 brackets outside those are text too.
## should_stop_nesting_images_inside_emphasis_at_the_cap
expect
    Test.html("${Str.repeat("![*", 5000)}a${Str.repeat("*](u)", 5000)}")
    == "<p>${Str.repeat("![<em>![</em>", 25)}<img src=\"u\" alt=\"${Str.repeat("![", 4900)}a${Str.repeat("](u)", 4900)}\" />${Str.repeat("<em>](u)</em>](u)", 25)}</p>\n"
## A label too tall to be a link is text, and so is every label around it,
## but each of those paired the refused label's emphasis again to find that
## out: 99 images around one, beside 25,000 `*a*`, took thirteen seconds.
## should_refuse_labels_around_a_tall_label_without_pairing_it_again
expect
    Test.html("${Str.repeat("![", 99)}${Str.repeat("*", 200)}x${Str.repeat("*", 200)} ${Str.repeat("*a* ", 25000)}${Str.repeat("](u)", 99)}")
    == "<p>${Str.repeat("![", 99)}${Str.repeat("<strong>", 100)}x${Str.repeat("</strong>", 100)} ${Str.repeat("<em>a</em> ", 25000)}${Str.repeat("](u)", 99)}</p>\n"

## --- comments, instructions and declarations --------------------------------------
##
## CommonMark ends each at the first occurrence of its own terminator: a comment
## at `-->`, an instruction at `?>`, CDATA at `]]>`, a declaration (`<!` and a
## letter) at `>`. Quotes mean nothing inside any of them. The scanner paired
## quotes in all four as if in a tag's attributes, so every `<!"` of a
## paragraph re-paired the quotes after it, and 20,000 took fifteen seconds.
## should_scan_twenty_thousand_bang_quote_openers
expect Test.html("a ${Str.repeat("<!\"", 20000)}'>") == "<p>a ${Str.repeat("&lt;!&quot;", 20000)}'&gt;</p>\n"
## should_leave_a_bang_without_a_letter_as_text
expect Test.unsafe_html("a <!\"x\"> b") == "<p>a &lt;!&quot;x&quot;&gt; b</p>\n"
## should_end_a_declaration_at_its_first_gt_even_inside_quotes
expect Test.unsafe_html("a <!x \"y>z\"> b") == "<p>a <!x \"y>z&quot;&gt; b</p>\n"
## should_keep_a_gt_inside_a_comment
expect Test.unsafe_html("a <!-- x > y --> b") == "<p>a <!-- x > y --> b</p>\n"
## should_keep_a_gt_inside_a_processing_instruction
expect Test.unsafe_html("a <?x > y ?> b") == "<p>a <?x > y ?> b</p>\n"
## should_leave_an_unterminated_processing_instruction_as_text
expect Test.unsafe_html("a <?x> b") == "<p>a &lt;?x&gt; b</p>\n"
## should_leave_an_empty_declaration_as_text
expect Test.unsafe_html("a <!> b") == "<p>a &lt;!&gt; b</p>\n"
