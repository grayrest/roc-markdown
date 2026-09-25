## The href policy, `Markdown.parse_with({ schemes })`. `just test` runs this.
##
## Asserted through `to_html` because that is where a refused href would do
## harm: a link that survives the policy is an `<a>`, and one that does not is
## its label as text. The default is pinned first so the allowlist cannot
## quietly change what `parse` has always done.
import Markdown
import Link

SchemesTest := [].{
    html : Str -> Str
    html = |src| Markdown.to_html(Markdown.parse(src))

    html_only : List(Str), Str -> Str
    html_only = |list, src| Markdown.to_html(Markdown.parse_with({ schemes: Only(list) }, src))

    any_html : Str -> Str
    any_html = |src| Markdown.to_html(Markdown.parse_with({ schemes: Any }, src))

    ## A link, a denied link, all three autolink forms and a relative link.
    mixed : Str
    mixed = "[a](irc:x) [b](javascript:y) <ftp://z> www.w.com a@b.co [c](/r)"

    mixed_html : Str
    mixed_html = "<p><a href=\"irc:x\">a</a> b <a href=\"ftp://z\">ftp://z</a> <a href=\"http://www.w.com\">www.w.com</a> <a href=\"mailto:a@b.co\">a@b.co</a> <a href=\"/r\">c</a></p>\n"

    ## The allowlist untrusted content is expected to use.
    web : List(Str)
    web = ["http", "https", "mailto"]

    web_html : Str -> Str
    web_html = |src| SchemesTest.html_only(SchemesTest.web, src)

    link : Str -> Str
    link = |href| "<p><a href=\"${href}\">x</a></p>\n"

    text : Str -> Str
    text = |t| "<p>${t}</p>\n"

    img : Str -> Str
    img = |src| "<p><img src=\"${src}\" alt=\"a\" /></p>\n"
}

## --- the default is the denylist ---------------------------------------------

## should_refuse_javascript_by_default
expect SchemesTest.html("[x](javascript:alert(1))") == SchemesTest.text("x")
## should_refuse_a_denied_scheme_in_any_case_by_default
expect SchemesTest.html("[x](JavaScript:y)") == SchemesTest.text("x")
## should_allow_an_unlisted_scheme_by_default
expect SchemesTest.html("[x](irc:chat)") == SchemesTest.link("irc:chat")
## should_link_host_port_by_default
expect SchemesTest.html("[x](localhost:5001/foo)") == SchemesTest.link("localhost:5001/foo")
## should_autolink_an_unlisted_scheme_by_default
expect SchemesTest.html("<irc://x>") == "<p><a href=\"irc://x\">irc://x</a></p>\n"
## should_keep_every_link_form_as_it_was_by_default (the output of `27b1392`)
expect SchemesTest.html(SchemesTest.mixed) == SchemesTest.mixed_html
## should_parse_as_parse_with_any
expect SchemesTest.any_html(SchemesTest.mixed) == SchemesTest.mixed_html

## --- Only: what passes ---------------------------------------------------------

## should_allow_a_listed_scheme
expect SchemesTest.web_html("[x](https://x)") == SchemesTest.link("https://x")
## should_match_a_listed_scheme_ignoring_case
expect SchemesTest.web_html("[x](HTTPS://x)") == SchemesTest.link("HTTPS://x")
## should_match_a_list_entry_ignoring_case
expect SchemesTest.html_only(["HTTPS"], "[x](https://x)") == SchemesTest.link("https://x")
## should_allow_mailto_when_listed
expect SchemesTest.web_html("[x](mailto:a@b)") == SchemesTest.link("mailto:a@b")
## should_allow_an_absolute_path
expect SchemesTest.web_html("[x](/rel)") == SchemesTest.link("/rel")
## should_allow_a_fragment
expect SchemesTest.web_html("[x](#f)") == SchemesTest.link("#f")
## should_allow_a_relative_path
expect SchemesTest.web_html("[x](relative/x)") == SchemesTest.link("relative/x")
## should_allow_a_query
expect SchemesTest.web_html("[x](?q)") == SchemesTest.link("?q")
## should_allow_a_colon_after_a_slash
expect SchemesTest.web_html("[x](./a:b)") == SchemesTest.link("./a:b")

## --- Only: what is refused -----------------------------------------------------

## should_refuse_an_unlisted_scheme
expect SchemesTest.web_html("[x](irc:x)") == SchemesTest.text("x")
## should_refuse_ftp_when_unlisted
expect SchemesTest.web_html("[x](ftp://x)") == SchemesTest.text("x")
## should_read_host_port_as_a_scheme
expect SchemesTest.web_html("[x](localhost:5001/foo)") == SchemesTest.text("x")
## should_refuse_javascript_even_when_listed
expect SchemesTest.html_only(["javascript"], "[x](javascript:alert(1))") == SchemesTest.text("x")
## should_refuse_a_malformed_scheme_a_browser_would_repair
expect SchemesTest.web_html("[x](<java&#9;script:alert(1)>)") == SchemesTest.text("x")
## should_refuse_an_unlisted_scheme_in_a_reference
expect SchemesTest.web_html("[x][r]\n\n[r]: irc:x") == SchemesTest.text("x")

## --- Only: autolinks obey the same policy ------------------------------------

## should_autolink_a_listed_scheme
expect SchemesTest.web_html("<https://x>") == "<p><a href=\"https://x\">https://x</a></p>\n"
## should_leave_an_unlisted_autolink_as_text
expect SchemesTest.web_html("<irc:x>") == SchemesTest.text("&lt;irc:x&gt;")
## should_autolink_an_email_when_mailto_is_listed
expect SchemesTest.web_html("<a@b.co>") == "<p><a href=\"mailto:a@b.co\">a@b.co</a></p>\n"
## should_leave_an_email_autolink_as_text_when_mailto_is_unlisted
expect SchemesTest.html_only(["https"], "<a@b.co>") == SchemesTest.text("&lt;a@b.co&gt;")
## should_link_a_bare_www_url_when_http_is_listed
expect SchemesTest.web_html("see www.x.com now") == "<p>see <a href=\"http://www.x.com\">www.x.com</a> now</p>\n"
## should_leave_a_bare_www_url_as_text_when_http_is_unlisted
expect SchemesTest.html_only(["https"], "see www.x.com now") == SchemesTest.text("see www.x.com now")
## should_leave_a_bare_email_as_text_when_mailto_is_unlisted
expect SchemesTest.html_only(["https"], "mail a@b.co now") == SchemesTest.text("mail a@b.co now")

## A refused autolink is text to the bracket walk too, so a `]` the scanner
## reads as inside a code span does not close the bracket.
## should_close_a_bracket_the_way_the_scanner_reads_a_refused_autolink_under_only
expect SchemesTest.html_only(["https"], "[<ftp:a`b> `]` ](u)") == SchemesTest.text("[&lt;ftp:a<code>b&gt; </code>]` ](u)")

## --- images: filtered only under Only ----------------------------------------
##
## `parse` leaves an image's `src` alone: a browser does not execute
## `javascript:` from `<img src>`, and trusted documents embed `data:` images.

## should_keep_a_data_image_by_default
expect SchemesTest.html("![a](data:image/png;base64,AA)") == SchemesTest.img("data:image/png;base64,AA")
## should_keep_a_denied_scheme_image_by_default
expect SchemesTest.html("![a](javascript:x)") == SchemesTest.img("javascript:x")
## should_keep_an_image_with_a_listed_scheme
expect SchemesTest.web_html("![a](https://x/i.png)") == SchemesTest.img("https://x/i.png")
## should_match_an_image_scheme_ignoring_case
expect SchemesTest.web_html("![a](HTTPS://x/i.png)") == SchemesTest.img("HTTPS://x/i.png")
## should_keep_a_scheme_less_image
expect SchemesTest.web_html("![a](/i.png)") == SchemesTest.img("/i.png")
## should_leave_an_unlisted_data_image_as_its_alt_text
expect SchemesTest.web_html("![a](data:image/png;base64,AA)") == SchemesTest.text("a")
## should_leave_an_unlisted_scheme_image_as_its_alt_text
expect SchemesTest.web_html("![a](irc:x)") == SchemesTest.text("a")
## should_refuse_a_javascript_image_even_when_listed
expect SchemesTest.html_only(["javascript"], "![a](javascript:x)") == SchemesTest.text("a")
## should_refuse_an_unlisted_scheme_in_an_image_reference
expect SchemesTest.web_html("![a][r]\n\n[r]: irc:x") == SchemesTest.text("a")

## --- the destination a browser would read ------------------------------------
##
## A browser's URL parser strips leading and trailing C0 controls and spaces,
## and drops every ASCII tab and newline, before it reads the scheme. The
## policy reads the same string, or `\u(1)javascript:` and `java\tscript:`
## would be scheme-less to it and `javascript:` to the browser.

## should_refuse_javascript_behind_a_leading_control_byte
expect !Link.allows(Any, "\u(1)javascript:alert(1)")
## should_refuse_javascript_behind_a_space_and_a_control_byte
expect !Link.allows(Any, " \u(1f)javascript:x")
## should_refuse_javascript_split_by_a_tab
expect !Link.allows(Any, "java\tscript:x")
## should_refuse_javascript_split_by_a_newline
expect !Link.allows(Any, "java\nscript:x")
## should_refuse_javascript_split_by_a_carriage_return
expect !Link.allows(Any, "java\rscript:x")
## should_refuse_javascript_with_trailing_controls_when_listed
expect !Link.allows(Only(["javascript"]), "\u(1)javascript:x\u(1)")
## should_read_the_scheme_behind_a_leading_control_byte
expect Link.allows(Only(["https"]), "\u(1)https://x")
## should_read_the_scheme_across_a_tab
expect Link.allows(Only(["https"]), "ht\ttps://x")
## should_allow_a_tab_in_a_scheme_less_path
expect Link.allows(Only(["https"]), "/a\tb")
## should_refuse_a_control_prefixed_javascript_link_by_default
expect SchemesTest.html("[x](<\u(1)javascript:alert(1)>)") == SchemesTest.text("x")
## should_refuse_an_entity_tab_javascript_link_by_default
expect SchemesTest.html("[x](<java&#9;script:alert(1)>)") == SchemesTest.text("x")
## should_refuse_a_control_prefixed_unlisted_scheme
expect SchemesTest.web_html("[x](<\u(1)irc:x>)") == SchemesTest.text("x")
