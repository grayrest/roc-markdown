## Conformance — GENERATED from `spec/`. Do not hand-edit.
##
## Regenerate with `just spec-regen`; run with `just spec`.
##
## CommonMark 0.31.2 plus GFM's extension sections, one `expect` per example,
## compared EXACTLY. The spec's expected output carries its own block separators
## and trailing newline, so matching it is part of conformance and not an
## artefact of the harness.
import Markdown

SpecTest := [].{
    ## `to_html_unsafe`, because the spec's expected output contains raw HTML.
    ## cmark runs this same suite with `--unsafe` for the same reason; the
    ## DEFAULT lowering is the safe one, and that is the point of the split.
    html : Str -> Str
    html = |src| Markdown.to_html_unsafe(Markdown.parse(src))
}


## EX000 [Tabs]
expect SpecTest.html("\tfoo\tbaz\t\tbim\n") == "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"

## EX001 [Tabs]
expect SpecTest.html("  \tfoo\tbaz\t\tbim\n") == "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"

## EX002 [Tabs]
expect SpecTest.html("    a\ta\n    ὐ\ta\n") == "<pre><code>a\ta\nὐ\ta\n</code></pre>\n"

## EX003 [Tabs]
expect SpecTest.html("  - foo\n\n\tbar\n") == "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"

## EX004 [Tabs]
expect SpecTest.html("- foo\n\n\t\tbar\n") == "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"

## EX005 [Tabs]
expect SpecTest.html(">\t\tfoo\n") == "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"

## EX006 [Tabs]
expect SpecTest.html("-\t\tfoo\n") == "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"

## EX007 [Tabs]
expect SpecTest.html("    foo\n\tbar\n") == "<pre><code>foo\nbar\n</code></pre>\n"

## EX008 [Tabs]
expect SpecTest.html(" - foo\n   - bar\n\t - baz\n") == "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"

## EX009 [Tabs]
expect SpecTest.html("#\tFoo\n") == "<h1>Foo</h1>\n"

## EX010 [Tabs]
expect SpecTest.html("*\t*\t*\t\n") == "<hr />\n"

## EX011 [Backslash escapes]
expect SpecTest.html("\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n") == "<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"

## EX012 [Backslash escapes]
expect SpecTest.html("\\\t\\A\\a\\ \\3\\φ\\«\n") == "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"

## EX013 [Backslash escapes]
expect SpecTest.html("\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n\\&ouml; not a character entity\n") == "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;\n&amp;ouml; not a character entity</p>\n"

## EX014 [Backslash escapes]
expect SpecTest.html("\\\\*emphasis*\n") == "<p>\\<em>emphasis</em></p>\n"

## EX015 [Backslash escapes]
expect SpecTest.html("foo\\\nbar\n") == "<p>foo<br />\nbar</p>\n"

## EX016 [Backslash escapes]
expect SpecTest.html("`` \\[\\` ``\n") == "<p><code>\\[\\`</code></p>\n"

## EX017 [Backslash escapes]
expect SpecTest.html("    \\[\\]\n") == "<pre><code>\\[\\]\n</code></pre>\n"

## EX018 [Backslash escapes]
expect SpecTest.html("~~~\n\\[\\]\n~~~\n") == "<pre><code>\\[\\]\n</code></pre>\n"

## EX019 [Backslash escapes]
expect SpecTest.html("<https://example.com?find=\\*>\n") == "<p><a href=\"https://example.com?find=%5C*\">https://example.com?find=\\*</a></p>\n"

## EX020 [Backslash escapes]
expect SpecTest.html("<a href=\"/bar\\/)\">\n") == "<a href=\"/bar\\/)\">\n"

## EX021 [Backslash escapes]
expect SpecTest.html("[foo](/bar\\* \"ti\\*tle\")\n") == "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"

## EX022 [Backslash escapes]
expect SpecTest.html("[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n") == "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"

## EX023 [Backslash escapes]
expect SpecTest.html("``` foo\\+bar\nfoo\n```\n") == "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"

## EX024 [Entity and numeric character references]
expect SpecTest.html("&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n") == "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"

## EX025 [Entity and numeric character references]
expect SpecTest.html("&#35; &#1234; &#992; &#0;\n") == "<p># Ӓ Ϡ �</p>\n"

## EX026 [Entity and numeric character references]
expect SpecTest.html("&#X22; &#XD06; &#xcab;\n") == "<p>&quot; ആ ಫ</p>\n"

## EX027 [Entity and numeric character references]
expect SpecTest.html("&nbsp &x; &#; &#x;\n&#87654321;\n&#abcdef0;\n&ThisIsNotDefined; &hi?;\n") == "<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;\n&amp;#87654321;\n&amp;#abcdef0;\n&amp;ThisIsNotDefined; &amp;hi?;</p>\n"

## EX028 [Entity and numeric character references]
expect SpecTest.html("&copy\n") == "<p>&amp;copy</p>\n"

## EX029 [Entity and numeric character references]
expect SpecTest.html("&MadeUpEntity;\n") == "<p>&amp;MadeUpEntity;</p>\n"

## EX030 [Entity and numeric character references]
expect SpecTest.html("<a href=\"&ouml;&ouml;.html\">\n") == "<a href=\"&ouml;&ouml;.html\">\n"

## EX031 [Entity and numeric character references]
expect SpecTest.html("[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n") == "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"

## EX032 [Entity and numeric character references]
expect SpecTest.html("[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n") == "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"

## EX033 [Entity and numeric character references]
expect SpecTest.html("``` f&ouml;&ouml;\nfoo\n```\n") == "<pre><code class=\"language-föö\">foo\n</code></pre>\n"

## EX034 [Entity and numeric character references]
expect SpecTest.html("`f&ouml;&ouml;`\n") == "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"

## EX035 [Entity and numeric character references]
expect SpecTest.html("    f&ouml;f&ouml;\n") == "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"

## EX036 [Entity and numeric character references]
expect SpecTest.html("&#42;foo&#42;\n*foo*\n") == "<p>*foo*\n<em>foo</em></p>\n"

## EX037 [Entity and numeric character references]
expect SpecTest.html("&#42; foo\n\n* foo\n") == "<p>* foo</p>\n<ul>\n<li>foo</li>\n</ul>\n"

## EX038 [Entity and numeric character references]
expect SpecTest.html("foo&#10;&#10;bar\n") == "<p>foo\n\nbar</p>\n"

## EX039 [Entity and numeric character references]
expect SpecTest.html("&#9;foo\n") == "<p>\tfoo</p>\n"

## EX040 [Entity and numeric character references]
expect SpecTest.html("[a](url &quot;tit&quot;)\n") == "<p>[a](url &quot;tit&quot;)</p>\n"

## EX041 [Precedence]
expect SpecTest.html("- `one\n- two`\n") == "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"

## EX042 [Thematic breaks]
expect SpecTest.html("***\n---\n___\n") == "<hr />\n<hr />\n<hr />\n"

## EX043 [Thematic breaks]
expect SpecTest.html("+++\n") == "<p>+++</p>\n"

## EX044 [Thematic breaks]
expect SpecTest.html("===\n") == "<p>===</p>\n"

## EX045 [Thematic breaks]
expect SpecTest.html("--\n**\n__\n") == "<p>--\n**\n__</p>\n"

## EX046 [Thematic breaks]
expect SpecTest.html(" ***\n  ***\n   ***\n") == "<hr />\n<hr />\n<hr />\n"

## EX047 [Thematic breaks]
expect SpecTest.html("    ***\n") == "<pre><code>***\n</code></pre>\n"

## EX048 [Thematic breaks]
expect SpecTest.html("Foo\n    ***\n") == "<p>Foo\n***</p>\n"

## EX049 [Thematic breaks]
expect SpecTest.html("_____________________________________\n") == "<hr />\n"

## EX050 [Thematic breaks]
expect SpecTest.html(" - - -\n") == "<hr />\n"

## EX051 [Thematic breaks]
expect SpecTest.html(" **  * ** * ** * **\n") == "<hr />\n"

## EX052 [Thematic breaks]
expect SpecTest.html("-     -      -      -\n") == "<hr />\n"

## EX053 [Thematic breaks]
expect SpecTest.html("- - - -    \n") == "<hr />\n"

## EX054 [Thematic breaks]
expect SpecTest.html("_ _ _ _ a\n\na------\n\n---a---\n") == "<p>_ _ _ _ a</p>\n<p>a------</p>\n<p>---a---</p>\n"

## EX055 [Thematic breaks]
expect SpecTest.html(" *-*\n") == "<p><em>-</em></p>\n"

## EX056 [Thematic breaks]
expect SpecTest.html("- foo\n***\n- bar\n") == "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"

## EX057 [Thematic breaks]
expect SpecTest.html("Foo\n***\nbar\n") == "<p>Foo</p>\n<hr />\n<p>bar</p>\n"

## EX058 [Thematic breaks]
expect SpecTest.html("Foo\n---\nbar\n") == "<h2>Foo</h2>\n<p>bar</p>\n"

## EX059 [Thematic breaks]
expect SpecTest.html("* Foo\n* * *\n* Bar\n") == "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"

## EX060 [Thematic breaks]
expect SpecTest.html("- Foo\n- * * *\n") == "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"

## EX061 [ATX headings]
expect SpecTest.html("# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n") == "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"

## EX062 [ATX headings]
expect SpecTest.html("####### foo\n") == "<p>####### foo</p>\n"

## EX063 [ATX headings]
expect SpecTest.html("#5 bolt\n\n#hashtag\n") == "<p>#5 bolt</p>\n<p>#hashtag</p>\n"

## EX064 [ATX headings]
expect SpecTest.html("\\## foo\n") == "<p>## foo</p>\n"

## EX065 [ATX headings]
expect SpecTest.html("# foo *bar* \\*baz\\*\n") == "<h1>foo <em>bar</em> *baz*</h1>\n"

## EX066 [ATX headings]
expect SpecTest.html("#                  foo                     \n") == "<h1>foo</h1>\n"

## EX067 [ATX headings]
expect SpecTest.html(" ### foo\n  ## foo\n   # foo\n") == "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"

## EX068 [ATX headings]
expect SpecTest.html("    # foo\n") == "<pre><code># foo\n</code></pre>\n"

## EX069 [ATX headings]
expect SpecTest.html("foo\n    # bar\n") == "<p>foo\n# bar</p>\n"

## EX070 [ATX headings]
expect SpecTest.html("## foo ##\n  ###   bar    ###\n") == "<h2>foo</h2>\n<h3>bar</h3>\n"

## EX071 [ATX headings]
expect SpecTest.html("# foo ##################################\n##### foo ##\n") == "<h1>foo</h1>\n<h5>foo</h5>\n"

## EX072 [ATX headings]
expect SpecTest.html("### foo ###     \n") == "<h3>foo</h3>\n"

## EX073 [ATX headings]
expect SpecTest.html("### foo ### b\n") == "<h3>foo ### b</h3>\n"

## EX074 [ATX headings]
expect SpecTest.html("# foo#\n") == "<h1>foo#</h1>\n"

## EX075 [ATX headings]
expect SpecTest.html("### foo \\###\n## foo #\\##\n# foo \\#\n") == "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"

## EX076 [ATX headings]
expect SpecTest.html("****\n## foo\n****\n") == "<hr />\n<h2>foo</h2>\n<hr />\n"

## EX077 [ATX headings]
expect SpecTest.html("Foo bar\n# baz\nBar foo\n") == "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"

## EX078 [ATX headings]
expect SpecTest.html("## \n#\n### ###\n") == "<h2></h2>\n<h1></h1>\n<h3></h3>\n"

## EX079 [Setext headings]
expect SpecTest.html("Foo *bar*\n=========\n\nFoo *bar*\n---------\n") == "<h1>Foo <em>bar</em></h1>\n<h2>Foo <em>bar</em></h2>\n"

## EX080 [Setext headings]
expect SpecTest.html("Foo *bar\nbaz*\n====\n") == "<h1>Foo <em>bar\nbaz</em></h1>\n"

## EX081 [Setext headings]
expect SpecTest.html("  Foo *bar\nbaz*\t\n====\n") == "<h1>Foo <em>bar\nbaz</em></h1>\n"

## EX082 [Setext headings]
expect SpecTest.html("Foo\n-------------------------\n\nFoo\n=\n") == "<h2>Foo</h2>\n<h1>Foo</h1>\n"

## EX083 [Setext headings]
expect SpecTest.html("   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===\n") == "<h2>Foo</h2>\n<h2>Foo</h2>\n<h1>Foo</h1>\n"

## EX084 [Setext headings]
expect SpecTest.html("    Foo\n    ---\n\n    Foo\n---\n") == "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr />\n"

## EX085 [Setext headings]
expect SpecTest.html("Foo\n   ----      \n") == "<h2>Foo</h2>\n"

## EX086 [Setext headings]
expect SpecTest.html("Foo\n    ---\n") == "<p>Foo\n---</p>\n"

## EX087 [Setext headings]
expect SpecTest.html("Foo\n= =\n\nFoo\n--- -\n") == "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr />\n"

## EX088 [Setext headings]
expect SpecTest.html("Foo  \n-----\n") == "<h2>Foo</h2>\n"

## EX089 [Setext headings]
expect SpecTest.html("Foo\\\n----\n") == "<h2>Foo\\</h2>\n"

## EX090 [Setext headings]
expect SpecTest.html("`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n") == "<h2>`Foo</h2>\n<p>`</p>\n<h2>&lt;a title=&quot;a lot</h2>\n<p>of dashes&quot;/&gt;</p>\n"

## EX091 [Setext headings]
expect SpecTest.html("> Foo\n---\n") == "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr />\n"

## EX092 [Setext headings]
expect SpecTest.html("> foo\nbar\n===\n") == "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n"

## EX093 [Setext headings]
expect SpecTest.html("- Foo\n---\n") == "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n"

## EX094 [Setext headings]
expect SpecTest.html("Foo\nBar\n---\n") == "<h2>Foo\nBar</h2>\n"

## EX095 [Setext headings]
expect SpecTest.html("---\nFoo\n---\nBar\n---\nBaz\n") == "<hr />\n<h2>Foo</h2>\n<h2>Bar</h2>\n<p>Baz</p>\n"

## EX096 [Setext headings]
expect SpecTest.html("\n====\n") == "<p>====</p>\n"

## EX097 [Setext headings]
expect SpecTest.html("---\n---\n") == "<hr />\n<hr />\n"

## EX098 [Setext headings]
expect SpecTest.html("- foo\n-----\n") == "<ul>\n<li>foo</li>\n</ul>\n<hr />\n"

## EX099 [Setext headings]
expect SpecTest.html("    foo\n---\n") == "<pre><code>foo\n</code></pre>\n<hr />\n"

## EX100 [Setext headings]
expect SpecTest.html("> foo\n-----\n") == "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"

## EX101 [Setext headings]
expect SpecTest.html("\\> foo\n------\n") == "<h2>&gt; foo</h2>\n"

## EX102 [Setext headings]
expect SpecTest.html("Foo\n\nbar\n---\nbaz\n") == "<p>Foo</p>\n<h2>bar</h2>\n<p>baz</p>\n"

## EX103 [Setext headings]
expect SpecTest.html("Foo\nbar\n\n---\n\nbaz\n") == "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"

## EX104 [Setext headings]
expect SpecTest.html("Foo\nbar\n* * *\nbaz\n") == "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"

## EX105 [Setext headings]
expect SpecTest.html("Foo\nbar\n\\---\nbaz\n") == "<p>Foo\nbar\n---\nbaz</p>\n"

## EX106 [Indented code blocks]
expect SpecTest.html("    a simple\n      indented code block\n") == "<pre><code>a simple\n  indented code block\n</code></pre>\n"

## EX107 [Indented code blocks]
expect SpecTest.html("  - foo\n\n    bar\n") == "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"

## EX108 [Indented code blocks]
expect SpecTest.html("1.  foo\n\n    - bar\n") == "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"

## EX109 [Indented code blocks]
expect SpecTest.html("    <a/>\n    *hi*\n\n    - one\n") == "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"

## EX110 [Indented code blocks]
expect SpecTest.html("    chunk1\n\n    chunk2\n  \n \n \n    chunk3\n") == "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"

## EX111 [Indented code blocks]
expect SpecTest.html("    chunk1\n      \n      chunk2\n") == "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"

## EX112 [Indented code blocks]
expect SpecTest.html("Foo\n    bar\n\n") == "<p>Foo\nbar</p>\n"

## EX113 [Indented code blocks]
expect SpecTest.html("    foo\nbar\n") == "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"

## EX114 [Indented code blocks]
expect SpecTest.html("# Heading\n    foo\nHeading\n------\n    foo\n----\n") == "<h1>Heading</h1>\n<pre><code>foo\n</code></pre>\n<h2>Heading</h2>\n<pre><code>foo\n</code></pre>\n<hr />\n"

## EX115 [Indented code blocks]
expect SpecTest.html("        foo\n    bar\n") == "<pre><code>    foo\nbar\n</code></pre>\n"

## EX116 [Indented code blocks]
expect SpecTest.html("\n    \n    foo\n    \n\n") == "<pre><code>foo\n</code></pre>\n"

## EX117 [Indented code blocks]
expect SpecTest.html("    foo  \n") == "<pre><code>foo  \n</code></pre>\n"

## EX118 [Fenced code blocks]
expect SpecTest.html("```\n<\n >\n```\n") == "<pre><code>&lt;\n &gt;\n</code></pre>\n"

## EX119 [Fenced code blocks]
expect SpecTest.html("~~~\n<\n >\n~~~\n") == "<pre><code>&lt;\n &gt;\n</code></pre>\n"

## EX120 [Fenced code blocks]
expect SpecTest.html("``\nfoo\n``\n") == "<p><code>foo</code></p>\n"

## EX121 [Fenced code blocks]
expect SpecTest.html("```\naaa\n~~~\n```\n") == "<pre><code>aaa\n~~~\n</code></pre>\n"

## EX122 [Fenced code blocks]
expect SpecTest.html("~~~\naaa\n```\n~~~\n") == "<pre><code>aaa\n```\n</code></pre>\n"

## EX123 [Fenced code blocks]
expect SpecTest.html("````\naaa\n```\n``````\n") == "<pre><code>aaa\n```\n</code></pre>\n"

## EX124 [Fenced code blocks]
expect SpecTest.html("~~~~\naaa\n~~~\n~~~~\n") == "<pre><code>aaa\n~~~\n</code></pre>\n"

## EX125 [Fenced code blocks]
expect SpecTest.html("```\n") == "<pre><code></code></pre>\n"

## EX126 [Fenced code blocks]
expect SpecTest.html("`````\n\n```\naaa\n") == "<pre><code>\n```\naaa\n</code></pre>\n"

## EX127 [Fenced code blocks]
expect SpecTest.html("> ```\n> aaa\n\nbbb\n") == "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n"

## EX128 [Fenced code blocks]
expect SpecTest.html("```\n\n  \n```\n") == "<pre><code>\n  \n</code></pre>\n"

## EX129 [Fenced code blocks]
expect SpecTest.html("```\n```\n") == "<pre><code></code></pre>\n"

## EX130 [Fenced code blocks]
expect SpecTest.html(" ```\n aaa\naaa\n```\n") == "<pre><code>aaa\naaa\n</code></pre>\n"

## EX131 [Fenced code blocks]
expect SpecTest.html("  ```\naaa\n  aaa\naaa\n  ```\n") == "<pre><code>aaa\naaa\naaa\n</code></pre>\n"

## EX132 [Fenced code blocks]
expect SpecTest.html("   ```\n   aaa\n    aaa\n  aaa\n   ```\n") == "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"

## EX133 [Fenced code blocks]
expect SpecTest.html("    ```\n    aaa\n    ```\n") == "<pre><code>```\naaa\n```\n</code></pre>\n"

## EX134 [Fenced code blocks]
expect SpecTest.html("```\naaa\n  ```\n") == "<pre><code>aaa\n</code></pre>\n"

## EX135 [Fenced code blocks]
expect SpecTest.html("   ```\naaa\n  ```\n") == "<pre><code>aaa\n</code></pre>\n"

## EX136 [Fenced code blocks]
expect SpecTest.html("```\naaa\n    ```\n") == "<pre><code>aaa\n    ```\n</code></pre>\n"

## EX137 [Fenced code blocks]
expect SpecTest.html("``` ```\naaa\n") == "<p><code> </code>\naaa</p>\n"

## EX138 [Fenced code blocks]
expect SpecTest.html("~~~~~~\naaa\n~~~ ~~\n") == "<pre><code>aaa\n~~~ ~~\n</code></pre>\n"

## EX139 [Fenced code blocks]
expect SpecTest.html("foo\n```\nbar\n```\nbaz\n") == "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"

## EX140 [Fenced code blocks]
expect SpecTest.html("foo\n---\n~~~\nbar\n~~~\n# baz\n") == "<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>\n"

## EX141 [Fenced code blocks]
expect SpecTest.html("```ruby\ndef foo(x)\n  return 3\nend\n```\n") == "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"

## EX142 [Fenced code blocks]
expect SpecTest.html("~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~\n") == "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"

## EX143 [Fenced code blocks]
expect SpecTest.html("````;\n````\n") == "<pre><code class=\"language-;\"></code></pre>\n"

## EX144 [Fenced code blocks]
expect SpecTest.html("``` aa ```\nfoo\n") == "<p><code>aa</code>\nfoo</p>\n"

## EX145 [Fenced code blocks]
expect SpecTest.html("~~~ aa ``` ~~~\nfoo\n~~~\n") == "<pre><code class=\"language-aa\">foo\n</code></pre>\n"

## EX146 [Fenced code blocks]
expect SpecTest.html("```\n``` aaa\n```\n") == "<pre><code>``` aaa\n</code></pre>\n"

## EX147 [HTML blocks]
expect SpecTest.html("<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n") == "<table><tr><td>\n<pre>\n**Hello**,\n<p><em>world</em>.\n</pre></p>\n</td></tr></table>\n"

## EX148 [HTML blocks]
expect SpecTest.html("<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n\nokay.\n") == "<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n<p>okay.</p>\n"

## EX149 [HTML blocks]
expect SpecTest.html(" <div>\n  *hello*\n         <foo><a>\n") == " <div>\n  *hello*\n         <foo><a>\n"

## EX150 [HTML blocks]
expect SpecTest.html("</div>\n*foo*\n") == "</div>\n*foo*\n"

## EX151 [HTML blocks]
expect SpecTest.html("<DIV CLASS=\"foo\">\n\n*Markdown*\n\n</DIV>\n") == "<DIV CLASS=\"foo\">\n<p><em>Markdown</em></p>\n</DIV>\n"

## EX152 [HTML blocks]
expect SpecTest.html("<div id=\"foo\"\n  class=\"bar\">\n</div>\n") == "<div id=\"foo\"\n  class=\"bar\">\n</div>\n"

## EX153 [HTML blocks]
expect SpecTest.html("<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n") == "<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n"

## EX154 [HTML blocks]
expect SpecTest.html("<div>\n*foo*\n\n*bar*\n") == "<div>\n*foo*\n<p><em>bar</em></p>\n"

## EX155 [HTML blocks]
expect SpecTest.html("<div id=\"foo\"\n*hi*\n") == "<div id=\"foo\"\n*hi*\n"

## EX156 [HTML blocks]
expect SpecTest.html("<div class\nfoo\n") == "<div class\nfoo\n"

## EX157 [HTML blocks]
expect SpecTest.html("<div *???-&&&-<---\n*foo*\n") == "<div *???-&&&-<---\n*foo*\n"

## EX158 [HTML blocks]
expect SpecTest.html("<div><a href=\"bar\">*foo*</a></div>\n") == "<div><a href=\"bar\">*foo*</a></div>\n"

## EX159 [HTML blocks]
expect SpecTest.html("<table><tr><td>\nfoo\n</td></tr></table>\n") == "<table><tr><td>\nfoo\n</td></tr></table>\n"

## EX160 [HTML blocks]
expect SpecTest.html("<div></div>\n``` c\nint x = 33;\n```\n") == "<div></div>\n``` c\nint x = 33;\n```\n"

## EX161 [HTML blocks]
expect SpecTest.html("<a href=\"foo\">\n*bar*\n</a>\n") == "<a href=\"foo\">\n*bar*\n</a>\n"

## EX162 [HTML blocks]
expect SpecTest.html("<Warning>\n*bar*\n</Warning>\n") == "<Warning>\n*bar*\n</Warning>\n"

## EX163 [HTML blocks]
expect SpecTest.html("<i class=\"foo\">\n*bar*\n</i>\n") == "<i class=\"foo\">\n*bar*\n</i>\n"

## EX164 [HTML blocks]
expect SpecTest.html("</ins>\n*bar*\n") == "</ins>\n*bar*\n"

## EX165 [HTML blocks]
expect SpecTest.html("<del>\n*foo*\n</del>\n") == "<del>\n*foo*\n</del>\n"

## EX166 [HTML blocks]
expect SpecTest.html("<del>\n\n*foo*\n\n</del>\n") == "<del>\n<p><em>foo</em></p>\n</del>\n"

## EX167 [HTML blocks]
expect SpecTest.html("<del>*foo*</del>\n") == "<p><del><em>foo</em></del></p>\n"

## EX168 [HTML blocks]
expect SpecTest.html("<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\nokay\n") == "<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\n<p>okay</p>\n"

## EX169 [HTML blocks]
expect SpecTest.html("<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\nokay\n") == "<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\n<p>okay</p>\n"

## EX170 [HTML blocks]
expect SpecTest.html("<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n") == "<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n"

## EX171 [HTML blocks]
expect SpecTest.html("<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\nokay\n") == "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\n<p>okay</p>\n"

## EX172 [HTML blocks]
expect SpecTest.html("<style\n  type=\"text/css\">\n\nfoo\n") == "<style\n  type=\"text/css\">\n\nfoo\n"

## EX173 [HTML blocks]
expect SpecTest.html("> <div>\n> foo\n\nbar\n") == "<blockquote>\n<div>\nfoo\n</blockquote>\n<p>bar</p>\n"

## EX174 [HTML blocks]
expect SpecTest.html("- <div>\n- foo\n") == "<ul>\n<li>\n<div>\n</li>\n<li>foo</li>\n</ul>\n"

## EX175 [HTML blocks]
expect SpecTest.html("<style>p{color:red;}</style>\n*foo*\n") == "<style>p{color:red;}</style>\n<p><em>foo</em></p>\n"

## EX176 [HTML blocks]
expect SpecTest.html("<!-- foo -->*bar*\n*baz*\n") == "<!-- foo -->*bar*\n<p><em>baz</em></p>\n"

## EX177 [HTML blocks]
expect SpecTest.html("<script>\nfoo\n</script>1. *bar*\n") == "<script>\nfoo\n</script>1. *bar*\n"

## EX178 [HTML blocks]
expect SpecTest.html("<!-- Foo\n\nbar\n   baz -->\nokay\n") == "<!-- Foo\n\nbar\n   baz -->\n<p>okay</p>\n"

## EX179 [HTML blocks]
expect SpecTest.html("<?php\n\n  echo '>';\n\n?>\nokay\n") == "<?php\n\n  echo '>';\n\n?>\n<p>okay</p>\n"

## EX180 [HTML blocks]
expect SpecTest.html("<!DOCTYPE html>\n") == "<!DOCTYPE html>\n"

## EX181 [HTML blocks]
expect SpecTest.html("<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\nokay\n") == "<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\n<p>okay</p>\n"

## EX182 [HTML blocks]
expect SpecTest.html("  <!-- foo -->\n\n    <!-- foo -->\n") == "  <!-- foo -->\n<pre><code>&lt;!-- foo --&gt;\n</code></pre>\n"

## EX183 [HTML blocks]
expect SpecTest.html("  <div>\n\n    <div>\n") == "  <div>\n<pre><code>&lt;div&gt;\n</code></pre>\n"

## EX184 [HTML blocks]
expect SpecTest.html("Foo\n<div>\nbar\n</div>\n") == "<p>Foo</p>\n<div>\nbar\n</div>\n"

## EX185 [HTML blocks]
expect SpecTest.html("<div>\nbar\n</div>\n*foo*\n") == "<div>\nbar\n</div>\n*foo*\n"

## EX186 [HTML blocks]
expect SpecTest.html("Foo\n<a href=\"bar\">\nbaz\n") == "<p>Foo\n<a href=\"bar\">\nbaz</p>\n"

## EX187 [HTML blocks]
expect SpecTest.html("<div>\n\n*Emphasized* text.\n\n</div>\n") == "<div>\n<p><em>Emphasized</em> text.</p>\n</div>\n"

## EX188 [HTML blocks]
expect SpecTest.html("<div>\n*Emphasized* text.\n</div>\n") == "<div>\n*Emphasized* text.\n</div>\n"

## EX189 [HTML blocks]
expect SpecTest.html("<table>\n\n<tr>\n\n<td>\nHi\n</td>\n\n</tr>\n\n</table>\n") == "<table>\n<tr>\n<td>\nHi\n</td>\n</tr>\n</table>\n"

## EX190 [HTML blocks]
expect SpecTest.html("<table>\n\n  <tr>\n\n    <td>\n      Hi\n    </td>\n\n  </tr>\n\n</table>\n") == "<table>\n  <tr>\n<pre><code>&lt;td&gt;\n  Hi\n&lt;/td&gt;\n</code></pre>\n  </tr>\n</table>\n"

## EX191 [Link reference definitions]
expect SpecTest.html("[foo]: /url \"title\"\n\n[foo]\n") == "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX192 [Link reference definitions]
expect SpecTest.html("   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n") == "<p><a href=\"/url\" title=\"the title\">foo</a></p>\n"

## EX193 [Link reference definitions]
expect SpecTest.html("[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n") == "<p><a href=\"my_(url)\" title=\"title (with parens)\">Foo*bar]</a></p>\n"

## EX194 [Link reference definitions]
expect SpecTest.html("[Foo bar]:\n<my url>\n'title'\n\n[Foo bar]\n") == "<p><a href=\"my%20url\" title=\"title\">Foo bar</a></p>\n"

## EX195 [Link reference definitions]
expect SpecTest.html("[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n") == "<p><a href=\"/url\" title=\"\ntitle\nline1\nline2\n\">foo</a></p>\n"

## EX196 [Link reference definitions]
expect SpecTest.html("[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n") == "<p>[foo]: /url 'title</p>\n<p>with blank line'</p>\n<p>[foo]</p>\n"

## EX197 [Link reference definitions]
expect SpecTest.html("[foo]:\n/url\n\n[foo]\n") == "<p><a href=\"/url\">foo</a></p>\n"

## EX198 [Link reference definitions]
expect SpecTest.html("[foo]:\n\n[foo]\n") == "<p>[foo]:</p>\n<p>[foo]</p>\n"

## EX199 [Link reference definitions]
expect SpecTest.html("[foo]: <>\n\n[foo]\n") == "<p><a href=\"\">foo</a></p>\n"

## EX200 [Link reference definitions]
expect SpecTest.html("[foo]: <bar>(baz)\n\n[foo]\n") == "<p>[foo]: <bar>(baz)</p>\n<p>[foo]</p>\n"

## EX201 [Link reference definitions]
expect SpecTest.html("[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n") == "<p><a href=\"/url%5Cbar*baz\" title=\"foo&quot;bar\\baz\">foo</a></p>\n"

## EX202 [Link reference definitions]
expect SpecTest.html("[foo]\n\n[foo]: url\n") == "<p><a href=\"url\">foo</a></p>\n"

## EX203 [Link reference definitions]
expect SpecTest.html("[foo]\n\n[foo]: first\n[foo]: second\n") == "<p><a href=\"first\">foo</a></p>\n"

## EX204 [Link reference definitions]
expect SpecTest.html("[FOO]: /url\n\n[Foo]\n") == "<p><a href=\"/url\">Foo</a></p>\n"

## EX205 [Link reference definitions]
expect SpecTest.html("[ΑΓΩ]: /φου\n\n[αγω]\n") == "<p><a href=\"/%CF%86%CE%BF%CF%85\">αγω</a></p>\n"

## EX206 [Link reference definitions]
expect SpecTest.html("[foo]: /url\n") == ""

## EX207 [Link reference definitions]
expect SpecTest.html("[\nfoo\n]: /url\nbar\n") == "<p>bar</p>\n"

## EX208 [Link reference definitions]
expect SpecTest.html("[foo]: /url \"title\" ok\n") == "<p>[foo]: /url &quot;title&quot; ok</p>\n"

## EX209 [Link reference definitions]
expect SpecTest.html("[foo]: /url\n\"title\" ok\n") == "<p>&quot;title&quot; ok</p>\n"

## EX210 [Link reference definitions]
expect SpecTest.html("    [foo]: /url \"title\"\n\n[foo]\n") == "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n<p>[foo]</p>\n"

## EX211 [Link reference definitions]
expect SpecTest.html("```\n[foo]: /url\n```\n\n[foo]\n") == "<pre><code>[foo]: /url\n</code></pre>\n<p>[foo]</p>\n"

## EX212 [Link reference definitions]
expect SpecTest.html("Foo\n[bar]: /baz\n\n[bar]\n") == "<p>Foo\n[bar]: /baz</p>\n<p>[bar]</p>\n"

## EX213 [Link reference definitions]
expect SpecTest.html("# [Foo]\n[foo]: /url\n> bar\n") == "<h1><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"

## EX214 [Link reference definitions]
expect SpecTest.html("[foo]: /url\nbar\n===\n[foo]\n") == "<h1>bar</h1>\n<p><a href=\"/url\">foo</a></p>\n"

## EX215 [Link reference definitions]
expect SpecTest.html("[foo]: /url\n===\n[foo]\n") == "<p>===\n<a href=\"/url\">foo</a></p>\n"

## EX216 [Link reference definitions]
expect SpecTest.html("[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n") == "<p><a href=\"/foo-url\" title=\"foo\">foo</a>,\n<a href=\"/bar-url\" title=\"bar\">bar</a>,\n<a href=\"/baz-url\">baz</a></p>\n"

## EX217 [Link reference definitions]
expect SpecTest.html("[foo]\n\n> [foo]: /url\n") == "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"

## EX218 [Paragraphs]
expect SpecTest.html("aaa\n\nbbb\n") == "<p>aaa</p>\n<p>bbb</p>\n"

## EX219 [Paragraphs]
expect SpecTest.html("aaa\nbbb\n\nccc\nddd\n") == "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"

## EX220 [Paragraphs]
expect SpecTest.html("aaa\n\n\nbbb\n") == "<p>aaa</p>\n<p>bbb</p>\n"

## EX221 [Paragraphs]
expect SpecTest.html("  aaa\n bbb\n") == "<p>aaa\nbbb</p>\n"

## EX222 [Paragraphs]
expect SpecTest.html("aaa\n             bbb\n                                       ccc\n") == "<p>aaa\nbbb\nccc</p>\n"

## EX223 [Paragraphs]
expect SpecTest.html("   aaa\nbbb\n") == "<p>aaa\nbbb</p>\n"

## EX224 [Paragraphs]
expect SpecTest.html("    aaa\nbbb\n") == "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"

## EX225 [Paragraphs]
expect SpecTest.html("aaa     \nbbb     \n") == "<p>aaa<br />\nbbb</p>\n"

## EX226 [Blank lines]
expect SpecTest.html("  \n\naaa\n  \n\n# aaa\n\n  \n") == "<p>aaa</p>\n<h1>aaa</h1>\n"

## EX227 [Block quotes]
expect SpecTest.html("> # Foo\n> bar\n> baz\n") == "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"

## EX228 [Block quotes]
expect SpecTest.html("># Foo\n>bar\n> baz\n") == "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"

## EX229 [Block quotes]
expect SpecTest.html("   > # Foo\n   > bar\n > baz\n") == "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"

## EX230 [Block quotes]
expect SpecTest.html("    > # Foo\n    > bar\n    > baz\n") == "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"

## EX231 [Block quotes]
expect SpecTest.html("> # Foo\n> bar\nbaz\n") == "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"

## EX232 [Block quotes]
expect SpecTest.html("> bar\nbaz\n> foo\n") == "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"

## EX233 [Block quotes]
expect SpecTest.html("> foo\n---\n") == "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"

## EX234 [Block quotes]
expect SpecTest.html("> - foo\n- bar\n") == "<blockquote>\n<ul>\n<li>foo</li>\n</ul>\n</blockquote>\n<ul>\n<li>bar</li>\n</ul>\n"

## EX235 [Block quotes]
expect SpecTest.html(">     foo\n    bar\n") == "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"

## EX236 [Block quotes]
expect SpecTest.html("> ```\nfoo\n```\n") == "<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>foo</p>\n<pre><code></code></pre>\n"

## EX237 [Block quotes]
expect SpecTest.html("> foo\n    - bar\n") == "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"

## EX238 [Block quotes]
expect SpecTest.html(">\n") == "<blockquote>\n</blockquote>\n"

## EX239 [Block quotes]
expect SpecTest.html(">\n>  \n> \n") == "<blockquote>\n</blockquote>\n"

## EX240 [Block quotes]
expect SpecTest.html(">\n> foo\n>  \n") == "<blockquote>\n<p>foo</p>\n</blockquote>\n"

## EX241 [Block quotes]
expect SpecTest.html("> foo\n\n> bar\n") == "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"

## EX242 [Block quotes]
expect SpecTest.html("> foo\n> bar\n") == "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"

## EX243 [Block quotes]
expect SpecTest.html("> foo\n>\n> bar\n") == "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"

## EX244 [Block quotes]
expect SpecTest.html("foo\n> bar\n") == "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"

## EX245 [Block quotes]
expect SpecTest.html("> aaa\n***\n> bbb\n") == "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"

## EX246 [Block quotes]
expect SpecTest.html("> bar\nbaz\n") == "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"

## EX247 [Block quotes]
expect SpecTest.html("> bar\n\nbaz\n") == "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"

## EX248 [Block quotes]
expect SpecTest.html("> bar\n>\nbaz\n") == "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"

## EX249 [Block quotes]
expect SpecTest.html("> > > foo\nbar\n") == "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"

## EX250 [Block quotes]
expect SpecTest.html(">>> foo\n> bar\n>>baz\n") == "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"

## EX251 [Block quotes]
expect SpecTest.html(">     code\n\n>    not code\n") == "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"

## EX252 [List items]
expect SpecTest.html("A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.\n") == "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"

## EX253 [List items]
expect SpecTest.html("1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n") == "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"

## EX254 [List items]
expect SpecTest.html("- one\n\n two\n") == "<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n"

## EX255 [List items]
expect SpecTest.html("- one\n\n  two\n") == "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"

## EX256 [List items]
expect SpecTest.html(" -    one\n\n     two\n") == "<ul>\n<li>one</li>\n</ul>\n<pre><code> two\n</code></pre>\n"

## EX257 [List items]
expect SpecTest.html(" -    one\n\n      two\n") == "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"

## EX258 [List items]
expect SpecTest.html("   > > 1.  one\n>>\n>>     two\n") == "<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>\n"

## EX259 [List items]
expect SpecTest.html(">>- one\n>>\n  >  > two\n") == "<blockquote>\n<blockquote>\n<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n</blockquote>\n</blockquote>\n"

## EX260 [List items]
expect SpecTest.html("-one\n\n2.two\n") == "<p>-one</p>\n<p>2.two</p>\n"

## EX261 [List items]
expect SpecTest.html("- foo\n\n\n  bar\n") == "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"

## EX262 [List items]
expect SpecTest.html("1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam\n") == "<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>\n"

## EX263 [List items]
expect SpecTest.html("- Foo\n\n      bar\n\n\n      baz\n") == "<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>\n"

## EX264 [List items]
expect SpecTest.html("123456789. ok\n") == "<ol start=\"123456789\">\n<li>ok</li>\n</ol>\n"

## EX265 [List items]
expect SpecTest.html("1234567890. not ok\n") == "<p>1234567890. not ok</p>\n"

## EX266 [List items]
expect SpecTest.html("0. ok\n") == "<ol start=\"0\">\n<li>ok</li>\n</ol>\n"

## EX267 [List items]
expect SpecTest.html("003. ok\n") == "<ol start=\"3\">\n<li>ok</li>\n</ol>\n"

## EX268 [List items]
expect SpecTest.html("-1. not ok\n") == "<p>-1. not ok</p>\n"

## EX269 [List items]
expect SpecTest.html("- foo\n\n      bar\n") == "<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>\n"

## EX270 [List items]
expect SpecTest.html("  10.  foo\n\n           bar\n") == "<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>\n"

## EX271 [List items]
expect SpecTest.html("    indented code\n\nparagraph\n\n    more code\n") == "<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n"

## EX272 [List items]
expect SpecTest.html("1.     indented code\n\n   paragraph\n\n       more code\n") == "<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"

## EX273 [List items]
expect SpecTest.html("1.      indented code\n\n   paragraph\n\n       more code\n") == "<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"

## EX274 [List items]
expect SpecTest.html("   foo\n\nbar\n") == "<p>foo</p>\n<p>bar</p>\n"

## EX275 [List items]
expect SpecTest.html("-    foo\n\n  bar\n") == "<ul>\n<li>foo</li>\n</ul>\n<p>bar</p>\n"

## EX276 [List items]
expect SpecTest.html("-  foo\n\n   bar\n") == "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"

## EX277 [List items]
expect SpecTest.html("-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz\n") == "<ul>\n<li>foo</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>\n"

## EX278 [List items]
expect SpecTest.html("-   \n  foo\n") == "<ul>\n<li>foo</li>\n</ul>\n"

## EX279 [List items]
expect SpecTest.html("-\n\n  foo\n") == "<ul>\n<li></li>\n</ul>\n<p>foo</p>\n"

## EX280 [List items]
expect SpecTest.html("- foo\n-\n- bar\n") == "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"

## EX281 [List items]
expect SpecTest.html("- foo\n-   \n- bar\n") == "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"

## EX282 [List items]
expect SpecTest.html("1. foo\n2.\n3. bar\n") == "<ol>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ol>\n"

## EX283 [List items]
expect SpecTest.html("*\n") == "<ul>\n<li></li>\n</ul>\n"

## EX284 [List items]
expect SpecTest.html("foo\n*\n\nfoo\n1.\n") == "<p>foo\n*</p>\n<p>foo\n1.</p>\n"

## EX285 [List items]
expect SpecTest.html(" 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.\n") == "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"

## EX286 [List items]
expect SpecTest.html("  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.\n") == "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"

## EX287 [List items]
expect SpecTest.html("   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.\n") == "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"

## EX288 [List items]
expect SpecTest.html("    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.\n") == "<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>\n"

## EX289 [List items]
expect SpecTest.html("  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.\n") == "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"

## EX290 [List items]
expect SpecTest.html("  1.  A paragraph\n    with two lines.\n") == "<ol>\n<li>A paragraph\nwith two lines.</li>\n</ol>\n"

## EX291 [List items]
expect SpecTest.html("> 1. > Blockquote\ncontinued here.\n") == "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"

## EX292 [List items]
expect SpecTest.html("> 1. > Blockquote\n> continued here.\n") == "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"

## EX293 [List items]
expect SpecTest.html("- foo\n  - bar\n    - baz\n      - boo\n") == "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz\n<ul>\n<li>boo</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"

## EX294 [List items]
expect SpecTest.html("- foo\n - bar\n  - baz\n   - boo\n") == "<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n<li>boo</li>\n</ul>\n"

## EX295 [List items]
expect SpecTest.html("10) foo\n    - bar\n") == "<ol start=\"10\">\n<li>foo\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"

## EX296 [List items]
expect SpecTest.html("10) foo\n   - bar\n") == "<ol start=\"10\">\n<li>foo</li>\n</ol>\n<ul>\n<li>bar</li>\n</ul>\n"

## EX297 [List items]
expect SpecTest.html("- - foo\n") == "<ul>\n<li>\n<ul>\n<li>foo</li>\n</ul>\n</li>\n</ul>\n"

## EX298 [List items]
expect SpecTest.html("1. - 2. foo\n") == "<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>foo</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>\n"

## EX299 [List items]
expect SpecTest.html("- # Foo\n- Bar\n  ---\n  baz\n") == "<ul>\n<li>\n<h1>Foo</h1>\n</li>\n<li>\n<h2>Bar</h2>\nbaz</li>\n</ul>\n"

## EX300 [Lists]
expect SpecTest.html("- foo\n- bar\n+ baz\n") == "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<ul>\n<li>baz</li>\n</ul>\n"

## EX301 [Lists]
expect SpecTest.html("1. foo\n2. bar\n3) baz\n") == "<ol>\n<li>foo</li>\n<li>bar</li>\n</ol>\n<ol start=\"3\">\n<li>baz</li>\n</ol>\n"

## EX302 [Lists]
expect SpecTest.html("Foo\n- bar\n- baz\n") == "<p>Foo</p>\n<ul>\n<li>bar</li>\n<li>baz</li>\n</ul>\n"

## EX303 [Lists]
expect SpecTest.html("The number of windows in my house is\n14.  The number of doors is 6.\n") == "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>\n"

## EX304 [Lists]
expect SpecTest.html("The number of windows in my house is\n1.  The number of doors is 6.\n") == "<p>The number of windows in my house is</p>\n<ol>\n<li>The number of doors is 6.</li>\n</ol>\n"

## EX305 [Lists]
expect SpecTest.html("- foo\n\n- bar\n\n\n- baz\n") == "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"

## EX306 [Lists]
expect SpecTest.html("- foo\n  - bar\n    - baz\n\n\n      bim\n") == "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"

## EX307 [Lists]
expect SpecTest.html("- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim\n") == "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<!-- -->\n<ul>\n<li>baz</li>\n<li>bim</li>\n</ul>\n"

## EX308 [Lists]
expect SpecTest.html("-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code\n") == "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<!-- -->\n<pre><code>code\n</code></pre>\n"

## EX309 [Lists]
expect SpecTest.html("- a\n - b\n  - c\n   - d\n  - e\n - f\n- g\n") == "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n<li>e</li>\n<li>f</li>\n<li>g</li>\n</ul>\n"

## EX310 [Lists]
expect SpecTest.html("1. a\n\n  2. b\n\n   3. c\n") == "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"

## EX311 [Lists]
expect SpecTest.html("- a\n - b\n  - c\n   - d\n    - e\n") == "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d\n- e</li>\n</ul>\n"

## EX312 [Lists]
expect SpecTest.html("1. a\n\n  2. b\n\n    3. c\n") == "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ol>\n<pre><code>3. c\n</code></pre>\n"

## EX313 [Lists]
expect SpecTest.html("- a\n- b\n\n- c\n") == "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"

## EX314 [Lists]
expect SpecTest.html("* a\n*\n\n* c\n") == "<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"

## EX315 [Lists]
expect SpecTest.html("- a\n- b\n\n  c\n- d\n") == "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"

## EX316 [Lists]
expect SpecTest.html("- a\n- b\n\n  [ref]: /url\n- d\n") == "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"

## EX317 [Lists]
expect SpecTest.html("- a\n- ```\n  b\n\n\n  ```\n- c\n") == "<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>\n"

## EX318 [Lists]
expect SpecTest.html("- a\n  - b\n\n    c\n- d\n") == "<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>\n"

## EX319 [Lists]
expect SpecTest.html("* a\n  > b\n  >\n* c\n") == "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>c</li>\n</ul>\n"

## EX320 [Lists]
expect SpecTest.html("- a\n  > b\n  ```\n  c\n  ```\n- d\n") == "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>d</li>\n</ul>\n"

## EX321 [Lists]
expect SpecTest.html("- a\n") == "<ul>\n<li>a</li>\n</ul>\n"

## EX322 [Lists]
expect SpecTest.html("- a\n  - b\n") == "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>\n"

## EX323 [Lists]
expect SpecTest.html("1. ```\n   foo\n   ```\n\n   bar\n") == "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"

## EX324 [Lists]
expect SpecTest.html("* foo\n  * bar\n\n  baz\n") == "<ul>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n<p>baz</p>\n</li>\n</ul>\n"

## EX325 [Lists]
expect SpecTest.html("- a\n  - b\n  - c\n\n- d\n  - e\n  - f\n") == "<ul>\n<li>\n<p>a</p>\n<ul>\n<li>b</li>\n<li>c</li>\n</ul>\n</li>\n<li>\n<p>d</p>\n<ul>\n<li>e</li>\n<li>f</li>\n</ul>\n</li>\n</ul>\n"

## EX326 [Inlines]
expect SpecTest.html("`hi`lo`\n") == "<p><code>hi</code>lo`</p>\n"

## EX327 [Code spans]
expect SpecTest.html("`foo`\n") == "<p><code>foo</code></p>\n"

## EX328 [Code spans]
expect SpecTest.html("`` foo ` bar ``\n") == "<p><code>foo ` bar</code></p>\n"

## EX329 [Code spans]
expect SpecTest.html("` `` `\n") == "<p><code>``</code></p>\n"

## EX330 [Code spans]
expect SpecTest.html("`  ``  `\n") == "<p><code> `` </code></p>\n"

## EX331 [Code spans]
expect SpecTest.html("` a`\n") == "<p><code> a</code></p>\n"

## EX332 [Code spans]
expect SpecTest.html("` b `\n") == "<p><code> b </code></p>\n"

## EX333 [Code spans]
expect SpecTest.html("` `\n`  `\n") == "<p><code> </code>\n<code>  </code></p>\n"

## EX334 [Code spans]
expect SpecTest.html("``\nfoo\nbar  \nbaz\n``\n") == "<p><code>foo bar   baz</code></p>\n"

## EX335 [Code spans]
expect SpecTest.html("``\nfoo \n``\n") == "<p><code>foo </code></p>\n"

## EX336 [Code spans]
expect SpecTest.html("`foo   bar \nbaz`\n") == "<p><code>foo   bar  baz</code></p>\n"

## EX337 [Code spans]
expect SpecTest.html("`foo\\`bar`\n") == "<p><code>foo\\</code>bar`</p>\n"

## EX338 [Code spans]
expect SpecTest.html("``foo`bar``\n") == "<p><code>foo`bar</code></p>\n"

## EX339 [Code spans]
expect SpecTest.html("` foo `` bar `\n") == "<p><code>foo `` bar</code></p>\n"

## EX340 [Code spans]
expect SpecTest.html("*foo`*`\n") == "<p>*foo<code>*</code></p>\n"

## EX341 [Code spans]
expect SpecTest.html("[not a `link](/foo`)\n") == "<p>[not a <code>link](/foo</code>)</p>\n"

## EX342 [Code spans]
expect SpecTest.html("`<a href=\"`\">`\n") == "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>\n"

## EX343 [Code spans]
expect SpecTest.html("<a href=\"`\">`\n") == "<p><a href=\"`\">`</p>\n"

## EX344 [Code spans]
expect SpecTest.html("`<https://foo.bar.`baz>`\n") == "<p><code>&lt;https://foo.bar.</code>baz&gt;`</p>\n"

## EX345 [Code spans]
expect SpecTest.html("<https://foo.bar.`baz>`\n") == "<p><a href=\"https://foo.bar.%60baz\">https://foo.bar.`baz</a>`</p>\n"

## EX346 [Code spans]
expect SpecTest.html("```foo``\n") == "<p>```foo``</p>\n"

## EX347 [Code spans]
expect SpecTest.html("`foo\n") == "<p>`foo</p>\n"

## EX348 [Code spans]
expect SpecTest.html("`foo``bar``\n") == "<p>`foo<code>bar</code></p>\n"

## EX349 [Emphasis and strong emphasis]
expect SpecTest.html("*foo bar*\n") == "<p><em>foo bar</em></p>\n"

## EX350 [Emphasis and strong emphasis]
expect SpecTest.html("a * foo bar*\n") == "<p>a * foo bar*</p>\n"

## EX351 [Emphasis and strong emphasis]
expect SpecTest.html("a*\"foo\"*\n") == "<p>a*&quot;foo&quot;*</p>\n"

## EX352 [Emphasis and strong emphasis]
expect SpecTest.html("* a *\n") == "<p>* a *</p>\n"

## EX353 [Emphasis and strong emphasis]
expect SpecTest.html("*$*alpha.\n\n*£*bravo.\n\n*€*charlie.\n") == "<p>*$*alpha.</p>\n<p>*£*bravo.</p>\n<p>*€*charlie.</p>\n"

## EX354 [Emphasis and strong emphasis]
expect SpecTest.html("foo*bar*\n") == "<p>foo<em>bar</em></p>\n"

## EX355 [Emphasis and strong emphasis]
expect SpecTest.html("5*6*78\n") == "<p>5<em>6</em>78</p>\n"

## EX356 [Emphasis and strong emphasis]
expect SpecTest.html("_foo bar_\n") == "<p><em>foo bar</em></p>\n"

## EX357 [Emphasis and strong emphasis]
expect SpecTest.html("_ foo bar_\n") == "<p>_ foo bar_</p>\n"

## EX358 [Emphasis and strong emphasis]
expect SpecTest.html("a_\"foo\"_\n") == "<p>a_&quot;foo&quot;_</p>\n"

## EX359 [Emphasis and strong emphasis]
expect SpecTest.html("foo_bar_\n") == "<p>foo_bar_</p>\n"

## EX360 [Emphasis and strong emphasis]
expect SpecTest.html("5_6_78\n") == "<p>5_6_78</p>\n"

## EX361 [Emphasis and strong emphasis]
expect SpecTest.html("пристаням_стремятся_\n") == "<p>пристаням_стремятся_</p>\n"

## EX362 [Emphasis and strong emphasis]
expect SpecTest.html("aa_\"bb\"_cc\n") == "<p>aa_&quot;bb&quot;_cc</p>\n"

## EX363 [Emphasis and strong emphasis]
expect SpecTest.html("foo-_(bar)_\n") == "<p>foo-<em>(bar)</em></p>\n"

## EX364 [Emphasis and strong emphasis]
expect SpecTest.html("_foo*\n") == "<p>_foo*</p>\n"

## EX365 [Emphasis and strong emphasis]
expect SpecTest.html("*foo bar *\n") == "<p>*foo bar *</p>\n"

## EX366 [Emphasis and strong emphasis]
expect SpecTest.html("*foo bar\n*\n") == "<p>*foo bar\n*</p>\n"

## EX367 [Emphasis and strong emphasis]
expect SpecTest.html("*(*foo)\n") == "<p>*(*foo)</p>\n"

## EX368 [Emphasis and strong emphasis]
expect SpecTest.html("*(*foo*)*\n") == "<p><em>(<em>foo</em>)</em></p>\n"

## EX369 [Emphasis and strong emphasis]
expect SpecTest.html("*foo*bar\n") == "<p><em>foo</em>bar</p>\n"

## EX370 [Emphasis and strong emphasis]
expect SpecTest.html("_foo bar _\n") == "<p>_foo bar _</p>\n"

## EX371 [Emphasis and strong emphasis]
expect SpecTest.html("_(_foo)\n") == "<p>_(_foo)</p>\n"

## EX372 [Emphasis and strong emphasis]
expect SpecTest.html("_(_foo_)_\n") == "<p><em>(<em>foo</em>)</em></p>\n"

## EX373 [Emphasis and strong emphasis]
expect SpecTest.html("_foo_bar\n") == "<p>_foo_bar</p>\n"

## EX374 [Emphasis and strong emphasis]
expect SpecTest.html("_пристаням_стремятся\n") == "<p>_пристаням_стремятся</p>\n"

## EX375 [Emphasis and strong emphasis]
expect SpecTest.html("_foo_bar_baz_\n") == "<p><em>foo_bar_baz</em></p>\n"

## EX376 [Emphasis and strong emphasis]
expect SpecTest.html("_(bar)_.\n") == "<p><em>(bar)</em>.</p>\n"

## EX377 [Emphasis and strong emphasis]
expect SpecTest.html("**foo bar**\n") == "<p><strong>foo bar</strong></p>\n"

## EX378 [Emphasis and strong emphasis]
expect SpecTest.html("** foo bar**\n") == "<p>** foo bar**</p>\n"

## EX379 [Emphasis and strong emphasis]
expect SpecTest.html("a**\"foo\"**\n") == "<p>a**&quot;foo&quot;**</p>\n"

## EX380 [Emphasis and strong emphasis]
expect SpecTest.html("foo**bar**\n") == "<p>foo<strong>bar</strong></p>\n"

## EX381 [Emphasis and strong emphasis]
expect SpecTest.html("__foo bar__\n") == "<p><strong>foo bar</strong></p>\n"

## EX382 [Emphasis and strong emphasis]
expect SpecTest.html("__ foo bar__\n") == "<p>__ foo bar__</p>\n"

## EX383 [Emphasis and strong emphasis]
expect SpecTest.html("__\nfoo bar__\n") == "<p>__\nfoo bar__</p>\n"

## EX384 [Emphasis and strong emphasis]
expect SpecTest.html("a__\"foo\"__\n") == "<p>a__&quot;foo&quot;__</p>\n"

## EX385 [Emphasis and strong emphasis]
expect SpecTest.html("foo__bar__\n") == "<p>foo__bar__</p>\n"

## EX386 [Emphasis and strong emphasis]
expect SpecTest.html("5__6__78\n") == "<p>5__6__78</p>\n"

## EX387 [Emphasis and strong emphasis]
expect SpecTest.html("пристаням__стремятся__\n") == "<p>пристаням__стремятся__</p>\n"

## EX388 [Emphasis and strong emphasis]
expect SpecTest.html("__foo, __bar__, baz__\n") == "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"

## EX389 [Emphasis and strong emphasis]
expect SpecTest.html("foo-__(bar)__\n") == "<p>foo-<strong>(bar)</strong></p>\n"

## EX390 [Emphasis and strong emphasis]
expect SpecTest.html("**foo bar **\n") == "<p>**foo bar **</p>\n"

## EX391 [Emphasis and strong emphasis]
expect SpecTest.html("**(**foo)\n") == "<p>**(**foo)</p>\n"

## EX392 [Emphasis and strong emphasis]
expect SpecTest.html("*(**foo**)*\n") == "<p><em>(<strong>foo</strong>)</em></p>\n"

## EX393 [Emphasis and strong emphasis]
expect SpecTest.html("**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**\n") == "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"

## EX394 [Emphasis and strong emphasis]
expect SpecTest.html("**foo \"*bar*\" foo**\n") == "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"

## EX395 [Emphasis and strong emphasis]
expect SpecTest.html("**foo**bar\n") == "<p><strong>foo</strong>bar</p>\n"

## EX396 [Emphasis and strong emphasis]
expect SpecTest.html("__foo bar __\n") == "<p>__foo bar __</p>\n"

## EX397 [Emphasis and strong emphasis]
expect SpecTest.html("__(__foo)\n") == "<p>__(__foo)</p>\n"

## EX398 [Emphasis and strong emphasis]
expect SpecTest.html("_(__foo__)_\n") == "<p><em>(<strong>foo</strong>)</em></p>\n"

## EX399 [Emphasis and strong emphasis]
expect SpecTest.html("__foo__bar\n") == "<p>__foo__bar</p>\n"

## EX400 [Emphasis and strong emphasis]
expect SpecTest.html("__пристаням__стремятся\n") == "<p>__пристаням__стремятся</p>\n"

## EX401 [Emphasis and strong emphasis]
expect SpecTest.html("__foo__bar__baz__\n") == "<p><strong>foo__bar__baz</strong></p>\n"

## EX402 [Emphasis and strong emphasis]
expect SpecTest.html("__(bar)__.\n") == "<p><strong>(bar)</strong>.</p>\n"

## EX403 [Emphasis and strong emphasis]
expect SpecTest.html("*foo [bar](/url)*\n") == "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"

## EX404 [Emphasis and strong emphasis]
expect SpecTest.html("*foo\nbar*\n") == "<p><em>foo\nbar</em></p>\n"

## EX405 [Emphasis and strong emphasis]
expect SpecTest.html("_foo __bar__ baz_\n") == "<p><em>foo <strong>bar</strong> baz</em></p>\n"

## EX406 [Emphasis and strong emphasis]
expect SpecTest.html("_foo _bar_ baz_\n") == "<p><em>foo <em>bar</em> baz</em></p>\n"

## EX407 [Emphasis and strong emphasis]
expect SpecTest.html("__foo_ bar_\n") == "<p><em><em>foo</em> bar</em></p>\n"

## EX408 [Emphasis and strong emphasis]
expect SpecTest.html("*foo *bar**\n") == "<p><em>foo <em>bar</em></em></p>\n"

## EX409 [Emphasis and strong emphasis]
expect SpecTest.html("*foo **bar** baz*\n") == "<p><em>foo <strong>bar</strong> baz</em></p>\n"

## EX410 [Emphasis and strong emphasis]
expect SpecTest.html("*foo**bar**baz*\n") == "<p><em>foo<strong>bar</strong>baz</em></p>\n"

## EX411 [Emphasis and strong emphasis]
expect SpecTest.html("*foo**bar*\n") == "<p><em>foo**bar</em></p>\n"

## EX412 [Emphasis and strong emphasis]
expect SpecTest.html("***foo** bar*\n") == "<p><em><strong>foo</strong> bar</em></p>\n"

## EX413 [Emphasis and strong emphasis]
expect SpecTest.html("*foo **bar***\n") == "<p><em>foo <strong>bar</strong></em></p>\n"

## EX414 [Emphasis and strong emphasis]
expect SpecTest.html("*foo**bar***\n") == "<p><em>foo<strong>bar</strong></em></p>\n"

## EX415 [Emphasis and strong emphasis]
expect SpecTest.html("foo***bar***baz\n") == "<p>foo<em><strong>bar</strong></em>baz</p>\n"

## EX416 [Emphasis and strong emphasis]
expect SpecTest.html("foo******bar*********baz\n") == "<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>\n"

## EX417 [Emphasis and strong emphasis]
expect SpecTest.html("*foo **bar *baz* bim** bop*\n") == "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"

## EX418 [Emphasis and strong emphasis]
expect SpecTest.html("*foo [*bar*](/url)*\n") == "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"

## EX419 [Emphasis and strong emphasis]
expect SpecTest.html("** is not an empty emphasis\n") == "<p>** is not an empty emphasis</p>\n"

## EX420 [Emphasis and strong emphasis]
expect SpecTest.html("**** is not an empty strong emphasis\n") == "<p>**** is not an empty strong emphasis</p>\n"

## EX421 [Emphasis and strong emphasis]
expect SpecTest.html("**foo [bar](/url)**\n") == "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"

## EX422 [Emphasis and strong emphasis]
expect SpecTest.html("**foo\nbar**\n") == "<p><strong>foo\nbar</strong></p>\n"

## EX423 [Emphasis and strong emphasis]
expect SpecTest.html("__foo _bar_ baz__\n") == "<p><strong>foo <em>bar</em> baz</strong></p>\n"

## EX424 [Emphasis and strong emphasis]
expect SpecTest.html("__foo __bar__ baz__\n") == "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"

## EX425 [Emphasis and strong emphasis]
expect SpecTest.html("____foo__ bar__\n") == "<p><strong><strong>foo</strong> bar</strong></p>\n"

## EX426 [Emphasis and strong emphasis]
expect SpecTest.html("**foo **bar****\n") == "<p><strong>foo <strong>bar</strong></strong></p>\n"

## EX427 [Emphasis and strong emphasis]
expect SpecTest.html("**foo *bar* baz**\n") == "<p><strong>foo <em>bar</em> baz</strong></p>\n"

## EX428 [Emphasis and strong emphasis]
expect SpecTest.html("**foo*bar*baz**\n") == "<p><strong>foo<em>bar</em>baz</strong></p>\n"

## EX429 [Emphasis and strong emphasis]
expect SpecTest.html("***foo* bar**\n") == "<p><strong><em>foo</em> bar</strong></p>\n"

## EX430 [Emphasis and strong emphasis]
expect SpecTest.html("**foo *bar***\n") == "<p><strong>foo <em>bar</em></strong></p>\n"

## EX431 [Emphasis and strong emphasis]
expect SpecTest.html("**foo *bar **baz**\nbim* bop**\n") == "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"

## EX432 [Emphasis and strong emphasis]
expect SpecTest.html("**foo [*bar*](/url)**\n") == "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"

## EX433 [Emphasis and strong emphasis]
expect SpecTest.html("__ is not an empty emphasis\n") == "<p>__ is not an empty emphasis</p>\n"

## EX434 [Emphasis and strong emphasis]
expect SpecTest.html("____ is not an empty strong emphasis\n") == "<p>____ is not an empty strong emphasis</p>\n"

## EX435 [Emphasis and strong emphasis]
expect SpecTest.html("foo ***\n") == "<p>foo ***</p>\n"

## EX436 [Emphasis and strong emphasis]
expect SpecTest.html("foo *\\**\n") == "<p>foo <em>*</em></p>\n"

## EX437 [Emphasis and strong emphasis]
expect SpecTest.html("foo *_*\n") == "<p>foo <em>_</em></p>\n"

## EX438 [Emphasis and strong emphasis]
expect SpecTest.html("foo *****\n") == "<p>foo *****</p>\n"

## EX439 [Emphasis and strong emphasis]
expect SpecTest.html("foo **\\***\n") == "<p>foo <strong>*</strong></p>\n"

## EX440 [Emphasis and strong emphasis]
expect SpecTest.html("foo **_**\n") == "<p>foo <strong>_</strong></p>\n"

## EX441 [Emphasis and strong emphasis]
expect SpecTest.html("**foo*\n") == "<p>*<em>foo</em></p>\n"

## EX442 [Emphasis and strong emphasis]
expect SpecTest.html("*foo**\n") == "<p><em>foo</em>*</p>\n"

## EX443 [Emphasis and strong emphasis]
expect SpecTest.html("***foo**\n") == "<p>*<strong>foo</strong></p>\n"

## EX444 [Emphasis and strong emphasis]
expect SpecTest.html("****foo*\n") == "<p>***<em>foo</em></p>\n"

## EX445 [Emphasis and strong emphasis]
expect SpecTest.html("**foo***\n") == "<p><strong>foo</strong>*</p>\n"

## EX446 [Emphasis and strong emphasis]
expect SpecTest.html("*foo****\n") == "<p><em>foo</em>***</p>\n"

## EX447 [Emphasis and strong emphasis]
expect SpecTest.html("foo ___\n") == "<p>foo ___</p>\n"

## EX448 [Emphasis and strong emphasis]
expect SpecTest.html("foo _\\__\n") == "<p>foo <em>_</em></p>\n"

## EX449 [Emphasis and strong emphasis]
expect SpecTest.html("foo _*_\n") == "<p>foo <em>*</em></p>\n"

## EX450 [Emphasis and strong emphasis]
expect SpecTest.html("foo _____\n") == "<p>foo _____</p>\n"

## EX451 [Emphasis and strong emphasis]
expect SpecTest.html("foo __\\___\n") == "<p>foo <strong>_</strong></p>\n"

## EX452 [Emphasis and strong emphasis]
expect SpecTest.html("foo __*__\n") == "<p>foo <strong>*</strong></p>\n"

## EX453 [Emphasis and strong emphasis]
expect SpecTest.html("__foo_\n") == "<p>_<em>foo</em></p>\n"

## EX454 [Emphasis and strong emphasis]
expect SpecTest.html("_foo__\n") == "<p><em>foo</em>_</p>\n"

## EX455 [Emphasis and strong emphasis]
expect SpecTest.html("___foo__\n") == "<p>_<strong>foo</strong></p>\n"

## EX456 [Emphasis and strong emphasis]
expect SpecTest.html("____foo_\n") == "<p>___<em>foo</em></p>\n"

## EX457 [Emphasis and strong emphasis]
expect SpecTest.html("__foo___\n") == "<p><strong>foo</strong>_</p>\n"

## EX458 [Emphasis and strong emphasis]
expect SpecTest.html("_foo____\n") == "<p><em>foo</em>___</p>\n"

## EX459 [Emphasis and strong emphasis]
expect SpecTest.html("**foo**\n") == "<p><strong>foo</strong></p>\n"

## EX460 [Emphasis and strong emphasis]
expect SpecTest.html("*_foo_*\n") == "<p><em><em>foo</em></em></p>\n"

## EX461 [Emphasis and strong emphasis]
expect SpecTest.html("__foo__\n") == "<p><strong>foo</strong></p>\n"

## EX462 [Emphasis and strong emphasis]
expect SpecTest.html("_*foo*_\n") == "<p><em><em>foo</em></em></p>\n"

## EX463 [Emphasis and strong emphasis]
expect SpecTest.html("****foo****\n") == "<p><strong><strong>foo</strong></strong></p>\n"

## EX464 [Emphasis and strong emphasis]
expect SpecTest.html("____foo____\n") == "<p><strong><strong>foo</strong></strong></p>\n"

## EX465 [Emphasis and strong emphasis]
expect SpecTest.html("******foo******\n") == "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"

## EX466 [Emphasis and strong emphasis]
expect SpecTest.html("***foo***\n") == "<p><em><strong>foo</strong></em></p>\n"

## EX467 [Emphasis and strong emphasis]
expect SpecTest.html("_____foo_____\n") == "<p><em><strong><strong>foo</strong></strong></em></p>\n"

## EX468 [Emphasis and strong emphasis]
expect SpecTest.html("*foo _bar* baz_\n") == "<p><em>foo _bar</em> baz_</p>\n"

## EX469 [Emphasis and strong emphasis]
expect SpecTest.html("*foo __bar *baz bim__ bam*\n") == "<p><em>foo <strong>bar *baz bim</strong> bam</em></p>\n"

## EX470 [Emphasis and strong emphasis]
expect SpecTest.html("**foo **bar baz**\n") == "<p>**foo <strong>bar baz</strong></p>\n"

## EX471 [Emphasis and strong emphasis]
expect SpecTest.html("*foo *bar baz*\n") == "<p>*foo <em>bar baz</em></p>\n"

## EX472 [Emphasis and strong emphasis]
expect SpecTest.html("*[bar*](/url)\n") == "<p>*<a href=\"/url\">bar*</a></p>\n"

## EX473 [Emphasis and strong emphasis]
expect SpecTest.html("_foo [bar_](/url)\n") == "<p>_foo <a href=\"/url\">bar_</a></p>\n"

## EX474 [Emphasis and strong emphasis]
expect SpecTest.html("*<img src=\"foo\" title=\"*\"/>\n") == "<p>*<img src=\"foo\" title=\"*\"/></p>\n"

## EX475 [Emphasis and strong emphasis]
expect SpecTest.html("**<a href=\"**\">\n") == "<p>**<a href=\"**\"></p>\n"

## EX476 [Emphasis and strong emphasis]
expect SpecTest.html("__<a href=\"__\">\n") == "<p>__<a href=\"__\"></p>\n"

## EX477 [Emphasis and strong emphasis]
expect SpecTest.html("*a `*`*\n") == "<p><em>a <code>*</code></em></p>\n"

## EX478 [Emphasis and strong emphasis]
expect SpecTest.html("_a `_`_\n") == "<p><em>a <code>_</code></em></p>\n"

## EX479 [Emphasis and strong emphasis]
expect SpecTest.html("**a<https://foo.bar/?q=**>\n") == "<p>**a<a href=\"https://foo.bar/?q=**\">https://foo.bar/?q=**</a></p>\n"

## EX480 [Emphasis and strong emphasis]
expect SpecTest.html("__a<https://foo.bar/?q=__>\n") == "<p>__a<a href=\"https://foo.bar/?q=__\">https://foo.bar/?q=__</a></p>\n"

## EX481 [Links]
expect SpecTest.html("[link](/uri \"title\")\n") == "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"

## EX482 [Links]
expect SpecTest.html("[link](/uri)\n") == "<p><a href=\"/uri\">link</a></p>\n"

## EX483 [Links]
expect SpecTest.html("[](./target.md)\n") == "<p><a href=\"./target.md\"></a></p>\n"

## EX484 [Links]
expect SpecTest.html("[link]()\n") == "<p><a href=\"\">link</a></p>\n"

## EX485 [Links]
expect SpecTest.html("[link](<>)\n") == "<p><a href=\"\">link</a></p>\n"

## EX486 [Links]
expect SpecTest.html("[]()\n") == "<p><a href=\"\"></a></p>\n"

## EX487 [Links]
expect SpecTest.html("[link](/my uri)\n") == "<p>[link](/my uri)</p>\n"

## EX488 [Links]
expect SpecTest.html("[link](</my uri>)\n") == "<p><a href=\"/my%20uri\">link</a></p>\n"

## EX489 [Links]
expect SpecTest.html("[link](foo\nbar)\n") == "<p>[link](foo\nbar)</p>\n"

## EX490 [Links]
expect SpecTest.html("[link](<foo\nbar>)\n") == "<p>[link](<foo\nbar>)</p>\n"

## EX491 [Links]
expect SpecTest.html("[a](<b)c>)\n") == "<p><a href=\"b)c\">a</a></p>\n"

## EX492 [Links]
expect SpecTest.html("[link](<foo\\>)\n") == "<p>[link](&lt;foo&gt;)</p>\n"

## EX493 [Links]
expect SpecTest.html("[a](<b)c\n[a](<b)c>\n[a](<b>c)\n") == "<p>[a](&lt;b)c\n[a](&lt;b)c&gt;\n[a](<b>c)</p>\n"

## EX494 [Links]
expect SpecTest.html("[link](\\(foo\\))\n") == "<p><a href=\"(foo)\">link</a></p>\n"

## EX495 [Links]
expect SpecTest.html("[link](foo(and(bar)))\n") == "<p><a href=\"foo(and(bar))\">link</a></p>\n"

## EX496 [Links]
expect SpecTest.html("[link](foo(and(bar))\n") == "<p>[link](foo(and(bar))</p>\n"

## EX497 [Links]
expect SpecTest.html("[link](foo\\(and\\(bar\\))\n") == "<p><a href=\"foo(and(bar)\">link</a></p>\n"

## EX498 [Links]
expect SpecTest.html("[link](<foo(and(bar)>)\n") == "<p><a href=\"foo(and(bar)\">link</a></p>\n"

## EX499 [Links]
expect SpecTest.html("[link](foo\\)\\:)\n") == "<p><a href=\"foo):\">link</a></p>\n"

## EX500 [Links]
expect SpecTest.html("[link](#fragment)\n\n[link](https://example.com#fragment)\n\n[link](https://example.com?foo=3#frag)\n") == "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"https://example.com#fragment\">link</a></p>\n<p><a href=\"https://example.com?foo=3#frag\">link</a></p>\n"

## EX501 [Links]
expect SpecTest.html("[link](foo\\bar)\n") == "<p><a href=\"foo%5Cbar\">link</a></p>\n"

## EX502 [Links]
expect SpecTest.html("[link](foo%20b&auml;)\n") == "<p><a href=\"foo%20b%C3%A4\">link</a></p>\n"

## EX503 [Links]
expect SpecTest.html("[link](\"title\")\n") == "<p><a href=\"%22title%22\">link</a></p>\n"

## EX504 [Links]
expect SpecTest.html("[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))\n") == "<p><a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a></p>\n"

## EX505 [Links]
expect SpecTest.html("[link](/url \"title \\\"&quot;\")\n") == "<p><a href=\"/url\" title=\"title &quot;&quot;\">link</a></p>\n"

## EX506 [Links]
expect SpecTest.html("[link](/url \"title\")\n") == "<p><a href=\"/url%C2%A0%22title%22\">link</a></p>\n"

## EX507 [Links]
expect SpecTest.html("[link](/url \"title \"and\" title\")\n") == "<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>\n"

## EX508 [Links]
expect SpecTest.html("[link](/url 'title \"and\" title')\n") == "<p><a href=\"/url\" title=\"title &quot;and&quot; title\">link</a></p>\n"

## EX509 [Links]
expect SpecTest.html("[link](   /uri\n  \"title\"  )\n") == "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"

## EX510 [Links]
expect SpecTest.html("[link] (/uri)\n") == "<p>[link] (/uri)</p>\n"

## EX511 [Links]
expect SpecTest.html("[link [foo [bar]]](/uri)\n") == "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"

## EX512 [Links]
expect SpecTest.html("[link] bar](/uri)\n") == "<p>[link] bar](/uri)</p>\n"

## EX513 [Links]
expect SpecTest.html("[link [bar](/uri)\n") == "<p>[link <a href=\"/uri\">bar</a></p>\n"

## EX514 [Links]
expect SpecTest.html("[link \\[bar](/uri)\n") == "<p><a href=\"/uri\">link [bar</a></p>\n"

## EX515 [Links]
expect SpecTest.html("[link *foo **bar** `#`*](/uri)\n") == "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"

## EX516 [Links]
expect SpecTest.html("[![moon](moon.jpg)](/uri)\n") == "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"

## EX517 [Links]
expect SpecTest.html("[foo [bar](/uri)](/uri)\n") == "<p>[foo <a href=\"/uri\">bar</a>](/uri)</p>\n"

## EX518 [Links]
expect SpecTest.html("[foo *[bar [baz](/uri)](/uri)*](/uri)\n") == "<p>[foo <em>[bar <a href=\"/uri\">baz</a>](/uri)</em>](/uri)</p>\n"

## EX519 [Links]
expect SpecTest.html("![[[foo](uri1)](uri2)](uri3)\n") == "<p><img src=\"uri3\" alt=\"[foo](uri2)\" /></p>\n"

## EX520 [Links]
expect SpecTest.html("*[foo*](/uri)\n") == "<p>*<a href=\"/uri\">foo*</a></p>\n"

## EX521 [Links]
expect SpecTest.html("[foo *bar](baz*)\n") == "<p><a href=\"baz*\">foo *bar</a></p>\n"

## EX522 [Links]
expect SpecTest.html("*foo [bar* baz]\n") == "<p><em>foo [bar</em> baz]</p>\n"

## EX523 [Links]
expect SpecTest.html("[foo <bar attr=\"](baz)\">\n") == "<p>[foo <bar attr=\"](baz)\"></p>\n"

## EX524 [Links]
expect SpecTest.html("[foo`](/uri)`\n") == "<p>[foo<code>](/uri)</code></p>\n"

## EX525 [Links]
expect SpecTest.html("[foo<https://example.com/?search=](uri)>\n") == "<p>[foo<a href=\"https://example.com/?search=%5D(uri)\">https://example.com/?search=](uri)</a></p>\n"

## EX526 [Links]
expect SpecTest.html("[foo][bar]\n\n[bar]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX527 [Links]
expect SpecTest.html("[link [foo [bar]]][ref]\n\n[ref]: /uri\n") == "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"

## EX528 [Links]
expect SpecTest.html("[link \\[bar][ref]\n\n[ref]: /uri\n") == "<p><a href=\"/uri\">link [bar</a></p>\n"

## EX529 [Links]
expect SpecTest.html("[link *foo **bar** `#`*][ref]\n\n[ref]: /uri\n") == "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"

## EX530 [Links]
expect SpecTest.html("[![moon](moon.jpg)][ref]\n\n[ref]: /uri\n") == "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"

## EX531 [Links]
expect SpecTest.html("[foo [bar](/uri)][ref]\n\n[ref]: /uri\n") == "<p>[foo <a href=\"/uri\">bar</a>]<a href=\"/uri\">ref</a></p>\n"

## EX532 [Links]
expect SpecTest.html("[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri\n") == "<p>[foo <em>bar <a href=\"/uri\">baz</a></em>]<a href=\"/uri\">ref</a></p>\n"

## EX533 [Links]
expect SpecTest.html("*[foo*][ref]\n\n[ref]: /uri\n") == "<p>*<a href=\"/uri\">foo*</a></p>\n"

## EX534 [Links]
expect SpecTest.html("[foo *bar][ref]*\n\n[ref]: /uri\n") == "<p><a href=\"/uri\">foo *bar</a>*</p>\n"

## EX535 [Links]
expect SpecTest.html("[foo <bar attr=\"][ref]\">\n\n[ref]: /uri\n") == "<p>[foo <bar attr=\"][ref]\"></p>\n"

## EX536 [Links]
expect SpecTest.html("[foo`][ref]`\n\n[ref]: /uri\n") == "<p>[foo<code>][ref]</code></p>\n"

## EX537 [Links]
expect SpecTest.html("[foo<https://example.com/?search=][ref]>\n\n[ref]: /uri\n") == "<p>[foo<a href=\"https://example.com/?search=%5D%5Bref%5D\">https://example.com/?search=][ref]</a></p>\n"

## EX538 [Links]
expect SpecTest.html("[foo][BaR]\n\n[bar]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX539 [Links]
expect SpecTest.html("[ẞ]\n\n[SS]: /url\n") == "<p><a href=\"/url\">ẞ</a></p>\n"

## EX540 [Links]
expect SpecTest.html("[Foo\n  bar]: /url\n\n[Baz][Foo bar]\n") == "<p><a href=\"/url\">Baz</a></p>\n"

## EX541 [Links]
expect SpecTest.html("[foo] [bar]\n\n[bar]: /url \"title\"\n") == "<p>[foo] <a href=\"/url\" title=\"title\">bar</a></p>\n"

## EX542 [Links]
expect SpecTest.html("[foo]\n[bar]\n\n[bar]: /url \"title\"\n") == "<p>[foo]\n<a href=\"/url\" title=\"title\">bar</a></p>\n"

## EX543 [Links]
expect SpecTest.html("[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]\n") == "<p><a href=\"/url1\">bar</a></p>\n"

## EX544 [Links]
expect SpecTest.html("[bar][foo\\!]\n\n[foo!]: /url\n") == "<p>[bar][foo!]</p>\n"

## EX545 [Links]
expect SpecTest.html("[foo][ref[]\n\n[ref[]: /uri\n") == "<p>[foo][ref[]</p>\n<p>[ref[]: /uri</p>\n"

## EX546 [Links]
expect SpecTest.html("[foo][ref[bar]]\n\n[ref[bar]]: /uri\n") == "<p>[foo][ref[bar]]</p>\n<p>[ref[bar]]: /uri</p>\n"

## EX547 [Links]
expect SpecTest.html("[[[foo]]]\n\n[[[foo]]]: /url\n") == "<p>[[[foo]]]</p>\n<p>[[[foo]]]: /url</p>\n"

## EX548 [Links]
expect SpecTest.html("[foo][ref\\[]\n\n[ref\\[]: /uri\n") == "<p><a href=\"/uri\">foo</a></p>\n"

## EX549 [Links]
expect SpecTest.html("[bar\\\\]: /uri\n\n[bar\\\\]\n") == "<p><a href=\"/uri\">bar\\</a></p>\n"

## EX550 [Links]
expect SpecTest.html("[]\n\n[]: /uri\n") == "<p>[]</p>\n<p>[]: /uri</p>\n"

## EX551 [Links]
expect SpecTest.html("[\n ]\n\n[\n ]: /uri\n") == "<p>[\n]</p>\n<p>[\n]: /uri</p>\n"

## EX552 [Links]
expect SpecTest.html("[foo][]\n\n[foo]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX553 [Links]
expect SpecTest.html("[*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"

## EX554 [Links]
expect SpecTest.html("[Foo][]\n\n[foo]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"

## EX555 [Links]
expect SpecTest.html("[foo] \n[]\n\n[foo]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">foo</a>\n[]</p>\n"

## EX556 [Links]
expect SpecTest.html("[foo]\n\n[foo]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX557 [Links]
expect SpecTest.html("[*foo* bar]\n\n[*foo* bar]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"

## EX558 [Links]
expect SpecTest.html("[[*foo* bar]]\n\n[*foo* bar]: /url \"title\"\n") == "<p>[<a href=\"/url\" title=\"title\"><em>foo</em> bar</a>]</p>\n"

## EX559 [Links]
expect SpecTest.html("[[bar [foo]\n\n[foo]: /url\n") == "<p>[[bar <a href=\"/url\">foo</a></p>\n"

## EX560 [Links]
expect SpecTest.html("[Foo]\n\n[foo]: /url \"title\"\n") == "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"

## EX561 [Links]
expect SpecTest.html("[foo] bar\n\n[foo]: /url\n") == "<p><a href=\"/url\">foo</a> bar</p>\n"

## EX562 [Links]
expect SpecTest.html("\\[foo]\n\n[foo]: /url \"title\"\n") == "<p>[foo]</p>\n"

## EX563 [Links]
expect SpecTest.html("[foo*]: /url\n\n*[foo*]\n") == "<p>*<a href=\"/url\">foo*</a></p>\n"

## EX564 [Links]
expect SpecTest.html("[foo][bar]\n\n[foo]: /url1\n[bar]: /url2\n") == "<p><a href=\"/url2\">foo</a></p>\n"

## EX565 [Links]
expect SpecTest.html("[foo][]\n\n[foo]: /url1\n") == "<p><a href=\"/url1\">foo</a></p>\n"

## EX566 [Links]
expect SpecTest.html("[foo]()\n\n[foo]: /url1\n") == "<p><a href=\"\">foo</a></p>\n"

## EX567 [Links]
expect SpecTest.html("[foo](not a link)\n\n[foo]: /url1\n") == "<p><a href=\"/url1\">foo</a>(not a link)</p>\n"

## EX568 [Links]
expect SpecTest.html("[foo][bar][baz]\n\n[baz]: /url\n") == "<p>[foo]<a href=\"/url\">bar</a></p>\n"

## EX569 [Links]
expect SpecTest.html("[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2\n") == "<p><a href=\"/url2\">foo</a><a href=\"/url1\">baz</a></p>\n"

## EX570 [Links]
expect SpecTest.html("[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2\n") == "<p>[foo]<a href=\"/url1\">bar</a></p>\n"

## EX571 [Images]
expect SpecTest.html("![foo](/url \"title\")\n") == "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"

## EX572 [Images]
expect SpecTest.html("![foo *bar*]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n") == "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"

## EX573 [Images]
expect SpecTest.html("![foo ![bar](/url)](/url2)\n") == "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n"

## EX574 [Images]
expect SpecTest.html("![foo [bar](/url)](/url2)\n") == "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n"

## EX575 [Images]
expect SpecTest.html("![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n") == "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"

## EX576 [Images]
expect SpecTest.html("![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\"\n") == "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"

## EX577 [Images]
expect SpecTest.html("![foo](train.jpg)\n") == "<p><img src=\"train.jpg\" alt=\"foo\" /></p>\n"

## EX578 [Images]
expect SpecTest.html("My ![foo bar](/path/to/train.jpg  \"title\"   )\n") == "<p>My <img src=\"/path/to/train.jpg\" alt=\"foo bar\" title=\"title\" /></p>\n"

## EX579 [Images]
expect SpecTest.html("![foo](<url>)\n") == "<p><img src=\"url\" alt=\"foo\" /></p>\n"

## EX580 [Images]
expect SpecTest.html("![](/url)\n") == "<p><img src=\"/url\" alt=\"\" /></p>\n"

## EX581 [Images]
expect SpecTest.html("![foo][bar]\n\n[bar]: /url\n") == "<p><img src=\"/url\" alt=\"foo\" /></p>\n"

## EX582 [Images]
expect SpecTest.html("![foo][bar]\n\n[BAR]: /url\n") == "<p><img src=\"/url\" alt=\"foo\" /></p>\n"

## EX583 [Images]
expect SpecTest.html("![foo][]\n\n[foo]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"

## EX584 [Images]
expect SpecTest.html("![*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"foo bar\" title=\"title\" /></p>\n"

## EX585 [Images]
expect SpecTest.html("![Foo][]\n\n[foo]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"Foo\" title=\"title\" /></p>\n"

## EX586 [Images]
expect SpecTest.html("![foo] \n[]\n\n[foo]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"foo\" title=\"title\" />\n[]</p>\n"

## EX587 [Images]
expect SpecTest.html("![foo]\n\n[foo]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"

## EX588 [Images]
expect SpecTest.html("![*foo* bar]\n\n[*foo* bar]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"foo bar\" title=\"title\" /></p>\n"

## EX589 [Images]
expect SpecTest.html("![[foo]]\n\n[[foo]]: /url \"title\"\n") == "<p>![[foo]]</p>\n<p>[[foo]]: /url &quot;title&quot;</p>\n"

## EX590 [Images]
expect SpecTest.html("![Foo]\n\n[foo]: /url \"title\"\n") == "<p><img src=\"/url\" alt=\"Foo\" title=\"title\" /></p>\n"

## EX591 [Images]
expect SpecTest.html("!\\[foo]\n\n[foo]: /url \"title\"\n") == "<p>![foo]</p>\n"

## EX592 [Images]
expect SpecTest.html("\\![foo]\n\n[foo]: /url \"title\"\n") == "<p>!<a href=\"/url\" title=\"title\">foo</a></p>\n"

## EX593 [Autolinks]
expect SpecTest.html("<http://foo.bar.baz>\n") == "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"

## EX594 [Autolinks]
expect SpecTest.html("<https://foo.bar.baz/test?q=hello&id=22&boolean>\n") == "<p><a href=\"https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"

## EX595 [Autolinks]
expect SpecTest.html("<irc://foo.bar:2233/baz>\n") == "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"

## EX596 [Autolinks]
expect SpecTest.html("<MAILTO:FOO@BAR.BAZ>\n") == "<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>\n"

## EX597 [Autolinks]
expect SpecTest.html("<a+b+c:d>\n") == "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"

## EX598 [Autolinks]
expect SpecTest.html("<made-up-scheme://foo,bar>\n") == "<p><a href=\"made-up-scheme://foo,bar\">made-up-scheme://foo,bar</a></p>\n"

## EX599 [Autolinks]
expect SpecTest.html("<https://../>\n") == "<p><a href=\"https://../\">https://../</a></p>\n"

## EX600 [Autolinks]
expect SpecTest.html("<localhost:5001/foo>\n") == "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"

## EX601 [Autolinks]
expect SpecTest.html("<https://foo.bar/baz bim>\n") == "<p>&lt;https://foo.bar/baz bim&gt;</p>\n"

## EX602 [Autolinks]
expect SpecTest.html("<https://example.com/\\[\\>\n") == "<p><a href=\"https://example.com/%5C%5B%5C\">https://example.com/\\[\\</a></p>\n"

## EX603 [Autolinks]
expect SpecTest.html("<foo@bar.example.com>\n") == "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"

## EX604 [Autolinks]
expect SpecTest.html("<foo+special@Bar.baz-bar0.com>\n") == "<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"

## EX605 [Autolinks]
expect SpecTest.html("<foo\\+@bar.example.com>\n") == "<p>&lt;foo+@bar.example.com&gt;</p>\n"

## EX606 [Autolinks]
expect SpecTest.html("<>\n") == "<p>&lt;&gt;</p>\n"

## EX607 [Autolinks]
expect SpecTest.html("< https://foo.bar >\n") == "<p>&lt; https://foo.bar &gt;</p>\n"

## EX608 [Autolinks]
expect SpecTest.html("<m:abc>\n") == "<p>&lt;m:abc&gt;</p>\n"

## EX609 [Autolinks]
expect SpecTest.html("<foo.bar.baz>\n") == "<p>&lt;foo.bar.baz&gt;</p>\n"

## EX610 [Autolinks]
expect SpecTest.html("https://example.com\n") == "<p>https://example.com</p>\n"

## EX611 [Autolinks]
expect SpecTest.html("foo@bar.example.com\n") == "<p>foo@bar.example.com</p>\n"

## EX612 [Raw HTML]
expect SpecTest.html("<a><bab><c2c>\n") == "<p><a><bab><c2c></p>\n"

## EX613 [Raw HTML]
expect SpecTest.html("<a/><b2/>\n") == "<p><a/><b2/></p>\n"

## EX614 [Raw HTML]
expect SpecTest.html("<a  /><b2\ndata=\"foo\" >\n") == "<p><a  /><b2\ndata=\"foo\" ></p>\n"

## EX615 [Raw HTML]
expect SpecTest.html("<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n") == "<p><a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 /></p>\n"

## EX616 [Raw HTML]
expect SpecTest.html("Foo <responsive-image src=\"foo.jpg\" />\n") == "<p>Foo <responsive-image src=\"foo.jpg\" /></p>\n"

## EX617 [Raw HTML]
expect SpecTest.html("<33> <__>\n") == "<p>&lt;33&gt; &lt;__&gt;</p>\n"

## EX618 [Raw HTML]
expect SpecTest.html("<a h*#ref=\"hi\">\n") == "<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>\n"

## EX619 [Raw HTML]
expect SpecTest.html("<a href=\"hi'> <a href=hi'>\n") == "<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>\n"

## EX620 [Raw HTML]
expect SpecTest.html("< a><\nfoo><bar/ >\n<foo bar=baz\nbim!bop />\n") == "<p>&lt; a&gt;&lt;\nfoo&gt;&lt;bar/ &gt;\n&lt;foo bar=baz\nbim!bop /&gt;</p>\n"

## EX621 [Raw HTML]
expect SpecTest.html("<a href='bar'title=title>\n") == "<p>&lt;a href='bar'title=title&gt;</p>\n"

## EX622 [Raw HTML]
expect SpecTest.html("</a></foo >\n") == "<p></a></foo ></p>\n"

## EX623 [Raw HTML]
expect SpecTest.html("</a href=\"foo\">\n") == "<p>&lt;/a href=&quot;foo&quot;&gt;</p>\n"

## EX624 [Raw HTML]
expect SpecTest.html("foo <!-- this is a --\ncomment - with hyphens -->\n") == "<p>foo <!-- this is a --\ncomment - with hyphens --></p>\n"

## EX625 [Raw HTML]
expect SpecTest.html("foo <!--> foo -->\n\nfoo <!---> foo -->\n") == "<p>foo <!--> foo --&gt;</p>\n<p>foo <!---> foo --&gt;</p>\n"

## EX626 [Raw HTML]
expect SpecTest.html("foo <?php echo $a; ?>\n") == "<p>foo <?php echo $a; ?></p>\n"

## EX627 [Raw HTML]
expect SpecTest.html("foo <!ELEMENT br EMPTY>\n") == "<p>foo <!ELEMENT br EMPTY></p>\n"

## EX628 [Raw HTML]
expect SpecTest.html("foo <![CDATA[>&<]]>\n") == "<p>foo <![CDATA[>&<]]></p>\n"

## EX629 [Raw HTML]
expect SpecTest.html("foo <a href=\"&ouml;\">\n") == "<p>foo <a href=\"&ouml;\"></p>\n"

## EX630 [Raw HTML]
expect SpecTest.html("foo <a href=\"\\*\">\n") == "<p>foo <a href=\"\\*\"></p>\n"

## EX631 [Raw HTML]
expect SpecTest.html("<a href=\"\\\"\">\n") == "<p>&lt;a href=&quot;&quot;&quot;&gt;</p>\n"

## EX632 [Hard line breaks]
expect SpecTest.html("foo  \nbaz\n") == "<p>foo<br />\nbaz</p>\n"

## EX633 [Hard line breaks]
expect SpecTest.html("foo\\\nbaz\n") == "<p>foo<br />\nbaz</p>\n"

## EX634 [Hard line breaks]
expect SpecTest.html("foo       \nbaz\n") == "<p>foo<br />\nbaz</p>\n"

## EX635 [Hard line breaks]
expect SpecTest.html("foo  \n     bar\n") == "<p>foo<br />\nbar</p>\n"

## EX636 [Hard line breaks]
expect SpecTest.html("foo\\\n     bar\n") == "<p>foo<br />\nbar</p>\n"

## EX637 [Hard line breaks]
expect SpecTest.html("*foo  \nbar*\n") == "<p><em>foo<br />\nbar</em></p>\n"

## EX638 [Hard line breaks]
expect SpecTest.html("*foo\\\nbar*\n") == "<p><em>foo<br />\nbar</em></p>\n"

## EX639 [Hard line breaks]
expect SpecTest.html("`code  \nspan`\n") == "<p><code>code   span</code></p>\n"

## EX640 [Hard line breaks]
expect SpecTest.html("`code\\\nspan`\n") == "<p><code>code\\ span</code></p>\n"

## EX641 [Hard line breaks]
expect SpecTest.html("<a href=\"foo  \nbar\">\n") == "<p><a href=\"foo  \nbar\"></p>\n"

## EX642 [Hard line breaks]
expect SpecTest.html("<a href=\"foo\\\nbar\">\n") == "<p><a href=\"foo\\\nbar\"></p>\n"

## EX643 [Hard line breaks]
expect SpecTest.html("foo\\\n") == "<p>foo\\</p>\n"

## EX644 [Hard line breaks]
expect SpecTest.html("foo  \n") == "<p>foo</p>\n"

## EX645 [Hard line breaks]
expect SpecTest.html("### foo\\\n") == "<h3>foo\\</h3>\n"

## EX646 [Hard line breaks]
expect SpecTest.html("### foo  \n") == "<h3>foo</h3>\n"

## EX647 [Soft line breaks]
expect SpecTest.html("foo\nbaz\n") == "<p>foo\nbaz</p>\n"

## EX648 [Soft line breaks]
expect SpecTest.html("foo \n baz\n") == "<p>foo\nbaz</p>\n"

## EX649 [Textual content]
expect SpecTest.html("hello $.;'there\n") == "<p>hello $.;'there</p>\n"

## EX650 [Textual content]
expect SpecTest.html("Foo χρῆν\n") == "<p>Foo χρῆν</p>\n"

## EX651 [Textual content]
expect SpecTest.html("Multiple     spaces\n") == "<p>Multiple     spaces</p>\n"

## EX652 [Tables (extension)]
expect SpecTest.html("| foo | bar |\n| --- | --- |\n| baz | bim |\n") == "<table>\n<thead>\n<tr>\n<th>foo</th>\n<th>bar</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>baz</td>\n<td>bim</td>\n</tr>\n</tbody>\n</table>\n"

## EX653 [Tables (extension)]
expect SpecTest.html("| abc | defghi |\n:-: | -----------:\nbar | baz\n") == "<table>\n<thead>\n<tr>\n<th align=\"center\">abc</th>\n<th align=\"right\">defghi</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td align=\"center\">bar</td>\n<td align=\"right\">baz</td>\n</tr>\n</tbody>\n</table>\n"

## EX654 [Tables (extension)]
expect SpecTest.html("| f\\|oo  |\n| ------ |\n| b `\\|` az |\n| b **\\|** im |\n") == "<table>\n<thead>\n<tr>\n<th>f|oo</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>b <code>|</code> az</td>\n</tr>\n<tr>\n<td>b <strong>|</strong> im</td>\n</tr>\n</tbody>\n</table>\n"

## EX655 [Tables (extension)]
expect SpecTest.html("| abc | def |\n| --- | --- |\n| bar | baz |\n> bar\n") == "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>bar</td>\n<td>baz</td>\n</tr>\n</tbody>\n</table>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"

## EX656 [Tables (extension)]
expect SpecTest.html("| abc | def |\n| --- | --- |\n| bar | baz |\nbar\n\nbar\n") == "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>bar</td>\n<td>baz</td>\n</tr>\n<tr>\n<td>bar</td>\n<td></td>\n</tr>\n</tbody>\n</table>\n<p>bar</p>\n"

## EX657 [Tables (extension)]
expect SpecTest.html("| abc | def |\n| --- |\n| bar |\n") == "<p>| abc | def |\n| --- |\n| bar |</p>\n"

## EX658 [Tables (extension)]
expect SpecTest.html("| abc | def |\n| --- | --- |\n| bar |\n| bar | baz | boo |\n") == "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>bar</td>\n<td></td>\n</tr>\n<tr>\n<td>bar</td>\n<td>baz</td>\n</tr>\n</tbody>\n</table>\n"

## EX659 [Tables (extension)]
expect SpecTest.html("| abc | def |\n| --- | --- |\n") == "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n</table>\n"

## EX660 [Task list items (extension)]
expect SpecTest.html("- [ ] foo\n- [x] bar\n") == "<ul>\n<li><input disabled=\"\" type=\"checkbox\"> foo</li>\n<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> bar</li>\n</ul>\n"

## EX661 [Task list items (extension)]
expect SpecTest.html("- [x] foo\n  - [ ] bar\n  - [x] baz\n- [ ] bim\n") == "<ul>\n<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> foo\n<ul>\n<li><input disabled=\"\" type=\"checkbox\"> bar</li>\n<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> baz</li>\n</ul>\n</li>\n<li><input disabled=\"\" type=\"checkbox\"> bim</li>\n</ul>\n"

## EX662 [Strikethrough (extension)]
expect SpecTest.html("~~Hi~~ Hello, world!\n") == "<p><del>Hi</del> Hello, world!</p>\n"

## EX663 [Strikethrough (extension)]
expect SpecTest.html("This ~~has a\n\nnew paragraph~~.\n") == "<p>This ~~has a</p>\n<p>new paragraph~~.</p>\n"

## EX664 [Autolinks (extension)]
expect SpecTest.html("www.commonmark.org\n") == "<p><a href=\"http://www.commonmark.org\">www.commonmark.org</a></p>\n"

## EX665 [Autolinks (extension)]
expect SpecTest.html("Visit www.commonmark.org/help for more information.\n") == "<p>Visit <a href=\"http://www.commonmark.org/help\">www.commonmark.org/help</a> for more information.</p>\n"

## EX666 [Autolinks (extension)]
expect SpecTest.html("Visit www.commonmark.org.\n\nVisit www.commonmark.org/a.b.\n") == "<p>Visit <a href=\"http://www.commonmark.org\">www.commonmark.org</a>.</p>\n<p>Visit <a href=\"http://www.commonmark.org/a.b\">www.commonmark.org/a.b</a>.</p>\n"

## EX667 [Autolinks (extension)]
expect SpecTest.html("www.google.com/search?q=Markup+(business)\n\nwww.google.com/search?q=Markup+(business)))\n\n(www.google.com/search?q=Markup+(business))\n\n(www.google.com/search?q=Markup+(business)\n") == "<p><a href=\"http://www.google.com/search?q=Markup+(business)\">www.google.com/search?q=Markup+(business)</a></p>\n<p><a href=\"http://www.google.com/search?q=Markup+(business)\">www.google.com/search?q=Markup+(business)</a>))</p>\n<p>(<a href=\"http://www.google.com/search?q=Markup+(business)\">www.google.com/search?q=Markup+(business)</a>)</p>\n<p>(<a href=\"http://www.google.com/search?q=Markup+(business)\">www.google.com/search?q=Markup+(business)</a></p>\n"

## EX668 [Autolinks (extension)]
expect SpecTest.html("www.google.com/search?q=(business))+ok\n") == "<p><a href=\"http://www.google.com/search?q=(business))+ok\">www.google.com/search?q=(business))+ok</a></p>\n"

## EX669 [Autolinks (extension)]
expect SpecTest.html("www.google.com/search?q=commonmark&hl=en\n\nwww.google.com/search?q=commonmark&hl;\n") == "<p><a href=\"http://www.google.com/search?q=commonmark&amp;hl=en\">www.google.com/search?q=commonmark&amp;hl=en</a></p>\n<p><a href=\"http://www.google.com/search?q=commonmark\">www.google.com/search?q=commonmark</a>&amp;hl;</p>\n"

## EX670 [Autolinks (extension)]
expect SpecTest.html("www.commonmark.org/he<lp\n") == "<p><a href=\"http://www.commonmark.org/he\">www.commonmark.org/he</a>&lt;lp</p>\n"

## EX671 [Autolinks (extension)]
expect SpecTest.html("http://commonmark.org\n\n(Visit https://encrypted.google.com/search?q=Markup+(business))\n\nAnonymous FTP is available at ftp://foo.bar.baz.\n") == "<p><a href=\"http://commonmark.org\">http://commonmark.org</a></p>\n<p>(Visit <a href=\"https://encrypted.google.com/search?q=Markup+(business)\">https://encrypted.google.com/search?q=Markup+(business)</a>)</p>\n<p>Anonymous FTP is available at <a href=\"ftp://foo.bar.baz\">ftp://foo.bar.baz</a>.</p>\n"

## EX672 [Autolinks (extension)]
expect SpecTest.html("foo@bar.baz\n") == "<p><a href=\"mailto:foo@bar.baz\">foo@bar.baz</a></p>\n"

## EX673 [Autolinks (extension)]
expect SpecTest.html("hello@mail+xyz.example isn't valid, but hello+xyz@mail.example is.\n") == "<p>hello@mail+xyz.example isn't valid, but <a href=\"mailto:hello+xyz@mail.example\">hello+xyz@mail.example</a> is.</p>\n"

## EX674 [Autolinks (extension)]
expect SpecTest.html("a.b-c_d@a.b\n\na.b-c_d@a.b.\n\na.b-c_d@a.b-\n\na.b-c_d@a.b_\n") == "<p><a href=\"mailto:a.b-c_d@a.b\">a.b-c_d@a.b</a></p>\n<p><a href=\"mailto:a.b-c_d@a.b\">a.b-c_d@a.b</a>.</p>\n<p>a.b-c_d@a.b-</p>\n<p>a.b-c_d@a.b_</p>\n"

## EX675 [Disallowed Raw HTML (extension)]
expect SpecTest.html("<strong> <title> <style> <em>\n\n<blockquote>\n  <xmp> is disallowed.  <XMP> is also disallowed.\n</blockquote>\n") == "<p><strong> &lt;title> &lt;style> <em></p>\n<blockquote>\n  &lt;xmp> is disallowed.  &lt;XMP> is also disallowed.\n</blockquote>\n"
