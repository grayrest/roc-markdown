## The document the harness parses, and the only Roc the measurement adds on
## top of the package itself.
##
## It exercises one of each construct that reaches a different part of the
## parser — heading, emphasis, strong, link, code span, list, task item,
## quote, table, fence — plus `&amp;` and `&copy;`, which are what reach the
## entity table the README weighs. Keeping all of it in one document is what
## makes the module link the whole parser rather than the subset one block
## type needs.
import md.Markdown
import md.Block

Sample := [].{
    run : I64 -> { message : Str, n : I64 }
    run = |seed| {
        blocks : List(Block)
        blocks = Markdown.parse(Sample.source(seed))
        { message: Markdown.to_html(blocks), n: List.len(blocks).to_i64_wrap() }
    }

    source : I64 -> Str
    source = |seed|
        "# Heading ${seed.to_str()}\n\nA *b* **c** [l](/u) `code` &amp; &copy;\n\n- one\n- [x] two\n\n> quoted\n\n| a | b |\n| --- | --- |\n| 1 | 2 |\n\n```roc\nmain = 1\n```\n"
}
