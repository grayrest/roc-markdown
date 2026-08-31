## The document every harness variant parses, so the only difference between
## their modules is which entity table the API was built with.
import md.Markdown
import md.Block exposing [Block]

Sample := [].{
    run : Markdown.Api, I64 -> { message : Str, n : I64 }
    run = |api, seed| {
        parse = api.parse
        to_html = api.to_html
        blocks : List(Block)
        blocks = parse(Sample.source(seed))
        { message: to_html(blocks), n: List.len(blocks).to_i64_wrap() }
    }

    source : I64 -> Str
    source = |seed|
        "# Heading ${seed.to_str()}\n\nA *b* **c** [l](/u) `code` &amp; &copy;\n\n- one\n- [x] two\n\n> quoted\n\n| a | b |\n| --- | --- |\n| 1 | 2 |\n\n```roc\nmain = 1\n```\n"
}
