## Markdown files in, HTML files out.
##
##     just md2html notes/*.md
##
## Without `just`:
##
##     roc examples/md2html.roc -- README.md
##
## Each argument is read, parsed, and written back beside itself with the
## extension swapped: `notes/intro.md` becomes `notes/intro.html`.
##
## The package's output is DATA, not HTML — `Markdown.parse` gives a
## `List(Block)`, and `Markdown.to_html` is one lowering of it. This example
## uses both: the document body comes from `to_html`, and the `<title>` comes
## from walking the block tree for its first level-1 heading.
 
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	md: "../package/main.roc",
}

import pf.OsStr exposing [OsStr]
import pf.Path exposing [Path]
import pf.Stdout
import pf.Stderr
import md.Markdown
import md.Block exposing [Block]
import md.Inl

main! : List(OsStr) => Try({}, _)
main! = |args| {
	inputs = args.drop_first(1)

	if inputs.is_empty() {
		Stderr.line!("usage: md2html <file.md>...")?
		Err(Exit(2))
	} else {
		# Error TAGS travel through `convert!`, and become a sentence here at
		# the edge of the program — the one place that knows which file was
		# being converted, and the only place a person reads.
		for arg in inputs {
			match convert!(arg) {
				Ok({}) => {}
				Err(err) => {
					Stderr.line!("md2html: ${arg.display()}: ${reason(err)}")?
					return Err(Exit(1))
				}
			}
		}

		Ok({})
	}
}

reason : _ -> Str
reason = |err|
	match err {
		NonUtf8Path(_) => "path is not valid UTF-8"
		ReadFailed(PathErr(NotFound)) => "no such file"
		ReadFailed(PathErr(IsADirectory)) => "that is a directory"
		ReadFailed(PathErr(PermissionDenied)) => "not readable"
		ReadFailed(other) => "could not be read — ${Str.inspect(other)}"
		WriteFailed(PathErr(PermissionDenied)) => "the .html file is not writable"
		WriteFailed(other) => "the .html file could not be written — ${Str.inspect(other)}"
		# The union is open — `convert!` also writes to stdout — so this arm is
		# required, and it is the reason the informative ones can stay narrow.
		other => Str.inspect(other)
	}

## One file: read, parse, lower, write.
convert! : OsStr => Try({}, _)
convert! = |arg| {
	source = Path.from_os_str(arg)
	# A path that is not valid UTF-8 is reported rather than guessed at. The
	# output name is derived from the input's TEXT, so there has to be some.
	name = source.to_str() ? NonUtf8Path

	markdown = source.read_utf8!() ? ReadFailed

	## The API, built once against the default HTML5 entity table. A record
	## field holding a function is bound before it is called — invoked in place
	## it reads as a method call.
	md = Markdown.setup(Markdown.default)
	parse = md.parse
	to_html = md.to_html

	## The parse, once. Both the title and the body are read off it.
	blocks : List(Block)
	blocks = parse(markdown)

	out = Path.from_raw(Utf8(html_name(name)))
	out.write_utf8!(document(title_of(blocks), to_html(blocks))) ? WriteFailed

	Stdout.line!("${name} -> ${out.display()}")?

	Ok({})
}

## `notes/intro.md` -> `notes/intro.html`, and a file with no markdown
## extension simply gains one — `CHANGELOG` -> `CHANGELOG.html`, never
## `CHANGELO.html`.
html_name : Str -> Str
html_name = |name| {
	stem =
		if name.ends_with(".md") {
			Str.drop_suffix(name, ".md")
		} else if name.ends_with(".markdown") {
			Str.drop_suffix(name, ".markdown")
		} else {
			name
		}

	Str.concat(stem, ".html")
}

## The document's own first level-1 heading, as plain text.
##
## `Inl.text_of` discards markup, which is what a `<title>` wants: `# A *fast*
## parser` titles the page "A fast parser" rather than leaking an `<em>` into
## an attribute-free context.
title_of : List(Block) -> Str
title_of = |blocks| {
	for block in blocks {
		match block.node {
			Heading(1, spans) => return Inl.text_of(spans)
			_ => {}
		}
	}

	"Untitled"
}

## A minimal standalone page. `Markdown.to_html` returns a FRAGMENT — it is
## the caller who decides what document, if any, to put it in.
document : Str, Str -> Str
document = |title, body|
	# `to_html` ends its fragment with a newline of its own, which is why
	# `</body>` sits on the same line as the interpolation rather than below it.
	# The bare `\\` at the end is what gives the file its trailing newline.
	\\<!doctype html>
	\\<html lang="en">
	\\<head>
	\\<meta charset="utf-8">
	\\<meta name="viewport" content="width=device-width, initial-scale=1">
	\\<title>${escape(title)}</title>
	\\</head>
	\\<body>
	\\${body}
	\\</body>
	\\</html>
	\\

## The title is the one place this example builds HTML itself, so it is the one
## place it has to escape. Everything inside `body` was escaped by the package.
escape : Str -> Str
escape = |s|
	s
	|> Str.replace_each("&", "&amp;")
	|> Str.replace_each("<", "&lt;")
	|> Str.replace_each(">", "&gt;")
