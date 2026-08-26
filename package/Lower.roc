## `Doc` to `Block` — collect the definitions, then parse the inlines.
##
## Two walks, and the order between them is the whole point. A link reference
## may be written before the definition it resolves to, so nothing inline can be
## parsed until every definition in the document is known. Walking the finished
## block tree gives that for free: the structure is already decided, so the
## first walk can see every paragraph in the document before the second walk
## reads any of them.
import Doc exposing [Doc]
import Loc exposing [Loc]
import Block exposing [Block]
import Inl exposing [Inl]
import Ref
import Defs
import Scan
import Table

Lower := [].{
    run : List(Doc) -> List(Block)
    run = |docs| {
        c = Lower.collect(docs, [], [])
        List.map(c.docs, |d| Lower.block(d, c.defs))
    }

    ## ## Walk one: lift the definitions out
    ##
    ## A definition may only start a paragraph, and only a paragraph — which is
    ## exactly the spec's rule, and here it needs no lookahead machinery to
    ## enforce, because a paragraph is already a paragraph by the time we look.
    ## A paragraph that is nothing but definitions leaves no block behind.

    collect : List(Doc), List(Doc), List(Ref.Def) -> { docs : List(Doc), defs : List(Ref.Def) }
    collect = |docs, out, defs|
        match List.first(docs) {
            Err(_) => { docs: out, defs: defs }
            Ok(d) => {
                rest = List.drop_first(docs, 1)
                match d.node {
                    Para(t) => {
                        s = Lower.strip_defs(t.split_on("\n"), defs)
                        next = if Str.is_empty(Str.trim(s.text)) {
                            out
                        } else {
                            List.append(out, Doc.mk(d.at, Para(s.text)))
                        }
                        Lower.collect(rest, next, s.defs)
                    }
                    Quote(ks) => {
                        c = Lower.collect(ks, [], defs)
                        Lower.collect(rest, List.append(out, Doc.mk(d.at, Quote(c.docs))), c.defs)
                    }
                    Items(i) => {
                        e = Lower.collect_entries(i.entries, [], defs)
                        Lower.collect(rest, List.append(out, Doc.mk(d.at, Items({ ..i, entries: e.entries }))), e.defs)
                    }
                    _ => Lower.collect(rest, List.append(out, d), defs)
                }
            }
        }

    collect_entries : List({ task : [NoTask, Todo, Done], blocks : List(Doc) }), List({ task : [NoTask, Todo, Done], blocks : List(Doc) }), List(Ref.Def) -> { entries : List({ task : [NoTask, Todo, Done], blocks : List(Doc) }), defs : List(Ref.Def) }
    collect_entries = |entries, out, defs|
        match List.first(entries) {
            Err(_) => { entries: out, defs: defs }
            Ok(e) => {
                c = Lower.collect(e.blocks, [], defs)
                Lower.collect_entries(
                    List.drop_first(entries, 1),
                    List.append(out, { task: e.task, blocks: c.docs }),
                    c.defs,
                )
            }
        }

    ## Peel definitions off the front of a paragraph's lines until one fails.
    strip_defs : List(Str), List(Ref.Def) -> { text : Str, defs : List(Ref.Def) }
    strip_defs = |lines, defs|
        match Defs.def_at(lines) {
            Ok(d) =>
                Lower.strip_defs(List.drop_first(lines, d.used), List.append(defs, d.def))
            Err(_) => { text: Str.join_with(lines, "\n"), defs: defs }
        }

    ## ## Walk two: the leaves become inlines

    block : Doc, List(Ref.Def) -> Block
    block = |d, defs| {
        loc = Loc.at(d.at.start, d.at.end)
        match d.node {
            Para(t) => Block.mk(loc, Para(Inl.shift_all(Scan.run(t, defs), loc.outer.start, loc.outer.end)))
            Heading(lvl, t) => Block.mk(loc, Heading(lvl, Inl.shift_all(Scan.run(t, defs), loc.outer.start, loc.outer.end)))
            Code(c) => Block.mk(loc, Code(c))
            Quote(ks) => Block.mk(loc, Quote(List.map(ks, |k| Lower.block(k, defs))))
            Items(i) =>
                Block.mk(loc, Items({
                    ordered: i.ordered,
                    start: i.start,
                    tight: i.tight,
                    entries: List.map(i.entries, |e| Lower.entry(e, defs)),
                }))
            Table(rows) => Table.parse(rows, defs, loc)
            Html(h) => Block.mk(loc, Html(h))
            Rule => Block.mk(loc, Rule)
        }
    }

    entry : { task : [NoTask, Todo, Done], blocks : List(Doc) }, List(Ref.Def) -> List(Block)
    entry = |e, defs|
        Lower.with_task(List.map(e.blocks, |b| Lower.block(b, defs)), e.task)

    ## Put the checkbox back where the marker was — at the head of the entry's
    ## first paragraph.
    with_task : List(Block), [NoTask, Todo, Done] -> List(Block)
    with_task = |blocks, state|
        match state {
            NoTask => blocks
            _ => {
                done = state == Done
                match List.first(blocks) {
                    Ok(first) =>
                        match first.node {
                            Para(ns) =>
                                List.concat(
                                    [Block.mk(first.loc, Para(List.concat([Inl.mk(Loc.none(first.loc.outer.start), Task(done))], ns)))],
                                    List.drop_first(blocks, 1),
                                )
                            _ => List.concat([Block.mk(Loc.none(0), Para([Inl.mk(Loc.none(0), Task(done))]))], blocks)
                        }
                    Err(_) => List.concat([Block.mk(Loc.none(0), Para([Inl.mk(Loc.none(0), Task(done))]))], blocks)
                }
            }
        }
}
