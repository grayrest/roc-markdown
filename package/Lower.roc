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
        Lower.lower_all(c.docs, c.defs)
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
                    Para(segs) => {
                        s = Lower.strip_defs(segs, defs)
                        next = if Str.is_empty(Str.trim(Loc.joined(s.segs))) {
                            out
                        } else {
                            List.append(out, Doc.mk(Lower.after_defs(d.at, s.segs), Para(s.segs)))
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

    collect_entries : List(Doc.Entry), List(Doc.Entry), List(Ref.Def) -> { entries : List(Doc.Entry), defs : List(Ref.Def) }
    collect_entries = |entries, out, defs|
        match List.first(entries) {
            Err(_) => { entries: out, defs: defs }
            Ok(e) => {
                c = Lower.collect(e.blocks, [], defs)
                Lower.collect_entries(
                    List.drop_first(entries, 1),
                    List.append(out, { ..e, blocks: c.docs }),
                    c.defs,
                )
            }
        }

    ## Peel definitions off the front of a paragraph's lines until one fails.
    ##
    ## Dropping SEGMENTS rather than joining what is left is what keeps the rest
    ## of the paragraph anchored: each surviving line still carries the offset it
    ## always had, so a definition above a reference costs the text below it
    ## nothing.
    strip_defs : List(Loc.Seg), List(Ref.Def) -> { segs : List(Loc.Seg), defs : List(Ref.Def) }
    strip_defs = |segs, defs|
        match Defs.def_at(List.map(segs, |g| g.text)) {
            Ok(d) =>
                Lower.strip_defs(List.drop_first(segs, d.used), List.append(defs, d.def))
            Err(_) => { segs: segs, defs: defs }
        }

    ## The paragraph's source span, once its leading definitions are gone.
    ##
    ## It starts where the surviving text does, not where the block did: the
    ## definitions vanish from the tree, so a span still covering them would put
    ## the editor's paragraph decoration over source the paragraph no longer
    ## holds. The bytes they occupied become a gap the app owns (D11b).
    after_defs : Loc.Ivl, List(Loc.Seg) -> Loc.Ivl
    after_defs = |at, segs|
        match List.first(segs) {
            Ok(g) => { start: Loc.max(at.start, g.off), end: at.end }
            Err(_) => at
        }

    ## ## Walk two: the leaves become inlines
    ##
    ## One `Doc` is usually one `Block`. A table run can be two: the table, and
    ## the rows past its padding budget as prose (`Table.parse`).

    block : Doc, List(Ref.Def) -> List(Block)
    block = |d, defs| {
        loc = Loc.at(d.at.start, d.at.end)
        match d.node {
            Para(segs) => [Block.mk(loc, Para(Lower.inlines(segs, defs)))]
            Heading(lvl, segs) => [Block.mk(loc, Heading(lvl, Lower.inlines(segs, defs)))]
            Code(c) => [Block.mk(loc, Code(c))]
            Quote(ks) => [Block.mk(loc, Quote(Lower.lower_all(ks, defs)))]
            Items(i) => [
                Block.mk(loc, Items({
                    ordered: i.ordered,
                    start: i.start,
                    tight: i.tight,
                    entries: List.map(i.entries, |e| Lower.entry(e, defs)),
                })),
            ]
            Table(rows) => Table.parse(rows, defs, loc)
            Html(h) => [Block.mk(loc, Html(h))]
            Rule => [Block.mk(loc, Rule)]
        }
    }

    lower_all : List(Doc), List(Ref.Def) -> List(Block)
    lower_all = |ds, defs| List.join(List.map(ds, |d| Lower.block(d, defs)))

    ## Scan a leaf's payload, then map the scanner's offsets back onto the
    ## source through the leaf's line table. The two steps are separate because
    ## `Scan` has no business knowing what phase 1 stripped — it reads a string
    ## and reports offsets into it, and the table is what those offsets mean.
    inlines : List(Loc.Seg), List(Ref.Def) -> List(Inl)
    inlines = |segs, defs| Inl.project_all(Scan.run(Loc.joined(segs), defs), segs)

    entry : Doc.Entry, List(Ref.Def) -> List(Block)
    entry = |e, defs|
        Lower.with_task(Lower.lower_all(e.blocks, defs), e.task, e.start)

    ## Put the checkbox back where the marker was — at the head of the entry's
    ## first paragraph.
    ##
    ## When there is no paragraph to put it in, the one invented to hold it sits
    ## at `start`, the entry's own offset. It used to sit at byte 0, outside its
    ## list and ahead of every block before it, which broke the ordering the
    ## editor maps the caret by.
    with_task : List(Block), [NoTask, Todo, Done], U64 -> List(Block)
    with_task = |blocks, state, start|
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
                            _ => List.concat([Block.mk(Loc.none(start), Para([Inl.mk(Loc.none(start), Task(done))]))], blocks)
                        }
                    Err(_) => List.concat([Block.mk(Loc.none(start), Para([Inl.mk(Loc.none(start), Task(done))]))], blocks)
                }
            }
        }
}
