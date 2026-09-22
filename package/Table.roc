## The table sub-parser: a run of `|` lines to one rectangular block.
##
## Split out of `Markdown.roc` because it is the one construct decided by the
## shape of a whole RUN rather than by a line's prefix — the header/delimiter
## distinction is invisible until the run is complete.
import Ref
import Loc exposing [Loc]
import Block exposing [Block]
import Inl exposing [Inl]
import Scan

Table := [].{
    ## One source line, split into cells that each remember where they came
    ## from. `src` is kept beside them so a padded cell has somewhere to sit.
    Row : { src : Loc.Seg, cells : List(Loc.Seg) }

    ## The delimiter row is dropped by SHAPE, not by position: any row made only
    ## of `-` and `:` is one. Dropping row 1 by index instead would corrupt a
    ## table whose delimiter is missing, and would silently accept a body row
    ## that happened to sit there.
    ##
    ## Alignment comes from the delimiter row too, one entry per column
    ## (`align_of`).
    parse : List(Loc.Seg), List(Ref.Def), Loc.T -> Block
    parse = |lines, defs, loc| {
        all = List.map(lines, |l| { src: l, cells: Table.cells_of(l) })
        rows = List.keep_if(all, |r| !Table.is_delim(r.cells))
        if !Table.well_formed(all) {
            ## Not a table at all: the delimiter row must be the SECOND line and
            ## carry exactly as many cells as the header. A run that fails that
            ## is prose that happens to contain pipes, and rendering it as a
            ## one-column table silently drops the rest of every row.
            Block.mk(loc, Para(Inl.project_all(Scan.run(Loc.joined(lines), defs), lines)))
        } else
        match List.get(rows, 0) {
            Ok(header) => {
                n = List.len(header.cells)
                align = match List.get(all, 1) {
                    Ok(delim) => List.map(delim.cells, Table.align_of)
                    Err(_) => []
                }
                Block.mk(loc, Table({
                    ncols: n,
                    align: align,
                    cells: List.join(List.map(rows, |r| Table.fit(r, n, defs))),
                }))
            }
            ## A run that was nothing but a delimiter row. Degrades to an empty
            ## table rather than crashing on the missing header.
            Err(_) => Block.mk(loc, Table({ ncols: 0, align: [], cells: [] }))
        }
    }

    ## `| a | b |` to `["a", "b"]`. The leading and trailing pipes are dropped
    ## by discarding an EMPTY first or last cell rather than by stripping the
    ## characters first — a row may legitimately open with an empty cell (`|| b |`),
    ## and stripping cannot tell that from the delimiter.
    ##
    ## A scanner rather than a `split_on`, because `\|` is a literal pipe and must
    ## not split the cell. The unescape happens HERE and not in the inline phases:
    ## the escape is defined to work inside other spans too, so `` `\|` `` is a
    ## code span containing a pipe, and an inline-phase unescape would never look
    ## inside the backticks.
    ## Each cell keeps the source offset of its first non-space byte, so an
    ## inline inside a cell anchors to the row it was written on rather than to
    ## the cell string the splitter handed the scanner.
    ##
    ## **One case still drifts: `\|` inside a cell.** The unescape happens here
    ## and shortens the cell by a byte, so anything after it in the SAME cell
    ## reports a byte early. Fixing it needs a segment per escape — the same
    ## machinery `Loc.T.content` already has, applied below the line — and no
    ## table in the corpus this targets contains one.
    cells_of : Loc.Seg -> List(Loc.Seg)
    cells_of = |row| {
        base = row.off + Table.lead(row.text)
        List.map(
            Table.trim_edges(Table.scan(Str.to_utf8(row.text.trim()))),
            |c| {
                raw = Str.from_utf8_lossy(c.bytes)
                { off: base + c.off + Table.lead(raw), text: raw.trim() }
            },
        )
    }

    ## How many bytes of leading whitespace a `trim` would take off.
    lead : Str -> U64
    lead = |t| Str.count_utf8_bytes(t) - Str.count_utf8_bytes(Str.trim_start(t))

    ## Split on unescaped `|`, turning `\|` into a literal `|`.
    ##
    ## `esc` holds a pending backslash rather than looking ahead, so `\\|` is an
    ## escaped BACKSLASH followed by a delimiter — the second `\` consumes the
    ## first and leaves nothing pending for the `|`. A trailing `\` at end of row
    ## is emitted as itself.
    ## A cell's bytes, and the index it started at in the row — `i` counts the
    ## row's own bytes while `cur` collects the unescaped ones, so the two do not
    ## have to agree in length.
    Cut : { off : U64, bytes : List(U8) }

    scan : List(U8) -> List(Table.Cut)
    scan = |bytes| {
        st = List.fold(
            bytes,
            { cur: [], out: [], esc: False, i: 0, at: 0 },
            |a, b|
                if a.esc {
                    if b == 124 {
                        { ..a, cur: List.append(a.cur, 124), esc: False, i: a.i + 1 }
                    } else {
                        { ..a, cur: List.append(List.append(a.cur, 92), b), esc: False, i: a.i + 1 }
                    }
                } else if b == 92 {
                    { ..a, esc: True, i: a.i + 1 }
                } else if b == 124 {
                    { ..a, out: List.append(a.out, { off: a.at, bytes: a.cur }), cur: [], i: a.i + 1, at: a.i + 1 }
                } else {
                    { ..a, cur: List.append(a.cur, b), i: a.i + 1 }
                },
        )
        last = if st.esc {
            List.append(st.cur, 92)
        } else {
            st.cur
        }
        List.append(st.out, { off: st.at, bytes: last })
    }

    ## Drop the cells the delimiting pipes produce: `| a |` scans to
    ## `["", "a", ""]`, and only an EMPTY cell at either end is a delimiter
    ## artefact. A row with no outer pipes (`a | b`) loses nothing.
    trim_edges : List(Table.Cut) -> List(Table.Cut)
    trim_edges = |cs| {
        head_dropped = match List.first(cs) {
            Ok(c) =>
                if List.is_empty(c.bytes) {
                    List.drop_first(cs, 1)
                } else {
                    cs
                }
            Err(_) => cs
        }
        match List.last(head_dropped) {
            Ok(c) =>
                if List.is_empty(c.bytes) {
                    List.drop_last(head_dropped, 1)
                } else {
                    head_dropped
                }
            Err(_) => head_dropped
        }
    }

    well_formed : List(Table.Row) -> Bool
    well_formed = |all|
        match List.first(all) {
            Ok(header) =>
                match List.get(all, 1) {
                    Ok(second) =>
                        Table.is_delim(second.cells)
                        and List.len(second.cells) == List.len(header.cells)
                    Err(_) => False
                }
            Err(_) => False
        }

    ## A leading colon means left, a trailing one right, both mean centre.
    align_of : Loc.Seg -> Block.Align
    align_of = |cell| {
        c = cell.text
        left = c.starts_with(":")
        right = c.ends_with(":")
        if left and right {
            Center
        } else if right {
            Right
        } else if left {
            Left
        } else {
            None
        }
    }

    is_delim : List(Loc.Seg) -> Bool
    is_delim = |cs|
        !List.is_empty(cs)
        and List.len(List.keep_if(cs, |c| Table.is_delim_cell(c.text))) == List.len(cs)

    is_delim_cell : Str -> Bool
    is_delim_cell = |c| {
        b = Str.to_utf8(c)
        !List.is_empty(b) and List.len(List.keep_if(b, |x| x == 45 or x == 58)) == List.len(b)
    }

    ## Force a row to exactly `n` cells: pad short rows, truncate long ones.
    ##
    ## This is what makes `cells.len() == nrows * ncols` an invariant every
    ## renderer can loop on without bounds-checking each access. Padding is
    ## visible — a ragged row gains blank cells — and that is the intended trade:
    ## it affected 2 of ~155 tables when measured, and the invariant is worth
    ## more than the fidelity.
    ##
    ## `concat` then `take_first` rather than a length comparison, so the pad and
    ## the truncate are the same expression and cannot disagree about the
    ## boundary case where the row is already exactly `n`.
    ## A padded cell has no source, so it collapses to a zero-width location at
    ## the end of the row that was short — the same treatment `Loc.none` gives
    ## any node the parser synthesized (D11b).
    fit : Table.Row, U64, List(Ref.Def) -> List(Block.Cell)
    fit = |row, n, defs| {
        pad = { off: row.src.off + Str.count_utf8_bytes(row.src.text), text: "" }
        List.map(
            List.take_first(List.concat(row.cells, List.repeat(pad, n)), n),
            |c| Inl.project_all(Scan.run(c.text, defs), [c]),
        )
    }
}
