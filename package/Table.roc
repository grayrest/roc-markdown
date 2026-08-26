## The table sub-parser: a run of `|` lines to one rectangular block.
##
## Split out of `Markdown.roc` because it is the one construct decided by the
## shape of a whole RUN rather than by a line's prefix — the header/delimiter
## distinction is invisible until the run is complete.
import Ref
import Loc exposing [Loc]
import Block exposing [Block]
import Scan

Table := [].{
    ## The delimiter row is dropped by SHAPE, not by position: any row made only
    ## of `-` and `:` is one. Dropping row 1 by index instead would corrupt a
    ## table whose delimiter is missing, and would silently accept a body row
    ## that happened to sit there.
    ##
    ## Alignment is not parsed. The corpus this targets had zero `:---` markers
    ## across 467 delimiter cells, and a field that is always the same value is
    ## worse than no field at all.
    parse : List(Str), List(Ref.Def), Loc.T -> Block
    parse = |lines, defs, loc| {
        all = List.map(lines, Table.cells_of)
        rows = List.keep_if(all, |r| !Table.is_delim(r))
        if !Table.well_formed(all) {
            ## Not a table at all: the delimiter row must be the SECOND line and
            ## carry exactly as many cells as the header. A run that fails that
            ## is prose that happens to contain pipes, and rendering it as a
            ## one-column table silently drops the rest of every row.
            Block.mk(loc, Para(Scan.run(Str.join_with(lines, "\n"), defs)))
        } else
        match List.get(rows, 0) {
            Ok(header) => {
                n = List.len(header)
                align = match List.get(all, 1) {
                    Ok(delim) => List.map(delim, Table.align_of)
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
    cells_of : Str -> List(Str)
    cells_of = |raw|
        List.map(
            Table.trim_edges(Table.scan(Str.to_utf8(raw.trim()))),
            |c| Str.from_utf8_lossy(c).trim(),
        )

    ## Split on unescaped `|`, turning `\|` into a literal `|`.
    ##
    ## `esc` holds a pending backslash rather than looking ahead, so `\\|` is an
    ## escaped BACKSLASH followed by a delimiter — the second `\` consumes the
    ## first and leaves nothing pending for the `|`. A trailing `\` at end of row
    ## is emitted as itself.
    scan : List(U8) -> List(List(U8))
    scan = |bytes| {
        st = List.fold(
            bytes,
            { cur: [], out: [], esc: False },
            |a, b|
                if a.esc {
                    if b == 124 {
                        { ..a, cur: List.append(a.cur, 124), esc: False }
                    } else {
                        { ..a, cur: List.append(List.append(a.cur, 92), b), esc: False }
                    }
                } else if b == 92 {
                    { ..a, esc: True }
                } else if b == 124 {
                    { ..a, out: List.append(a.out, a.cur), cur: [] }
                } else {
                    { ..a, cur: List.append(a.cur, b) }
                },
        )
        last = if st.esc {
            List.append(st.cur, 92)
        } else {
            st.cur
        }
        List.append(st.out, last)
    }

    ## Drop the cells the delimiting pipes produce: `| a |` scans to
    ## `["", "a", ""]`, and only an EMPTY cell at either end is a delimiter
    ## artefact. A row with no outer pipes (`a | b`) loses nothing.
    trim_edges : List(List(U8)) -> List(List(U8))
    trim_edges = |cs| {
        head_dropped = match List.first(cs) {
            Ok([]) => List.drop_first(cs, 1)
            _ => cs
        }
        match List.last(head_dropped) {
            Ok([]) => List.drop_last(head_dropped, 1)
            _ => head_dropped
        }
    }

    well_formed : List(List(Str)) -> Bool
    well_formed = |all|
        match List.first(all) {
            Ok(header) =>
                match List.get(all, 1) {
                    Ok(second) => Table.is_delim(second) and List.len(second) == List.len(header)
                    Err(_) => False
                }
            Err(_) => False
        }

    ## A leading colon means left, a trailing one right, both mean centre.
    align_of : Str -> Block.Align
    align_of = |c| {
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

    is_delim : List(Str) -> Bool
    is_delim = |cs|
        !List.is_empty(cs)
        and List.len(List.keep_if(cs, Table.is_delim_cell)) == List.len(cs)

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
    fit : List(Str), U64, List(Ref.Def) -> List(Block.Cell)
    fit = |row, n, defs|
        List.map(List.take_first(List.concat(row, List.repeat("", n)), n), |c| Scan.run(c, defs))
}
