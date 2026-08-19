#!/usr/bin/env python3
#
# This script checks all the RUL0 files for errors such as sinkhole bugs.
# If any are found, they are printed to stdout and the script exits with a non-zero return code.
#
# Minimum requirement: Python 3.12+
# Further info: https://www.wiki.sc4devotion.com/index.php?title=RUL0

import sys
import os
import itertools
import re

SRC_DIRS = [
        "Controller/RUL0",
        "Lite Controller/RUL0",
]


def drop_comments(lines):
    for line in lines:
        idx = line.find(";")
        yield line if idx == -1 else line[:idx]


# create mapping of (x,y)-cell to char
def parse_layout(lines):
    layout = [line[(line.index("=")+1):].strip() for line in drop_comments(l for _, l in lines)]
    markers_x = [line.index("^") for line in layout if "^" in line]
    markers_y = [i for i, line in enumerate(layout) if "<" in line]
    if len(markers_x) != 1 or len(markers_y) != 1:
        raise Exception(f"Missing or incorrect origin markers '^'/'<' in layout starting at line {lines[0][0]}")
    origin_x = markers_x[0]
    origin_y = markers_y[0]
    cells = {(j-origin_x, i-origin_y): char
             for i, row in enumerate(layout)
             for j, char in enumerate(row)
             if char not in ".^<"
             }
    return cells


def parse_checktypes(lines):
    definitions = [line[(line.index("=")+1):].strip() for line in drop_comments(l for _, l in lines)]
    static_cells = {row[0]: "optional" not in row and "check" not in row
                    for row in definitions if row}
    return static_cells


def _stringify_layout(lines):
    return "".join(f"  {line_no}: {line}" for line_no, line in lines)


def check_cons_layout(cell_lines, checktype_lines, cons_lines):
    cell_layout = parse_layout(cell_lines)
    cons_layout = parse_layout(cons_lines)
    static_cells = parse_checktypes(checktype_lines)
    undefined_cells = set(c for c in cell_layout.values() if c != '+' and c not in static_cells)
    if undefined_cells:
        raise Exception(f"Missing CheckType definition for cell {', '.join(undefined_cells)}:\n{_stringify_layout(cell_lines)}")
    bad_cells = [xy for xy in cons_layout.keys() if xy not in cell_layout]  # constraint != '.' but cell == '.'
    if not bad_cells:
        bad_cells = [xy for xy, c in cell_layout.items()
                     if static_cells.get(c) and
                     (xy not in cons_layout or cons_layout[xy] == '.')]  # cell != '.' but constraint == '.'
    if not bad_cells:
        num_static = sum(static_cells.values())  # True: 1, False: 0
        if num_static > 1:  # in particular ignores handles of FLEX pieces
            # Detects cases where constraint == '+' and the constraint is adjacent to something like '|' or '-'
            bad_cells = [(x, y) for (x, y), c in cons_layout.items()
                         if c == '+' and
                         (static_c := cell_layout.get((x, y))) != '+' and
                         static_cells.get(static_c) and
                         any(adj_c == ('|' if horiz else '-')
                             for (adj_c, horiz) in [
                                 (cons_layout[adj_xy], horiz)
                                 for (adj_xy, horiz) in [((x+1, y), True), ((x-1, y), True), ((x, y+1), False), ((x, y-1), False)]
                                 if adj_xy in cons_layout
                                 and static_cells.get(cell_layout.get(adj_xy))
                             ])
                         ]
    if bad_cells:
        raise Exception(f"Potential sinkhole bug in ConsLayout at cells {' '.join(map(str, bad_cells))}:\n{_stringify_layout(cell_lines)}  ---\n{_stringify_layout(cons_lines)}")


_relevant_line_starts = (
        re.compile(r"^CellLayout", re.IGNORECASE),
        re.compile(r"^CheckType", re.IGNORECASE),
        re.compile(r"^ConsLayout", re.IGNORECASE),
        re.compile(r"^\[HighwayIntersectionInfo"),
        )


def scan_rul0_file(lines):
    def relevant_lines():
        for line_no, line in enumerate(lines, 1):
            line = line.lstrip()
            if line.startswith(";###RHD###"):  # TODO for simplicity, we ignore LHD for now
                line = line[10:]
            for re_idx, pattern in enumerate(_relevant_line_starts):
                if pattern.match(line):
                    yield line_no, line, re_idx
                    break

    grouped = [list(it) for heading, it in
               itertools.groupby(relevant_lines(), key=lambda tup: tup[2] == 3)
               if not heading]
    for grouped_lines in grouped:
        cell_lines      = [(line_no, line) for line_no, line, re_idx in grouped_lines if re_idx == 0]
        checktype_lines = [(line_no, line) for line_no, line, re_idx in grouped_lines if re_idx == 1]
        cons_lines      = [(line_no, line) for line_no, line, re_idx in grouped_lines if re_idx == 2]
        if not cell_lines and not cons_lines:  # checktype_lines might be non-empty in `CopyFrom` case
            continue
        elif not cell_lines or not checktype_lines or not cons_lines:
            yield f"Found no matching CellLayout, ConsLayout or CheckType definitions starting at line {grouped_lines[0][0]}"
        else:
            try:
                check_cons_layout(cell_lines, checktype_lines, cons_lines)
            except Exception as err:
                yield str(err)


def main() -> int:
    validated = 0
    errors = 0
    for src_dir in SRC_DIRS:
        for (parent, dirs, files) in os.walk(src_dir):
            for fname in files:
                if not fname.endswith(".rul") and not fname.endswith(".txt"):
                    continue
                msgs = []
                p = os.path.join(parent, fname)
                with open(p, encoding='utf-8') as f:
                    validated += 1
                    msgs.extend(scan_rul0_file(f))
                if msgs:
                    errors += len(msgs)
                    print(f"===> {p}")
                    for msg in msgs:
                        print(msg)
    if errors > 0:
        print(f"Finished with {errors} errors in RUL0 files.")
        return 1
    else:
        print(f"Successfully validated {validated} RUL0 files.")
        return 0


if __name__ == '__main__':
    sys.exit(main())
