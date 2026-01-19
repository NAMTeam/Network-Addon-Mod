#!/usr/bin/env python3
#
# This script checks all the RUL0 files for errors such as sinkhole bugs..
# If any are found, they are printed to stdout and the script exits with a non-zero return code.
#
# Minimum requirement: Python 3.12+
# Further info: https://www.wiki.sc4devotion.com/index.php?title=RUL0

import sys
import os
import itertools

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


def check_cons_layout(cell_lines, cons_lines):
    cell_layout = parse_layout(cell_lines)
    cons_layout = parse_layout(cons_lines)
    bad_cells = [xy for xy in cons_layout.keys() if xy not in cell_layout]
    if bad_cells:
        cell_layout_str = "".join(f"  {line_no}: {line}" for line_no, line in cell_lines)
        cons_layout_str = "".join(f"  {line_no}: {line}" for line_no, line in cons_lines)
        raise Exception(f"Potential sinkhole bug in ConsLayout at cells {' '.join(map(str, bad_cells))}:\n{cell_layout_str}  ---\n{cons_layout_str}")


def scan_rul0_file(lines):
    def relevant_lines():
        for line_no, line in enumerate(lines, 1):
            line = line.lstrip()
            if line.startswith(";###RHD###"):  # TODO for simplicity, we ignore LHD for now
                line = line[10:]
            if line.startswith("CellLayout") or line.startswith("ConsLayout"):
                yield line_no, line

    grouped = list((b, list(it)) for b, it in itertools.groupby(relevant_lines(), key=lambda tup: tup[1].startswith("ConsLayout")))
    if grouped and grouped[0][0]:
        yield "Found no matching CellLayout for first ConsLayout in file"
    elif grouped and not grouped[-1][0]:
        yield "Found no matching ConsLayout for last CellLayout in file"
    else:
        for ((_, cell_lines), (_, cons_lines)) in itertools.batched(grouped, 2):
            try:
                check_cons_layout(cell_lines, cons_lines)
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
