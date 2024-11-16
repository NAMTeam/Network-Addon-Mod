#!/bin/bash
#
# This script checks all the RUL2 files for syntax errors.
# If any are found, they are printed to stdout and the script exits with a non-zero return code.
set -e

INDIR="./Controller"
OUT="./syntax-check-rul2.log"

sorted_input_files() {
    find -L "$INDIR/$1" -type f  \( -name '*.txt' -o -name '*.rul' -o -name '*.ini' \) -print0 | sort -z
}

strip_drive_side() {
    # Uncomments the RHD _and_ LHD code.
    # If an argument is passed, this uses xargs to read file names from stdin,
    # else stdin is read as input directly.
    ${1:+ xargs -0} sed -e "s/;###[RL]HD###//"
}
# Note that we used sed and tail for concatenation which handle missing
# newlines at the end of input files gracefully (unlike cat).

strip_comments_and_whitespace() {
    sed -e "s/;.*//" -e "s/\s\+//g" -e '/^\s*$/d'
}

strip_valid_rul2() {
    # matches RUL2 code as well as prevents
    IDROTFLIP="0[xX][[:xdigit:]]\{4,8\},[0-3],[01]"
    sed -e "/^$IDROTFLIP,$IDROTFLIP=\($IDROTFLIP,$IDROTFLIP\|0,0,0,0,0,0\)$/d"
}

echo "Checking RUL2 syntax..."

# The `tail -n +2` skips the first line, containing `[RuleOverrides]`.
sorted_input_files "RUL2" | strip_drive_side -0 | strip_comments_and_whitespace | strip_valid_rul2 | tail -n +2 > "$OUT"

if [ -s "$OUT" ]; then
    # the log file is not-empty
    echo "Syntax errors in RUL2:"
    cat "$OUT"
    rm -f "$OUT"
    exit 1
else
    echo "No syntax errors found."
    rm -f "$OUT"
    exit 0
fi
