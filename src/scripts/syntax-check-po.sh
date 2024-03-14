#!/bin/bash
#
# This script checks all the po/pot files for correct multi-line format and uppercased TGIs.
# If any issues are found, they are printed to stdout and the script exits with a non-zero return code.
set -e

INDIR="./ltext"
OUT="./syntax-check-po.log"

sorted_input_files() {
    find -L "$INDIR" -type f \( -name '*.po' -o -name '*.pot' \) -print0 | sort -z
}

strip_comments_and_whitespace() {
    ${1:+ xargs -0} sed -e 's/^#.*//' -e 's/\s\+$//g' -e '/^\s*$/d'
}
# Note that we used sed and tail for concatenation which handle missing
# newlines at the end of input files gracefully (unlike cat).

strip_valid_po() {
    # TGIs must be upper-case.
    ID="[0-9A-F]\{8\}"
    # Every line of a multi-line string should start and end with quotes.
    QUOTEDTEXT='".*"'
    sed -e "/^msgctxt\s\+\"$ID-$ID-$ID\"$/d" \
        -e "/^msgid\s\+$QUOTEDTEXT$/d" \
        -e "/^msgstr\s\+$QUOTEDTEXT$/d" \
        -e "/^msgstr\s\+$QUOTEDTEXT$/d" \
        -e "/^$QUOTEDTEXT$/d"
}

echo "Checking .po file syntax..."

sorted_input_files | strip_comments_and_whitespace -0 | strip_valid_po > "$OUT"

if [ -s "$OUT" ]; then
    # the log file is not-empty
    echo "Syntax errors in .po files (use uppercase TGIs and quotes on every line of text):"
    cat "$OUT"
    rm -f "$OUT"
    exit 1
else
    echo "No syntax errors found."
    rm -f "$OUT"
    exit 0
fi
