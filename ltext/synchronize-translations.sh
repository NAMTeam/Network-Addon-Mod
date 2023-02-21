#!/bin/bash
# This script runs msgmerge on every .po file to synchronize them with the
# corresponding English template .pot files.
set -e
# first change to ltext directory, so script can be run from anywhere
cd "${0%/*}"

for lang in */ ; do
    echo "synchronizing $lang"
    for pofile in "$lang"*.po ; do
        potfile="${pofile##*/}t"
        msgmerge --update --previous --no-wrap --backup=off --quiet "$pofile" "$potfile"
    done
done

# Note: To fix line wrapping of a .po file use:
# msgcat --no-wrap -o <po-file> <po-file>
