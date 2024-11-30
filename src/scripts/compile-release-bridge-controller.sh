#!/bin/bash
#
# This script compiles all bridge rul files for inclusion in the installer.
#
#   ./src/scripts/compile-release-bridge-controller.sh
#
# The compiled bridge controller is located at `./target/controllers/`.

set -e

if [ ! -e "Controller" ]
then
    echo "Call this script from the root directory of the Network-Addon-Mod repository."
    exit 1
fi

PROJECT_ROOT="$(pwd)"
TEMP="target/controllers/temp"
mkdir -p "$TEMP"

BUILDRULS_ARCHIVE="target/BuildRULs_01.zip"
BUILDRULS_URL="https://www.dropbox.com/s/ckwhy11xxaz3z1q/BuildRULs_01.zip?dl=1"

if [ ! -e "$BUILDRULS_ARCHIVE" ]
then
    # download compiler if it does not yet exist
    curl -L "$BUILDRULS_URL" > "$BUILDRULS_ARCHIVE"
fi
echo "08816a61cb64c80392328f874c3b5bc031580514c156839af9a8359784a72071  $BUILDRULS_ARCHIVE" | sha256sum --check
unzip -d "$TEMP" "$BUILDRULS_ARCHIVE"

# build bridge controller
(cd "$TEMP/BuildRULs_01" && java -jar BuildRULs.jar -f "$PROJECT_ROOT/Controller/Bridge Controller/" "$PROJECT_ROOT/Controller/Bridge Controller/")

# copy to target
DESTDIR="target/controllers/"
mkdir -p "$DESTDIR"
cp -p "$PROJECT_ROOT/Controller/Bridge Controller/NetworkAddonMod_Bridge_Controller.dat" "$DESTDIR"

rm -rf "$TEMP"
