#!/bin/bash
#
# This script compiles all INRUL files for inclusion in the installer.
#
#   ./src/scripts/compile-release-inruls.sh
#
# The compiled INRUL files are located at `./target/controllers/`.

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

# build all INRULs
(cd "$TEMP/BuildRULs_01" && java -jar BuildRULs.jar -f "$PROJECT_ROOT/Controller/INRULs/" "$PROJECT_ROOT/Controller/INRULs/")

# RHD 4GB Full
DESTDIR="target/controllers/@0=0 NAM Controller_RHD_4GB_Full/"
mkdir -p "$DESTDIR"
cp -p "$PROJECT_ROOT/Controller/INRULs/NetworkAddonMod_IndividualNetworkRULs_RHD.dat" "$DESTDIR"

# LHD 4GB Full
DESTDIR="target/controllers/@1-0 NAM Controller_LHD_4GB_Full/"
mkdir -p "$DESTDIR"
cp -p "$PROJECT_ROOT/Controller/INRULs/NetworkAddonMod_IndividualNetworkRULs_LHD.dat" "$DESTDIR"

# RHD no-RHW
DESTDIR="target/controllers/@2-0 NAM Controller_RHD_LowRAM_NoRHW/"
mkdir -p "$DESTDIR"
cp -p "$PROJECT_ROOT/Controller/INRULs/NetworkAddonMod_IndividualNetworkRULs_RHD.dat" "$DESTDIR"

# LHD no-RHW
DESTDIR="target/controllers/@3-0 NAM Controller_LHD_LowRAM_NoRHW/"
mkdir -p "$DESTDIR"
cp -p "$PROJECT_ROOT/Controller/INRULs/NetworkAddonMod_IndividualNetworkRULs_LHD.dat" "$DESTDIR"

# Avenue turning lanes
cp -p "$PROJECT_ROOT/Controller/INRULs/NetworkAddonMod_TurningLanes_Avenues_Plugin_INRULs.dat" "target/controllers/"

rm -rf "$TEMP"
