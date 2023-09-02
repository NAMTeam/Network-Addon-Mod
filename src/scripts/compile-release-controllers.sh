#!/bin/bash
#
# This script compiles the different variants of the NAM controller for
# release, using the CLI of the controller compiler.
#
# Call it from the project root:
#
#   ./src/scripts/compile-release-controllers.sh
#
# The compiled controllers are located at `./target/controllers/`.
#
# The LText files still need to be adjusted manually.
set -e

if [ ! -e "Controller" ]
then
    echo "Call this script from the root directory of the Network-Addon-Mod repository."
    exit 1
fi

PROJECT_ROOT="$(pwd)"
DEST="target/controllers"
TEMP="$DEST/temp"
mkdir -p "$TEMP"

COMPILER_ARCHIVE="target/NAMControllerCompiler_1.3.1.zip"
COMPILER_URL="https://github.com/memo33/NAMControllerCompiler/releases/download/v1.3.1/NAMControllerCompiler_1.3.1.zip"

if [ ! -e "$COMPILER_ARCHIVE" ]
then
    # download compiler if it does not yet exist
    wget -O "$COMPILER_ARCHIVE" "$COMPILER_URL"
fi
unzip -d "$TEMP" "$COMPILER_ARCHIVE"


# RHD 4GB Full
cp "src/scripts/RUL2_IID_structure_full.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@1=0 NAM Controller_RHD_4GB_Full' 1 0 0 0 0 0 0 0 0 0 0)

# LHD 4GB Full
cp "src/scripts/RUL2_IID_structure_full.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@2-0 NAM Controller_LHD_4GB_Full' 0 0 0 0 0 0 0 0 0 0 0)

# RHD 4GB Full
cp "src/scripts/RUL2_IID_structure_noRHW.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@3-0 NAM Controller_RHD_LowRAM_NoRHW' 1 0 0 0 0 0 0 0 0 0 0)

# LHD 4GB Full
cp "src/scripts/RUL2_IID_structure_noRHW.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@4-0 NAM Controller_LHD_LowRAM_NoRHW' 0 0 0 0 0 0 0 0 0 0 0)

rm -rf "$TEMP"
