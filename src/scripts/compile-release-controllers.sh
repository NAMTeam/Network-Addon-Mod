#!/bin/bash
#
# This script compiles the different variants of the NAM controller for
# release, using the CLI of the controller compiler.
#
# Call it from the project root:
#
#   ./src/scripts/compile-release-controllers.sh "<version>"
#
# The LText files are updated with the concrete version information.
# If the <version> argument is missing, the LTexts default to just the timestamp, as usual:
#
#   ./src/scripts/compile-release-controllers.sh
#
# The compiled controllers are located at `./target/controllers/`.
# The selections for Full/Low-RAM variants are taken from these files:
#   src/scripts/RUL2_IID_structure_full.xml
#   src/scripts/RUL2_IID_structure_noRHW.xml
set -e

if [ ! -e "Controller" ]
then
    echo "Call this script from the root directory of the Network-Addon-Mod repository."
    exit 1
fi

PROJECT_ROOT="$(pwd)"
TEMP="target/controllers/temp"
mkdir -p "$TEMP"

COMPILER_ARCHIVE="target/NAMControllerCompiler_2.0.0.zip"
COMPILER_URL="https://github.com/memo33/NAMControllerCompiler/releases/download/2.0.0/NAMControllerCompiler_2.0.0.zip"

if [ ! -e "$COMPILER_ARCHIVE" ]
then
    # download compiler if it does not yet exist
    curl -L "$COMPILER_URL" > "$COMPILER_ARCHIVE"
fi
echo "66fb918af3791d4e9f2ab04580e26863d7882fec16b4bdd571bdeeb340b6cfe5  $COMPILER_ARCHIVE" | sha256sum --check
unzip -d "$TEMP" "$COMPILER_ARCHIVE"


VERSION="$1"
DRIVESIDE='XHD'
VARIANT=''
DATESTRING="$(date --utc)"
ltext() {
    if [ ! -z "$VERSION" ]
    then
        # If an argument was passed to the script, the version is non-empty.
        printf "NAM Version $VERSION $DRIVESIDE $VARIANT compiled on $DATESTRING"
    else
        # Otherwise pass the empty string to the compiler.
        printf ""
    fi
}


# RHD 4GB Full
VARIANT='(4GB Full)'
DRIVESIDE='RHD'
cp "src/scripts/RUL2_IID_structure_full.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@1=0 NAM Controller_RHD_4GB_Full' 1 "$(ltext)")

# LHD 4GB Full
VARIANT='(4GB Full)'
DRIVESIDE='LHD'
cp "src/scripts/RUL2_IID_structure_full.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@2-0 NAM Controller_LHD_4GB_Full' 0 "$(ltext)")

# RHD no-RHW
VARIANT='(Low-RAM no-RHW)'
DRIVESIDE='RHD'
cp "src/scripts/RUL2_IID_structure_noRHW.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@3-0 NAM Controller_RHD_LowRAM_NoRHW' 1 "$(ltext)")

# LHD no-RHW
VARIANT='(Low-RAM no-RHW)'
DRIVESIDE='LHD'
cp "src/scripts/RUL2_IID_structure_noRHW.xml" "$TEMP/resources/xml/RUL2_IID_structure.xml"
(cd "$TEMP" && java -jar NAMControllerCompiler.jar "$PROJECT_ROOT/Controller" '../@4-0 NAM Controller_LHD_LowRAM_NoRHW' 0 "$(ltext)")

rm -rf "$TEMP"
