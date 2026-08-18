#!/bin/bash
# Builds a release-ready jar installer, taking as input:
#   --version <version> (preferably no spaces)
#   --data <installation-dir> (path to dat files)
#   --sidebar <png> (installer sidebar image file)
#   --splash <jpg> (installer splash image file)
#   --output <dir> (optional output folder, defaults to `target/installer`)
#
# Example:
#   ./src/scripts/build-installer-jar.sh --version 50.rc1 --data "installation" --sidebar "sidebar.png" --splash "splash.jpg"

set -e

if [ ! -e "Controller" ]
then
    echo "Error: Call this script from the root directory of the Network-Addon-Mod repository."
    exit 2
fi

# Initialize variables
version=""
data_dir=""
sidebar_image=""
splash_image=""
license_file="src/scripts/installer-resources/license.txt"
settings_file="src/scripts/installer-resources/settings.txt"
output_dir=""

# Function to display usage
usage() {
  echo "Usage: $(basename "$0") --version <version> --data <installation-dir> --sidebar <png> --splash <jpg> [--output <dir>]"
  exit 2
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --version|--data|--sidebar|--splash|--output)
      option_name="$1"
      shift
      if [[ -z "$1" ]]; then
        echo "Error: $1 requires a value"
        usage
      fi
      case "$option_name" in
        --version) version="$1" ;;
        --data) data_dir="$1" ;;
        --sidebar) sidebar_image="$1" ;;
        --splash) splash_image="$1" ;;
        --output) output_dir="$1" ;;
      esac
      shift
      ;;
    *)
      echo "Unknown argument: $1"
      usage
      ;;
  esac
done

# Check for missing mandatory arguments
if [[ -z "$version" || -z "$data_dir" || -z "$sidebar_image" || -z "$splash_image" ]]; then
  echo "Error: Missing required arguments."
  usage
fi

if [[ "$sidebar_image" != *.png || "$splash_image" != *.jpg ]]
then
    echo "Error: sidebar image should be png, splash image jpg: $sidebar_image, $splash_image"
    exit 2
fi

check_exists() {
    if [ ! -e "$1" ]
    then
        echo "Error: file/directory does not exist: $1"
        exit 2
    fi
}

# Check existence of files
for f in "$data_dir" "$sidebar_image" "$splash_image" "$license_file" "$settings_file"
do
    check_exists "$f"
done

if [[ -z "$output_dir" ]]; then
    output_dir="target/installer"
fi

INSTALLER_URL="https://github.com/NAMTeam/nam-installer/releases/download/47.0.4/NAM-installer-version47.0.4.jar"
INSTALLER_ARCHIVE="target/NAM-installer-version47.0.4.jar"

if [ ! -e "$INSTALLER_ARCHIVE" ]
then
    # download installer if it does not yet exist
    curl -L --output "$INSTALLER_ARCHIVE" "$INSTALLER_URL"
fi
echo "a447a06a759dccf13bbb6fce2657317a326035419dab74029e395a11a99cc8b6  $INSTALLER_ARCHIVE" | sha256sum --check

TEMP="target/installer/temp"
rm -rf "$TEMP"
mkdir -p "$TEMP"
unzip -q -d "$TEMP/jar" "$INSTALLER_ARCHIVE"
rm -rf "$TEMP/jar/installation"

cp "$splash_image" "$TEMP/jar/splash.jpg"
cp "$sidebar_image" "$TEMP/jar/sidebar.png"
sed "s/__NAMVERSION__/$version/g" "$license_file" > "$TEMP/jar/license.txt"
sed "s/__NAMVERSION__/$version/g" "$settings_file" > "$TEMP/jar/settings.txt"

mkdir -p "$output_dir"
out_jar="$output_dir/NetworkAddonMod_Setup_Version$version.jar"
jar cfm "$out_jar" "$TEMP/jar/META-INF/MANIFEST.MF" -C "$TEMP/jar" .
# add dat files to jar (without copying them into the TEMP folder as intermediate step)
ln -s "$(realpath "$data_dir")" "$TEMP/installation"
jar uf "$out_jar" -C "$TEMP" "installation"

rm -rf "$TEMP"
echo "Jar installer has been created: $out_jar"
