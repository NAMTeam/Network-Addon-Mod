# Network-Addon-Mod

Network Addon Mod (NAM) for SimCity 4 Deluxe/Rush Hour @ https://sc4devotion.com/forums/index.php?board=90.0

This repository contains the RUL files of the NAM.

## Documentation

For installation and usage of the NAM, see https://www.sc4nam.com/.

## Translating

[![gitlocalized ](https://gitlocalize.com/repo/8289/de/badge.svg)](https://gitlocalize.com/repo/8289/de?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/es/badge.svg)](https://gitlocalize.com/repo/8289/es?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/fr/badge.svg)](https://gitlocalize.com/repo/8289/fr?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/it/badge.svg)](https://gitlocalize.com/repo/8289/it?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/ja/badge.svg)](https://gitlocalize.com/repo/8289/ja?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/nl/badge.svg)](https://gitlocalize.com/repo/8289/nl?utm_source=badge)

To help translate the NAM to different languages, see the [Translation Guide](ltext/README.md#translating-the-nam).

## Developer notes

The remainder of this README file is intended for developers.

### Structure of the repository

    ├─┐ Controller/
    │ ├── Bridge Controller/
    │ ├── INI/
    │ ├── INRULs/
    │ ├── RUL0/
    │ ├── RUL1/
    │ └── RUL2/
    ├── Lite Controller/
    ├─┐ ltext/
    │ ├── de/
    │ ├── fr/
    │ ...
    ├─┐ src/
    │ ├─┐ main/
    │ │ ├── resources/
    │ │ └── scala/
    │ └── test/
    └── target/

The main code is contained in the folders `RUL0`, `RUL1`, `RUL2`.
The files in there are used as input for the
the NAM Controller Compiler.

The folders `Bridge Controller`, `INI`, `INRULs`
contain network related code that is not part of the NAM controller,
but is contained in other .dat files of the NAM.

The directory `ltext` contains sources for NAM [LText](https://wiki.sc4devotion.com/index.php?title=LTEXT) files.  Translations are contained within its subdirectories.

The directory `src/main/scala` contains code related to Metarules.
These are files written in the programming language Scala
that help automate the process of writing RUL2 code
as well as generating models and paths.
The generated files are written to the `target` directory.

### Compiling the NAM controller

The latest version of the NAM comes with the most up-to-date version of the
[NAM Controller Compiler](https://github.com/memo33/NAMControllerCompiler).
Use the directory `Controller` as input for the Controller Compiler to generate the Controller .dat file.
Usually, this is the only task you need to do after modifying files in the `RUL0`, `RUL1` or `RUL2` folders.

### Compiling INRULs

Get the Java utility [BuildRULs](https://www.dropbox.com/s/ckwhy11xxaz3z1q/BuildRULs_01.zip?dl=0) and run the command

    java -jar /path/to/.../BuildRULs.jar -f Controller/INRULs/ Controller/INRULs/

with the path to the `BuildRULs.jar` file replaced by the one on your system.
This creates or updates the files

    ┐ Controller/
    └─┐ INRULs/
      ├── NetworkAddonMod_IndividualNetworkRULs_LHD.dat
      ├── NetworkAddonMod_IndividualNetworkRULs_RHD.dat
      └── NetworkAddonMod_Lite_INRULs.dat

### Compiling Metarules

Compiling the Metarules code is only needed when modifying any of the Scala code.
For executing the Scala code, make sure you have [sbt](https://www.scala-sbt.org/) installed.
It is a build tool that will download all the required dependencies.

Then execute

    sbt run

to compile and run the Scala code.
This takes the Metarules as input to generate RUL2 code and to create some models and paths.
Currently, this generates the following files:

    ┐ target/
    ├── 11_FlexFly_falsies_MANAGED.txt
    ├── 5B00_FlexFly5x5_MANAGED.txt
    ├── FlexFly.dat
    ├── nwmTurnPaths.dat
    └── Sec7j_FLEXFly_MANAGED.txt

You need to manually move the generated RUL2 files from the `target` directory
to the correct locations in the `Controller` directory and then commit them into the git repository.
For more information on Metarules, see https://github.com/memo33/metarules.

### Compiling Locale Files

To convert all the translations contained in the directory [ltext/](ltext/)
to a format that can be used by the game,
use the tool [sbt](https://www.scala-sbt.org/) and run

    sbt generateLocales

The generated `.dat` files are found at

    ┐ target/locale/
    ├── NetworkAddonMod_Locale_en.dat
    ├── NetworkAddonMod_Locale_it.dat
    ...

For information on adding and modifying LTexts, see the [Translation & LText Guide](ltext/README.md#maintaining-ltext-sources).
