# Translation & LText Guide

This guide covers translation of the mod's [LText](https://wiki.sc4devotion.com/index.php?title=LTEXT) files.  It also contains instructions for developers regarding creating and modifying the default (English) LText sources.

## Translating the NAM

The NAM uses the [gettext](https://en.wikipedia.org/wiki/Gettext) standard for internationalization and recommends the use of Poedit for translation tasks.

### Poedit

[Poedit](https://poedit.net/) is a cross-platform offline program.

Open an existing `.po` translation file and add missing translations,
or start from scratch from an English `.pot` template file.

**Important:** Make sure that under

    Edit > Preferences > Advanced

**both** checkboxes "Wrap at: …" and "Preserve formatting of existing files" are **deselected**.
This is necessary so that Poedit does not alter the wrapping of lines.

Also, within

    Edit > Preferences > General

you should deselect "Automatically compile MO file when saving".  This will prevent the program from creating unnecessary `.mo` files while you translate.

### General notes

- Every linebreak must be explicictly denoted by `\n`.

---

## Maintaining LText Sources

When new content is added to the mod or existing content updated, LTexts are added and modified.

### Adding New LTexts

New LTexts must be added to the English `.pot` in the `\ltext` directory.  Each entry must conform to the format

    msgctxt "2026960B-2A592FD1-55262500"
    msgid "Ped Mall X Elevated Highway Puzzle Piece"
    msgstr ""

where

`msgctxt` contains the LText TGI,
`msgid` contains the untranslated string,
and `msgstr` is left blank.

### Modifying Existing LTexts

Changes to existing LTexts should be made in the English `.pot` files within the `\ltext` directory.  Updating `.pot` LTexts is not possible using Poedit and must be done using a standard text editor.  To make a change, edit the `msgid` string.

### Synchronizing Translations

Whenever LTexts are added or modified, synchronization to the `.po` translation files is also necessary.

#### Synchronizing translations with Poedit

Open the `.po` translation file needing to be synchronized with its parent English `.pot`.  Synchronize changes with

    Translation > Update from POT file...

**Important:** Make sure that under

    Edit > Preferences > Advanced

**both** checkboxes "Wrap at: …" and "Preserve formatting of existing files" are **deselected**.
This is necessary so that Poedit does not alter the wrapping of lines.

#### Synchronizing translations with gettext tools

Synchronize `translation.po` with `template.pot` using

    msgmerge -U --previous --no-wrap --backup=off <translation.po> <template.pot>

In a Unix shell, you can run the script `./synchronize-translations.sh` to synchronize all the translation files at once.

##### Gettext Installation

###### Linux

    apt-get install gettext

###### Windows

1. Download and install appropriate [gettext binary](https://mlocati.github.io/articles/gettext-iconv-windows.html).

2. Update system PATH:

    - Control Panel > System > Advanced > Environment Variables.
    - In the System variables list, click Path, click Edit.
    - Add `;C:\Program Files\gettext-utils\bin` at the end of the Variable value field.
