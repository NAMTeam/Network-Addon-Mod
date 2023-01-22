# Translation Guide

The NAM uses the [gettext](https://en.wikipedia.org/wiki/Gettext) standard for internationalization,
so any compatible tool can be used for translating, such as GitLocalize or Poedit.

[![gitlocalized ](https://gitlocalize.com/repo/8289/de/badge.svg)](https://gitlocalize.com/repo/8289/de?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/fr/badge.svg)](https://gitlocalize.com/repo/8289/fr?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/it/badge.svg)](https://gitlocalize.com/repo/8289/it?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/ja/badge.svg)](https://gitlocalize.com/repo/8289/ja?utm_source=badge)
[![gitlocalized ](https://gitlocalize.com/repo/8289/nl/badge.svg)](https://gitlocalize.com/repo/8289/nl?utm_source=badge)

## GitLocalize

[GitLocalize](https://gitlocalize.com/repo/8289) is an online localization tool.

TBC

## Poedit

[Poedit](https://poedit.net/) is a cross-platform offline client.

Open an existing `.po` translation file and add missing translations,
or start from scratch from an English `.pot` template file.

**Important:** Make sure that under

    Edit > Preferences > Advanced

**both** checkboxes "Wrap at: …" and "Preserve formatting of existing files" are **deselected**.
This is necessary so that Poedit does not alter the wrapping of lines.

TBC

## General notes

- Every linebreak must be explicictly denoted by `\n`.
