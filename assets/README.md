<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# assets/ — binary content the running game loads

⚠ **One file today.**  This directory exists because a font is not
source, not a map and not an example: it is a binary blob the program
opens at runtime and that carries **its own licence**.

| File | What | Licence |
|---|---|---|
| `DejaVuSans-Bold.ttf` | ⚠⚠ **The ONE font.**  Loaded by [`src/font.loft`](../src/font.loft) — see `TEXT_FONT_FILE` | Bitstream Vera — [`LICENSE-DejaVu.txt`](LICENSE-DejaVu.txt) |

## Why DejaVu Sans **Bold**

- **Bold** — text is drawn over a busy world, and a hairline face is
  unreadable there.
- **DejaVu** — its Bitstream Vera licence is **permissive**, and this
  repo is LGPL-3.0-or-later.  ⚠ The obvious alternative was refused on
  **licence rather than looks**: Liberation Sans Narrow (which the
  `graphics` package itself bundles, and which is narrower and a sixth
  of the size) is **GPL-2 with a font exception** — copyleft.

⚠⚠ **The licence file travels with the font because that licence says
it must**: *"the above copyright and trademark notices and this
permission notice shall be included in all copies"*.  Deleting
`LICENSE-DejaVu.txt` is a licence violation, not tidying.

## Changing the font

One constant — `src/font.loft::TEXT_FONT_FILE`.  ⚠ Put the new file
here with **its** licence beside it, and check
`tests/b1_the_font_seam.loft` still passes: its metric assertions are
deliberately **relations** (a longer string is wider, a bigger size is
taller) rather than pixel counts, so a different face does not
re-baseline them.

## See also

- [`src/font.loft`](../src/font.loft) — the seam, and why the path is
  absolute.
- [`docs/DECISIONS.md`](../docs/DECISIONS.md) — `@X268`, `@M047`.
