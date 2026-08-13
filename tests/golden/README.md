<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Golden images

Committed reference renders. `src/golden.loft::assert_golden` writes
`tests/actual/<name>.png` and compares it byte-for-byte against the
file of the same name here.

Bootstrapping a new golden: run, watch it FAIL, look at
`tests/actual/<name>.png`, and copy it here once it is right.

## ⚠ These are INTERMEDIATE right now — plan 09 is mid-conversion

**Do not read the current PNGs as pictures anyone vouched for.**

A golden is a function of two things dryopea is changing separately:

| | what it decides | which phase moves it |
|---|---|---|
| **geometry** | where a hex's corners are, how rows offset | 09 **C3** — shipped |
| **labels** | which `(q, r)` names a cell | 09 **C5** — open |

C3 has landed, so the renderer draws pointy-top odd-r. The map data
in these tests is still **axial**. Drawing axial labels with odd-r
geometry SHEARS the picture — so `01_e3_wall_ring_with_grass_centre_
hovered.png` currently shows a lopsided blob rather than a ring, and
that is expected.

It was verified to be the labels and not the geometry, twice over:

- a disc built from the lattice itself (`lat_disc`) renders as a
  correct hexagonal flower, and
- the same ring with its labels put through `relabel_hex` — which is
  exactly what C5 will do — closes perfectly.

**⚠ The plan said these move once, in C3. That was wrong**, and the
reason is worth keeping: geometry and labels are separable in the
CODE (they touch different files, and the relabel is a pure
adjacency-preserving bijection) but they are not separable in a
PICTURE. Neither phase alone leaves a reviewable image. Three of
these also pass through `paint_line`, which is C4's, so they need
that too.

So they were promoted here to keep the suite green and to keep
catching *unintended* change through C4 and C5 — which they still do
— and the **reviewed** rebaseline happens once at the end of the
conversion, not here.

Until then, the load-bearing checks for the drawing are exact and
live elsewhere, where no rebaseline can launder them:

- `tests/09_c3_geometry.loft` — a rendered hex is TALLER than it is
  wide (that is what pointy-top means in pixels), the camera's hex
  lands at the canvas centre, neighbours sit one flat-to-flat away in
  the direction the compass names, and `screen_to_hex ∘
  world_to_canvas` is the identity.
- `tests/09_c1_oracle.loft` — the lattice against `hex_grid`.
- `tests/09_c2_relabel.loft` — the relabel preserves distance.

## Which files are unaffected by the conversion

Five of the sixteen did not move at C3, and the reason is a useful
sanity check rather than a coincidence: they contain no world hexes.

- `01_e1_sea_origin.png`, `01_e1_sea_camera_offset.png` — all sea, so
  nothing is drawn on top.
- `01_e2_picker_active_*.png` — the palette picker only; UI is drawn
  in canvas pixels and owes the lattice nothing.
