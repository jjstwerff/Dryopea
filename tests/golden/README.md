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

## These are current — plan 09's conversion is complete

The lattice conversion finished on 2026-08-13, and these PNGs were
reviewed by eye in **C5c** on a system that is finally self-consistent:
the ring is a ring again. Read them as pictures someone vouched for.

### ⚠ Why the rebaseline took two phases, not one

Worth keeping, because the next coordinate change will meet it again.
A golden is a function of TWO things, and plan 09 moved them
separately:

| | what it decides | which phase moved it |
|---|---|---|
| **geometry** | where a hex's corners are, how rows offset | 09 **C3** |
| **labels** | which `(q, r)` names a cell | 09 **C5** |

**The plan said these move once, in C3. That was wrong.** Geometry and
labels are separable in the CODE — they touch different files, and the
relabel is a pure distance-preserving bijection — but they are **not
separable in a PICTURE**. In between the two phases the renderer drew
pointy-top odd-r while the map data was still axial, which SHEARS the
image: `01_e3_wall_ring_with_grass_centre_hovered.png` was a lopsided
blob rather than a ring for the whole of C3–C5. Three of these also
pass through `paint_line`, which is C4's, so they needed that too.

That it was the labels and not the geometry was verified twice over
before anyone touched a PNG:

- a disc built from the lattice itself (`lat_disc`) rendered as a
  correct hexagonal flower, and
- the same ring with its labels put through `relabel_hex` — exactly
  what C5 then did — closed perfectly.

So the intermediate PNGs were promoted unreviewed, to keep the suite
green and keep catching *unintended* change through C4 and C5, and the
**reviewed** rebaseline happened once at the end. Eleven of the sixteen
moved at C3; two of those moved again at C5c.

A golden is a review aid. The load-bearing checks for the drawing are
exact and live elsewhere, where no rebaseline can launder them:

- `tests/09_c3_geometry.loft` — a rendered hex is TALLER than it is
  wide (that is what pointy-top means in pixels), the camera's hex
  lands at the canvas centre, neighbours sit one flat-to-flat away in
  the direction the compass names, and `screen_to_hex ∘
  world_to_canvas` is the identity.
- `tests/09_c1_oracle.loft` — the lattice against `hex_grid`.
- `tests/09_c2_relabel.loft` — the relabel preserves distance.

## Which files were unaffected by the conversion

Five of the sixteen never moved, and the reason is a useful sanity
check rather than a coincidence: they contain no world hexes.

- `01_e1_sea_origin.png`, `01_e1_sea_camera_offset.png` — all sea, so
  nothing is drawn on top.
- `01_e2_picker_active_*.png` — the palette picker only; UI is drawn
  in canvas pixels and owes the lattice nothing.
