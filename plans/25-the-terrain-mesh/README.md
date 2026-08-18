<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 25 — the terrain mesh: the painting becomes a place

**Value:** `G` · **Effort:** `H`

## Status

**COMPLETE** — M0-M4 done (2026-08-17).  Suite **1242** green
(1198 + 4 + 9 + 16 + 10 + 5), gate 33 scripts / **654 measurements unchanged**
across all five — a mesher is not a simulation, and the day it re-prices a
scenario is the day something is reading it that should not be.  **M3 adds a
THIRD gate**: `scripts/validate_gl.sh`, 2 fixtures / 26 measurements, under
`xvfb`.

⚠ **Every test in every phase passed on the first run, which is when to check
the gate can FAIL.**  Twenty-four load-bearing assertions were falsified
deliberately and all twenty-four fired, each at the value predicted for it.
§ M0, § M1, § M2, § M3 and § M4 have the tables.

⚠⚠ **M3's headline is that the gate everyone would have written is BLIND to
the defect `render_camera.loft` is most afraid of.**  A fully mirrored world
passes `other == 0`, passes `sea == 0`, and passes every per-kind pixel count
in band — because a count is permutation-invariant.  Only a LANDMARK, compared
against `camera_screen`'s own prediction, can see it, and it sees it at
**490.8 px** (`@M027`).  § M3.

⚠⚠ **M4's headline is that the phase's own stated invariant was wrong.**  It
was specified as a timing RATIO and the clock cannot carry it here — two
IDENTICAL back-to-back calls differed by **5.4x** when a registry fetch stalled
inside one of them.  The gate counts **floats baked** instead, which is
deterministic and is what a mesher's cost regression actually is.  ⚠ It then
prices M1's two *invisible* breaks for the first time: a zero-area sliver at
every hex boundary **triples the world's geometry** and draws not one pixel.
§ M4.

⚠⚠ **M3's headline is that the gate everyone would have written is BLIND to
the defect `render_camera.loft` is most afraid of.**  A fully mirrored world
passes `other == 0`, passes `sea == 0`, and passes every per-kind pixel count
in band — because a count is permutation-invariant.  Only a LANDMARK, compared
against `camera_screen`'s own prediction, can see it, and it sees it at
**490.8 px** (`@M027`).  § M3.

⚠⚠ **M1's falsification found a defect in the TEST rather than in the code**,
and it is the reusable half: four counts bundled into one function are RANKED,
not independent — loft abandons a test function at its first failed assertion,
so three of the four could never be printed.  ⚠ M3 met the same shape from the
other side: an IMPURE falsification (mirroring the tops but not the sides) trips
an earlier assertion and so proves a different claim than the one it aimed at.

⚠⚠ **M1's falsification found a defect in the TEST rather than in the code**,
and it is the reusable half: four counts bundled into one function are RANKED,
not independent — loft abandons a test function at its first failed assertion,
so three of the four could never be printed.  Splitting them turned one
diagnosis into three for the same break.  § M1.

⚠⚠ **M2's own control killed M2's headline gate before the code was
falsified at all**, which is the same lesson from the other end: the test
written to prove the tile walk is by COORDINATE could not fail, because loft's
keyed collections iterate in key order rather than insertion order (`@M025`).
The walk is still load-bearing — for COVERAGE, not determinism.  § M2.

**Opened 2026-08-17.**  This plan takes
[`plan 21`](../21-the-renderer/README.md)'s `R3`-`R5`, which that plan said
twice should be split out before they were started (*"R3 alone is plausibly its
own plan"*).  Plan 21 closes at R2 as **the camera**; this one is **what the
camera looks at**.

⚠ Four things were probed before a line of this was written, and three of them
changed its shape.  They are § What was measured first.

⚠⚠ **The headline: this is much smaller than plan 21 said it was.**  That
document calls R3 *"the largest single item in the repo's history"* on the
strength of moros's 3 257-line `hex_mesh`.  Measured against dryopea's actual
world, the two mechanisms that make `hex_mesh` big — the corner-height **mean**
and the **halo** it needs — are both no-ops here, and the GPU-side caching layer
is already published.  What is left is a mesher over a flat plane with pillars
on it, a flat-unlit shader, and the gate chain.

## Goal

The world the simulation runs on is drawn as triangles from the game's camera,
and a GL frame of it is measured by the same exact-colour instrument that
measures the editor's.

## Anchors

Implements, and does not restate:

- [`docs/RENDERER.md`](../../docs/RENDERER.md) § R2 (the pipeline), § R3 (the
  terrain), § R4 (the gate chain), § R5 (cost).
- [`plan 21`](../21-the-renderer/README.md) — the camera this draws from, and
  § R0, whose measurement makes this affordable at all.
- [`probe/r0/`](../../probe/r0/) — the kept probe: a GL frame captured with no
  display and classified at **zero** colour drift.
- `src/passable.loft` (`hex_height` / `hex_ground`), `src/painted.loft`,
  `src/height.loft`, `src/lattice.loft` (`lat_corner_metres`),
  `src/render_camera.loft` (`lat_to_world`), `src/chunks.loft`,
  `src/editor_view.loft`, `src/measure.loft`.
- `../moros/lib/hex_mesh/src/hex_mesh.loft` — the **construction** to follow,
  not a dependency (`@X001`'s argument; it is `hex_voxel`-backed and
  unpublished).  ⚠ Read § What was measured first before reading it: most of
  what it does, dryopea does not need yet.

## What was measured first

Four probes, run before this document existed.  ⚠ None of them is kept as a
`probe/` directory — each answer graduates directly into a phase's assertions,
which is what `probe/README.md` says to do with a throwaway.

### ⚠⚠ 1. dryopea's ground is FLAT, so the corner MEAN is a no-op

`examples/palette.json`, all twelve kinds: `height_override` is non-null on
exactly **two** — `wall` at 3.0 m and `wall_high` at 5.0 m.  Every other kind
is 0, and `hex_height` is *that* plus the runtime rubble layer.  There is no
heightfield: dryopea's ground is a **flat plane with discrete objects standing
on it**, and terrain elevation is [`plan 02`](../02-solver-validation-viewer/README.md)'s
slope solver, unbuilt.

That kills the mechanism `docs/RENDERER.md` § R3 spends most of its length on.
`@X044` (*a corner height is a MEAN of the hex and the neighbours sharing that
corner*) is what makes moros's terrain slope instead of step — and here:

- across flat ground every term of the mean is 0, so it changes nothing;
- across a wall's edge the mean must be **skipped** anyway, because `@X045`'s
  `faced_between` skips neighbours across a face and every structure edge is a
  face.

⚠ So the mean is a no-op at every hex in the world, in both directions.  The
mesher this plan builds emits **flat top faces and vertical side quads** and
does not blend at all.  `@X072`.

⚠⚠ **And that is the honest picture, not a shortcut.**  The simulation asks
`can_step`, which is a height DIFFERENCE — a step, never a slope.  A mesh that
sloped where the rule steps would draw a ramp the vehicle cannot climb, which
is a lie the player pays for.  Flat tops and vertical sides make a height
difference **readable as the size of a quad**, which is what the player is
actually deciding about.

⚠ **The tripwire is M2's halo.**  A blend reads neighbours, so it needs a halo
wider than the one this plan ships; when plan 02 lands, the halo has to grow in
the same change.  M2 asserts the halo width against the mesher's actual
neighbour reach rather than hard-coding it, so adding a blend fails there.

### ⚠⚠ 2. The GPU-side cache is already published — this plan writes no renderer

Both halves of what plan 21 § R5 called *"the terrain rebuild, and that is
exactly what `gridmesh`'s dirty-chunk set exists to bound"* exist as library
surface:

| needed | published |
|---|---|
| a `Mesh` flattened for GL upload | `mesh3d::mesh_to_floats(m)` → `vector<single>`, stride 6 (pos + normal), straight into `gl_upload_vertices(data, 6)` |
| a per-chunk VBO cache with a dirty path | `graphics::GroupVboSet` — *"upload (or RE-upload) one group's baked vertices … call this only for the DIRTY groups each edit; untouched groups keep their cached VBOs"* |

⚠ `group_vbos_upsert` takes its bounds as `min_x, min_z, max_x, max_z` — a
**Y-up, ground-in-XZ** frame, which is moros's and not dryopea's.  The bounds
are only stored (for a frustum cull that does not exist yet), so it is a naming
mismatch rather than a fault — but it is exactly the shape of thing that gets
the wrong pair passed to it.  ⚠ dryopea's ground is in **XY** with `+z` up
(`lat_to_world`), so the `_z` arguments take dryopea's **y**.

### ⚠⚠ 3. The corner ring winds COUNTER-CLOCKWISE in the camera's world

`lat_corner_metres` negates y (dryopea's screen frame: clockwise, per
`CLAUDE.md` § Hex convention) and `lat_to_world` negates it again (the camera's
frame).  Two negations restore `hex_grid`'s own winding.  Measured on hex
`(0, 0)`, corners at 90° / 150° / 210° / 270° / 330° / 30°:

```
normal.z of (c0, c1, c2)      = +0.48713929
normal.z of (centre, c0, c1)  = +0.48713929
```

Positive z is counter-clockwise seen from above, which is GL's default front
face.  ⚠ **So a top face emitted as a centre fan in corner order 0→5 is
front-facing and `GL_CULL_FACE` needs no reversal.**  Get it backwards and the
ground is invisible while every other valve reads healthy — which is why M3
turns culling **on** rather than leaving it off.

⚠ The number above is not carried into the code.  M0 recomputes the winding
from the triangles the mesher actually emitted, because a constant copied out
of a plan is a claim about a version of the lattice that may have moved.

### ⚠ 4. The corner↔direction relation is the LIBRARY's, and it agrees

A side quad needs *which two corners bound the edge towards neighbour `dir`*.
`hex_grid::hex_edge_corners(dir)` already answers it, and it matches a
hand-derivation from the measured corner angles exactly:

| dir | 0 (E) | 1 (SE) | 2 (SW) | 3 (W) | 4 (NW) | 5 (NE) |
|---|---|---|---|---|---|---|
| corners | (4, 5) | (3, 4) | (2, 3) | (1, 2) | (0, 1) | (5, 0) |

⚠ **dryopea delegates and never tabulates** — `src/lattice.loft` gains a
`lat_edge_corners`, for the same reason `lat_neighbour` is the only stepper: a
hand-written table is a second lattice, and moros#10 is what a second lattice
costs.  `@X073`.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **M0** | one `grass` hex meshes as **6 triangles**, every vertex at z = 0 and at `lat_corner_metres` negated; a `wall` carrying 1.0 m of rubble meshes its top at **4.0 m** and colours **`rubble`** | the surface is not the painted kind — height off `hex_height` (structure **+** layer), colour off `hex_ground` | ⚠ `CLAUDE.md`'s oldest trap: swap the two lookups and piling debris on a wall **LOWERS** it to 1.0 m.  ⚠ Plus R1's generic control — assert the mesh is non-empty and that the fixture's heights actually DIFFER, or every per-vertex assertion holds vacuously |
| **M1** | a lone `wall` in grass emits **6** side quads; two adjacent walls emit **10**, not 12; each quad's normal points AWAY from the column that owns it | a face is emitted once, by the side that STANDS (`@X046`) | ⚠ Reverse `hh <= nh` and the count must move — otherwise the test is measuring "some quads exist".  ⚠ Two equal-height neighbours must emit **nothing** between them: an unconditional quad passes the lone-wall count and fails this one |
| **M2** | the CRC of one chunk rebuilt alone **equals** its CRC out of a rebuild-all; the mesher's **measured** reach equals `MESH_HALO_K`; every tile an edit moves is one the dirty rule NAMES | a local rebuild is not an approximation of a global one | ⚠ The `11_f8` shape, and it needs both directions: a paint edit **inside** the halo must change the CRC, one **outside** must not.  ⚠⚠ Plus two the plan did not foresee: `mesh_crc` of an **empty** mesh is **0**, so a mesher stubbed to emit nothing satisfies every equality — every one carries a non-zero floor; and the dirty rule needs an **interior** edit (to show it is not simply *everything*) AND a **boundary** one (to show it reaches across), because neither is enough alone |
| **M3** | a known fixture drawn under `xvfb`, captured and classified: `other` = **0** *and* the per-kind counts land in bands | the GL path and the software path are two rasterisers over ONE geometry (`@X010`) | ⚠⚠ **`other == 0` is satisfied perfectly by a BLACK FRAME**, which is R0's own trap arriving one layer down — probe 2 exists because probe 1 would have passed against one.  The gate must assert the ground's pixel count is non-zero and in band, never merely the absence of unknowns.  ⚠ `GL_CULL_FACE` **on**, so a reversed winding fails here rather than looking dim |
| **M4** | a RATIO inside `loft test` | cost is a ratio, never a stopwatch | ⚠ `CLAUDE.md` § Cost: an unchanged file timed 173 / 737 / 754 ms on three interpreter runs.  ⚠ And the ratio needs a FLOOR as well as a ceiling — a mesher stubbed to emit nothing has an excellent ratio |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **M0** — the top face | S | `tests/25_m0_the_top_face.loft` (4 fns) — one hex as six triangles at `hex_height`, keyed by `hex_surface_index`, winding recomputed from the emitted triangles.  ⚠ Both assertions falsified deliberately and both fired | **Done** (2026-08-17) |
| **M1** — the sides | M | `tests/25_m1_the_sides.loft` (9 fns) — a quad per faced edge, emitted from the standing side only, over `lat_edge_corners`.  ⚠ Five breaks tried, five fired | **Done** (2026-08-17) |
| **M2** — the world, in chunks | M | `tests/25_m2_the_rebuild.loft` (16 fns) — `mesh_crc` (ported, and it folds the TRIANGLES too), the drawn region, one-tile == all-tiles, and the reach measured against `MESH_HALO_K`.  ⚠ Eight breaks tried, eight fired | **Done** (2026-08-17) |
| **M3** — it is DRAWN, and gated | MH | `scripts/validate_gl.sh` — 2 fixtures under `xvfb`, 26 measurements; plus `tests/25_m3_the_ground_gl.loft` (10 fns) for the half that needs no context.  ⚠ Seven breaks tried, seven fired | **Done** (2026-08-17) |
| **M4** — cost | S | `tests/25_m4_the_mesh_budget.loft` (5 fns) — ⚠ **float counts, not a ratio**: the clock could not carry the claim.  Two breaks tried, two fired | **Done** (2026-08-17) |

## M0 — the top face (2026-08-17)

`src/ground_mesh.loft::ground_top_face` + `passable.loft::hex_surface_index` +
`tests/25_m0_the_top_face.loft` (4 fns).  One function emits one hex's top as a
six-triangle fan around the centre, at `hex_height(pal, pw, hl, q, r)`, into a
`mesh3d::Mesh`.

### ⚠⚠ Four tests green on the first run — so both were falsified on purpose

A phase whose gate passes immediately has proved nothing yet: the assertions may
be true, or they may be unable to see the thing they name.  Both load-bearing
ones were broken deliberately and both fired, at the predicted value:

| the break | what the gate said |
|---|---|
| `add_triangle(m, mid, b, a)` — the fan reversed | *triangle 0 winds CLOCKWISE (normal.z **−0.48713928962874675**) — with GL_CULL_FACE on this hex draws NOTHING* |
| height taken off the SURFACE instead of the authored entry | *every vertex of the piled wall's top sits at 4.0 m, got **1*** |

⚠ The winding magnitude came back as the probe's own number with the sign
flipped, which is the second confirmation that the two negations cancel — and it
arrived from the emitted triangles rather than from the plan.

⚠⚠ **The reversed fan is the one worth dwelling on.** It changes no count, no
height, no colour and no vertex position — six triangles at the right heights in
the right places — and under `GL_CULL_FACE` it draws *nothing at all*.  There is
no valve on the mesh that reads unhealthy.  That is why M0 gates the winding as
DATA, three phases before anything is drawn: by M3 the symptom is an empty
frame, and an empty frame has a dozen plausible causes.

### ⚠ `hex_surface_index` is new, and the index rather than the entry is the point

`hex_ground` is private and answers a `GroundType`.  The mesher groups hexes by
surface (`@X074`: one mesh per palette kind), and a key has to be comparable —
so `passable.loft` gained `hex_surface_index`, which is the same question given
back as the identity.  ⚠ It carries `hex_ground`'s own guard: a palette too
short to hold `rubble` gives back the map that was painted, rather than a kind
the renderer has no colour for.

⚠ **The colour is NOT on the vertex.**  `mesh3d::Vertex` is position + normal +
uv and carries none, and that turns out to be the right shape rather than a gap:
the mesher emits **one mesh per palette kind**, which is moros's own
`chunk_mesh_mat(cx, cz, wld, want_mat)` signature, and the kind's colour becomes
a **uniform** on the draw call.  `@X074`.

⚠⚠ **That is what keeps R0's measurement alive.**  `classify_world`'s founding
rule is an EXACT colour lookup — *"a pixel that is not a palette colour lands in
`unknown` and is a FAULT"* — and a frame drawn flat-unlit with a palette colour
as a uniform **can only contain palette colours**, by construction.  A
per-vertex colour would be interpolated across the triangle and every gradient
pixel would be a fault.  So `docs/RENDERER.md` § Open 3's *"flat unlit render
mode for the gate"* is not a mode this plan adds later; it is the only way the
mesher is written.

## M1 — the sides (2026-08-17)

`src/ground_mesh.loft::ground_side_faces` + `lattice.loft::lat_edge_corners` +
`tests/25_m1_the_sides.loft` (9 fns).  For each of the six directions, if this
hex stands **above** the neighbour, one vertical quad from the neighbour's
height to this one's, over the two corners `lat_edge_corners(dir)` names.

⚠ `@X046` is the whole rule: `if hh <= nh { continue; }`, or the same quad is
emitted twice — once from each cell — *"and the second copy is back-facing, so
it is invisible and pure cost."*  With `GL_CULL_FACE` on (M3) the second copy is
invisible, which is exactly why the count has to be gated here where it is data.

⚠ **Absent is zero.**  `PaintedWorld` is sparse and sea-default, so a wall at
the edge of the painted region has a neighbour at height 0 and gets its side
quad.  Asserted, because the alternative — a neighbour lookup that answers "no
such hex" and skips — draws a base with one open side, and only where the author
stopped painting.  ⚠ Its control is the same wall ringed by PAINTED grass giving
the identical six: the two worlds have to disagree about the map and agree about
the mesh, or the assertion is comparing a fixture with itself.

### ⚠⚠ Both halves of the guard fail INVISIBLY, so the gate is four counts

Neither way of getting `hh <= nh` wrong changes a single pixel:

| the break | what it does to the picture | what it does to the mesh |
|---|---|---|
| no guard at all | nothing — the second copy is back-facing | every faced edge twice |
| `<` instead of `<=` | nothing — a sliver has no area | a zero-area quad at **every** hex boundary in the world |

So M1 gates COUNTS on four fixtures that disagree about them — a lone wall
(**6**), two adjacent walls (**10**, never 12), two grass hexes (**0**), and a
step (**5** for the short column, **6** for the tall one).  ⚠ No one of them is
enough: an unconditional quad passes the first, a mesher that skipped every
shared edge passes the third, and the fourth is the only one that can see the
face being drawn by the **wrong** side — the two-wall fixture cannot, because
both its columns are the same height.

### ⚠⚠ Four FUNCTIONS, not four asserts — and this is the reusable finding

The first version of the file bundled all four counts into one test function.
Falsifying the guard showed why that is wrong: **loft abandons a test function
at its first failed assertion**, so the four were ranked rather than
independent.  Dropping the `=` breaks the two-wall count *and* the flat-ground
count, and only the two-wall one was ever printed.

Split into four functions, the same break reports **three** independent
failures.  ⚠ The rule generalises past this file: *a count that can never be the
diagnosis is decoration*, and the way to tell is to falsify and see whether it
speaks.

### ⚠⚠ A quad carries TWO facts about which way it faces, and they can disagree

M0 could only get the fan backwards.  A side quad has a stored **normal** and a
triangle **winding**, and they are computed from different things — the normal
from the two hex CENTRES (`lat_to_world(neighbour) − lat_to_world(here)`,
outward by construction, which is moros's rule for the same reason), the winding
from the corner RING.  They agree only because two y-negations cancel.

⚠ So the test asserts each of them *and asserts they agree*.  A mesh whose
normals point out and whose triangles wind in draws nothing under
`GL_CULL_FACE` while every normal reads healthy — M0's reversed fan, arriving
one layer down and now with a second valve to read healthy alongside it.

### ⚠ Five breaks tried, five fired, each at its predicted value

| the break | what the gate said |
|---|---|
| guard deleted (a quad per direction) | **three** failures: two walls `got 12`, flat ground `got 24 triangles`, the short column `got 6` |
| `<` instead of `<=` | two walls `got 12`, flat ground `got 24 triangles` |
| quad wound `(a_bot, a_top, b_top, b_bot)` | *face 0 winds against its own normal (agreement **−2.25**)* |
| normal taken from centre−neighbour | *face 0's normal (−1, 0) points back INTO the column (outwardness **−0.6495**)* |
| quad run to the ground instead of the neighbour's height | *exactly two vertices sit there; got **0** — the quad ran to the ground and buried the neighbour's top face* |
| `lat_edge_corners` hand-tabulated as `(d, (d+1) % 6)` | the geometric check **and** the outwardness check, `3.9375 vs 0.5625` |

⚠ The two magnitudes are the predicted ones and that is the point of quoting
them: **−2.25** is the cross product of a 0.75 m edge with a 3 m wall, sign
flipped, and **−0.6495** is the hexagon's apothem — so each break produced the
number its geometry says it should, rather than merely *a* failure.

### ⚠ `lat_edge_corners` takes no `Hex`, and the split is the point

The neighbour LABEL delta is parity-dependent — that is what makes
`lat_neighbour` the only stepper.  The corner relation is **not**: a hexagon is
the same hexagon on both row parities and only its coordinates move.  Same shape
as `lat_direction_unit`, which is uniform for the same reason.

⚠ And the test **re-derives** the pairing rather than restating `hex_grid`'s six
rows: the two corners it names must be the two geometrically nearest the
neighbour's centre, on both parities, and they must be consecutive in the ring.
A test that copied the table would certify the copy — and would have been green
through moros#10.  `@X073`.

## M2 — the world, in chunks (2026-08-17)

`src/mesh_crc.loft` + `src/mesh_chunks.loft` +
`ground_mesh.loft::ground_chunk_kinds` / `ground_chunk_mesh` +
`tests/25_m2_the_rebuild.loft` (16 fns).  A tile is 32×32 hexes, meshed one
palette kind at a time, and two builds of one tile are compared by folding the
geometry to an integer.

⚠ **The mesher gets its own tiling** (`@X047`): `chunks.loft` is a **store**
tiling at 32 with `CHUNK_HALO_K = 0`, sized for windowing, and its own header
says the halo stays 0 *"while ground fill is per-cell-independent"*.  It is not
any more: a side quad reads one neighbour.  So the mesh tile is separate, and it
carries a halo of **1** — not moros's `MESH_MARGIN = 2`, because there is no
corner mean to be correct across a tile edge (§ What was measured first, 1).

### ⚠⚠ The headline gate could not fail, and its own control said so

The test written first was *one map painted in two orders gives one mesh* — the
theory being that a walk over `pw.painted` would follow the paint order, making
a tile's checksum a fact about its edit history rather than about the map.  It
went red immediately, on the control rather than on the claim: **the two
fixtures iterate their painted hexes in the same order.**

Measured over six fixtures — reversed insertion, a scrambled spread, a key
landing in the middle of the range, negative coordinates, and a
grow-then-erase — loft's `hash<PaintedHex[q, r]>` comes back sorted
lexicographically every time, and stays sorted when an unrelated key is added.
⚠ **loft's keyed collections are an ordered index, not a bucket hash**
(`@M025`).  So insertion order is not a hazard in this language and the gate was
aimed at a mechanism that does not exist.

⚠ The coordinate walk stays, for a reason the plan had not connected to it:
**the drawn region is wider than the painted set** (below), so a walk over the
painted entries structurally cannot reach the coastline.  It is a COVERAGE
argument, not a determinism one, and
`test_the_walk_visits_hexes_the_painted_set_does_not` is the gate that can see
it — 63 vertices against 0.

⚠⚠ **The reusable half is the shape, not the finding**: a gate aimed at the
mechanism you EXPECT to be the hazard is not one aimed at the hazard.  Plan 12
B3's fence tripwire made the identical mistake from the other end — written to
go red the day a steering rule landed, still green when it did.

### ⚠⚠ What draws SEA — `@X075`, and it is an interim with a named limit

Open question 3 is answered: **the drawn region is the painted set plus a
one-hex ring.**  `painted.loft` ERASES a hex painted `sea`, so "mesh the painted
set" leaves an erased region as a **hole in the ground at exactly the height of
the land around it** — and no side quad covers it either, because sea is 0 m and
so is grass, so there is no face on that boundary at all.

The two rejected candidates each fail on a principle rather than on taste:

| candidate | why not |
|---|---|
| the painted set's **bounding box** | draws sea over every gap, so two bases 500 hexes apart mesh a quarter-million hexes of ocean — unbounded cost on a sparse world |
| the **tile's full extent** | makes how far the ocean reaches a function of where the tile boundaries fell.  ⚠ That is `plans/22`'s rule in a new place: a picture that depends on an implementation tiling is one nobody can reason about from inside |

A one-hex ring depends on the MAP alone and is bounded by 7× the painted set
(~1.3× for any compact region).

⚠ **Its limit is that an erased gap wider than two hexes still holes**, and
`test_a_gap_wider_than_the_ring_still_holes` asserts exactly that — so closing
it is a deliberate act and not a silent one.  ⚠ The case is narrower than it
sounds: a real lake is painted `water` (kind 1), which IS stored; only the `sea`
brush erases, so a hole is a region the author explicitly rubbed out.

⚠⚠ **And M3 inherits a requirement from it.**  A hole shows the clear colour,
which is not a palette colour, so `classify_world` lands those pixels in
`unknown` and `other == 0` goes red.  **Set M3's clear colour to a palette
colour and this interim's one failure mode becomes invisible.**  ⚠ Which
surfaces a second M3 problem the invariant table had not noticed: a 3-D frame
has a HORIZON, so `other == 0` cannot hold at all unless the background is a
classified colour in its own right — the gate is `other == the sky's count`, or
the camera looks at nothing but ground.

### ⚠⚠ There is no `ChunkField`, and that is a change of plan

A second `gridmesh::ChunkField` at halo 1 was the obvious way to spell "the mesh
tile is separate", and writing it turned up two reasons it is the wrong way:

- ⚠⚠ `collect_dirty_inputs` **skips a dirty chunk that owns no cells.**  With a
  one-hex ring a tile owning NOTHING can still have sea to draw — a hex painted
  on a tile boundary puts its ring in the next tile — so the incremental path
  would leave that tile's mesh stale, and only ever at a tile edge.
- ⚠ `mark_borders` steps CHUNK coordinates **rectangularly** in (x, y).  That is
  right here only because odd-r's six neighbours all lie within one step of `q`
  and of `r` — a property of this lattice that `gridmesh` cannot know, and
  `CLAUDE.md`'s rule is that nothing may step a coordinate except
  `lat_neighbour`.

So the reach is asked directly, and `mesh_chunks_touched` is **one mechanism
with two callers**: read as an edit it is the dirty rule, and summed over the
painted set it is the tile list.  ⚠ A dirty rule derived differently from the
build rule is two rules that can disagree about a tile edge.  What stays
`gridmesh`'s is `chunk_of`, whose `chunk_div` is floor division rather than
truncating — the arithmetic worth borrowing and the one a hand-rolled `>>` gets
wrong left of the origin.

### ⚠⚠ TWO reaches, both 1 today, and deliberately not one constant

`MESH_HALO_K` is how far an edit's consequences REACH (one hex, because a side
quad reads one neighbour's height).  The ring is how far past the painted set
anything is DRAWN (one hex, because sea is stored as absence).  ⚠ They share a
value and must not share a constant: plan 02's slope solver raises the READ
reach to two rings without changing what should be drawn.

⚠ So `test_the_measured_reach_is_the_declared_halo` **measures** the reach —
sweeping a paint edit 1, 2 and 3 hexes outside the tile and reporting the
furthest that can still move its checksum — and compares *that* to
`MESH_HALO_K`.  Written this way it fails in both directions, and the blend
break below shows it firing as the plan-02 tripwire with the right diagnosis.

### ⚠⚠ `mesh_crc` deviates from the port, and the reason is M0

moros's `hex_mesh::mesh_crc` folds positions and normals only.  Safe there, and
NOT safe here: `ground_top_face` **shares its six rim vertices between the six
fan triangles**, so M0's reversed fan moves no vertex at all — same positions,
same normals, same order, only the indices differ.  A vertex-only checksum is
blind to precisely the defect M0 exists for, and that defect draws nothing under
`GL_CULL_FACE` while every other valve reads healthy.  ⚠ So the triangle indices
are folded after the vertices.

⚠ It also **rounds** where moros's truncates: every height the palette carries
(0.0, 3.0, 5.0) is an exact multiple of `1/MESH_CRC_SCALE`, which is where
truncation puts its discontinuity.  ⚠⚠ **And that rounding was a guard nothing
could reach** — every comparison in the file runs identical arithmetic on
identical inputs, so both sides land on the same side of any discontinuity
whatever the code does.  `CLAUDE.md` § Timers and epsilons is the rule and
`test_the_scale_is_the_tolerance` is the fix: 3.0 against 3.0 − 1e-9 must AGREE
and 3.0 against 3.0 + 1e-5 must DIFFER, which reaches the branch directly.

⚠ `mesh_crc` belongs in `mesh3d` — a checksum over a `Mesh` is adjacent to
`mesh_to_floats`, reuse is this repo's rule, and moros carries a private copy
too, which is two.  Local today because M2 needs it now and the triangle fold is
a dryopea finding nobody has argued upstream.  Trigger to move it: a second
non-test caller, or plan 20 wanting it for entity meshes.

### ⚠ Eight breaks tried, eight fired, each at its predicted value

| the break | what the gate said |
|---|---|
| the triangle fold deleted (moros's function) | *two meshes with IDENTICAL vertices and opposite winding checksum the same (3342803724)* |
| mesh the PAINTED SET instead of the drawn region | **two** failures: *nine unpainted sea hexes … got 0*, and the lone wall's coastline *got 0* |
| a corner-height BLEND — the side face reads two neighbours out | **two**: *TWO hexes outside the tile moved tile (1, 0)*, and *the mesher's measured reach is 2 hexes and `MESH_HALO_K` declares 1* |
| `MESH_HALO_K` declared 2, mesher unchanged | **two**: *measured reach is 1 … declares 2*, and the fixture guard *the sweep covers 1..3 and cannot measure a larger one* |
| the dirty rule stops at the tile edge | *an edit at (31, 1) does not name tile (1, 0), which holds the wall that reads it* |
| `mesh_chunk_origin` off by one hex | **eight**, led by *the tile holding (32, 1) starts at q 33, so the wall is not on its edge and no outside edit can reach it* |
| the kind list DESCENDING | *the kind list gives 5 after 9 — two builds of one map can upload its tiles in different orders* |
| the fold TRUNCATES instead of rounding | *3.0 and 3.0 − 1e-9 checksum differently — every exact palette height sits on the discontinuity* |

⚠ The blend break is the one worth dwelling on: it is plan 02's slope solver in
miniature, it changes nothing a count or a picture could see, and the gate named
the reach and the declared halo in one sentence.

⚠ Note what did NOT move: `test_one_tile_alone_equals_that_tile_out_of_a_rebuild_of_everything`
stayed green through the blend, because both sides call the same mesher.  **The
plan's stated invariant is the weakest assertion in the file**, and the reach
sweep is what carries the phase.

### ⚠ Cost is noted, not solved

`ground_chunk_mesh` walks its tile once per kind and `ground_chunk_kinds` walks
it again to find them — ~6 passes over 1024 hexes for a typical tile.  The
one-pass shape is a `Mesh` per kind held together, and `CLAUDE.md` § Loft
gotchas is why it is not written that way (a struct in a container is a copy).
⚠ The trigger to solve it is **M4's ratio**, not a hunch.

## M3 — it is drawn, and gated (2026-08-17)

`src/ground_gl.loft` + `src/gl_gate.loft` + `src/gl_gate_main.loft` +
`scripts/validate_gl.sh` + `tests/gl/*.keys` (2 fixtures) +
`tests/25_m3_the_ground_gl.loft` (10 fns).  One `graphics::GroupVboSet` **per
palette kind**, keyed by chunk; a flat-unlit shader; the colour as a uniform;
`GL_DEPTH_TEST` and `GL_CULL_FACE` on; `camera_view_projection` supplies the
matrix.  The frame is captured with `gl_screenshot`, decoded by `imaging` and
counted by **`classify_canvas` itself** — the same instrument every software
band in the repo was sized against.

**Suite 1227 → 1237.  Gate 33 scripts / 654 measurements UNCHANGED** — the
fourth phase in a row, and still the point: a mesher is not a simulation.
**New third gate: 2 fixtures / 26 measurements.**

⚠ **`imaging` joins `loft.toml` here**, with this phase as the reason —
`probe/README.md`: *"adding it to dryopea's manifest on the strength of a probe
is how an experiment becomes a commitment nobody decided to make."*

⚠ **The edit-mode view does not move.**  `render_editor_frame` keeps the
software rasteriser and all 654 measurements.  ⚠ Wiring the GL path into
`play_mode` is **not** in M3: this phase gives the ground a gate, not a window.

### ⚠⚠ 1. The probe first — R0 answered for a BLIT, not for a SHADER

`probe/m3/` was written before a line of this phase.  R0 measured
`gl_upload_canvas` + `draw_texture_at`: pixels that were already 8-bit
integers, handed to GL and blitted back.  M3 draws something else — a fragment
shader writing a **float** colour into the default framebuffer, which has its
own ways to lose a bit (`GL_DITHER` is on by default in the GL spec, an
sRGB-capable visual re-encodes on write, and float→unorm8 rounding is the
driver's business).  ⚠ One bit of drift and `other == 0` is not a gate that
can be written at all.

Measured over 691 200 pixels, on the real mesher and the real camera with
culling on: **drift 0**, `unknown` 0.  `@M026`.  ⚠ The probe carries its own
`loft.toml` with a path-dep back to dryopea, deliberately — a hand-written
triangle would have answered for a triangle nobody ships.

### ⚠⚠ 2. `other == 0` in a frame with a HORIZON — Open 5, decided

Both halves of § Open 5's tension are real, and the resolution is to refuse the
easy half:

- **The clear colour is NOT classified**, and stays magenta —
  `palette_color`'s own "unknown kind" answer, so it is outside the twelve
  buckets by construction as the palette grows.  Classify it and `@X075`'s one
  named failure mode (an erased gap draws a HOLE, and a hole shows the clear
  colour) goes silent.  `@X077`.
- **So the camera looks at nothing but ground.**  `the-ground.keys` paints
  q ±22, r ±14 against a frustum of about q ±12, r ±11 at the overview's near
  boom, and `sea == 0` is the assertion that says the paint really does outrun
  the camera.  `other == 0` then means what it says: **every pixel of that
  frame is an exact palette colour.**

⚠ The fixture and the boom are one fact stated in two files, so
`test_the_fixture_paints_past_the_frustum` projects the fixture's four corners
and requires them OUTSIDE the viewport — with the origin as its control, or
"outside the frame" would be true of a camera pointed at nothing.

### ⚠⚠ 3. THE HEADLINE: a per-kind count cannot see a MIRRORED WORLD

Pixel counts are permutation-invariant.  `render_camera.loft`'s header calls a
mirrored world the failure that *"reads as a base that is its own reflection
rather than as an error"* — so a gate built only of counts is blind to the one
defect that file is most afraid of.

⚠ **Measured, not argued.**  The mesher was mirrored in y — tops and sides
together, with the winding reversed to match, so the geometry is a clean
reflection — and the gate reported:

| measurement | mirrored world |
|---|---|
| pixels that are not a palette colour | **0** — passes |
| background pixels | **0** — passes |
| sea | **0** — passes |
| grass | 521 636 (true 520 408) — **in band, passes** |
| wall | 158 998 (true 160 118) — **in band, passes** |
| wall_high | 5 432 (true 5 489) — **in band, passes** |
| rubble | 2 297 (true 2 335) — **in band, passes** |
| **landmark `sand` at (-7, 7)** | **490.8 px away — FAILS** |

Every count passes.  Only the landmark can see it.  `@M027`.

So the gate asks WHERE two uniquely-coloured hexes landed and compares that to
**`camera_screen`**'s prediction — the same function `@M022` measured the
overview against the editor with.  That chains the drawn pixels to the camera
and the camera to the software rasteriser, which is `@X010`'s *two rasterisers
over ONE geometry* stated as something a frame can fail.  `@X078`.

### ⚠⚠ 4. A landmark must be FLAT, and the gate said so before the plan did

The first landmarks were `wall_high` and the rubble pile — the two most
distinctive things in the fixture.  The gate went red at **29 px**, and the
reason is geometry rather than a defect: a column that STANDS draws its sides
in the same colour, and those sides sit **between the top face and the screen
centre**, because a camera above the middle of the frame sees the inward face
of an off-centre column.  A 1 m pile reads 2.3 px off for the same reason; a
5 m column reads 29.

⚠ **Loosening the tolerance would have hidden that rather than measured it.**
Moved to two FLAT hexes (`sand`, `rock`) in flat surroundings, which emit no
side quad at all (`@X046`), the disagreement is **0.6 px and 0.5 px** — so the
6 px tolerance keeps ten times its headroom while a mirror is 491 px out.
⚠ `test_the_landmark_hexes_stand_on_nothing` pins the flatness in the palette,
because `.keys` has no verb for it, and
`test_a_mirrored_landmark_is_far_outside_the_tolerance` pins the headroom.

### ⚠⚠ 5. The fixture that proves the gate can FAIL

`other == 0` is what a correct frame reports — and also what an instrument that
cannot see background reports.  So `an-island.keys` draws a patch far too small
to fill the frame and asserts the opposite: `unknown` **large**, and every one
of those pixels **exactly** the clear colour.

⚠ It earned its keep in the falsification round.  Set the clear colour to a
palette colour and `the-ground` still reports *pixels that are not a palette
colour: **0*** — a perfect reading from a broken instrument — while
`an-island` says `0, wanted at least 500000`.  That is `tests/21_r1`'s finding
in a fourth place: **a gate that reads PERFECT is as suspect as one that reads
wrong.**

### ⚠ Where the GL gate runs — Open 1, decided: a SEPARATE entry

`scripts/validate_gl.sh`, not rows in `scripts/validate.sh`.  Plan 21 § R4's
literal reading puts all 33 existing scripts behind an X server, and R0's probe
went out of its way to prove the readback does not need a display.  A machine
with no xvfb still runs the gate it can, and `validate.sh`'s 654 measurements
never wait on a GL context.  `@X076`.

⚠ **The consequence is a rule**: `.keys` has no GL verb, so a fixture's
expectations live in `src/gl_gate.loft` and a fixture with no case there is
**refused by name** rather than skipped.

### ⚠ Two things the gate found in its own harness

- ⚠ **`gl_gate_all` opened a GL context before checking the fixture name
  existed**, so a typo cost a window and a full sweep.  Found by
  `test_naming_a_missing_fixture_is_a_failure`, which could not otherwise run
  at all — **a GL call inside `loft test` answers "native function not
  loaded"**, so every branch a test can reach has to come before the context.
  Fixed; it is better behaviour independently.
- ⚠ **The fixture's own `kind 7 -6 rubble` was wrong** and the runner said so:
  `kind` reads the AUTHORED ground and a pile is a LAYER over it.  The fixture
  now asserts both halves, which is the pair a mesher can fail — assert only
  the pile and a mesher that lost the surface lookup still passes.

### ⚠ Seven breaks tried, seven fired, each at its predicted value

| the break | what the gate said |
|---|---|
| `ground_gl_colour` divides by 256 | *kind 1: palette_color says 2784960, the uniform round-trips to **2784959*** — one bit, and every pixel of that kind would land in `unknown` |
| the clear colour made a palette colour | **two**: `an-island` *pixels that are not a palette colour: **0**, wanted at least 500000*, and `the-ground` *background pixels: 520408, wanted exactly 0* |
| the gate camera zoomed out to the default boom | **two**: *the boom is 80 and the fixture was painted for 20*, and *the fixture's corner (-22, -14) lands INSIDE the frame — so `other == 0` is measuring the coastline* |
| cull the FRONT face | *pixels that are not a palette colour: **587425**, wanted exactly 0* |
| the drawn world MIRRORED (tops + sides, winding matched) | every count green; *landmark sand: the camera says (218.5, 116.2) and the frame drew it at (214.3, 606.9) — **490.8 px** away* |
| colour off the PAINTED kind instead of the surface | *rubble: **0**, wanted 1000..3000* — the oldest trap in the repo, seen in a frame |
| the top-face fan mirrored WITHOUT matching the sides | *pixels that are not a palette colour: 43795* — ⚠ the impure version of the mirror, and it fires on the wrong assertion |

⚠ **That last row is a finding about the GATE, not about the code.**  The
first mirror attempt moved only the tops, so tops and sides disagreed and
opened background — and because the gate returns its FIRST failure, the
landmark check was never reached.  The counts in `gl_case_the_ground` are
RANKED, not independent, exactly as M1 found for a bundled test function.
⚠ Here that is `validate.loft`'s deliberate contract (*the first failure is the
verdict*) rather than an accident — but it means **a falsification has to be
clean, or it proves a different assertion than the one you aimed at.**

## M4 — cost (2026-08-17)

`tests/25_m4_the_mesh_budget.loft` (5 fns) + `ground_gl.loft::ground_gl_bake`.
**Suite 1237 → 1242**, gate 33 scripts / 654 measurements and the GL gate's 26
all unchanged.

### ⚠⚠ The phase's own invariant was wrong: the CLOCK could not carry it

M4 was specified as *"a RATIO inside `loft test`"*, the `tests/11_f8` shape.
Built that way it does not work here, and the reasons are all on the record:

- `CLAUDE.md` § Cost — an UNCHANGED file timed **173 / 737 / 754 ms** on three
  interpreter runs, because discarded structs are not freed;
- the box is shared — M1 read the suite at 293 s against a documented 177 s;
- ⚠⚠ **and while this file was being written, two IDENTICAL back-to-back calls
  differed by 5.4x** (1.84 s against 0.34 s), because a registry fetch timed
  out inside the first one.

A ratio survives a uniformly slow machine.  It does not survive a 1.5-second
stall landing inside one of its two halves — which is the failure mode `11_f8`
never met because both its halves are `wave_tick` and neither touches the disk.

⚠ **So the load-bearing gates count FLOATS BAKED** — the size of the buffer
`gl_upload_vertices` is handed.  Deterministic, machine-independent, and it is
what a cost regression in a mesher actually IS: geometry that should not be
there.  The clock is still read for the two coarse claims, and **no absolute
microsecond figure is asserted anywhere**.

### ⚠⚠ It is the cost gate M1 needed and did not have

M1's two invisible breaks are *pure cost* — `plans/25` § M1 says so in prose
(*"nothing — the second copy is back-facing"*, *"nothing — a sliver has no
area"*) and gates them as COUNTS on four small fixtures.  M4 prices them, and
the price is the surprise:

| the break | flat tile bakes | the world |
|---|---|---|
| shipped | 110 592 floats (**108/hex** = 6 triangles) | 1.29 s |
| `<` instead of `<=` | **331 776** — exactly **3x** | **3.2 s** |
| the guard deleted | **331 776** | 3.2 s |

⚠ A zero-area sliver at every hex boundary **triples the world's geometry** and
draws not one pixel.  Both breaks were tried and both fired at the predicted
value.

### ⚠⚠ THE FINDING: a one-hex edit re-bakes ~4 000 hexes' worth

`mesh_chunks_touched` names the edited hex's tile and its neighbours', and
every one is re-meshed WHOLE.  At a tile corner that is four tiles of 1024
hexes to move one hex — **447 048 floats, 4 139 hexes' worth**, gated in floats
so a busy machine cannot argue with it.

⚠ Swept over `MESH_CHUNK_SHIFT`, the worst-case edit and the cold build of one
world (`@M028`):

| tile | worst-case edit | cold build | tiles |
|---|---|---|---|
| 8x8 | **38.7 ms** | 1.16 s | 192 |
| 16x16 | 96.1 ms | 1.18 s | 48 |
| **32x32 (shipped until 2026-08-18)** | **334.8 ms** | 1.72 s | 16 |

⚠⚠ **The edit tracks tile AREA and the cold build barely moves** — the biggest
tile is in fact the *slowest* cold build, because a 32x32 tile at the world's
edge walks more hexes that are not drawn.  So shrinking the tile is nearly free
on the CPU side.

⚠ **It is NOT free on the GPU side** — 192 tiles instead of 16 is twelve times
the draw calls, and `GroupVboSet` has no frustum cull — and that half cannot be
measured from `loft test`.  **So M4 measures the table and changes nothing**,
which is `plans/22`'s discipline: do not optimise against an unmeasured half.

⚠ **Nothing pays this cost today**: M3 gave the ground a gate, not a window, so
`ground_gl_upload_edit` has no caller with a clock on it.  The deadlines it
*would* face are `PAINT_DEBOUNCE_US` (50 ms) for a brush stroke and the tick
(667 ms) for a play-mode terrain change — so at 32x32 an edit is **7x over** a
paint stroke and half a tick.  **That is the number the phase that wires GL
into `play_mode` inherits**, and `@M028` is where it is written down.

### ⚠⚠ SETTLED 2026-08-18 by [plan 19](../19-the-interactive-loop/README.md) P6 — the tile is now **8x8**

The inheritor named above measured the half this phase could not, in a real
window under `xvfb`: **the draw cost does not move at all.**  96 tiles draw in
the same 7 ms/frame as 8, and 24 in the same 5 ms as 4 — so *twelve times the
draw calls costs nothing measurable*, and the tile size is decided by the EDIT
alone, where 8x8 is **7 ms against 54 ms** (this world) and **7 ms against
112 ms** (a world four times the area).  ⚠ At 32x32 the edit cost GROWS with
the map; at 8x8 it is constant.  `@X096`, `@M041`.

⚠ **The cold build got faster too** (218 ms against 259), which is this
section's own finding — a big tile at the world's edge walks more hexes that
are not drawn — confirmed rather than merely predicted.

⚠⚠ **The cost was two test fixtures that encoded the number 32**, and both are
now derived from `mesh_chunk_span()` and pass at 8, 16 and 32.  `25_m2` builds
its band on a TILE BOUNDARY rather than at q = 32, and its own
`test_the_fixture_is_not_degenerate` is what said so.  `25_m4`'s two thresholds
— a literal `> 500` hexes' worth, and a `> 10x` sparse/dense ratio — were each
a statement about the world they were measured in rather than about the mesher,
and are now the tile's area and two exact counts.  ***A threshold that names the
world it was measured in refuses the world it is measuring.***

### ⚠ M2's parked one-pass rewrite: measured, and NOT worth doing

`ground_chunk_mesh` walks its tile once per kind and `ground_chunk_kinds` walks
it again, so a tile holding K kinds is walked K+1 times.  § M2 parked the
one-pass rewrite and named M4's ratio as the trigger.

**Measured: the redundant walks are 7 % of a tile's bake.**  The other 93 % is
mesh building, which a one-pass shape would still have to do.  ⚠ So the rewrite
buys at most 7 % for a structure loft makes awkward (a struct in a container is
a copy), and the test is written as a **trigger at 25 %** — K is what grows it,
so adding palette kinds is what eventually trips it.

⚠⚠ **The first version of that test priced the WRONG alternative** and it is an
easy mistake to repeat: it compared the shipped bake against `ground_chunk_kinds`
and reported *"5634 % of one pass"*.  A kind census does no mesh building, so
almost all of that number is work the rewrite could never save.  **Pricing an
alternative means pricing the DIFFERENCE, not the whole.**

### ⚠⚠ The gate had to be affordable, and the suite's watchdog said so

The first version cost **63 s** — and `loft test` hard-kills at **300 s**.  The
suite is **224 s** without it, so M4 pushed the whole run over the cliff and it
died in the parse phase of an unrelated file, which is exactly the symptom
`CLAUDE.md` documents for a cdylib build in flight.  ⚠ **The diagnosis was
wrong twice before it was right**: the registry fetches timing out in the log
looked like the cause (they were not — failing them fast changed nothing), and
the cdylib looked like the cause (every `.so` was present).

⚠ Profiled, **every one of the seven hottest paths was one test** — the one
that re-derived the whole world's bake twice to `print` two numbers that belong
in `@M028`.  Deleting it and shrinking the fixture to the four tiles the claim
actually needs took the file **63 s → 5 s** with every reading preserved.
⚠ That is `docs/PROFILING.md`'s own lesson arriving in a test file: *a test that
RE-DERIVES an expensive value a sibling already computed is the cheapest thing
to find.*

### ⚠ Two breaks tried, two fired

| the break | what the gate said |
|---|---|
| `<` instead of `<=` | **two**: *a solid tile of flat ground bakes **331 776** floats and six triangles a hex over 1024 hexes is 110 592*, and *a tile with one wall bakes 331 560, expected 110 808* |
| the guard deleted | **two**, the same pair at 331 776 — geometry nobody can see and everybody uploads |

## What this plan does NOT build

**No entity art.**  Enemies, the vehicle, the crew and the towers are
[`plan 20`](../20-entity-art/README.md), whose A5 is what this unblocks.  This
plan draws the GROUND.

**No lighting.**  Flat unlit is what the gate needs and it is what ships.  ⚠ A
lit frame is a real want and it is a real hazard — `docs/RENDERER.md` § R4:
lighting turns one palette colour into a range and `unknown` stops meaning
"fault".  The trigger for solving it is a gate that needs to tell two
same-coloured surfaces apart, and the answer is an **ID buffer**, never a
loosened lookup.

**No terrain elevation.**  The ground is flat because the simulation's ground is
flat; [`plan 02`](../02-solver-validation-viewer/README.md) is where that
changes, and § What was measured first, 1 says what has to move with it.

**No frustum cull, no LOD.**  `GroupVboSet` already stores the bounds for the
first; the second is `plans/22` § What this plan does NOT build's rule — ⚠
granularity must not follow the CAMERA.

**No retirement of the software rasteriser.**

## Open questions

1. ✅ **Where the GL gate runs — ANSWERED by M3** (`@X076`): a separate entry,
   `scripts/validate_gl.sh`, so the 33 headless scripts stay headless and a
   machine with no xvfb still runs the gate it can.  ⚠ Its consequence is a
   rule — `.keys` has no GL verb, so a fixture's expectations live in
   `src/gl_gate.loft` and a fixture with no case there is refused by name.
2. **Does a rubble heap draw as a step or a heap?**  This plan draws it as a
   step, because `can_step` treats it as one and § What was measured first, 1
   argues the picture must not disagree with the rule.  ⚠ But rubble is the one
   surface the design calls a *ramp* (*"bodies ramp a kill zone shut"*), so the
   day the SIMULATION gains a sub-hex slope, this answer changes with it — not
   before.  Not a rendering decision.
3. ✅ **What draws SEA — ANSWERED as an interim by M2** (`@X075`): the painted
   set plus a one-hex ring.  § M2 has the argument, the two rejected candidates
   and the limit that survives (an erased gap wider than two hexes still holes,
   and a test asserts it).  ⚠ The real answer is still to give water the **DROP**
   `examples/palette.json` already carries per kind (sea 0, water 1, rapids 3,
   waterfall 8) and nothing reads — which changes `hex_height` for every
   consumer, so it is a decision about the SIMULATION and belongs with
   [`plan 02`](../02-solver-validation-viewer/README.md).
5. ✅ **What `other == 0` means in a frame with a HORIZON — ANSWERED by M3**
   (`@X077`): the clear colour stays OUTSIDE the palette (magenta,
   `palette_color`'s own "unknown kind" answer) and **the gate's camera looks
   at nothing but ground** — `the-ground.keys` paints past the frustum, and
   `sea == 0` is the assertion that says so.  Classifying the background as a
   bucket was refused, because it would silence question 3's hole.  ⚠ The
   consequence lands on any FUTURE fixture: one that cannot fill the frame
   cannot use `other == 0`, and `an-island` is the worked example of what such
   a fixture asserts instead.
4. **What draws the marker layer?**  Spawn markers, targets and tower sites are
   drawn by `marker_render.loft` into the software canvas today.  ⚠ They are UI
   over the world rather than part of it, so they are not the mesher's — but the
   GL path has nothing to draw them with, and a play-mode frame with no markers
   in it is a frame the player cannot aim from.  *Recommendation: out of scope
   here, and the first row of plan 20's HUD work.*

## See also

- [`plan 21`](../21-the-renderer/README.md) — the camera, complete at R2.
- [`plan 20`](../20-entity-art/README.md) — what stands on this ground.
- [`plan 02`](../02-solver-validation-viewer/README.md) — the slope solver, and
  the trigger that widens M2's halo.
- [`plan 22`](../22-the-field-cache/README.md) — the simulation's cost, which
  this plan does not touch.
- [`docs/RENDERER.md`](../../docs/RENDERER.md) — the decisions.
