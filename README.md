<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea

A non-standard sci-fi tower-defence built on
[loft](https://github.com/jjstwerff/loft).

Status: **pre-alpha, under active implementation.** There is a working
hex editor (paint, markers, undo, save/load), a wave engine that routes
enemies round walls per class, and combat that resolves — towers fire,
enemies die and leave bodies, walls take damage and break into rubble.
No player vehicle yet, and no game loop around the base.

The design is canonical: [`docs/DESIGN.md`](docs/DESIGN.md); what
actually exists is [`plans/README.md`](plans/README.md), where each
plan's own Status is the source of truth.

## What sets it apart — the scramble phase

At the start of each base the player places a **core building** —
the thing the enemies attack and the player must defend. The player
rides a **semi-floating vehicle** and rather than placing structures
directly, issues **build orders** that NPC workers construct over
time.

A base is **not** win-or-lose-forever. When it's about to be overrun,
the player can **scramble**: fire a rocket out of the core building,
evacuating key components — each evacuation **disables the tower it
came from**, so grabbing salvage *hastens* the overrun. Hold longer
for more salvage; launch now to keep what you already have. That
tension is the core decision of the whole game.

Evacuated components give an advantage at the **next** base. A run
is a sequence of bases, chained by what you carry out — a roguelike
structure on top of a tower-defence base loop.

The core itself is **invulnerable**; enemies that reach it nibble the
player's **point wallet** instead, and a wallet at zero is what ends
the run. A successful scramble is a tactical retreat, not a defeat.

## What kind of game it is

A **strategy game built out of tower-defence mechanisms** — and one in
which the player cannot lean back the way a normal tower defence lets
them.

In a classic tower defence you spend, place, and watch. Here almost
every advantage has to be collected in person, at a moment when
collecting it costs something. A tower's shot budget decays per *shot*
and only a player standing at it can refill it. Bodies pile into a ramp
that shuts your own kill zone, so someone has to drive into it and
clear them. Salvage rots, so it has to be collected early — which is
the worst moment to be out there.

That constraint is what buys the variety: because so many of the
counters are **architectural** — where the walls went, how wide the
funnel is, what a tower can see from a ridge — one rule set poses
genuinely different problems from map to map.

## Why on loft

dryopea is the first **real consumer** of two upstream library
arcs, and so doubles as their acceptance test:

- **Terrain height-map** — slope-based height-field generation
  (will live in a shared world / graphics library).
- **gridmesh Phase C** — per-chunk meshing + dirty incremental
  re-mesh.

Both currently sit as design docs in the loft tracker; loft is
expected to drop outside-project references soon (language /
runtime bug fixes only) and the upstream library plans will
migrate to their own homes. Until then, links into the loft repo
are temporary placeholders.

dryopea's authoring cost is small (a handful of materials + one
drainage seed), so it exercises the **full** pipeline (per-chunk
meshing, frustum culling, LOD, slope faces, 3rd-person GL camera) at
low content cost.

## Repo layout

```
docs/                 — design + design-history references
  DESIGN.md           — canonical @PLAN46 design (mirrors loft tracker)
  DESIGN_HISTORY.md   — design seeds salvaged from the 2023 prototype
plans/                — phased work tracking, loft-style
  current/            — active phases
  future/             — drafted but not yet started
  finished/           — closed phases
  README.md           — index + workflow
  DEFERRED.md         — items parked behind a trigger
examples/             — sample input data (terrain palette, map.png/.xcf)
archive/              — preserved 2023 prototype artefacts (proto-loft .gcp,
                        partial world.loft, gameplay/terrain data)
src/                  — game source (empty until D0 starts)
loft.toml             — package manifest
```

## Moddability

dryopea is built for **immediate modding** — open source,
text-format data files, and a runtime config that holds every
tunable.  A developer can change tower fire rate or damage by
editing one line in `numbers.json` and re-launching.  A player
can author a new starter map in the **in-game editor** and
share the resulting JSON.  Both are intended first-day
behaviours, not aspirational late-game features.  See
[`docs/DESIGN.md`](docs/DESIGN.md) § "Moddability is a
first-class principle" for the full design stance.

## For library consumers (bumperplane et al.)

Dryopea exposes a curated subset of its internals as a loft
library for downstream consumers — primarily the
[@PLAN50 bumper-airplanes audience demo](https://github.com/jjstwerff/loft/tree/main/doc/claude/plans/50-bumper-airplanes)
which reads dryopea MapFile JSON + the palette and extrudes the
painted world into a 3D scene.  Other consumers (map-loading
tools, validation viewers, future audience demos) plug in the
same way.

**Entry point.**  `src/dryopea_core.loft` re-exports the
data + persistence + content layers.  Consumers add dryopea as
a path-dep in their `loft.toml` and write `use dryopea_core;`:

```toml
# Consumer's loft.toml
[dependencies]
dryopea = { path = "../dryopea" }
```

```loft
use dryopea_core;

// Loads ground hexes (the painted layer + camera state).
loaded = load_map_or_empty("starter_01.json", load_palette("palette.json"));
pw  = loaded.0;
cam = loaded.1;
// Loads spawn + target markers from the sidecar.
mw  = load_markers_or_empty("starter_01_markers.json");
```

**What's excluded from `dryopea_core`** (and why):
- `render`, `picker`, `marker_render`, `hud`, `editor_mode`,
  `golden` — Canvas-based 2D editor visuals; 3D consumers
  ship their own renderer.
- `history` — editor undo/redo state machine, not a
  runtime concern.
- `spawn` — dryopea's wave engine + enemy approach-mode
  tick; consumers with their own runtime (bumperplane's
  plane physics) don't need it.  Available via
  `use spawn;` directly if they do.
- `main.loft` — the GL editor binary entry point.

**Stability contract.**  The shapes below are the
cross-consumer commitments; dryopea won't break these without
a major-version bump:
- `MapFile { version, name, cam_q, cam_r, cam_zoom, ground }`
  + `GroundEntry { q, r, kind }` — the painted-layer save shape.
- `MarkerFile { version, name, markers }` + `MarkerSaveEntry
  { q, r, kind, direction }` — the marker sidecar save shape.
- `MARKER_KIND_SPAWN = 0` / `MARKER_KIND_TARGET = 1` —
  append-only, never reordered.
- `GroundType.extrusion_kind: text` ∈ `{"flat", "ramp",
  "pillar", "cliff"}` + `GroundType.height_override: float`
  — the 3D-extrusion contract.  See
  [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) § Extrusion
  mapping.

Loft-side outstanding bug workarounds dryopea carries (e.g.
`MarkerSaveEntry` is integer-widened on disk vs. u8 in-memory)
are listed in [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md);
they're invisible to consumers via the contract above.

## License

LGPL-3.0-or-later — see [LICENSE](LICENSE).
