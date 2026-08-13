<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `10` — Extract what dryopea owns into reusable libraries

**Value:** `C` (clean features — keeps the game↔library seam honest) ·
**Effort:** `MH`

## Status

**Gated — E0 is startable, the rest is not.** Plans 07 and 09 decide which
candidates survive (§ Extract what SURVIVES), so everything past the E0 probe
waits on them.  **Plan 09 completed on 2026-08-13** — half the gate is
released, and its verdict was that `world.loft` extracts to nothing:
`hex_grid` already is it.  Plan 07 still gates the rest.  Listed as Gated
rather than Active so the active cap in
[`plans/README.md`](../README.md) reflects what is really moving.

dryopea has taken the reuse rule in one direction only: consume what exists.
This plan takes the other direction — the code dryopea wrote that is **not**
game-specific becomes a published library, so the next project does not
write it a third time.

## Goal

Everything dryopea holds locally that another project would want is a
package in the registry, under a **descriptive** name, consumed by dryopea
through the same registry path as `graphics` and `gridmesh` — with no
dryopea-shaped edges left in its API.

## Naming: descriptive, never a brand

`hex_voxel`'s header states the principle, and this plan adopts it verbatim:

> *"The name is not a claim on Moros: `LAVITION.md`'s naming principle puts
> the brand in **org and repo names** and keeps `use X;` **descriptive**."*

So: **no `moros_*`, and equally no `dryopea_*`.** A `use` line names what
the library *does*. The registry's own set is the model — `graphics`,
`gridmesh`, `imaging`, `input`, `shapes`, `hex_grid`.

⚠ This cuts both ways and is worth saying once: the brand-named libraries
that already exist (`moros_map`, `moros_render`, `moros_sim`,
`moros_editor`, `lavition_ui`) are **out of scope here** — they live in
another repo with its own flow. Named only because the same principle
applies to them, and renaming-on-publication is that repo's call, not
dryopea's. See § Open questions.

## ⚠ Extract what SURVIVES, not what exists today

**This section is the plan.** Half of dryopea's `src/` is scheduled for
deletion or replacement by plans 07 and 09:

| module | fate |
|---|---|
| `world.loft` | → `hex_grid` (plan 09 C1–C6) |
| `painted.loft` | → `hex_field`'s `HexSet` + `Labels` (plan 07) |
| `camera.loft`'s `InputState` | → `input` (plan 09 I1) |
| `save.loft` / `map_file.loft` / `marker_file.loft` | → `hex_voxel`'s `.hxw` (plan 07 W1) |
| `markers.loft` | → the placed-item layer (plan 07 § Evaluated) |
| `chunks.loft` | already `gridmesh` |

Packaging any of those is packaging a corpse: the work lands, and then the
adoption plan deletes it. **Only what dryopea still owns after 07 and 09
land is a candidate here** — which is a short and much better list.

Equally, a phase here must not become a reason to *keep* something the
adoption plans should remove. If a candidate turns out to duplicate a
library, the answer is to adopt that library, not to publish a rival.

## The candidates

Ranked by how clean the extraction is, which is what the phase order follows.

| # | today | what the library is | proposed name |
|---|---|---|---|
| 1 | `golden.loft` | assert a Canvas render byte-equal to a committed PNG; write the actual beside it on failure | `goldenpng` |
| 2 | `measure.loft` | classify a canvas into palette buckets by EXACT lookup; a non-palette pixel is a fault, not a nearest match | `pixelcensus` |
| 3 | `script.loft` (828 lines) | a scripted-run runner: tokenise, dispatch, measurements that assert with bands, the run record, the transcript-is-the-verdict discipline | `keyscript` |
| 4 | `history.loft` (490 lines) | linear undo/redo with a cursor, coalesced strokes and a truncating redo branch | `undostack` |

**Stays local, deliberately:** `spawn.loft` (a tower-defence wave engine is
the game), `editor_step` / `editor_view` (dryopea's editor is dryopea's),
`picker` / `hud` (its UI), `validate.loft` (a gate over dryopea's own
scripts — though it thins to nothing once `keyscript` carries the sweep).

**Nothing in the registry covers 1–4.** Checked 2026-08-12: no library
mentions undo/redo outside `graphics`' key-code comments, and none
classifies an image against a palette. These are genuinely dryopea's to
give.

## Phases

Cut against [`plans/README.md`](../README.md) § What makes a step SAFE.

| Phase | Effort | Shape | Verify | Status |
|---|---|---|---|---|
| **E0** — probe: does a 4-target library even build from here? | XS | a probe first | package the SMALLEST candidate (`goldenpng`) and run the loft-ship parity gate — interpret · native · wasm · html. **Deliverable is the answer**: native is broken for dryopea's own shapes today ([loft#866](https://github.com/loft-lang/loft/issues/866)), so if a Canvas library cannot pass native, every later phase's gate changes before it is built | Open |
| **E1** — `goldenpng` | S | parallel run | dryopea consumes the package and its 16 golden tests pass **unchanged**, byte-for-byte against the same committed PNGs; then `src/golden.loft` is deleted | Open |
| **E2** — `pixelcensus` | S | parallel run | plan 08's 24 measurement tests pass unchanged — including the eleven-entry separation sweep and the off-palette-pixel fault; `frame` reports the same shares to 6 decimals on the same scenes | Open |
| **E3** — `undostack` | M | one site at a time | the generic core takes an OPAQUE delta; dryopea's `PaintedDelta` / `MarkerDelta` become its payload. Per site: the same action sequence leaves the same world, and the 50-deep truncation + stroke-coalescing tests pass unchanged | Open |
| **E4** — `keyscript` | MH | one site at a time | the five scenarios play identically — same commands, same frames, same measurement count (233 over 14 scripts as of 2026-08-12 — read it, do not trust this number) — through the extracted runner driving dryopea via an injected step function; `do levitate` still errors, `do Tab` still fails | Open |

⚠ **Each Verify is "the existing tests pass unchanged".** That is the whole
safety argument: an extraction that needs its tests edited is not an
extraction, it is a rewrite wearing one's clothes. The tests move to the
library, but what they assert does not change — and dryopea keeps consuming
them from outside, which is the only way to notice that the API still fits a
real caller.

## What makes 3 and 4 hard, stated up front

`golden.loft` and `measure.loft` are near-pure already. The other two are
tangled, and the tangle IS the design work:

- **`keyscript`** reaches into dryopea at ~12 points: `editor_step`,
  `render_editor_frame`, `painted_count`, `lookup_painted`,
  `palette_index_of`, `classify_world`, `spawn_wave`, `wave_tick`. The
  reusable core is the *vocabulary and discipline* — commands name actions,
  an unknown command is an error, a measurement asserts inline and prints
  either way, the transcript is the verdict. The library needs a **driver
  contract** (step · render · measure) that dryopea implements; inventing
  that contract is E4, not moving the file.
- **`undostack`** is typed to dryopea's deltas throughout. Generic means an
  opaque payload, which loft's type system will have opinions about — E3
  starts by finding out which.

## Invariant gate

| Phase | Concrete expected result | Invariant pinned | Negative control |
|---|---|---|---|
| **E0** | the smallest package passes all four targets | a library dryopea publishes works everywhere, not just where dryopea runs | a package that only passes on the interpreter is not a library, it is a local file with a manifest |
| **E1** | 16 goldens pass byte-for-byte, unedited | extraction changes packaging, never behaviour | a golden that needed rebaselining means the render moved |
| **E2** | `frame` reports identical shares to 6 dp | the classifier is still exact, not nearest-colour | one blended pixel silently absorbed = the fault detection was lost in the move |
| **E3** | same actions → same world; 50-deep truncation holds | the payload became opaque without the semantics moving | an undo that restores a different state means the delta lost a field |
| **E4** | the scenarios, same measurement count | the driver contract fits a real caller | a scenario needing an edited `.keys` file means the vocabulary changed under it |

## Open questions

1. **Where do these live?** `graphics` and `gridmesh` share
   `loft-libs-graphics`. A test/measurement family (`goldenpng`,
   `pixelcensus`) and a harness family (`keyscript`, `undostack`) are not
   obviously that repo's subject. Decide before E1 publishes anything —
   a package that moves repos after release is expensive.
2. **Do the brand-named libraries get renamed?** `moros_map` /
   `moros_render` / `moros_sim` / `moros_editor` / `lavition_ui` all break
   the naming principle their own sibling states. dryopea is a consumer of
   the first three and has an interest, but the call belongs to that repo.
   Raise it there; do not act on it from here.
3. **Does `validate.loft` survive `keyscript`?** If the library carries the
   directory sweep and the verdict, dryopea's gate becomes a `.sh` and a
   config. Fold it into E4 rather than publishing a fifth package.
4. **Is `hex_voxel` published by then?** If plan 07's W0x lands first,
   `save.loft` leaves before this plan looks at it — one fewer candidate,
   correctly.

## See also

- [`plans/07-shared-world-substrate`](../07-shared-world-substrate/README.md)
  and [`plans/09-lattice-conversion`](../09-lattice-conversion/README.md) —
  the adoption side; between them they decide which candidates here survive.
- [`plans/08-game-validation`](../08-game-validation/README.md) — where
  `goldenpng`, `pixelcensus` and `keyscript` all come from, and whose tests
  are every phase's gate.
- `CLAUDE.md` § Loft consumer relationship — the reuse rule this plan is the
  other half of.
