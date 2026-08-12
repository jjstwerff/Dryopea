<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# plans/ — dryopea's plan structure

dryopea organises multi-phase work the way **moros** and **loft** do, so one
convention spans every repo. This file is the **binding** — the conventions,
and where dryopea differs.

- A **reference doc** ([`docs/DESIGN.md`](../docs/DESIGN.md),
  [`docs/GROUND_TYPES.md`](../docs/GROUND_TYPES.md), …) describes **how the
  thing works** — the durable truth, updated in place as the code changes.
- A **plan** describes **a change we intend to make** — phases, ordering,
  verification. It is temporary: when a phase ships, its reference content
  **moves out** to the doc that owns it, and the plan keeps only the closure
  record.

If you cannot say what *changes* when the plan is done, it is a doc, not a
plan.

## Pick the lightest workflow that fits

| Work shape | Path |
|---|---|
| **Bug fix** (one root cause, one commit) | Fix + a test in `tests/` + commit. No plan. |
| **Upstream defect** (loft, or a library) | File it in [`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) and fix it in the owning repo. **Never a dryopea plan, never a local workaround.** |
| **dryopea-internal bug** | A `@D<NNN>` row in [`PROBLEMS.md`](../PROBLEMS.md). |
| **Content work** (a ground type, a palette entry, a map) | Nothing, or one line in the doc that owns it. |
| **Light TODO** *(the default)* | An `## Open work` row in the reference doc that owns the area. |
| **Plan** | A directory here. Earns it only when the work is genuinely **multi-phase**. Cap active plans at **2–3**. |

Most work is not a plan. A row in the doc that owns the area beats a plan
directory that only points back at that doc.

## Identity — the plan number

A plan's identity is its **zero-padded integer**, and the directory is
**flat**: `plans/<NN>-<slug>/README.md`.

- **Never renumber an existing plan.** New plans take the next unused
  integer. Numbers appear in commits and prose, so a collision is expensive
  to unwind.
- **Numbering carries no priority.** [`ROADMAP.md`](ROADMAP.md) carries the
  logical ordering.
- **No `future/` · `finished/` · `deferred/` subdirectories.** Lifecycle
  state is a **field in the plan's own `## Status` section**, not a path — a
  plan that ships should not move on disk and invalidate every link to it.

> **Where dryopea differs from moros.** moros keys plan identity to its
> GitHub **issue number** and derives the overview from `gh issue list`.
> dryopea has no issues filed and no `plan` label, so identity stays a local
> integer and the index below is hand-maintained. If dryopea starts using
> issues, switching to moros's scheme is the better end state — that is an
> open decision, not a settled difference.

## Index

Each plan's own `## Status` section is the source of truth; this table is a
pointer, not a second copy.

| Plan | Value | Effort | Lifecycle | One line |
|---|---|---|---|---|
| [`01-ground-editor`](01-ground-editor/README.md) | G | M | Active | In-game ground-type editor; E1–E4 + smoke + E1-live shipped |
| [`02-solver-validation-viewer`](02-solver-validation-viewer/README.md) | G | MH | Future | 3D solver-output viewer; painted layer + height mesh |
| [`03-marker-layer-and-spawns`](03-marker-layer-and-spawns/README.md) | G | M | Shipped (M1–M5) | Second sparse layer; multi-direction spawn points |
| [`04-map-library`](04-map-library/README.md) | G | M | Future | MapFile schema + map index + browser + content |
| [`05-validation-scenario`](05-validation-scenario/README.md) | G | M | Future | Minimum playable thing; integration spec |
| [`06-editor-stencil-pipeline`](06-editor-stencil-pipeline/README.md) | F | MH | Future | Editor-as-content-pipeline; stencil mode + mesh baker |
| [`07-shared-world-substrate`](07-shared-world-substrate/README.md) | F | H–VH | Active (W0 partial) | Go 3D; adopt the shared hex substrate |
| [`08-game-validation`](08-game-validation/README.md) | S | MH | Active (V0 next) | Scripted play, measured effects, PNGs for inspection |

Parked plans: [`DEFERRED.md`](DEFERRED.md). Roadmap entries without a plan
slot get one when their trigger fires.

## Value categories — what KIND of value

Same letters as moros and loft, so the convention reads the same across
repos. Read top-down and pick from the highest category with open work.

| Tag | Meaning | dryopea examples |
|---|---|---|
| **S** | **Silent failure / content corruption** — it "works" but the result is wrong, with no error | a renderer that draws the wrong shape and no test can see it; a map that round-trips to different bytes |
| **R** | **Regression / gate-blocker** — `scripts/test.sh` red, or a toolchain bump that breaks the build | a loft release that breaks the parse; a library migration that strands the deps |
| **G** | **Goal-enabling** — directly advances the playable game | the editor, the wave engine, the scramble loop |
| **F** | **Foundation** — unblocks 2+ downstream plans | the shared hex substrate, the map file format |
| **U** | **Player experience** — feel, readability, controls, art coherence | editor ergonomics, HUD legibility, proxy art |
| **C** | **Clean features** — removes special cases; keeps the game↔library seam honest | moving hex math out to the shared library |
| **Q** | **Internal quality** — perf, refactor, cleanup with a clear payoff | warning cleanups, test-suite speed |
| **N** | **Niche / opportunistic** — small, low-priority | one-off tools, conveniences |

**Effort letters, never calendar time** — `XS / S / M / MH / H / VH`.
"Two weeks" ships in two days and "quick" takes weeks; effort buckets stay
stable, projections don't.

## The verification rule

Every phase names a **gate** — how you see it works. dryopea has three, in
increasing order of what they can catch:

1. **Unit + round-trip tests** (`scripts/test.sh`) — exact invariants.
2. **Golden images** (`golden.loft::assert_golden`) — byte-equal renders.
   Exact, and brittle by design: any renderer change invalidates every
   golden at once.
3. **Measured frames** (plan 08) — scripted play, thresholds over classified
   pixel shares, PNGs kept for human inspection. Survives re-lighting and
   re-styling, and still catches "the thing is not drawn".

⚠ **A gate that cannot separate the things it measures is not a gate.** A
threshold over a bucket that mixes two subjects moves for reasons that have
nothing to do with the change under test — fix the instrument *before* you
trust the number over it. See plan 08 § The instrument comes first.

## See also

- [`_TEMPLATE.md`](_TEMPLATE.md) — copy this for a new plan
- [`ROADMAP.md`](ROADMAP.md) — logical-order feature list across all tiers
- [`../docs/DESIGN.md`](../docs/DESIGN.md) — master design
- [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) — 2023 seed material
