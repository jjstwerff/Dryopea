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
| [`07-shared-world-substrate`](07-shared-world-substrate/README.md) | F | H–VH | **Blocked** (needs `hex_voxel` published; W0c cut out to plan 09) | Go 3D; adopt the shared hex substrate |
| [`08-game-validation`](08-game-validation/README.md) | S | MH | Complete (V0-V4) | Scripted play, measured effects, PNGs for inspection |
| [`09-lattice-conversion`](09-lattice-conversion/README.md) | F | MH | Active (C0 + I0 shipped) | dryopea moves to pointy-top odd-r offset (+ adopts `input`), checked against `hex_grid` |
| [`10-extract-local-libraries`](10-extract-local-libraries/README.md) | C | MH | **Gated** (extract what survives 07 + 09) | The code dryopea owns becomes published libraries — descriptive names, never a brand |
| [`11-flow-field`](11-flow-field/README.md) | G | MH | **Complete** (F0-F8) | Enemies route round walls to the core, per class, spread rather than stack, and besiege a sealed perimeter |

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

## What makes a step SAFE — and it is not how few lines it is

Adopted from moros, which paid for it: two steps of equal effort an hour
apart, one green at every moment, the other reverted whole.

> **A step should be as small as possible while STILL BEING VALIDATED — and
> those are two bounds, not one.**
>
> **Upper bound (safety).** A step is safe when the OLD path and the NEW one
> can both run at once and be COMPARED exactly. If the only way to see
> whether it worked is to swap and look, it is too big.
>
> **Lower bound (validity).** A step must be able to **go red on its own,
> for a real reason.** If the only way to test it is to also do the next
> step, they are ONE step and dividing them buys nothing but a green tick on
> an empty claim.

Two questions when cutting a phase, and a step has to pass both:

1. *At the moment this step is half done, what exactly am I comparing
   against?* If the answer is "nothing, I look at it afterwards", the step is
   **too big** — one big step wearing a small step's effort letter, whose
   failure mode is `git revert`.
2. *What test would go red if I did this step wrong?* If the honest answer is
   "none until the next step lands", the step is **too small** — merge it
   forward.

⚠ **A step that ends with something built and called by nobody cannot fail.**
Splitting "add the function" from "call it" manufactures that state on
purpose. If the first half cannot go red, it was never a step.

⚠ **A self-test is not validation.** "The key table exists and every key maps
to one action" is a claim about the table, checked against the table — it
cannot be surprised. The discriminator is not *is there an assert*, it is
*could this assert ever be surprised*.

**Three shapes that pass:**

- **Parallel run.** Build the new thing beside the old, compare exactly
  (bytes, a count, a histogram), *then* delete the old.
- **A probe first.** An `XS` step whose only job is to try to falsify the
  design before anything is built on it. The `fill_triangle` diagnosis was
  exactly this: two triangles side by side, one library call and one
  reordered, for the cost of a compile.
- **One site at a time, each with its own comparison.** "Wire four callers"
  is four steps, and each wants the same gate: *the old call and the new call
  leave the same world.*

⚠ **The comparison is the step; the edit is the easy part.**

### The two mechanical checks, when a plan STARTS

A phase is mis-cut in two ways a reader can see without judgement:

| it fails on | because |
|---|---|
| an open phase with an **empty Verify** | nothing about that step could go red — the lower bound |
| an open phase at **`H`/`VH`** | too big to have a half-done state with anything exact to compare against — the upper bound |

⚠ **One moment, not every run.** A design may be anything until it becomes
work — a sketch, a paragraph, half-formed rows. Demanding cut steps of every
idea is how a rule becomes something people route around. And these two
checks are the *mechanical* half only: whether a `Verify` cell names a **real
comparison** is judgement, and no checklist has it.

## See also

- [`_TEMPLATE.md`](_TEMPLATE.md) — copy this for a new plan
- [`ROADMAP.md`](ROADMAP.md) — logical-order feature list across all tiers
- [`../docs/DESIGN.md`](../docs/DESIGN.md) — master design
- [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) — 2023 seed material
