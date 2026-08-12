<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan template

Copy this file to `plans/<NN>-<slug>/README.md`. Delete the guidance blocks
(marked *(delete)*) as you fill it in. Conventions and the lightest-workflow
table: [`README.md`](README.md).

**Before you copy — is this actually a plan?** If it fits in one row of a
reference doc's `## Open work` section, it isn't. Add the row instead. A plan
earns its directory only when the work is genuinely multi-phase.

**Numbering:** the next unused integer, zero-padded. **Never renumber an
existing plan** — numbers are referenced from commits and prose, so a
collision is expensive to unwind. Numbering carries no priority;
[`ROADMAP.md`](ROADMAP.md) carries the ordering.

---

# `<NN>` — `<Plan title>`

**Value:** `<S|R|G|F|U|C|Q|N>` · **Effort:** `<XS|S|M|MH|H|VH>`

## Status (REQUIRED)

*(delete)* The **single source of truth** for what is shipped / open /
deferred / blocked in this plan. One paragraph: the state of the world today
and what this plan changes. `plans/README.md` only points here — do not keep
a second copy of the per-phase truth there, it drifts.

## Goal (REQUIRED)

*(delete)* One sentence — what ships when this plan is complete. No strategy
or advertising language.

## Anchors (REQUIRED)

*(delete)* The reference docs this plan implements or extends, and the source
files it touches. A plan never restates its anchors' content — it links.

## Invariant gate (REQUIRED for exact-invariant work)

*(delete)* Hex geometry, serialisation, rotation, round-trips and file
formats are **exact invariants, not open spaces**. State per phase: the
**concrete expected result** (the exact target output for one specific
input), the **invariant** it pins (save/load → *round-trip = identity*;
direction rotation → *six 60° steps are the identity*), and the **negative
control** — the input that must be *refused*, not silently accepted.

Say so in one line if a phase has no exact-invariant surface. Silence reads
as "gate done", not "gate N/A".

## Phases (REQUIRED if multi-phase)

*(delete)* One row per phase. **Verify** is how you *see* it works — name the
gate: a test file, a round-trip check, a measured frame, a rendered image.
"It compiles" is not a gate.

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **A** — short title | S | `scripts/test.sh` / `tests/<x>.loft` | Open |
| **B** — short title | M | round-trip fixture | Blocked on A |

## Cross-repo coordination (if any)

*(delete)* Which sibling repo or library owns what, and what "done" means on
both sides. A library API change is done when **every** consumer is green —
name them. Outbound asks go to
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md), never fixed locally.

## Open questions

*(delete)* Numbered, each with a resolution path (which phase decides it).
Delete the section if there are none.
