<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `11` — The flow field: enemies that route, not enemies that drift

**Value:** `G` (goal-enabling — Tier A "defend through some waves" needs
it) · **Effort:** `MH`

## Status

**Active — F0 is next.** Today `spawn.loft::enemy_tick` is:

```loft
et_off = hex_offset(e.heading);
e.q = e.q + et_off.0;
e.r = e.r + et_off.1;
```

One hex along a fixed heading, forever. No terrain, no walls, no core. An
enemy walks **through** a wall ring without noticing it, and
`a-wave-approaches` — dryopea's proudest scenario — currently asserts that
straight-line motion works.

This is not a refinement of that; it replaces it.
[`docs/DESIGN.md`](../../docs/DESIGN.md) § 6 already specifies the
destination: *"flow field toward the core"*, entrances that the field routes
through, and a nibble fallback when no path exists.

⚠ **Active-plan cap.** [`plans/README.md`](../README.md) caps active plans
at 2–3; with 01, 07, 09 and 10 this is the fifth. See § Sequencing — two of
those are gated, not active, and should say so.

## Goal

An enemy released at any spawn reaches the core by a route that respects
walls and terrain — or, if the perimeter is closed, gives up and nibbles the
nearest wall. Per enemy class, because `wall` stops a robot and an insect
climbs it.

## ⚠ Computed from the NEIGHBOUR RELATION, never from coordinates

**This section is the plan, and it is what keeps this work independent of
plan 09.**

A distance field is a graph property. It is correct in *any* lattice
provided it is built from the world's own neighbour function and never from
arithmetic on `q` / `r`. Write it that way and plan 09's conversion moves
every coordinate underneath it without changing a single distance.

Write it the other way and this plan reproduces moros#10 exactly. That bug
was **a distance computation in the wrong lattice** — `max(|dq|, |dr|,
|dq+dr|)` applied to odd-r coordinates — and what it broke was *"road width,
scatter reach, storey footprint and house outline… sheared blobs rather than
discs"*. Every one of those is a reach computation. So is a flow field.

**The rule, and it is greppable:** nothing under this plan may compute
adjacency, distance or reach from `q` and `r` directly. It calls `nb(...)`.
A `+ 1` on a coordinate outside the neighbour function is the bug.

The gate for that rule falls out of plan 09 for free: **the field's test
expectations must not change when the lattice converts.** If a single
expected distance moves, this rule was broken somewhere.

## The negative control already exists

Rare and worth spending. **Today's code fails the first scenario this plan
writes, by construction** — a straight-line enemy walks into a wall ring and
keeps going, so "no live enemy ever stands on a wall hex" is red before a
line of flow-field code exists.

That means the gate can be proven able to fail *before* the feature is
built, which is the thing plan 08 § The instrument comes first is about, and
it costs nothing here because the broken behaviour is already in the tree.

## The instrument is owed first

plan 08's vocabulary can say `count alive`, `range` (span from the core) and
`kind`. None of them can say **where an enemy is**, so none can express "it
went through the entrance rather than through the wall".

`range` is not enough on its own: an enemy walking *through* a wall in a
straight line and an enemy routing *around* it both show a decreasing range.
The measurement that separates them is the one this plan needs, and per plan
08's own law it lands **before** the scenario that leans on it — F1, not
somewhere in the middle.

## Sequencing

**Not gated on plan 09**, by the § Computed from the neighbour relation
rule. F0 can start now.

**Gated on nothing else.** It touches `spawn.loft` and adds a field module;
plans 07 and 10 do not overlap it. Plan 05 (validation scenario) is the
consumer — this is the mechanic its "minimum playable thing" is waiting on.

**What should give way for it:** plan 07 is blocked (needs `hex_voxel`
published) and plan 10 is gated (extract what survives 07 and 09). Neither
is *active* in any sense a reader would recognise; marking them so brings
the count back inside the cap without parking real work.

## Phases

Cut against [`plans/README.md`](../README.md) § What makes a step SAFE.

| Phase | Effort | Shape | Verify | Status |
|---|---|---|---|---|
| **F0** — probe: does an entrance need DETECTING? | XS | a probe first | DESIGN says two wall ends 1–3 apart form an entrance the field routes through. Hand-build that world, run a BFS, and look: if the shortest path already goes through the gap, entrance detection is **emergent** and is a HUD hint, not a mechanic. **Deliverable is the answer** — it decides whether F4 exists | Open |
| **F1** — the measurement: where is an enemy? | S | — | a new `.keys` assertion (`enemy <i> <q> <r>`, and `enemies clear of <kind>`) that goes RED against today's straight-line mover walking through a wall ring, and green when hand-fed a routed path. An assertion that cannot fail today is not the instrument this needs | Open |
| **F2** — the distance field | S | parallel run | on a hand-built world, every cell equals a BFS worked by hand; cells adjacent to the core read 1; **unreachable is a distinct value, not 0** — 0 means "at the core", and conflating them makes a walled-off spawn read as arrived. Negative control: a closed ring → every outside cell unreachable | Open |
| **F3** — the flow direction per cell | S | parallel run | from EVERY reachable cell in a swept world, following the arrows reaches the core in exactly `distance` steps. This catches loops and local minima, which no spot-check does | Open |
| **F4** — entrances, if F0 says they are a mechanic | S | — | *(exists only if F0's answer is "not emergent")* the field prefers the gap over the shortest wall-break | Open |
| **F5** — enemies follow the field | M | one site at a time | the maze scenario: one entrance, `enemies clear of wall` holds every tick, `range` decreases monotonically to 0. Its negative control is the code being replaced — see § The negative control already exists | Open |
| **F6** — per-class passability | M | one site at a time | same maze, one field per traversal class: the insect crosses the wall, the robot goes round, both arrive, and their **paths differ**. A per-class field that produces identical paths has not been keyed on anything | Open |
| **F7** — no path: nibble the nearest wall | S | — | closed perimeter → every enemy targets a wall hex, and the scenario asserts WHICH hex, exactly. Not "some wall" | Open |
| **F8** — recompute on edit | M | parallel run | after a sequence of paint edits, the incrementally-updated field equals a from-scratch rebuild, cell for cell. `gridmesh`'s dirty set is the mechanism; this is the phase that proves it was used correctly | Open |

⚠ **No phase is `H`.** F5 and F6 are the largest and both are "one site at a
time" with a scenario each.

### F2 — the trap in the sea-default world

dryopea stores only painted hexes; an unpainted hex **is** sea. A distance
field over a sparse world therefore has three states, not two — *at the
core* (0), *n steps away*, and *no route* — and the natural loft default for
an absent entry is 0, which is the first of them.

That is the same class of defect as `EditorInput`'s `-1`-becomes-`0`
sentinel (plan 08 § Neutral must be the zero value): the neutral value
collides with a meaningful one. **Unreachable must not be 0**, and F2's
negative control — a closed ring where every outside cell is unreachable —
is precisely the test that would catch it.

### F8 — and why it is last

A field that is correct only when rebuilt from scratch is still a correct
field; a field that is incrementally wrong is a game that routes enemies
through a wall the player just built. So the incremental path lands **after**
the from-scratch one is proven, and its gate is equality with it, not
plausibility.

## Invariant gate

| Phase | Concrete expected result | Invariant pinned | Negative control |
|---|---|---|---|
| **F1** | the new assertion goes red against today's mover | the instrument can see the failure it exists for | an assertion green on a wall-walker measures nothing |
| **F2** | closed ring → outside cells unreachable | unreachable ≠ 0 ≠ at-the-core | a walled-off spawn reading 0 = "already arrived" |
| **F3** | arrows reach the core in exactly `distance` steps, from every reachable cell | the field has no loop and no local minimum | one cell whose arrow increases distance is a permanent enemy stall |
| **F5** | `enemies clear of wall` every tick | routing respects the world | today's code fails this — the gate is proven before the feature exists |
| **F6** | insect and robot paths DIFFER on one map | passability is per class | identical paths mean the class key is ignored |
| **F7** | the exact wall hex, named | the fallback is deterministic | "some wall" is not repeatable, so a run cannot assert it |
| **F8** | incremental field == from-scratch field | the dirty set is used correctly | equal-but-stale after an edit is the bug this catches |
| **all** | test expectations survive plan 09 unchanged | the field is built from `nb()`, not from `q`/`r` | one expected distance moving = moros#10, again |

## Open questions

1. **One field per class, or one field with per-class costs?** F6 assumes
   per-class fields (simple, cacheable, N small). Costs would be one field
   with class-weighted edges — cheaper in memory, harder to reason about.
   F0 should answer it while it has the BFS in front of it.
2. **Does the field live on the map or beside it?** It is *derived*, so it
   should be recomputed, not saved — the reasoning is in
   [`plans/07`](../07-shared-world-substrate/README.md) § Evaluated. If it
   ever does need a home, `hex_field::Labels` is a per-cell integer field
   already, and dryopea should not invent a second one.
3. **What replaces the spawn heading?** A spawn marker carries a direction
   (0..5) that today *is* the enemy's whole movement. Once the field routes,
   the heading is either the first step (redundant) or a genuine
   "approach from this side" constraint. DESIGN § 6 says approach heading;
   decide before F5 whether the field is seeded per-spawn or shared.
4. **Boss 2×2 footprint** — DESIGN says the boss *"forces gaps or breaks"*.
   A 2-hex-wide unit cannot use a 1-hex-wide field. Out of scope here, but
   F6's per-class shape is where it will have to fit.

## See also

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 6 (walls, entrances,
  climbability) and § 7 (targeting priority, nibble) — the spec.
- [`plans/05-validation-scenario`](../05-validation-scenario/README.md) —
  the consumer; "defend through some waves" needs this mechanic.
- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  instrument F1 extends, and the gate every F-phase asserts through.
- [`plans/09-lattice-conversion`](../09-lattice-conversion/README.md) — not
  a dependency, deliberately; § Computed from the neighbour relation is why.
