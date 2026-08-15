<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 22 — The field: cache it, bound it, stop sweeping the world

**Value:** `C` · **Effort:** `MH`

## Status

**Designed, nothing built** (2026-08-15).  S0 is next and it is a measurement,
not a change — it decides the ORDER of everything after it.

⚠ **Every phase here is EXACT.**  Nothing in this plan approximates anything:
the field it produces is cell-for-cell the field the from-scratch sweep
produces, and the gate is that equality plus the 520 measurements.  Simulation
LOD — cohorts, coarser granularity away from the player — is a **different**
plan with a different risk profile; § What this plan does NOT build says why it
is not this one.

## Goal

`wave_tick` stops rebuilding the whole world's distance field every tick.

## The four measured facts this rests on

1. ⚠ **`flow_sweep` and the lookups under it are ~75 % of the suite's
   interpreted time** — re-profiled 2026-08-15 (`docs/PROFILING.md`).  It is the
   single largest cost in the project by a wide margin.
2. ⚠ **The sweep is UNBOUNDED.**  `flow_sweep` runs `while len(fb_frontier) > 0`
   over every walkable hex reachable from the core, capped only by
   `FLOW_MAX_CELLS = 1048576`.
3. ⚠ **It is only READ inside the 25-hex scrambler bubble.**  Outside it
   `enemy_tick` takes the heading branch and never asks the field
   (`spawn.loft` § Which mode?).  In a radius-40 world the bubble is ~1 951
   hexes against ~4 921 — **about 60 % of every sweep is computed and never
   looked at**, and under exploration that fraction goes to nearly 100 %.
4. ⚠ **The field is a PURE FUNCTION of `(pal, pw, hl, climb, core)`** and
   nothing else — not occupancy, not the roster's positions.  During a run
   `core` never moves and `climb` is fixed per class, so the only inputs that
   can change are the painted world and the height layer.

⚠⚠ **And the field's useful domain is FIXED for a whole run**, because the
bubble is centred on the CORE.  It does not follow the player, it does not
follow the camera, and it does not move.  That is what makes "cache it" the
right first instinct rather than a windowing problem.

## The invalidation surface is TWO functions wide

This is what makes caching safe rather than hopeful:

| layer | every mutator | callers during play |
|---|---|---|
| `HeightLayer` | `height_raise`, `height_clear` — **and that is all** | a body (`wave_deaths`), a break (`break_structure`), salvage (`vehicle_salvage`, via a negative rise) |
| `PaintedWorld` | `paint`, and `paint_line` which calls it | the editor — ⚠ **which still runs during play** (plan 19 P3 hands every frame to `editor_step` first) |

⚠ **So an epoch counter belongs INSIDE those mutators, never at their call
sites.**  A mutation that forgets to bump is silent and catastrophic — the field
goes stale and enemies route through a wall that broke, which is exactly the
failure `CLAUDE.md` names when it says the incremental rebuild is deliberately
not built.  Structural placement is what makes "did we get them all" a question
about two functions instead of about seven call sites.

## Anchors

- `src/flow.loft` — `flow_sweep` / `flow_build` / `flow_desire`, and
  `FLOW_MAX_CELLS`.
- `src/spawn.loft` — `wave_tick`'s *"from scratch every tick, deliberately"*,
  `wave_fields`, `wave_desire` (already lazy — `wave_needs_desire`),
  `SCRAMBLER_BUBBLE_RADIUS`, `enemy_in_bubble`.
- `src/height.loft`, `src/painted.loft` — the two invalidation surfaces.
- `tests/11_f8_the_tick_budget.loft` — ⚠ **the gate, already written**:
  `test_the_field_a_tick_uses_equals_a_fresh_build` edits the world mid-run
  (paints a wall, raises a pile) and requires a held field to agree cell-for-cell
  with a fresh one.  Its own comment says it is green today *"because there is
  nothing to go stale, and that is the property"*.
- [`docs/PROFILING.md`](../../docs/PROFILING.md) — ⚠ re-profile and quote the
  date; the reading in this repo has inverted once already.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **S0** | numbers: sweeps per tick, cells per sweep, cells actually READ, and **what fraction of ticks change the field at all** | none — it is an instrument | ⚠ **a cost gate over a world where the optimisation cannot engage is not a gate** — `11_f8` once ticked a MARKERLESS world and would have stayed green through any line-of-sight cost whatever.  S0's world must be one where sweeps dominate |
| **S1** | every mutation of `hl` or `pw` moves an epoch; every READ moves nothing | the epoch is bumped INSIDE the mutator | ⚠ add a mutator without bumping and the suite must go red — so the test enumerates the mutators and asserts each one moves it, rather than asserting "the epoch works" |
| **S2** | the cached field equals a fresh build, cell for cell, across a mid-run wall break and a mid-run pile | the field is a pure function of its epoch'd inputs | ⚠ **`11_f8`'s existing equality test IS this control** and it is already green — which means it is currently proving nothing.  S2 is the phase that gives it teeth |
| **S3** | a roster-bounded sweep agrees with the unbounded one on **every cell it emits**, and every in-bubble enemy has a cell | bound by **PATH** distance (the ring index), never straight-line | ⚠⚠ the bubble is a STRAIGHT-LINE distance and a route can be far longer — an enemy 20 hexes away round a lake has a 60-hex route.  Bound by radius and that enemy silently loses its route and besieges instead.  The control is exactly that map |
| **S4** | most deaths do **not** invalidate | invalidate on EFFECT, not on EVENT: the field changes only if an EDGE flipped | ⚠ a body that raises a hex 0.5 m under a 2.0 m climb flips no edge and changes no distance — treating "the layer was written" as "the field is stale" is correct but throws the win away |
| **S5** | *(deferred)* | — | — |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **S0** — the instrument: where does the sweep actually go? | S | extend `tests/11_f8_the_tick_budget.loft` with a world where sweeps dominate, and print/assert: cells swept vs cells read, and the **dirty-tick fraction** over a real scenario.  ⚠ Nothing is optimised in this phase | **Next** |
| **S1** — the epoch | XS | `tests/22_s1_the_epoch.loft` — each of `height_raise` / `height_clear` / `paint` / `paint_line` moves it; readers do not | Blocked on S0 |
| **S2** — the cache | S | `11_f8`'s existing equality test, which stops being vacuous + the whole gate: **520 measurements unchanged** | Blocked on S1 |
| **S3** — the roster bound | M | `tests/22_s3_the_bound.loft` — equality with the unbounded field on every emitted cell; the long-route map as the control; 520 unchanged | Blocked on S0 |
| **S4** — invalidate on EFFECT | S | `tests/22_s4_the_edge_test.loft` — a sub-climb pile does not invalidate; a pile that crosses the climb does; equality holds either way | Blocked on S2 |
| **S5** — local repair | — | ⚠ **Deferred, with a trigger** — see below | Deferred |

### Why the order is this order

**S0 first, and it changes nothing.**  ⚠ The two big mechanisms here win in
*different* worlds and S0 says which: the cache (S2) wins when most ticks are
clean, and the bound (S3) wins when the sweep is large relative to the roster.
Building both blind is how a plan optimises the half that was not the problem —
and `CLAUDE.md` § Profiling is emphatic that the reading here has already
inverted once.

⚠ **S0 also has to answer the awkward question honestly**: during an active wave
a tower kills roughly every tick, and every death raises a hex.  If that means
every tick is dirty, the cache is worth little **exactly when the game is
busiest** — which is when it matters.  That is what S4 exists for, and S0 is
what says whether S4 is optional or essential.

**S1 before S2** because the cache is only as sound as its invalidation, and the
epoch is ten lines.

**S3 independent of S2** — they compose but neither needs the other, so whichever
S0 favours goes first.

**S4 after S2** because it is a refinement of the invalidation, and it is only
worth measuring once there is a cache to keep alive.

## What this plan does NOT build

**No LOD, no cohorts, no coarser simulation.**  ⚠ That is a *different* subject
with a *different* risk profile: this plan is exact and its gate is equality,
while LOD's gate is "the outcome is unchanged", which is a much weaker statement
that has to be defended per interaction.  ⚠ It is also aimed at a different cost:
this plan attacks the FIELD (which is O(world) and view-independent); LOD attacks
the ROSTER (which is O(enemies)).  **Trigger for opening it: S0 showing the
per-enemy work dominating after S2/S3 land, or a world big enough that the
roster grows past the authored wave list.**

⚠ **And when it is opened, one constraint from the design discussion must go in
its first paragraph: granularity must NOT follow the camera.**  If it does, where
the player looks changes the outcome — the most direct possible violation of
*"the end result should still be the same"*, and unfalsifiable from inside
because there is no run to compare against.  The boundary is the interaction
radii (tower range 15, bubble 25, nibble reach 1, salvage reach 1, blocker =
same hex), which are stable under camera movement.

**No incremental repair** — that is S5, and it stays deferred for the reason
`CLAUDE.md` already gives: *"an incrementally wrong field routes enemies through
a wall the player just built"*.  ⚠ **Trigger: S0's numbers still red after S2,
S3 and S4.**  Its equality gate is already written and green
(`11_f8::test_the_field_a_tick_uses_equals_a_fresh_build`), so the phase that
builds it starts with its own falsifier in hand — which is unusual and worth not
squandering.

**No change to `wave_targets` or the measurement paths.**  ⚠ They build their own
fields **on purpose** — `spawn.loft` says why: *"a measurement runs between
ticks, and one that reused a stale field could report a siege the world had
already ended"*.  A cache that leaks into them turns a measurement into a
photograph of the previous tick.

## The trap that would cost the whole win, silently

⚠⚠ **A cache that COPIES on read is not a cache.**  `CLAUDE.md` § Cost:
*"NEVER bind a `FlowField` (or any struct with a big hash) to a local in a
per-enemy path — a whole-value bind COPIES the heap value"*, and an accessor
that returned the field once did it per enemy per lookup, at **2250×** the cost
of reading it in place.  That defect was live for four phases across 490 green
tests, because **a copy changes no behaviour, only cost.**

So the cached fields live on `WaveState` and are passed into `const` parameters,
exactly as `wave_fields`' result is today.  ⚠ There is deliberately no accessor
that returns one, and this plan must not add the first.  `11_f8`'s **ratio**
gate is the only thing in the repo that can see this happen.

## Open questions

1. **Does the cache survive a save/load?**  A `FlowField` is derived state and
   `map_file.loft` saves none of it.  *Decision: derived state is never saved —
   the epoch starts at zero on load and the first tick rebuilds.  Recorded so
   nobody adds it to the save record to "warm" the cache.*
2. **One epoch or two?**  `hl` and `pw` change at very different rates during
   play (the height layer constantly, the painted world almost never).
   *Recommendation: two, summed into the cache key — a paint should not
   invalidate what only a pile could have changed, and S4's edge test applies to
   the height half only.*  S1 decides.
3. **What is the margin ring on the roster bound?**  `flow_steps` reads an
   enemy's neighbours, and enemies move before the next rebuild.  *Recommendation:
   one ring for the neighbour read plus the fastest class's per-tick hexes;
   derived from `climb_limit`'s roster the way `play_steer_reach` derives its
   reach from the vehicle, never a constant.*  S3 decides.

## See also

- [`plans/11`](../11-flow-field/README.md) § F8 — where the incremental rebuild
  was measured, deferred, and had its gate written anyway.
- [`docs/PROFILING.md`](../../docs/PROFILING.md) — ⚠ re-profile before
  optimising, and quote the date.
- [`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) — the two steering
  modes, and why the field is only read inside the bubble.
