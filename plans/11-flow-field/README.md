<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `11` — The flow field: enemies that route, not enemies that drift

**Value:** `G` (goal-enabling — Tier A "defend through some waves" needs
it) · **Effort:** `MH`

## Status

**Active — F0 is next.**

⚠ **Corrected 2026-08-12, before any code was written.** This plan opened by
calling `spawn.loft::enemy_tick` — one hex along a fixed heading — a
placeholder the flow field would *replace*. It is not. It is **approach
mode, exactly as designed**, and `CLAUDE.md` has been calling it that all
along ("approach-mode enemy tick"). [`docs/DESIGN.md`](../../docs/DESIGN.md)
§ 6:

> enemies appear at each and **head along the marker's direction until they
> enter the scrambler bubble**, at which point they **pivot to engage mode
> (flow field toward the core)**.

So there are **two movement modes and a trigger**, and only the last two are
missing:

| mode | rule | state today |
|---|---|---|
| **approach** — outside the bubble | straight along the spawn marker's heading, `speed_approach`, **stopping at anything its class cannot traverse** | **built, minus the stopping** — `enemy_tick`, F1b |
| **engage** — inside the bubble | flow field toward the core, `speed_engage` | this plan |
| **the handoff** | `core.scrambler_bubble_radius` — [`docs/NUMBERS.md`](../../docs/NUMBERS.md) says it outright: *"The bubble boundary IS the approach→engage trigger"* | this plan, F5b |

Nothing is deleted. The flow field is bolted on beside a mode that already
works, which is a much safer plan than the one first written here — and the
misreading is worth leaving on the page, because "the existing code is a
placeholder" is the assumption that would have thrown away a correct
mechanic.

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

And it holds **anywhere on the map**, not only inside the bubble — because
approach mode stops at walls too (§ Open questions 5, answered). A wall-walker
is wrong in both modes, so the assertion needs no scoping by bubble, only by
class: *no enemy occupies a hex its own class cannot traverse.* An insect on a
wall it is climbing is correct; a robot there is not.

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

## What the movement spec costs to build

The rules themselves live in
[`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) — design belongs
there, not in a plan. Three of them change what this plan
*builds*, and those are here.

**1. The no-path fallback is a second field, not a targeting system.**
Blocked enemies still want the core and meet the wall wherever their own
approach runs into it — that is what produces the spread. "The nearest wall"
computed once hands every enemy the same target and collapses the siege back
to one chokepoint. So:

| field | walls | used for |
|---|---|---|
| passability field (F2/F3) | impassable | routing, when a route exists |
| **desire field** | **passable** | where an enemy wants to go when there is none |

An enemy with no route follows the desire field and attacks the first
impassable hex it meets. The spread comes free; F7 costs one more BFS.

**2. The field must store DISTANCE, not a baked arrow.** Enemies do not
queue — one whose step is taken by a companion moves *beside* them. That is
"take the best FREE neighbour", which needs a preference ordering at move
time. A single precomputed direction per cell cannot express it, and
discovering that after F3 means rebuilding the field's representation.
F3 still validates the arrows as an invariant of the field; the mover just
reads distances.

**3. Occupancy is a movement constraint and never a target.** Companions
block a step; they are not attacked, and never divert an enemy from the core.

**4. Passability is a height step, so build it as one.** DESIGN settled it
as `height(to) - height(from) <= climb(class)` rather than a material
lookup. Written that way, walls, insects and body piles are one rule with a
per-class limit; written as "is this hex a wall", every later mechanic is a
special case bolted onto F1b. This is the cheapest possible moment to get it
right and an expensive one to retrofit.

**5. The field is recomputed on DEATH — batched ONCE PER TICK.** Bodies
raise height (ENEMY_MOVEMENT § Bodies are terrain), so every kill changes
passability; F8
was written as "recompute after edits, with combat reusing it", which is
backwards. ENEMY_MOVEMENT § The tick resolves once settles when: one
rebuild per tick, never per event.

That is not primarily a cost decision. **It is what makes a tick
order-independent**, and therefore what makes a scripted run reproducible —
which is the entire premise plan 08 gates on. A field rebuilt mid-loop makes
the outcome depend on which enemy the roster happened to visit first, and no
`.keys` scenario could assert a stable number over that.

⚠ **And a body pile is runtime state, not map data** — the § Evaluated
reasoning in [`plans/07`](../07-shared-world-substrate/README.md) applies
exactly: authored → a layer, derived → recompute, runtime → sim state. Pile
heights live with the wave, not in the save.

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
| **F1** — the measurement: where is an enemy? | S | — | a new `.keys` assertion (`enemy <i> <q> <r>`, and `enemies passable` — no enemy on a hex its CLASS cannot traverse) that goes RED against today's mover walking through a wall ring, and green when hand-fed a legal path. An assertion that cannot fail today is not the instrument this needs | Open |
| **F1b** — approach mode stops at walls | S | one site at a time | fired at a wall ring, an enemy halts at the EXACT hex before it; fired at a gap, it passes through. Both fail today. **Ships before any flow-field code** — it needs only the existing `walk_*` palette fields, and it is the smallest real gameplay fix in this plan | Open |
| **F2** — the distance field | S | parallel run | on a hand-built world, every cell equals a BFS worked by hand; cells adjacent to the core read 1; **unreachable is a distinct value, not 0** — 0 means "at the core", and conflating them makes a walled-off spawn read as arrived. Negative control: a closed ring → every outside cell unreachable | Open |
| **F3** — the flow direction per cell | S | parallel run | from EVERY reachable cell in a swept world, following the arrows reaches the core in exactly `distance` steps. This catches loops and local minima, which no spot-check does | Open |
| **F4** — entrances, if F0 says they are a mechanic | S | — | *(exists only if F0's answer is "not emergent")* the field prefers the gap over the shortest wall-break | Open |
| **F5** — enemies follow the field | M | one site at a time | the maze scenario: one entrance, `enemies clear of wall` holds every tick, `range` decreases monotonically to 0. Its negative control is the code being replaced — see § The negative control already exists | Open |
| **F5b** — the approach→engage handoff | S | one site at a time | an enemy crossing `core.scrambler_bubble_radius` switches mode at the EXACT hex the radius names, and its steps change from "along the heading" to "along the field" there and not before. Negative control: an enemy whose heading never enters the bubble keeps its heading forever — the handoff must not fire on proximity-in-general | Open |
| **F5c** — enemies spread, they do not stack | S | one site at a time | two enemies with the same desired hex end on DIFFERENT hexes; N enemies converging on one wall face occupy N distinct hexes along it and attack N distinct wall hexes. Negative control: a mover that reads one baked arrow per cell physically cannot pass this — which is why F3 stores distances | Open |
| **F6** — per-class passability, as a height step | M | one site at a time | one field per climb limit, not per material: same maze, the insect crosses the wall, the robot goes round, both arrive, **paths differ**. Then the same predicate re-run with a raised hex must flip who can pass — a class table that only reads materials cannot do that, and body piles need it | Open |
| **F7** — no path: the siege | S | parallel run | closed perimeter → each enemy attacks the wall hex where ITS OWN route to the core first meets an impassable hex, so N enemies from different sides attack N different hexes. The scenario asserts the **set** and that it is spread: an implementation that collapses to one hex has lost the mechanic (§ Sealing is punished, not forbidden) | Open |
| **F8** — rebuild once per tick, on edits AND deaths | M | parallel run | after a sequence of paint edits **and of bodies dropped mid-wave**, the incrementally-updated field equals a from-scratch rebuild, cell for cell; and **the same wave with the roster iterated in REVERSE produces an identical result** — the order-independence ENEMY_MOVEMENT § The tick resolves once requires. A gate that only exercises editor strokes tests the rarer half | Open |

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
| **F8** | reverse-iterated roster → identical wave | one rebuild per tick, so no enemy sees a world its neighbour changed | an order-dependent tick makes every scripted number unrepeatable — plan 08 could gate nothing |
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
3. ~~**What replaces the spawn heading?**~~ **ANSWERED** (project owner +
   DESIGN § 6, 2026-08-12): **nothing replaces it — it is a real approach
   constraint**, and it governs approach mode outright. The consequence for
   this plan is the useful half:

   **The field is SHARED, not seeded per spawn.** The heading's job is
   finished at the bubble boundary, so engage mode needs exactly one field
   toward the core — not one per spawn marker, and not a per-spawn bias on a
   shared field. That removes the largest open cost in F5 (N fields, N
   rebuilds on every edit) and it is why F5b exists as its own phase: the
   handoff is where the heading stops mattering, so it is the one place the
   two mechanics touch.

   ⚠ It also means **`speed_approach` and `speed_engage` are separate
   numbers** (`examples/numbers.json`, equal today at 1.5 hex/s). A tick that
   assumes one hex per tick is fine now and wrong the moment they diverge —
   F5b is where that assumption becomes visible.
5. ~~**Does approach mode respect walls at all?**~~ **ANSWERED** (project
   owner, 2026-08-12): **it stops at walls; it does not walk through.**

   The general form is worth stating, because it is simpler than the
   question was: **passability is a property of `(hex, enemy class)` and is
   independent of mode.** Only the *steering* differs — approach mode steers
   by the marker's heading, engage mode by the field, and both are blocked
   by the same hexes. That collapses what looked like two rule sets into
   one, and it is why F1b can ship before any flow-field code exists.

   Two consequences already folded in above: F1's assertion is a whole-run
   invariant rather than an engage-mode one, and it must be scoped **by
   class** — an insect standing on a wall it climbs is correct, a robot
   there is not.

   ⚠ Normal mobs only — bosses break instead of stopping, and the rule has
   since generalised to a height step. Both live in
   [`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md); the build
   consequences are § What the movement spec costs to build, points 4–5.

6. ~~**What does a stopped approach-mode enemy DO?**~~ **ANSWERED** (project
   owner, 2026-08-12): it **attacks the wall**, because it still wants the
   core. No enemy halts permanently, in either mode — which is exactly what
   makes § Sealing is punished, not forbidden work. See F7.
7. **Boss 2×2 footprint** — a 2-hex-wide unit cannot use a 1-hex-wide
   field, and per § 5 above it also *breaks* rather than routes. Out of
   scope here, but F6's per-class shape and F8's runtime-dirty path are the
   two places it will have to fit, so neither should be built assuming a
   1-hex unit that never edits the world.

## See also

- [`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) — the spec this
  plan builds. [`docs/DESIGN.md`](../../docs/DESIGN.md) § 5 (wall topology,
  entrances) and § 7 (targeting priority, nibble) for what it sits between.
- [`plans/05-validation-scenario`](../05-validation-scenario/README.md) —
  the consumer; "defend through some waves" needs this mechanic.
- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  instrument F1 extends, and the gate every F-phase asserts through.
- [`plans/09-lattice-conversion`](../09-lattice-conversion/README.md) — not
  a dependency, deliberately; § Computed from the neighbour relation is why.
