<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `11` — The flow field: enemies that route, not enemies that drift

**Value:** `G` (goal-enabling — Tier A "defend through some waves" needs
it) · **Effort:** `MH`

## Status

**Active — F0 shipped 2026-08-12; F1 is next.**

⚠ **Corrected 2026-08-12, before any code was written.** This plan opened by
calling `spawn.loft::enemy_tick` — one hex along a fixed heading — a
placeholder the flow field would *replace*. It is not: it is **approach
mode, exactly as designed**, and `CLAUDE.md` had been calling it that all
along. There are two steering modes and a handoff
([`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) § Two modes);
approach is **built**, engage and the handoff are this plan, and **nothing
is deleted**.

The misreading stays on the page, because "the existing code is a
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

## What can already see a failure

**The negative control exists in the tree.** Today's straight-line enemy
walks into a wall ring and keeps going, so *no enemy occupies a hex its class
cannot traverse* is red before a line of flow-field code is written — and it
holds anywhere on the map, since approach mode stops at walls too. It needs
scoping by **class**, not by bubble: an insect on a wall it climbs is
correct, a robot there is not.

That means the gate is provably able to fail *before* the feature exists,
which is what plan 08 § The instrument comes first is about, and it costs
nothing because the broken behaviour is already there.

**What cannot see it yet:** plan 08 can say `count alive`, `range` and
`kind`, but none of them says *where* an enemy is — and `range` cannot
separate an enemy routing AROUND a wall from one walking THROUGH it, since
both show a decreasing range. That measurement lands in F1, before the
scenario that leans on it.

## F0, the answer (2026-08-12)

Four hand-built worlds, a plain BFS from the core, and the routes printed.
The probe was thrown away; this is what it said.

⚠ **First, the probe caught itself lying.** The initial ring builder walked
from the wrong corner: 18 hexes painted, **16 of them off-ring**, so the
"sealed" world was not sealed and every route through it was meaningless —
while looking entirely plausible. The pre-flight that caught it (a ring is
18 hexes, all at radius 3, and a sealed one makes the outside *unreachable*)
is now the shape F2's gate should keep.

**1. An entrance does not need detecting. F4 is cancelled.**

| world | result |
|---|---|
| ring, one gap | routes through the gap; ring hexes unreachable |
| ring, two gaps | south spawn takes the south gap, north the north — each `d=6` |
| ring, **five-hex opening** (too wide to be an "entrance" by DESIGN's 1–3 rule) | routes through it identically |
| ring, sealed | outside `d=-1`, inside `d=2` |

Shortest path *is* "preferred entry point", and the field does not care what
the opening is called. DESIGN's recognised-entrance concept is a **HUD and
telegraph** idea — it tells the player where the fight will be — not a
routing mechanic. Nothing needs to detect it for enemies to use it.

**2. One field per class; no edge weights.** The palette's movement data is
`walk_ground` / `walk_vehicle` — booleans. `slope` and `drop` are terrain
*shape*, not movement cost, and no per-hex cost exists anywhere. Passability
is binary, so open question 1 resolves to per-class fields. *(Answered.)*

**3. ⚠ The trap the probe found, which was worth more than the question.**
`wall` and `wall_high` both carry **`walk_ground = true`** — correctly, since
the walkable thing about a wall is its top. So the obvious passability
predicate is the bug: it lets robots walk through 3 m walls. The height the
step rule needs is already in the palette (`height_override` 3.0 / 5.0), and
terrain heights are **not** — they need plan 02's slope solver, so F1b and F6
build against structure heights only.

Both facts are reference, not plan, so they live in
[`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) § Where `height`
comes from.

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

**4. Passability is a height step, so build it as one — and NOT
`walk_ground`.** See § F0 point 3: walls are `walk_ground = true`, so the
obvious predicate is the bug. DESIGN settled it
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
| **F0** — probe: does an entrance need DETECTING? | XS | a probe first | four hand-built worlds + a BFS. **Shipped — see § F0, the answer.** No: routing is emergent, F4 is cancelled, and the probe found a trap worth more than the question | **Shipped** |
| **F1** — the measurement: where is an enemy? | S | — | a new `.keys` assertion (`enemy <i> <q> <r>`, and `enemies passable` — no enemy on a hex its CLASS cannot traverse) that goes RED against today's mover walking through a wall ring, and green when hand-fed a legal path. An assertion that cannot fail today is not the instrument this needs | Open |
| **F1b** — approach mode stops at walls | S | one site at a time | fired at a wall ring, an enemy halts at the EXACT hex before it; fired at a gap, it passes through. Both fail today. **Ships before any flow-field code** — it needs only the existing `walk_*` palette fields, and it is the smallest real gameplay fix in this plan | Open |
| **F2** — the distance field | S | parallel run | on a hand-built world, every cell equals a BFS worked by hand; cells adjacent to the core read 1; **unreachable is a distinct value, not 0** — 0 means "at the core", and conflating them makes a walled-off spawn read as arrived. Negative control: a closed ring → every outside cell unreachable | Open |
| **F3** — the flow direction per cell | S | parallel run | from EVERY reachable cell in a swept world, following the arrows reaches the core in exactly `distance` steps. This catches loops and local minima, which no spot-check does | Open |
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

**Answered, kept as a record:**

1. ~~One field per class, or per-class edge costs?~~ **Per-class fields** —
   F0: the palette's movement data is boolean, so nothing weighs edges.
3. ~~What replaces the spawn heading?~~ **Nothing** — it is a real approach
   constraint, so the field is **shared**, not seeded per spawn. Its job ends
   at the bubble boundary, which is why F5b is its own phase.
5. ~~Does approach mode respect walls?~~ **It stops at them.** Generalised
   since into the height-step rule; normal mobs stop, insects climb, bosses
   break (ENEMY_MOVEMENT § Two modes).
6. ~~What does a stopped enemy do?~~ **Attacks the wall** — it still wants
   the core. No enemy halts permanently, which is what makes the siege work.

**Still open:**

2. **Does the field live on the map or beside it?** It is *derived*, so it
   should be recomputed, not saved — the reasoning is
   [`plans/07`](../07-shared-world-substrate/README.md) § Evaluated. If it
   ever needs a home, `hex_field::Labels` is a per-cell integer field
   already, and dryopea should not invent a second one.
4. **`speed_approach` vs `speed_engage`** are separate numbers, equal at
   1.5 hex/s today. A tick that assumes one hex per tick is fine now and
   wrong the moment they diverge; F5b is where that becomes visible.
7. **Boss 2×2 footprint** — a 2-hex-wide unit cannot use a 1-hex-wide field,
   and it *breaks* rather than routes. Out of scope, but F6's per-class shape
   and F8's runtime-dirty path are where it will have to fit, so neither
   should assume a 1-hex unit that never edits the world.

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
