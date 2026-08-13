<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `11` — The flow field: enemies that route, not enemies that drift

**Value:** `G` (goal-enabling — Tier A "defend through some waves" needs
it) · **Effort:** `MH`

## Status

**Complete — F0 + F1 + F1b + F2 + F3 + F5 + F5b + F5c + F6 + F7 + F8
shipped 2026-08-12.**  F4 was cancelled by F0's probe (an entrance
needs no detecting).  F8's incremental rebuild is deliberately NOT
built — measured, not skipped; see § F8 for the number and the trigger.

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
scenario that leans on it.  *(Shipped — § F1, the instrument.)*

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

## F1, the instrument (2026-08-12)

Two measurements — `enemy <i> <q> <r>` (the exact hex) and `enemies
passable` (no live enemy stands where its CLASS cannot) — plus
[`src/passable.loft`](../../src/passable.loft), which is the height-step
rule and nothing else, so F2's field, F5's mover and this assertion read
one implementation rather than three that agree today.

**The gate is a pair one tick apart.** The same script, a grass corridor
with a wall face across it: at tick 8 the enemy is on the corridor and
`enemies passable` is green; at tick 9 it is standing *inside* the wall and
the assertion goes red — *"'wall' is a 3.0 m step and a robot climbs 0.0
m"*. Red-then-green one hex apart is what proves the instrument can fail;
an assertion that were always red would be as useless as one that never
could be.

**⚠ The sea default makes the corridor load-bearing.** An unpainted hex IS
sea, sea is `walk_ground = false`, so `enemies passable` is RED over a blank
map — a wave of robots walking on water. That is the right answer, and the
consequence is that every scenario asserting it must paint the ground its
enemies walk on. `a-wave-approaches` does not, which is why F1's scenario is
a new file rather than four lines added to that one.

**The palette's optional fields were lying, and the compiler took their
side.** `height_override`, `slope` and `drop` were declared non-null;
`palette.json` writes `null` in all three, and the JSON cast stores it
regardless of loft's DN1 rule. Two ways that bites: `null > 0.0` is
**false**, so an unguarded height read gives the right answer for flat
ground *by luck of null-comparison semantics*; and the compiler reports the
`?? 0.0` that defends against it as a **redundant coalesce** — it advises
deleting the guard doing the work. The fields are now declared `?`. Filed:
[`QUESTIONS_FOR_LOFT.md`](../../QUESTIONS_FOR_LOFT.md).

**Class scoping lives in the rule, not in the caller.** `climb_limit(kind)`
is the per-class table the spec's wall matrix implies, and its test asserts
it against the *palette's* heights rather than against its own literals: a
robot's limit is below `wall`, an insect's reaches `wall` and not
`wall_high`. Retune the wall heights and the test still states the design.
An unknown class takes the strictest limit — a permissive default would hand
a class nobody has tuned the run of the map, silently.

**What it does not do.** It measures a POSITION with a rule the spec states
about a MOVE. Those coincide only while everything that is not a structure
is at height 0 — true until plan 02's slope solver and F8's body piles, and
written so both are additive rather than a special case
([`src/passable.loft`](../../src/passable.loft) § The step).

## F1b, the first wall that works (2026-08-12)

`enemy_tick` asks `can_occupy` about the hex it is about to enter and stays
put when the answer is no. Six lines. Everything interesting about it is
what the six lines cost elsewhere.

**Verified by removing it.** With the check short-circuited, 8 of the 10
new tests go red — including both halves of the pair. The two that stay
green are the dead-enemy case and the file-exists check, neither of which
is about passability. A gate nobody has watched fail is a gate with an
unknown failure mode, and this one now has a measured one.

**⚠ The stop is only meaningful next to a march that does NOT stop.**
"The enemy halted" is passed by a mover that never moves — and after F1b
that failure is *live*, because an unpainted hex is sea and a whole-map
refusal looks exactly like a wall working. So every halt is asserted beside
the same march with the wall removed (reaches the core) or with a gap in it
(walks through), and beside `range 12 12` → `range 4 4`, which says they
moved eight hexes first.

**The signature changed, deliberately, and 12 call sites with it.**
`enemy_tick(e)` became `enemy_tick(e, pal, pw)`. Default parameters would
have kept every caller compiling and made the passability check *skippable*
— and an empty palette answers "no ground anywhere", so a caller who
skipped it would walk enemies through walls silently. The compile errors
were the point: every site now says which world it ticks against.

**What it cost: every scenario that walked on water.**
`a-wave-approaches`, the V2 wave test, the V3 `range` fixtures and five M5
movement tests all marched enemies across unpainted hexes — which F1 had
already established is sea. They now paint the corridor they walk. That is
not accommodation of a test-harness quirk; it is the same rule the game
enforces, arriving in the fixtures.

**⚠ F1b consumed F1's negative control, and it was re-pointed rather than
rebaselined.** F1's red half was "the enemy is standing inside the wall at
tick 9" — the bug F1b fixed, so the test would now fail because the enemy is
no longer *there*. Deleting it would leave `enemies passable` with nothing
proving it can fail. It now asserts a fault F1b cannot fix: an enemy that
*starts* inside a wall, because the spawn marker was authored on top of one.
No mover is involved, so no later movement phase can quietly make it green.

**What it does not do.** An enemy stopped by a wall stops for good — approach
mode has one heading and no way round. It does not attack (no combat yet)
and it does not route (F5). Four enemies stopped at one face all stand on
the same hex; F5c is where they spread.

## F2, the distance field (2026-08-12)

[`src/flow.loft`](../../src/flow.loft): a breadth-first sweep out from the
core over the hexes one class can occupy, storing a distance per cell.

**No route is a LARGE number, not `-1`, and that is the phase's real
decision.** The plan asked only that unreachable ≠ 0. But F3 validates
"every arrow reaches the core in exactly `distance` steps" and F5 moves an
enemy to "the best free neighbour" — both meaning *the smallest distance*.
Against a `-1`, a cell with no route is the most attractive neighbour on the
map, so the mover would walk enemies into precisely the places they cannot
go. Made larger than any real distance, the ordering refuses it with no
caller having to remember a special case. `FLOW_UNREACHABLE` and a
`flow_reachable` predicate; the accessor is the only door, because reading
`cells[q, r].dist` directly gives null and `?` turns that into 0.

**Verified by breaking it.** With `flow_distance` returning 0 on a miss —
the exact defect § F2 — the trap in the sea-default world describes — 8 of
the 17 tests go red, including the one named for it. The instrument
demonstrably sees the bug it was built for.

**The neighbour relation moved OUT of the wave engine, and that is what
makes this independent of plan 09.** `hex_offset` was living in
`spawn.loft`, where the wave engine held the geometry the whole world needs.
F2 moved it to `world.loft` § The neighbour relation together with
`hex_neighbor` / `hex_neighbours`, marked as the ONLY place a coordinate may
be stepped. Every distance in this plan is therefore a graph property, and
plan 09's conversion had one table to change.

⚠ **Plan 09 C6 then deleted `world.loft` itself** — the relation now lives
in [`lattice.loft`](../../src/lattice.loft) as `lat_neighbour` /
`lat_neighbours`, delegating to `hex_grid`. The independence claim held
under the real conversion: 233 measurements over 14 scripts, unchanged.

**⚠ Two drafts of the bend test were wrong, and working the answer by hand
caught both.** The point of the test is a distance no coordinate formula can
produce. Draft one bent 60° (directions 1 then 0) — still a shortest path,
12 steps to a hex 12 away, proving nothing; its own control assertion caught
it. Draft two expected 12 and the field said 11 — and **the field was
right**: the two legs touch at the corner, so the route cuts it and never
visits the corner hex at all. Reading the number off the implementation
would have hidden both. The hand-worked path is now in the test.

**Reuse checked first, and the answer is recorded.** Open question 2 asked
whether `hex_field::Labels` should host the field. Measured: no, not today
— `Labels` is a bounded rectangle (`labels_new(q0, r0, w, h)`) addressed in
odd-r, while dryopea's world is unbounded and sparse. (It was also axial at
the time; plan 09 has since converted it, and the verdict is unchanged —
BOUNDEDNESS is the reason, not the lattice.)
Nothing in the `hex_*` family carries a distance field to reuse; the nearest
thing is `hex_shape::flood_outside`, which is boolean reachability over a
bounded box by relaxation. So dryopea writes its own — a frontier sweep,
which suits an unbounded sparse world where a box does not.

**What is deliberately NOT here.** The arrow — "which neighbour is closest"
— is F3's, because its gate is the sweep that catches loops and local
minima. Shipping the function without that check would ship a field where
one wrong arrow is a permanent enemy stall.

**A cap with a name, not a silent one.** The sweep terminates because every
passable hex is a painted one (unpainted is sea, sea is not walkable) and
the painted set is finite — an argument that rests on the *palette*. Make
sea walkable and the frontier expands forever. `FLOW_MAX_CELLS` turns that
from a hang into a `truncated` flag, and a test asserts it is false on a
real base.

## F3, the arrow — and what the sweep can actually catch (2026-08-12)

`flow_step(f, q, r)` reads the distances and answers which neighbour is
closest to the core. It is **computed, never stored**: baking a direction
into `FlowCell` would be cheaper per query and would make F5c impossible,
since an enemy whose step is taken by a companion needs the ordering over
all six neighbours at move time. A test pins that property by editing a
distance and watching the arrow swing to the other tied neighbour with no
rebuild.

**The gate is an exhaustive sweep over five worlds** — open ground, a base
with one entrance, a sealed base, a folded corridor, and two perimeters with
opposite gaps. From every reachable cell, following the arrows must reach
the core in exactly `distance` steps. Each sweep also asserts **how many
cells it visited**, so a sweep of nothing can never be read as a sweep that
found nothing.

**Load-bearing, measured:** with the arrow deliberately broken, 9 of the 14
tests go red — every sweep among them.

**⚠ The plan said the sweep catches "loops and local minima". Loops cannot
happen, and it is worth knowing why.** A step is only ever taken to a
*strictly* smaller distance, so a walk cannot revisit a cell and cannot run
forever; termination is free. The real failure modes are a **local
minimum** (the arrow points at the cell itself — an enemy that stands there
for the rest of the wave) and a walk that ends somewhere that reads 0 but is
not the core. Both are proved catchable by hand-corrupting a good field,
because an exhaustive check that cannot fail is worth no more than a
spot-check that can.

**A probe that came back negative, and why it stays on the page.** Changing
the comparison from `<` to `<=` looked like it should create a cycle. It
does not: a BFS field always offers a `d-1` neighbour, so the step still
strictly decreases and every sweep stayed green. What it *did* break was the
tie-break — and those tests went red, which is the reason they exist.

**Ties break by lowest direction index, and that is pinned by a test.** Any
deterministic rule would do; having none would not, because plan 08 gates
the game by replaying written-down runs and a tie broken differently on a
different day makes every asserted number unrepeatable.

## F5, enemies follow the field (2026-08-12)

`enemy_tick` reads the field and steps down it; `wave_tick` rebuilds the
field **once per tick, before anybody moves**, one per class present in the
roster. `tests/scripts/a-maze-and-a-core.keys` is the scenario.

**⚠ The gate had to be invented, because every existing world was blind to
this phase.** Every enemy scenario dryopea had was a ONE-HEX-WIDE corridor —
and on those, field-following and heading-following produce the *identical*
path. All 382 tests stayed green when the mover changed, which is exactly the
kind of green that means nothing. What separates the two is a route that
leaves the heading's line: heading 4 is `(-1, 0)`, so it changes `q` and
never `r`, and **`enemy 0 3 -1` is a hex no heading could reach.** That one
coordinate is the phase.

**Measured negative control.** With the engage branch short-circuited, 6 of
the 13 unit tests go red and the scenario fails at exactly that line —
*"enemy 0 is at (4, 0), expected (3, -1)"*, the pre-F5 behaviour of stopping
dead in front of the wall. The gate is the code being replaced, as the plan
said, and now it is a number.

**⚠ Precision about what the class tests prove.** `the insect and the robot
route differently` passes *with the field disabled too* — an insect climbing
a wall by heading arrives just as it does by field. So it is a class-axis
test, not an F5 discriminator, and it is F6 that owns it. Six tests
discriminate; the rest are invariants and preserved behaviour.

**The invariant, which needs no hand-worked route.** "Follows the field" has
an exact meaning: each tick the enemy's field distance decreases by **exactly
1**, so it arrives in exactly `flow_distance(start)` ticks. Asserted that
way, the tests never hand-derive a maze route — which is where F2's bend test
went wrong twice — and every intermediate hex is checked against
`can_occupy` as the walk proceeds.

**"Has a route" is a STAND-IN for the mode selector, and it is marked as
one.** The spec says the *scrambler bubble* decides the mode; F5 uses "can
this enemy reach the core?" instead, so engage is what runs whenever a route
exists and approach is what is left. Two later phases replace it: F5b makes
the bubble the selector (so an enemy outside it follows its heading even when
a route exists), and F7 replaces the no-route fallback with the desire field
and an attack. Until then the composition keeps both behaviours alive, and
F1b's tests still pass unchanged.

**Rebuilt from scratch every tick, deliberately.** It is what
`ENEMY_MOVEMENT.md` § The tick resolves once asks for, and it buys the
order-independence plan 08's replayable runs rest on — asserted directly:
the same wave with its roster in reverse lands identically. The visible
payoff is a test where the player walls the corridor *while the wave is
walking* and the wave goes round, with no cache to invalidate. F8 makes it
incremental, and its gate is equality with this path — so this stays the
reference and is not optimised away.

**One field per class, chosen by the roster.** Not one field per class the
game knows about: `wave_tick` builds a field for each class actually present,
once each. A shared field would have moved insects along a route computed for
something that cannot climb — a correctness hole worth closing here rather
than leaving for F6, which keeps the height-step generalisation and its
raised-hex gate.

## F5b, the handoff at the bubble (2026-08-12)

The mode selector is now the thing the spec always said it was: inside
`core.scrambler_bubble_radius` an enemy follows the field, outside it follows
its spawn heading. F5's "has a route?" stand-in is gone.

**It needed no new parameter.** `FlowField` has carried its core since F2,
written down there as "F5b's approach→engage handoff needs to know which core
it is steering to". That is the whole cost of the plumbing.

**⚠ The bubble is a STRAIGHT-LINE distance, not a route length**, and that is
a design decision rather than an implementation shortcut. It is a
comms-jamming sphere, so an enemy three hexes from the core with a forty-step
route around a wall is *inside* it — jamming does not care how far you would
have to walk. Measuring it in field steps would make the bubble bulge and
shrink as the player builds walls, which is neither the fiction nor the
number. A test asserts the case that separates them: an enemy on an island
with **no route at all** is still inside.

**Inclusive at the boundary**, matching `active_spawn_markers`' `>=
disable_radius`: the hex the radius names is in.

**⚠ No existing fixture could see this phase.** Every scenario spawns 12
hexes out and the bubble is 25, so all 395 tests stayed green when the
selector changed. The new world starts at 30 and is built so both hexes
discriminate — measured with a probe before anything was asserted:

| hex | straight-line | the field says | the heading says | what happens |
|---|---|---|---|---|
| (26, 0) | 26 — **outside** | (26, -1) | (25, 0) | heading wins: *"…and not before"* |
| (25, 0) | 25 — **inside** | (25, -1) | (24, 0), unpainted | field wins: `r` changes, which a heading of `(-1, 0)` never can |

`(24, 0)` is deliberately left unpainted, so a broken handoff is loud: an
enemy that never switches walks into the dead end and stands there.

**Measured negative control — the one the plan named.** With the bubble test
short-circuited, 4 tests go red, and the sharpest is the enemy heading *away*
from the core: it turns round and walks to (1, -1) instead of east to the end
of the land. That is precisely "the handoff must not fire on
proximity-in-general", caught as a coordinate. Its own control is the same
enemy released one hex *inside*, which does turn round — without that, the
negative control would pass against a mover that ignores the field entirely.

**What is still a stand-in.** An enemy inside the bubble with no route still
presses along its heading. F7 replaces that with the desire field and an
attack; the code says so at the site.

## F5c, they spread — and the order stopped being free (2026-08-12)

A companion blocks a step. `enemy_tick` takes the first FREE entry of
`flow_steps` (F5c's addition to the field — the whole preference
ordering, which is what F3 chose to store distances for), and
`enemy_walk_heading` refuses an occupied hex too.
[`src/occupancy.loft`](../../src/occupancy.loft) is that rule and
nothing else, so the mover and the new `enemies distinct` assertion
read one implementation.

**⚠ A corridor cannot see this phase either — and the reason is a
number.** On a hex AXIS the BFS field offers **one** closer neighbour;
off the axis it offers **two**. So in every world dryopea had, "it
moves beside them" has no beside, and a blocked enemy can only wait.
The world that discriminates is open — rows `r = 0..4` over `q = 0..8`,
where distance is exactly `q + r` — and the gate is *two enemies
leaving one hex on two different routes*: from (8, 4) both want
(8, 3), so the second takes (7, 4) instead of queueing.
`tests/scripts/they-do-not-stack.keys` is that, as coordinates.

**⚠ The snapshot rule the spec literally describes is WRONG, and a
probe caught it before any of this was written.** "A hex a companion
vacated is still taken, so the follower steps beside" halves a
column's speed in a corridor: measured, the second enemy moves every
*other* tick and the third every third, falling behind for ever.
Occupancy is therefore updated as each enemy moves — a vacated hex is
free — and the column advances in lockstep.

| rule | 1-wide corridor, 3 stacked | open ground |
|---|---|---|
| start-of-tick snapshot | leader 4 ahead of the tail after 8 ticks, and growing | no spread either |
| **live, closest-first** | a column, every enemy moving every tick | two routes out of one hex |

**Which is why WHO MOVES FIRST is now a decision.** Once movement
writes the world it reads, `ENEMY_MOVEMENT.md` § The tick resolves once
is no longer free. `move_order` sorts on the STATE — field distance
ascending, then hex, then class, with roster index only as a last
resort between enemies that are interchangeable — so the order is a
function of the wave and not of the roster.

- **Closest to the core first**, and that is not arbitrary: tail-first,
  every enemy is blocked by one that has not moved yet and the column
  stretches. It is the FIELD distance rather than a coordinate, so it
  means the same thing whichever way the column runs.
- **Order independence is asserted where it can now fail.** The
  reversed-roster test with distinct starts passes with occupancy
  disabled — it is a jam that discriminates, so `test_a_jam_is_order_
  independent_too` plays nine enemies both ways round and compares the
  occupied SETS.

**⚠ What F5c deliberately does not do.** A blocked enemy takes the best
free step that is strictly CLOSER, or it stands still — it never
sidesteps to an equally distant hex, so F5's "distance decreases by
exactly 1" survives untouched and occupancy stays a movement
constraint rather than a second steering rule. At a wall, four enemies
therefore end in a QUEUE along their heading, on four distinct hexes,
and not spread along the face. That is honest: approach mode has no
field to say which way "beside" is. **F7 supplies it** — the desire
field is the gradient, and the spread along the face falls out of the
same rule with no special case. A test asserts the queue *is* on one
line, so F7's gate can still fail.

**Measured negative control.** With `occupancy_taken` short-circuited,
**19 of 432 tests go red** — 11 of the 26 in the phase's own file, plus
8 across the six wave scenarios it changed. The survivors are the
`flow_steps` ordering tests and the occupancy unit tests that never ask
`taken`, neither of which is about spreading.

**What it cost: every fixture where a wave collapsed to a point.**
Six scripts and four tests asserted `range 4 4` or "all five arrived at
(0, 0)". A wave spawns on ONE hex and leaves it one enemy per tick, so
those numbers are now spans and columns — `range 4 7`, and "exactly one
enemy at each of distances 0, 1, 2, 3". The band is a *better*
assertion than the point was: `range 4 7` over four enemies says they
are strung out along the route, which is the mechanic.

**⚠ `enemies distinct` is RED at the moment a wave spawns**, and that
is the instrument working. `spawn_wave` emits the whole wave onto the
marker hex, so the red/green pair is a few lines apart in one world
with no code changed between them — the same shape as F1's `enemies
passable`. The red half lives in the test as an inline string, because
every file in `tests/scripts/` must play green.

**A loft bug, filed, that this phase found by refactoring.**
`flow_step` became `flow_steps(f, q, r)[0] ?? Hex { q: q, r: r }` and
every arrow on the map started pointing at the hex it stood on — an
index on a call's result in TAIL position reads the absent sentinel.
Silent on the interpreter (the `??` fallback, which is *designed* to
look plausible), a panic on native.
[loft#877](https://github.com/loft-lang/loft/issues/877); the
workaround is to bind the call to a local first.

## F6, the height step — and the rule that turned out to be vacuous (2026-08-12)

Passability is now the spec's rule verbatim: `height(to) - height(from)
<= climb(class)`, an EDGE and not a hex.
[`src/height.loft`](../../src/height.loft) is the runtime layer that
makes the difference measurable, and the fields are keyed by the CLIMB
LIMIT rather than by the class.

**⚠ The design decision, and a probe killed the obvious version of it.**
The natural shape was "keep `can_occupy` as the field's node filter and
add the step on top". Worked out on paper first, that composition is
**vacuous**: `can_occupy(x)` means `height(x)` is within a climb of its
LOWEST standable neighbour, so for any two adjacent occupiable hexes
`h(x) - h(y) <= climb` holds in *both* directions by construction — the
step check could have been deleted and no test would have moved. The
field therefore filters **nodes by the surface** and **edges by the
step**, which are genuinely two questions.

That is not a small distinction: the vacuous version compiles, reads
well, passes every test, and silently makes F6 a no-op.

**⚠ The BFS runs OUTWARD and the enemy walks INWARD, so the step is
checked backwards.** Expanding a labelled cell `a` to a new neighbour
`n`, the move an enemy will make is `n -> a`. Reversed, 2 tests go red —
so it is gated, but only because F6 went looking for the case: on flat
ground the two directions are identical, and *every world dryopea had
before this phase was flat*.

**The visible consequence, which is a changed number and not a
rebaseline.** A `wall` beside the core now JOINS a robot's field — a
robot cannot climb up there, but one standing there could step down and
walk home, and the field says so. `test_a_sealed_base_sweeps_clean` went
from 37 cells to 61 (37 inside a radius-4 ring plus its 24 wall hexes).
The number that must NOT move is beside it, and is now asserted
explicitly: the outside is still unreachable, which is what *sealed*
means.

**The class's whole contribution is `climb_limit`, and that is asserted
rather than commented.** `wave_fields` keys on the limit, so two classes
that climb alike share one sweep — pinned by giving an unknown kind 99
(which falls to the strictest limit) a field and comparing it cell for
cell with a robot's. Give a class a second movement axis — a vehicle
reading `walk_vehicle` — and that test goes red instead of the key
quietly sharing a field it should not.

**⚠ Measured negative controls, and one of them is deliberately thin.**

| what was broken | red | what that says |
|---|---|---|
| the height layer ignored | **13 of 465** | the layer is load-bearing everywhere, scenario included |
| the step read as the DESTINATION height, not the rise | **3** | thin *by nature* — the two rules agree whenever the source hex is at 0 m, which is every world before F6 |
| the BFS step direction reversed | **2** | observable only because F6 built the asymmetric case |
| fields keyed by kind again | **1** | honest: that one is a DUPLICATION, not a wrong answer, so only the test that counts fields can see it |

The 3-red row is the one worth reading. The discriminating case had to
be constructed: a robot walking ALONG a plateau it could never climb
onto — every step level, every step off it a drop. Read as an absolute
height, that enemy never moves at all;
`test_a_robot_walks_along_a_plateau_it_could_never_climb_onto` is the
whole difference between the two rules.

**⚠ The class half of the plan's own gate was already live at F5, and
saying so is the point.** F5 recorded that *"the insect and the robot
route differently" passes with the field disabled too* and handed the
row to F6. It is now a scenario
([`tests/scripts/two-classes-two-routes.keys`](../../tests/scripts/two-classes-two-routes.keys)) —
the insect stands ON the wall at `(3, 0)` while the robot stops at
`(4, 0)` — but F6's own discriminator is the third act: `raise 4 -1 1.5`
shuts the robot's bypass with nothing painted, nothing edited and no
save touched. That is `ENEMY_MOVEMENT.md` § Bodies are terrain point 1,
playable.

**The mechanic that fell out with no code.** Point 3 of that section —
*enemies climb their own dead onto the wall* — is not implemented
anywhere. A 3 m pile beside a 5 m `wall_high` leaves a 2 m step, an
insect climbs 3, and the anti-insect barrier is breached without the
wall being broken. It is a test, not a feature, which is what building
the rule as a height step rather than a material lookup bought.

**What F6 did NOT need.** No new numbers: the climb limits and the wall
heights were fixed at F1, and the ramp arithmetic is theirs. No change
to `wave_tick`'s signature either — the layer rides on `WaveState`,
because plan 11 already said where it belongs (*pile heights live with
the wave, not in the save*).

**What is still a stand-in.** Nothing drops a body yet, so `raise` is
the only thing that fills the layer and combat is what will replace it.
The layer's contract is already the one bodies need — it ACCUMULATES,
and a negative rise floors at the ground so collecting more than fell
digs no hole.

## F7, the siege — and a gate that turned out to measure the other half (2026-08-12)

An enemy with no route follows a **desire field** — the same BFS with the
climb lifted, so walls are passable — and attacks where the height rule
refuses the next step. `enemy_target` names that hex; `target <i> <q>
<r>` and `count targets <lo> <hi>` measure it;
[`tests/scripts/a-sealed-base.keys`](../../tests/scripts/a-sealed-base.keys)
is the scenario.

**It cost one number.** `flow_build` and `flow_desire` are now the same
sweep with a different `climb` — `FLOW_CLIMB_ANY` — so "walls are
passable" needed no second traversal that could disagree with the first
about what ground is. The whole phase is that plus a branch in the
mover.

**⚠ The gate the plan named measures the TARGETING, not the steering,
and only a negative control could have said so.** The plan's row asks
for *"N enemies from different sides attack N different hexes"*. Six
enemies on the rim of a sealed base do attack six distinct wall hexes —
and they still do with the desire field **disabled**, because six
different spawn headings already walk them to six different places. The
same shape as F5's insect/robot row, and it means `count targets` is a
gate on `enemy_target` (which did not exist before F7) rather than on
what steers.

What discriminates the steering is a corridor that BENDS: heading 4 is
`(-1, 0)`, so `enemy 0 2 -1` is a hex no heading can reach, and a
heading-follower stops at `(5, 0)` where the land turns. That is
`bent_siege()`, and it is the third time in this plan that a gate had to
be built around "a straight line cannot tell a field from a heading".

**Measured negative controls:**

| what was broken | red | what it gates |
|---|---|---|
| the desire field never built | **11 of 490** | everything downstream of the second sweep |
| `enemy_target` never names a hex | **6** | the targeting, including the whole scenario |
| the siege branch reverted to the heading | **3** | the STEERING — and only the bent-corridor tests see it |

**⚠ F5c's prediction was wrong, and the test that was waiting for this
phase stayed green.** F5c wrote *"the desire field is the gradient, and
the spread along the face falls out of the same rule with no special
case"*, and pinned the queue so F7's gate could fail. F7 shipped and it
did not fail — because the desire gradient points **at** the wall, not
**along** it, and `flow_steps` only ever offers a strictly-closer hex.
Four enemies down one corridor still meet the face at one point.

The spread this phase delivers is **by approach**, which is what
`ENEMY_MOVEMENT.md` § Sealing actually derives it from ("they arrive
already spread"). Chewing a single face along its length needs an
equal-distance sidestep — a second steering rule, which F5c deliberately
refused and F7 does not add. The test is renamed to stop claiming a
phase is coming for it, and the limit is stated in
[`tests/11_f7_the_siege.loft`](../../tests/11_f7_the_siege.loft)'s header
rather than left as a promise.

⚠⚠ **Plan 12 B7 priced that refusal, and it turns out to set the whole
GAME BALANCE rather than only the shape of a siege.** Thirteen robots
reach an undefended core and exactly **two** ever nibble it: they
arrive down one axis from one spawn, and on a hex axis the field offers
ONE closer neighbour, so a blocked enemy waits where an off-axis one
would have a second choice. So **the drain does not scale with the
wave** — a column of four and a column of twelve drain identically —
and by the same token a base's width and its roster are scenery.

The perverse consequence is the one worth carrying into any future
phase here: **anything that pushes the column off its axis lets more of
it reach the core**, so a defending tower's own kills accelerate the
base's fall. Building the sidestep is therefore not a polish item; it
is the difference between wave size mattering and not.
[`tests/12_b7_the_clock.loft`](../../tests/12_b7_the_clock.loft) prices
it and asserts today's answer, so building it turns that file red.

**You attack what you could STAND on and cannot climb.** The target is
always a hex whose surface is walkable, so an enemy at the water's edge
besieges nothing — there is nothing there to break. Free in the siege
branch (every cell of the desire field is walkable by construction);
approach mode has to ask, and a pair of tests separates "refuses the
sea" from "refuses approach mode".

**Built only when somebody is besieging.** `wave_desire` returns an
empty field unless some alive enemy inside the bubble has no route —
the same argument `wave_fields` makes about classes, and asserted both
ways.

**⚠ A loft bug, filed, that this phase found by refactoring.** Pulling
the BFS out of `flow_build` into a shared `flow_sweep` made every
CONSUMER's one-line helper a second tail call, and a struct returned
through two nested tail calls loses everything its loop wrote — 1 cell
interpreted, 0 native, 13 expected. Every flow field on the map came
back empty and the game stopped moving, while the nine red tests all
named the movement rather than the wrapper.
[loft#880](https://github.com/loft-lang/loft/issues/880); the workaround
is to bind the call to a local. It took a four-way boundary matrix
(wrapper binds/tail-returns × caller binds/inlines) to locate, because
the defect appears at call sites nobody edited.

## F8, the tick budget — and the phase asked the wrong question (2026-08-12)

F8 was written as *"make the rebuild incremental, gated on equality with
the from-scratch one"*. **Measured first, and the rebuild was not the
problem.** The tick was over budget, and what was eating it was a
`FlowField` being COPIED once per enemy.

**The numbers the design actually fixes**, so the measurement had
something to be measured against: `numbers.json` authors a wave list
topping out at **80 enemies**, bounds the world at the **radius-40**
haze, and moves an enemy at **1.5 hex/s** — so a movement tick has
**~667 ms**.

| r=40, 80 enemies, one `wave_tick` | before | after |
|---|---|---|
| measured by the committed tests | **830 ms** | **125 ms** |
| against the 667 ms budget | **over** | 19% |

**⚠ The defect was a whole-value bind, and it read as correct code.**
`field_of(fields, kind) -> FlowField` did `fo_out = fo_f.field`, which
copies the entire cell hash. Every caller looked right — `enemy_tick(e,
…, field_of(fields, e.kind), …)` — and it ran once per enemy per lookup,
three lookups per enemy per tick. Reading the field in place instead of
binding it is **2250×** cheaper at r=40, and O(1) where the copy is
O(cells). There is now no accessor at all: callers loop the fields and
pass `cf.field` straight into a `const` parameter.

**⚠ It had been there since F5, and nothing could see it.** A copy
changes no behaviour, only cost, so 490 tests were green over a tick
running 25% over its budget. **dryopea had no gate that could see cost**
— that gap is the phase's real finding, and closing it is the phase's
real deliverable.

**The gate had to be a RATIO, not a stopwatch.** Sixteen times the
enemies over the *same* world: the rebuild is paid once either way, so
what is left is the per-enemy work. Measured on both sides, three suite
runs each — **115-125% reading in place, 316% copying** — with the
threshold at 200% and 1.6× on either side. Both halves run back to back
and both pay the same rebuild, so a busy machine moves them together;
the readings proved stable to ±2% under a full suite run.

**The incremental rebuild is NOT built, and that is a decision.** At
125 ms against 667 ms the from-scratch sweep leaves 5× headroom, and §
F8 — and why it is last is explicit that *an incrementally wrong field
is a game routing enemies through a wall the player just built*.
Building a dirty-set path now would buy ~19% of a budget nobody is
near, at the cost of the one failure mode this plan most wants to
avoid.

⚠ **The trigger for revisiting**, so the decision is not silently
permanent: `test_a_tick_at_maximum_load_fits_inside_its_budget` going
red, or the design raising the wave list or the world radius past what
`numbers.json` fixes today. The equality gate the incremental path would
need is already written and already green against the reference —
`test_the_field_a_tick_uses_equals_a_fresh_build`.

**What F8 did build beyond the fix.** The half of its own gate that was
never asserted: **bodies dropped mid-wave**. F5 covers a wall the player
*builds*; plan 11 § point 5 says a gate exercising only editor strokes
tests the rarer half, and once combat exists every kill raises a hex.
So: a pile dropped mid-wave diverts the wave, collecting it reopens the
route, a change lands on the *next* tick and never the one running, and
a wave over ground being raised under it is order-independent.

**⚠ A blind instrument nearly made this phase's conclusion wrong, in the
safe-looking direction.** A throwaway probe took `ticks` as a parameter
name — shadowing loft's clock builtin, the exact trap `CLAUDE.md`
records for `now`. It compiled clean and reported a tick **4× cheaper
than it was**. Two correct-clock measurements disagreeing by 3× is what
exposed it; had the shadowed number been the only one, F8 would have
concluded "plenty of headroom" from a reading that measured nothing.
The committed test names its parameter `n_ticks` and says why.

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
| **F1** — the measurement: where is an enemy? | S | — | a new `.keys` assertion (`enemy <i> <q> <r>`, and `enemies passable` — no enemy on a hex its CLASS cannot traverse) that goes RED against today's mover walking through a wall ring, and green when hand-fed a legal path. An assertion that cannot fail today is not the instrument this needs. **Shipped — see § F1, the instrument.** The gate is the same script one tick apart: red standing in a wall face, green a hex earlier. A face, not a ring — F0's hand-built ring painted 16 of 18 hexes off-ring, so a ring needs the pre-flight F2 will carry, and three hexes on a line need none | **Shipped** |
| **F1b** — approach mode stops at walls | S | one site at a time | fired at a wall face, an enemy halts at the EXACT hex before it; fired at a gap, it passes through. Both failed today — **measured**, by short-circuiting the check: 8 of 10 tests go red without it. **Shipped — see § F1b, the first wall that works.** ⚠ The plan said "it needs only the existing `walk_*` palette fields"; F0 had already disproved that — `walk_*` is the bug, and it uses the height step | **Shipped** |
| **F2** — the distance field | S | parallel run | on a hand-built world, every cell equals a BFS worked by hand; cells adjacent to the core read 1; **unreachable is a distinct value, not 0** — 0 means "at the core", and conflating them makes a walled-off spawn read as arrived. Negative control: a closed ring → every outside cell unreachable. **Shipped — see § F2, the distance field.** Measured against the negative control: with unreachable collapsed to 0, 8 of 17 tests go red. Unreachable is a LARGE value, not `-1`, so "smallest distance wins" refuses it | **Shipped** |
| **F3** — the flow direction per cell | S | parallel run | from EVERY reachable cell in a swept world, following the arrows reaches the core in exactly `distance` steps. This catches loops and local minima, which no spot-check does. **Shipped — see § F3, the arrow.** Swept over five worlds, each asserting how many cells it visited; 9 of 14 tests go red against a broken arrow. ⚠ Loops turn out to be impossible by construction (a step is only ever to a strictly smaller distance) — what the sweep really catches is a local minimum and a walk ending on a second zero | **Shipped** |
| **F5** — enemies follow the field | M | one site at a time | the maze scenario: one entrance, `enemies passable` (F1) holds every tick, `range` decreases monotonically to 0. Its negative control is the code being replaced — see § The negative control already exists. **Shipped — see § F5, enemies follow the field.** ⚠ The gate had to be INVENTED: every existing world was a 1-wide corridor, where field and heading give the identical path, so the whole phase could pass without doing anything. The discriminator is `enemy 0 3 -1` — a hex no heading can reach. Measured: 6 of 13 tests red with the engage branch disabled | **Shipped** |
| **F5b** — the approach→engage handoff | S | one site at a time | an enemy crossing `core.scrambler_bubble_radius` switches mode at the EXACT hex the radius names, and its steps change from "along the heading" to "along the field" there and not before. Negative control: an enemy whose heading never enters the bubble keeps its heading forever — the handoff must not fire on proximity-in-general. **Shipped — see § F5b, the handoff at the bubble.** No new parameter: the field has carried its core since F2. Measured: 4 tests red without the bubble test, the negative control among them | **Shipped** |
| **F5c** — enemies spread, they do not stack | S | one site at a time | two enemies with the same desired hex end on DIFFERENT hexes; N enemies converging on one wall face occupy N distinct hexes along it and attack N distinct wall hexes. Negative control: a mover that reads one baked arrow per cell physically cannot pass this — which is why F3 stores distances. **Shipped — see § F5c, they spread.** ⚠ A corridor is blind to this one too: on a hex AXIS the field offers ONE closer neighbour and off it TWO, so "beside" only exists off-axis and the gate needs an open world. Measured: 19 of 432 red without the occupancy check. ⚠ Two corrections to the row above — the spec's own snapshot rule (a vacated hex stays taken) halves a column's speed and was rejected by probe; and the four at the wall queue along their HEADING rather than spreading along the face, because approach mode has no gradient to say which way beside is. The attack, and the spread along the face, are F7's | **Shipped** |
| **F6** — per-class passability, as a height step | M | one site at a time | one field per climb limit, not per material: same maze, the insect crosses the wall, the robot goes round, both arrive, **paths differ**. Then the same predicate re-run with a raised hex must flip who can pass — a class table that only reads materials cannot do that, and body piles need it. **Shipped — see § F6, the height step.** ⚠ Two corrections to the row above. The "same maze, paths differ" half was ALREADY live at F5 (F5 said so and handed the row on), so F6's own discriminator is the raise — `raise 4 -1 1.5` shuts the robot's bypass with nothing painted. And the obvious composition — keep `can_occupy` as the field's node filter, add the step on top — is **vacuous**: it makes the height rule deletable without a test moving, so the field filters nodes by the SURFACE and edges by the STEP. Measured: 13 of 465 red without the layer, 3 without the rise, 2 with the BFS direction reversed | **Shipped** |
| **F7** — no path: the siege | S | parallel run | closed perimeter → each enemy attacks the wall hex where ITS OWN route to the core first meets an impassable hex, so N enemies from different sides attack N different hexes. The scenario asserts the **set** and that it is spread: an implementation that collapses to one hex has lost the mechanic (§ Sealing is punished, not forbidden). **Shipped — see § F7, the siege.** It cost one number: the desire field is `flow_build` with `FLOW_CLIMB_ANY`. ⚠ Two corrections. The row's own gate measures the TARGETING and not the steering — six headings already spread six enemies, measured — so the discriminator had to be a corridor that BENDS (`enemy 0 2 -1`). And F5c's promise that the face-spread "falls out with no special case" was wrong: the desire gradient points AT the wall, not along it, so one approach still queues. Measured: 11 of 490 red without the field, 6 without the target, 3 without the steering |  **Shipped** |
| **F8** — rebuild once per tick, on edits AND deaths | M | parallel run | after a sequence of paint edits **and of bodies dropped mid-wave**, the incrementally-updated field equals a from-scratch rebuild, cell for cell; and **the same wave with the roster iterated in REVERSE produces an identical result**. A gate that only exercises editor strokes tests the rarer half. **Shipped — see § F8, the tick budget.** ⚠ The row asked the wrong question: measured against the design's own numbers the tick was 830 ms over a 667 ms budget, and the REBUILD was not why — a `FlowField` was being COPIED per enemy per lookup, 2250x the cost of reading it in place. Removing that gives 125 ms. The incremental path is therefore NOT built (5x headroom, and an incrementally wrong field is the failure this plan most wants to avoid); its equality gate is written and green against the reference, with a named trigger. What F8 DID add is the gate dryopea had no form of — a cost gate, as a RATIO — plus the bodies-mid-wave half nobody had asserted | **Shipped** |

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
| **F1** ✅ | the new assertion goes red against today's mover | the instrument can see the failure it exists for | an assertion green on a wall-walker measures nothing |
| **F2** ✅ | closed ring → outside cells unreachable | unreachable ≠ 0 ≠ at-the-core | a walled-off spawn reading 0 = "already arrived" |
| **F3** ✅ | arrows reach the core in exactly `distance` steps, from every reachable cell | the field has no local minimum (a loop cannot occur — the step strictly decreases) | one cell whose arrow stalls is an enemy that stands there for the rest of the wave |
| **F5** ✅ | `enemies passable` every tick, AND a hex off the heading's line | routing respects the world, and it is the FIELD that steered | a 1-wide corridor cannot tell field from heading — the gate needed a route that leaves the line |
| **F5c** ✅ | two enemies leave ONE hex on two different routes | a companion blocks a step, and is never a target | a 1-wide corridor has no "beside" — off the axis the field offers two closer neighbours, on it one |
| **F5c** ✅ | a jam played with the roster reversed occupies the same SET | the move order reads the state, not the roster | order-independence stopped being free the moment movement wrote what it reads |
| **F6** ✅ | insect and robot paths DIFFER on one map | passability is per class | identical paths mean the class key is ignored — but F5 already passed this, so it gates the class axis and not F6 |
| **F6** ✅ | one hex raised at RUNTIME, and who can pass moves | height is a property of the WORLD, not of the ground types | a material lookup answers the same thing forever, so nothing else in the phase could fail |
| **F6** ✅ | a robot walks ALONG a plateau it could never climb onto | the step is a RISE between two hexes, not the destination's height | with the source at 0 m the two rules agree — which is every world before F6, so the case had to be built |
| **F7** ✅ | the exact wall hex, named | the fallback is deterministic | "some wall" is not repeatable, so a run cannot assert it |
| **F7** ✅ | a besieged enemy reaches `r = -1` | it walks the DESIRE field, not its heading | a straight corridor gives both the same path — the third time this plan needed a bend |
| **F7** ✅ | six approaches → six distinct target hexes | the target is per-route, never a global "nearest wall" | ⚠ passes with the steering disabled too: six headings already spread. It gates the targeting |
| **F8** ✅ | the tick's field == a fresh build after edits AND piles | there is no cache to go stale | written and green against the reference, so an incremental path has its gate waiting |
| **F8** ✅ | reverse-iterated roster → identical wave, over ground being RAISED under it | one rebuild per tick, so no enemy sees a world its neighbour changed | an order-dependent tick makes every scripted number unrepeatable — plan 08 could gate nothing |
| **F8** ✅ | 16x the enemies costs <200% of the time | per-enemy work does not scale with the world | a COPY changes no behaviour, so 490 green tests sat over a tick 25% past its budget for four phases |
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

2. ~~Does the field live on the map or beside it?~~ **Beside it, and
   recomputed** — F2 measured the alternative: `hex_field::Labels` is a
   BOUNDED rectangle addressed in odd-r, and dryopea's world is unbounded,
   sparse and axial until plan 09, so it cannot host the field today.  No
   `hex_*` library carries a distance field to reuse either.  *(Answered.)*
   The original reasoning, kept: it is *derived*, so it
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
