<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 14 — Helpers: the crew becomes a cooperative

**Value:** `G` · **Effort:** `MH`

## Status

**H0 + H1 + H2 shipped** (2026-08-14). H3 is next, and is blocked on a
carry model.

**H2 put the crew to work, and it added no mechanic to do it.**
Clearing already existed (plan 13 V2 + V3), so H2 extracted it onto the
shared chassis — `vehicle.loft::salvage_at`, one implementation — and
gave the roster a turn in the tick. Suite **849 green**, gate **22
scripts / 395 measurements**.

⚠ **A roster's value is COVERAGE, not throughput**, and one pair of
scenarios separates them. On a base with two fronts:

| | ticks |
|---|---|
| nobody clearing (`a-base-on-two-fronts`) | 77 |
| one helper, east front (`a-crew-on-one-front`) | 214 |
| **two helpers, both on the east front** | **214** — the second is worth nothing |
| one helper on each front (`a-crew-on-two-fronts`) | 242 |

One tower makes ~0.03 m of body a tick and one vehicle clears 0.33, so
**the first helper on a front is already ten times faster than the
front makes work**. What a second one is for is the other front.

⚠ **And a helper is worth exactly what the player was.** Swap `park 7 0`
for `crew 7 0` in plan 13's `a-crew-that-clears-up.keys` and every
number in that file comes back unchanged — 145 ticks, the same emptied
pile, the same third of a wall gone. That is `DESIGN.md` § 9's *"same
chassis as the player"* as a measurement.

**H1 put a crew in the world.** `src/helper.loft` holds an NPC vehicle
that is placed, pointed at a hex, and covers **exactly 2.5 hex/s** —
five hexes every three ticks, for ever. `crew <q> <r>` / `send <i> <q>
<r>` / `helper <i> <q> <r>` are the verbs. Suite **832 green**.

⚠ **Two wrong implementations, both of which arrive and look fine.**
Truncating `1.667` to 1 moves a helper at 1.5 hex/s — a 40% loss, the
same trap plan 13 V1 named for the player. Banking without an epsilon
gives 2.333 hex/s and gets worse the longer it runs. Both reach the
far end of a corridor; **only counting the ticks separates them**, so
the gate is the step PATTERN — 1, 2, 2 repeating — and not the
arrival.

⚠ **The chassis is now shared, and that is `DESIGN.md` § 9's opening
line.** `vehicle.loft::drive_along` walks the straight `lat_line` and
stops at the first refused step; the player and every helper read it,
differing only in how far and how high. Extracted before the helper
existed, so the suite proved the extraction behaviour-preserving
(818 green either side) before anything new leaned on it.

⚠ **A helper banks while BLOCKED**, so being stopped costs it distance
and not rate — the tick it is finally pointed somewhere reachable it
leaves with everything it had saved.

## Goal

The work the base needs doing stops being one vehicle's problem.

Plan 13 built a crew of one and measured what it is worth: a base
defended by a sealed wall and a tower falls in 95 ticks unattended and
145 with a player clearing up after it. Everything that crew does — it
clears, it earns, it can be destroyed for standing in the wrong place
— is work an NPC could be doing while the player is somewhere else.
`DESIGN.md` § 9 calls them *"NPC vehicles that do the cooperative's
actual work"*.

⚠ **And they are the mover dryopea has been deferring since plan 12
B0.** Every entity so far has moved a whole number of hexes per tick,
because the tick was DEFINED as an enemy crossing one. A helper is
2.5 hex/s. It is the first thing in the design that does not fit.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 9 (roster, skills,
  damage → wreck → retrieve → recover, stranded), § Carry visibility.
- [`examples/numbers.json`](../../examples/numbers.json) § `helper` —
  every value this plan consumes already exists there. **This plan
  adds no new tunable without a row in that file.**

Source files it touches: a new `src/helper.loft`, plus
`src/vehicle.loft` (the shared chassis — § 9: *"same chassis as the
player"*), `src/spawn.loft` (the tick) and `src/script.loft` (the
verbs).

## H0 — the probe (2026-08-14)

No code, one measurement, and it **narrowed the job rather than
widening it**.

### ⚠ 1. A helper needs FRACTIONAL hexes, not a shorter tick

`CLAUDE.md` § Cost and `spawn.loft` § What a tick is worth both warn
that decoupling speed from the tick makes the tick a **timestep**,
which shrinks the per-tick budget in proportion and re-opens plan 11
F8's incremental rebuild. Read quickly, "helpers force the speed
decoupling" sounds like that whole change.

**It is not.** A helper needs 1.667 hexes per tick *at today's tick
length*. Nothing about it asks for a shorter timestep, so:

- **F8's budget trigger does NOT fire.** The tick stays 667 ms and the
  rebuild that fits in it still fits.
- What is actually needed is a per-mover **progress accumulator** —
  bank `speed × tick_seconds` and step the whole hexes out of it —
  which is the pattern `tower.loft` already uses for a fire interval
  and plan 13 V4 uses for a boost timer. Third instance, same shape.

⚠ So the machinery is LOCAL to the mover that needs it. The enemy
mover does not change, the player does not change (plan 13 V0: both
its speeds are exact), and nothing global moves.

### ⚠ 2. The epsilon is worth 6.7% of a helper's speed, compounding

Measured over nine ticks, banking `2.5 × (1/1.5)`:

| | pattern of hexes per tick | in 9 ticks | effective |
|---|---|---|---|
| bare `floor` | 1, 2, 1, 2, 2, 1, 2, 2, 1 | 14 | **2.333 hex/s** |
| with an epsilon | 1, 2, 2, 1, 2, 2, 1, 2, 2 | 15 | **2.500 hex/s** |

The carry lands on 0.99999999999999956 where it should be 1.0, so a
whole hex is deferred — and the residue DEGRADES each cycle
(…956, …911, …867), so the helper falls further behind the longer it
runs. It is `tower.loft::TOWER_CHARGE_EPSILON`'s trap for the third
time, and this is its most expensive appearance yet: the tower lost a
third of its shots visibly, a boost ran one tick long, and this loses
7% of a speed *silently and forever*.

⚠ **The clean 1, 2, 2 pattern is itself the gate.** A helper that
covers five hexes every three ticks is at 2.5 hex/s exactly; one that
covers 14 in nine is not, and no assertion about "it arrived" would
tell them apart.

### 3. The chassis is shared, so the drive is too

`DESIGN.md` § 9 opens with *"same chassis as the player"*. Plan 13
V1's `vehicle_tick` already walks a straight `lat_line` and stops at
the first step the ground refuses; a helper does the same thing with a
different speed and a different climb. H1 extracts that walk so both
read one implementation — the rule plan 11 F1 states for passability,
applied to movement.

## H2 — the roster works (2026-08-14)

No new mechanic, and that was the phase's shape rather than its
shortcut: clearing, its rate, its reach and what a metre of it is worth
were all decided by plan 13. A phase that had invented a helper-only
job would have gated the JOB; this one can only be read as gating the
ROSTER.

What it built is two extractions and one loop: `vehicle.loft::
salvage_at` (the second half of the shared chassis, after H1's
`drive_along`), `helper.loft::helper_salvage`, and a crew turn in
`wave_tick` that pays into the same wallet.

### ⚠ 1. Two wrong rosters both empty the heap

The unit gate is a RATE for the reason H1's was a step PATTERN — the
obvious assertion cannot see the defect:

- a crew **not in the tick** clears exactly what one helper does, and
  every "the rubble is gone" assertion stays green;
- a crew **sharing one bite** — the shape you get if the tick asks the
  layer once and hands the answer round — also clears exactly what one
  helper does.

Both read the same as the right answer unless you measure how MUCH went
in a fixed number of ticks. So `crew_took(n, 3)` is the whole gate, for
every `n` up to the roster cap.

### ⚠ 2. The base could not see the roster at all, and the reason is arithmetic

The plan's gate said *"the crewed base's clock rises again"*. Measured
on plan 13's base, **it does not move by a tick**: one tower kills about
eight robots over 145 ticks, which is 0.03 m of body a tick, and one
vehicle clears 0.33. The first clearer already has ten times the
capacity the front demands, so a second one has nothing to do.

That is not a defect and it is not the roster failing — it is what
"twice as fast" means when the work is not the bottleneck. The
measurement that CAN see a roster is a base whose work is in two
places, which is where `a-base-on-two-fronts.keys` came from, and the
answer is the table in § Status.

### ⚠ 3. A mirrored base is not a symmetric one — bracing decides

The first two-front base read **112 ticks with a helper east and 211
with one west**, on a map that looks mirror-symmetric. The crew had
nothing to do with it: a wall's END is worth 30% of a braced hex (plan
12 B3), the siege chews where the ROUTE meets the wall (also B3), and
odd-r rows are offset — so the eastern approach fan happened to include
an end hex and the western one did not. One front was breaking 30 HP
while the other chewed 100.

Extending both walls two hexes past the walkable band makes every
REACHABLE wall hex fully braced, and the fronts come back to within
three ticks of each other (214 against 211). ⚠ **So a scenario that
compares two sides of a base has to control for bracing before it can
measure anything else**, and the cost of not doing it is a 99-tick
artefact that reads exactly like a finding.

### What H2 did NOT touch

Helpers still do not block enemies (§ Open questions 2 — `plans/13` V5's
predicate takes one vehicle, and widening it to a roster is H3's), do
not boost, and cannot be hurt in the world (`helper_hurt` exists and
`wave_tick` never calls it). A crew inside a sealed base is still locked
in, and one outside is still locked out — the one-way commitment plan 13
V2 recorded, now with NPCs making it.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **H0** ✓ | the table above | the probe measures what a speed costs before anything banks it | — (H0 asserts arithmetic; H1 is its gate) |
| **H1** ✓ | a helper covers 5 hexes every 3 ticks, forever | speed is a RATE and the remainder is BANKED, never dropped | a helper that covers 1 hex a tick has truncated (2.5 → 1.5 hex/s); one that covers 14 in 9 has no epsilon |
| **H2** ✓ | two helpers clear a heap twice as fast as one; on a base, the second is worth 28 ticks on the OTHER front and 0 beside the first | a roster is N of the same thing, not a special case | a second helper that changes nothing is not in the tick — and it reads identically to one that shares a single bite, which is why the gate is a rate |
| **H3** | a helper destroyed by blocking wrecks where it stood and does NOT respawn | retrieval is the only way back (§ 9) — unlike the player, who always returns | a helper that respawns at the core has been given the player's rule |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **H0** — the probe: what a helper's speed costs | XS | the arithmetic above, measured against `numbers.json` | **Done** |
| **H1** — a helper exists and moves at exactly 2.5 hex/s | S | `tests/14_h1_the_helper.loft` — 5 hexes every 3 ticks, and both wrong implementations are red | **Done** |
| **H2** — a roster that works: helpers clear rubble | S | `tests/14_h2_the_roster.loft` — N helpers clear N times as fast, and three `.keys` scenarios that differ only in their crew lines read 77 / 214 / 242 | **Done** |
| **H3** — wreck, retrieve, recover | M | a downed helper stays down until someone carries it home | Blocked on H1, and on a carry model |

### Why the order is this order

H1 first because every later phase is *"a helper is somewhere, doing
something"*, and a helper that cannot be positioned makes all of them
ungateable — the same argument plan 13 V1 made for the player.

H2 before H3 because clearing is work that already exists
(`vehicle_salvage`, plan 13 V2) and needs no new mechanic, so it
tests the ROSTER rather than a new verb. H3 needs a carry model —
§ Carry visibility says a downed helper is a carried object like loot
or a tower-top — and that is a thing dryopea does not have at all.

## What this plan does NOT build

No ordering and no landers (they need the core's NPC face and a
points debit — `wallet.loft` has a credit but nothing spends), no
skill profiles (§ 9 says validation is *"interchangeable, opaque
id"*), no construction or repair (a construction SITE is a concept
dryopea lacks), no stranded-helper persistence, no carry rendering.

⚠ **The roster is authored, not ordered.** Helpers are placed the way
the player is — by a `.keys` verb — so H1-H2 can measure what a crew
of N is worth without an economy to buy them with. `numbers.json`
§ helper.roster_start (2) and roster_cap (6) are what the eventual
ordering respects; this plan just refuses to exceed the cap.

## Open questions

1. **Does a helper choose its own work?** ⚠ **Answered for now by
   shipping H2 without a decision in it**: helpers clear whatever
   rubble is in reach, so the player's only input is where they STAND
   — which is what makes the two-front table a statement about
   positioning rather than about NPC cleverness. Anything beyond that
   — go to the nearest heap, resume another helper's frozen task —
   is a *dispatcher*, and dispatchers are where NPC AI quietly becomes
   pathfinding for the player. *Recommendation: keep helpers passive
   and positional for as long as possible; make the player place them.*
2. **Do helpers block enemies the way the player does?** § 8's blocker
   exception names *"a player vehicle (or NPC helper)"*, so yes — and
   plan 13 V5's rule is already written in terms of a single blocker.
   *H3's job; it wants the predicate to take a roster rather than one
   vehicle, which is a signature change and not a new rule.*

## See also

- [`plans/13-the-vehicle`](../13-the-vehicle/README.md) — the chassis,
  the salvage mechanic H2 reuses, and V0's note that helpers are what
  force the banking.
- [`plans/12-combat-resolution`](../12-combat-resolution/README.md) —
  § B7's scenarios are H2's gate, as they were plan 13's.
