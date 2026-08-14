<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 13 — The vehicle: a player in the world

**Value:** `G` · **Effort:** `MH`

## Status

**COMPLETE — V0 through V5 shipped** (2026-08-14). The player exists,
drives, clears up, boosts, gets paid, and can now be destroyed.

**V5 gave standing somewhere a price**, closing the caveat every phase
since V2 had carried: *"nothing can hurt the vehicle yet, so standing
in a kill zone is currently free — it is not meant to be."*

⚠ **The whole phase is one condition, and it is not "standing on a
route".** `DESIGN.md` § 8 says an enemy attacks a blocker *and* that
*"in the absence of a blocker, the player and NPCs are ignored"*. Both
clauses come out of the same three-part rule: the vehicle is on a
closer step, every other closer step is taken, **and every sidestep is
taken too**. In the OPEN an enemy simply walks round (plan 11 F7b) and
nobody is hurt; in a **narrow entrance or a kill funnel** — the two
examples § 8 gives — there is nowhere to go and the player is a
liability. **The mechanic is emergent from the map**, not a rule about
parking. Falsified: dropping the third condition makes an open-field
player take damage and turns that assertion red.

⚠ **A companion is never attacked for the same obstruction**
(`occupancy.loft`), which is why the blocker exception needed its own
predicate rather than reusing occupancy. Two robots jammed nose to
tail cost the player nothing.

⚠ **It cannot tank, on a clock the player can compute.** 100 HP at
5 HP/s is twenty seconds for one attacker and five for four — measured
at 20.7 s — so blocking buys a moment and costs the trip home.
`DESIGN.md` § 8's *"the vehicle cannot tank for the core"* is that
arithmetic and nothing else.

⚠ **A respawn puts the player back where the wave is going.** Death is
never a game-loss (§ 8), so the vehicle reappears at the core, whole —
and in a corridor it is blocking again on the next tick. The design
says a respawn also starts the launch countdown, with driving out the
only way to cancel; **that half is NOT built**, because there is no
scramble in dryopea yet. Today the player simply rematerialises in
front of the wave.

**V3 gave the wallet its first income.** Clearing wreckage credits 20
points a metre — derived, not picked: the design prices a kill at 10
points (`enemy_regular.loot_value`) and a body is 0.5 m
(`body_height`), so 20/m is the one number that makes those the same
statement. Suite **805 green**.

⚠ **Masonry pays nothing, and that is what `height.loft`'s SOURCE was
for.** It has stored one per pile since plan 12 B1 under a note saying
*"nothing reads it back yet"*; this is its first reader. A dead
machine is salvage and a heap of your own broken wall is a mess you
made — paying for masonry would have made demolishing your own
perimeter an income stream.

⚠ **Plan 12 B6's promise survives, enforced differently.** B6 shipped
with no credit verb at all and credited the invariant *"the wallet
never refills unattended"* to that absence. There is a verb now, and
the promise holds because **the only caller needs a vehicle**. Proved
by falsification: crediting a flat point per tick turns FIVE of B6's
own assertions red, including its unattended base reporting *"the
wallet never refills unattended, 200 -> 200.33"*. An invariant held by
a mechanism rather than by an absence.

⚠ **A crew now pays twice**, and the halves are separable because V2
shipped without the second:

| base | clock |
|---|---|
| sealed wall + tower | 95 |
| + a crew that only DENIES the ramp (V2) | 121 |
| + a crew that is also PAID for it (V3) | **145** |

Eight dead robots is 4 m of wreckage — 80 points against a 200-point
budget. A gun that cost the player a base now funds one.

**V4 gave the crew a way out and back.** Boost is the same height rule
with a bigger number — `hover_clearance_boost` is 3.0 m and a `wall` is
3.0 m — so a boosting vehicle crosses a wall, a 5.0 m `wall_high`
still refuses it, and § 8's *"can cross steep_rock, walls, closed
perimeters"* is arithmetic rather than a special case. Four hexes a
tick for three ticks, then eight ticks of cooldown. Suite **795
green**.

⚠⚠ **V0 recorded a blocker that was already solved, and that is worth
more than the phase.** V0 § 3 said a climb changing with STATE could
not be expressed, because `climb_limit(kind)` is a function of CLASS —
and named it V4's blocker. The answer was in the tree the whole time:
`passable.loft::can_climb` takes the climb **directly**, built by plan
11 F7 for the desire field, which needed the same shape for a
completely unrelated reason (*"walls are passable"* is *"the climb does
not bind"*). The rule needed no change at all. **The blocker was a
survey that had not been done** — and V0 is exactly the phase whose job
that survey was.

⚠ **The trap V4 actually paid for.** A boost is 2.0 s and a tick is
`1 / 1.5` s, so three ticks sum to 1.9999999999999998 and a bare
`> 0.0` hands the player a FOURTH tick — a third more boost than the
design says. Exactly `tower.loft::TOWER_CHARGE_EPSILON`'s trap on a
timer instead of a fire interval, and **no assertion about getting over
the wall could ever have seen it**; the tick-count test is what does.

⚠ **The cooldown is the cost, and nobody designed the ratio.** A crew
that boosts out to clear a 1.5 m ramp works for five ticks and is then
stranded in the open for five more waiting on its ride home. The
dangerous part of the trip is the waiting, and it falls straight out of
two `numbers.json` rows meeting.

**V2 closed the loop plan 12 B7 opened.** A vehicle clears rubble it is
standing on or beside, at one dead robot a second, with no key pressed
— and the tower stops being a liability:

| base | clock | what happened |
|---|---|---|
| no defences | 61 | they walked in |
| sealed wall | 104 | they chewed through |
| sealed wall + tower | 95 | **the pile went over** — the tower LOST 9 ticks |
| sealed wall + tower **+ a crew** | **121** | they chewed through, and the tower GAINED 17 |

⚠ **The wall's HP is the mechanism; the clock is only the
consequence.** Without a crew that wall ends at **96.7 of 100** —
nobody ever had to break it. With one it ends at **69.3**. The crew
adds no damage whatsoever; every extra second comes from denying the
attackers a staircase.

⚠⚠ **V2 could not meet its own gate as written, and that is its second
output.** The gate said *"the towered base's clock RISES"*. A crew
INSIDE that base changes nothing at all — 95 ticks either way. The
ramp forms where enemies die, which is OUTSIDE the wall, and a hover
unit climbs 0.4 m against a 3.0 m wall: **a sealed base locks its own
crew in.** Two more configurations, both measured, both flat:

- the same wall with a GATE: 90 ticks with a crew and 90 without.
  Where there is a way in, the ramp was never what got them in.
- a crew parked in the gateway clears the heap perfectly (1.0 m →
  0.0 m) and the clock does not move by a tick.

So the mechanic pays **only where the ramp is the way in**, and
reaching it needs `hover_clearance_boost` — 3.0 m, exactly a wall.
`a-crew-that-clears-up.keys` is therefore a player who committed:
parked outside before the wave and with no way back. **V4 is what
makes it a decision rather than a commitment**, which promotes it from
the phase nobody needed to the one the story turns on.

⚠ **The reach is FORCED, not chosen.** The obvious rule is § 11's
*"drive over loot = auto-pickup"* applied to rubble — stand on it and
clear it. The vehicle cannot get on: 0.4 m of clearance against a
1.5 m heap. So it clears what it is BESIDE, which is what § 7 §
Tower-core retrieval already calls *"drive next to it"*, and a heap is
eaten from the edge until it is low enough to drive onto and finish
from on top. One rule, because the reach covers its own hex too.

**V1 put a player in the world.** `src/vehicle.loft` holds a hover
unit that parks, is pointed at a hex, and covers **two hexes a tick**
to a robot's one — read from `numbers.json` § player_vehicle.speed_
normal rather than counted in steps. `park <q> <r>` / `drive <q> <r>`
/ `vehicle <q> <r>` are the verbs. Suite **774 green**.

⚠ **The trap this phase was built around**: a tick is *defined* as
what a robot takes to cross a hex, so "one hex per tick" is the shape
of every other mover in the codebase and would have looked entirely
correct while silently halving the player. The gate runs BOTH movers
over the same ticks in the same world, so it compares the game against
itself rather than against a number somebody typed — and falsifying it
(forcing one hex per tick) turns exactly that assertion red.

⚠ **There is no vehicle passability code, and that is the phase's
second claim.** The rule is `passable.loft`'s and the player is a
third KIND with a climb of 0.4 m — so a `wall` stops it with no branch
anywhere naming a wall, and a pile at exactly the clearance is
drivable while one a hand's breadth deeper is not. Deleting the
`can_step` call turns three assertions red.

⚠ **`vehicle_tick` takes the tick's DURATION as a parameter**, and the
compiler is what settled it: `TICK_SECONDS` lives in `spawn.loft`,
which `use`s `vehicle.loft`, so reading it from there is a cycle. The
error was right about the design — the wave engine may know about the
player, and the player must not need the wave engine to move.

## Goal

The player exists in the world, drives, and does the one thing that
makes a tower worth building: **clears up after it**.

Plan 12 ended with a measurement nobody planned and nobody can act on:
a base defended by a sealed wall and a tower falls **sooner** than the
same base with the wall alone, because the tower's own dead pile into a
ramp over it. `plans/12` § B7 called the missing piece *"a crew to
collect bodies, which arrives with the vehicle"*. This plan is that
crew's first member.

⚠ **The clock B7 already measures is this plan's gate.** `tests/12_b7_
the_clock.loft` plays three authored bases and compares their
time-to-zero; a vehicle that clears bodies must move the towered base's
number **up**, and nothing else about the scenario changes. That is a
gate this plan did not have to build and cannot argue with.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 8 (the vehicle: hover,
  boost, noncombatant, blocker exception), § 11 (position triggers, not
  key presses; the handful of keys), § Carry visibility.
- [`examples/numbers.json`](../../examples/numbers.json)
  § `player_vehicle` — every value this plan consumes already exists
  there. **This plan adds no new tunable without a row in that file.**

Source files it touches: a new `src/vehicle.loft`, plus
`src/passable.loft` (a third kind), `src/spawn.loft` (the tick),
`src/wallet.loft` (a credit, at V3), `src/height.loft` (V2 takes a
BITE out of a pile through `height_raise`, not `height_clear` — the
whole-pile verb still has no caller) and `src/script.loft` (the
verbs).

## V0 — the probe (2026-08-14)

No code, four measurements, and **two of them inverted what this plan
was about to assume**.

### ⚠ 1. The vehicle is the LAST entity whose speed fits the tick

`docs/DESIGN.md` § Speed must NOT be tied to the tick, and the ledger
in `plans/12` § What it needs that does not exist, both imply the
vehicle forces the speed-decoupling work — a second mover that banks
progress the way `tower.loft` banks a fire interval. **Measured
against `numbers.json`, it does not.** A tick is `1 / 1.5` s, so:

| entity | `numbers.json` | hexes per tick | fits a whole-hex tick? |
|---|---|---|---|
| enemy regular | 1.5 hex/s | 1.000 | ✓ |
| **player vehicle** | **3.0 hex/s** | **2.000** | **✓** |
| **vehicle, boosting** | **6.0 hex/s** | **4.000** | **✓** |
| helper | 2.5 hex/s | 1.667 | ✗ |
| boss (phase 3) | 1.0 hex/s | 0.667 | ✗ |

So the vehicle moves **exactly two hexes a tick** and needs no banking
whatever. What forces the decoupling is the **helper** (§ 9) and the
boss — and neither is this plan's. That is worth knowing before V1,
because building a progress-banking mover here would have been
machinery with no case to justify it, gated by nothing.

⚠ The corollary is the trigger to write down: **the day a helper moves,
the tick stops being a hex.** `spawn.loft` § What a tick is worth
already names the pattern to reuse (`tower.loft`'s charge, epsilon
included).

### ⚠ 2. Passability needs no new rule — the vehicle is a CLIMB

`src/passable.loft` line 287 anticipated this in as many words: *"The
day a class reads `walk_vehicle` instead, this gains a kind."* Two
things the probe found on top of it:

- **`walk_vehicle` is `true` for every one of the twelve palette
  entries** — sea, water and `steep_rock` included. The vehicle
  hovers, so the SURFACE question is a no-op for it and the **height
  step is its whole passability**. One field of the two that
  `passable.loft` asks does all the work.
- Which means the vehicle's climb is its **hover clearance**, straight
  out of `numbers.json`: 0.4 m idle. `climb_limit` already keys on a
  kind, so this is a constant and a branch, not a mover.

⚠ **And the numbers land exactly on the design's claims.**
`hover_clearance_boost` is 3.0 m and a `wall` is 3.0 m, so a boosting
vehicle clears a wall *at the limit* and a 5.0 m `wall_high` not at
all. § 8 says boost *"can cross steep_rock, walls, closed
perimeters"* — true, and the anti-insect barrier turns out to be
anti-PLAYER too. Nobody wrote that down; it falls out of the two
numbers meeting.

### ⚠ 3. But BOOST breaks the shape `climb_limit` has

`climb_limit(kind: u8)` is a function of CLASS. The vehicle's climb is
0.4 m idle and 3.0 m boosting — a function of **state**, which that
signature cannot express. It is the first mover in dryopea whose
passability changes during its own life.

Not V1's problem and named here so V4 does not discover it: the fix is
a climb that is *passed* rather than *looked up*, or a second kind for
"boosting", and the choice wants measuring rather than picking. ⚠ It
is also the same shape the design's *"a damaged robot moves slower"*
has, so whatever settles it settles that too.

### 4. Where it lives: on `WaveState`, and the name is now a scar

`WaveState` already carries the rubble layer, the damage ledger, the
towers and the run's wallet — everything runtime that a tick threads.
The wallet's own note says why: *"a second runtime container holding
one float would be a second thing every caller has to remember to
pass."* A vehicle is the same argument again.

⚠ The honest cost: the struct is named for the wave and now holds the
player. A rename to `RunState` is a mechanical change across ~60 call
sites and it is **not** this plan's — recorded so the next person does
not read the name as a claim.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **V0** ✓ | the speed table above; `walk_vehicle` uniformly true | the probe records what IS, before anything assumes it | — (V0 asserts the present; V1 is its gate) |
| **V1** ✓ | the vehicle covers 2 hexes while an enemy covers 1, over the same ticks | speed is a RATE, read from `numbers.json`, not a step count | a vehicle that moves 1 hex a tick has silently adopted the enemy's rate; a vehicle that reaches a hex `can_step` refuses has no passability at all |
| **V2** ✓ | driving BESIDE a pile clears it, and the towered base's clock rises 95 → 121 | the crew is what makes a tower pay | ⚠ the gate's own wording was falsified: a crew INSIDE the sealed base moves the clock by nothing, and so does one at a gate — the mechanic pays only where the ramp is the way IN |
| **V3** ✓ | a collected body credits the wallet; the wallet can go UP, above its starting budget | loot is income, and it is the first thing that ever refills the budget | ✓ refused — crediting a flat point per tick turns five of B6's assertions red; and masonry paying would make demolishing your own wall an income |
| **V5** ✓ | an enemy with a way round ignores the player; one with none attacks at 5 HP/s | blocking is a property of the MAP, not of parking | ✓ refused — drop the way-round condition and an open-field player takes damage; and a COMPANION blocking the same hex is never attacked |
| **V4** ✓ | boosting clears a 3.0 m `wall` and never a 5.0 m `wall_high` | boost is a bigger CLIMB, not a new movement mode | a boost that crosses `wall_high` has stopped reading the height; one that lasts FOUR ticks has no epsilon |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **V0** — the probe: speed, passability, and where it lives | XS | measurements against `numbers.json`; three of the four answers are recorded above | **Done** |
| **V1** — the vehicle exists, and it drives | S | `tests/13_v1_the_vehicle.loft` — two hexes to an enemy's one, and it stops at what it cannot climb | **Done** |
| **V2** — it clears rubble, and a tower starts paying | S | `tests/13_v2_the_crew.loft` + `a-crew-that-clears-up.keys` — 121 ticks against 95, and the wall is CHEWED rather than climbed | **Done** |
| **V3** — a body is worth points | S | `tests/13_v3_loot.loft` — wreckage pays and masonry does not, the wallet exceeds its starting budget, and an unattended run still earns nothing | **Done** |
| **V5** — the blocker exception: standing somewhere costs | S | `tests/13_v5_the_blocker.loft` — ignored in the open, a liability in a chokepoint, destroyed in twenty seconds, back at the core whole | **Done** |
| **V4** — boost | S | `tests/13_v4_boost.loft` — clears a `wall`, refused by a `wall_high`, three ticks exactly, and a crew that boosts out of a sealed base, clears, and comes home | **Done**.  ⚠ Promoted by V2, and its stated blocker turned out to be already solved — see § Status |

### Why the order is this order

V1 before everything because a vehicle that cannot be positioned cannot
trigger anything — every later phase is *"drive somewhere and something
happens"*, which is § 11's whole input philosophy (**position triggers,
not key presses**).

V2 before V3 because clearing is the mechanic plan 12 measured a need
for, and its gate already exists. V3 adds an income the wallet has
never had, and B6 deliberately left no verb for it — so it is a
contract change and wants its own phase.

V4 last because § 3 above says it needs a decision `climb_limit` cannot
currently express, and everything before it can be built without one.

## What this plan does NOT build

No helpers (§ 9 — and they are what forces the speed decoupling, so
they want that work first), no wall-paint mode, no tower repair /
boost / ordering, no force-launch or scramble, no carry rendering, no
camera.  (The blocker damage model WAS on this list; V5 built it.) **No 3D and no GL**: the vehicle
is simulation state driven by `.keys`, exactly as the wave engine was
for plans 11 and 12, and it reaches a player's hands the day a play
mode exists.

⚠ **Consequence worth stating: this plan gives the player no way to
DIE.** § 8's blocker-damage model is the only thing that can hurt a
vehicle and it is not built, so V1's vehicle is invulnerable and the
run still ends only at the wallet. That is the right order — plan 12
made the wallet the single end state on purpose — but it means "the
player is safe" is a temporary property of an unfinished plan rather
than a design claim.

## Open questions

1. ~~**How does a climb that changes with STATE get expressed?**~~ —
   **ANSWERED by looking: it already was.**
   `passable.loft::can_climb` takes the climb directly and plan 11 F7
   built it. `vehicle_climb(v)` returns 0.4 or 3.0 and hands it over;
   `climb_limit(kind)` is untouched and stays the convenience for
   callers that have a kind. ⚠ The design's *"a damaged robot moves
   slower"* is a SPEED question and this does not settle it — V0
   claimed it would, and that was one guess too many.
2. ~~**Does the vehicle collect a body, or clear a pile?**~~ —
   **DECIDED in V3: one act, priced per METRE**, as recommended, with
   one addition the phase found. Clearing IS collecting, at 20 points
   a metre, and `vehicle_salvage` returns a `Salvage { metres,
   source }` so the caller can price it. The addition: the SOURCE
   decides whether it pays at all, which made `height.loft`'s
   long-unread source field this phase's most useful existing part.
   ⚠ A pile is named by its NEWEST deposit, so masonry dropped onto
   wreckage makes the whole heap worthless — the layer's
   one-source-per-hex simplification showing through, and what plan 06
   S1's stacked layer eventually fixes.
3. **What drives it in a `.keys` script?** A destination is
   pathfinding and § 11 says the player DRIVES. *Recommendation for
   V1: `drive <q> <r>` walks the straight `lat_line` and stops at the
   first step the height rule refuses — which is what driving into a
   wall does, needs no route, and maps onto WASD later.*

## See also

- [`plans/12-combat-resolution`](../12-combat-resolution/README.md) —
  § B7 is the measurement that motivates this plan, and its scenario is
  V2's gate.
- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  instrument every phase here asserts through.
- [`plans/05-validation-scenario`](../05-validation-scenario/README.md)
  — the consumer; its minimum-playable thing needs a player.
