<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Errands — what a mob is DOING, and when it stops doing it

⚠⚠ **DESIGN, not built.**  What exists today is
[`src/errand.loft`](../src/errand.loft): a robot's "business" is a bare
HEADING, it walks that way until it cannot, and then it is deleted.  This
document is the thing that replaces the heading.

⚠ It sits between two documents that already exist and does not repeat
either: [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) is the GRAPH (what the
economy is), [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) is the MOVER (how a
body crosses a hex).  This is **what a mob is doing between those two**.

## The ask, in one line

> *"they have a resource/hauling/guarding task to fulfil that has a fixed
> route/pattern that can be quite elaborate, but in the action that is
> visible to a player they can deviate in the moment from their set
> plans.  So they move around obstacles like other mobs or players.  And
> in their routine they can get distracted by the player base and come to
> invade it for their specific reasons."*
>
> *"instead of going to sleep the robots get back to a known maintenance
> point once in a while, and insects to their nest."*
>
> — project owner, 2026-08-28

## ⚠⚠ The two scales, and the ratio is the whole architecture  `@X298`

Owner, 2026-08-28:

> *"the economy simulation is inactive on this scale, but runs on the
> common server where each scenario is a snapshot fed from the current
> state"* … *"the whole economy is less detailed than each scenario, it
> holds a far coarser map (1.5 km hexes) with economic activity mapped to
> that."*

| | hex diameter | what a cell holds |
|---|---|---|
| **the economy** (server) | **1500 m** | a node, or a route passing through, and its rate |
| **a scenario** (a sortie) | **1.5 m** (`HEX_DIAMETER`) | ground, walls, mobs — the game |

**A thousand to one, linear.  A million to one, by area.**

⚠⚠ **So a scenario does not contain the graph — it contains ONE CELL of
it, and only part of one.**  Measured against dryopea's own lattice:

| a scenario map | across | of one economy cell |
|---|---|---|
| 40 hexes wide | 52 m | 3.5 % |
| 80 hexes wide | 104 m | 6.9 % |
| 120 hexes wide | 156 m | **10.4 %** — about **1 %** of its area |
| the scrambler bubble (25 hexes) | 32.5 m | **2.2 %** |

Three things fall straight out of the ratio, and each of them is a
design decision the arithmetic makes for us:

1. ⚠⚠ **A NODE IS ALMOST NEVER ON THE MAP.**  A cell holding a mine means
   the mine is *somewhere* in 1.5 km; the odds a 156 m base lands on it
   are about one in a hundred.  **So a node on a scenario map is a
   DELIBERATE choice at sortie selection, never an accident** — *"land
   next to the repair point"* is a decision, and it is the decision
   [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The graph says picking a
   neighbourhood should be about.
2. ⚠⚠ **WHAT YOU GET IS ROUTES, AND THEY CROSS.**  A route through a cell
   is 1.5 km long and your patch covers a tenth of it, so a route either
   crosses your ground or it does not.  That is
   [`SETTING.md`](SETTING.md) § They were on an ERRAND made literal:
   **the player's base is an accident on somebody's commute**, and both
   ends of the commute are off the map.
3. ⚠ **The bubble is 2 % of a cell**, so *where you land inside the cell*
   decides whether it touches a road at all.  § Transport routes already
   calls the bubble the aggro radius; the ratio is why that matters.

### ⚠ What the snapshot actually is

The server hands a sortie **one cell's economic state**, and it is small:

```
cell:      which coarse hex, and what is in it (a node, or nothing)
crossings: for each route through this cell — the bearing it enters on,
           the bearing it leaves on, robots per minute, and the mix
```

⚠⚠ **Nothing in it ticks during the sortie.**  The economy advances on
the server between sorties; a scenario is a frozen read.  That is not a
refusal like [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § What this design
does NOT do's *no economy simulation* — it is a **division of labour**,
and it supersedes that bullet: the simulation exists, it is simply
somewhere else.  ⚠ `@X243` already says a server here is *a persistent
world-state store plus identity*, not a live host, so the advance is
**lazy** — applied when a sortie reads the cell, from the elapsed time
since it was last touched.

⚠ **And the sortie hands back a DELTA**, which is the only thing that
makes the player's interference matter: throughput denied, a node raided,
mobs destroyed for good.  What exactly is in it is § Open questions 2.

## ⚠⚠ The scenario GROWS, and the tracked radius does not  `@X299`

Owner, 2026-08-28:

> *"the scenario can become bigger via player movement, but the radius
> where each mob should be tracked is quite shorter than that"* …
> *"because most mobs have a rule instead of a state during the
> scenario."*

⚠ The first half alone would be **exactly the simulation LOD
[`plans/22`](../plans/22-the-field-cache/README.md) § What this plan does
NOT build refuses**, and refuses for a good reason:

> *"LOD's gate is 'the outcome is unchanged', which is a much weaker
> statement that has to be defended per interaction … **granularity must
> NOT follow the camera.**  If it does, where the player looks changes
> the outcome — the most direct possible violation, and unfalsifiable
> from inside because there is no run to compare against."*

⚠⚠ **The second half is what dissolves it, and it is not a compromise —
it removes the thing the refusal was about.**

### ⚠⚠ A RULE is not a coarser simulation.  It is the same answer, computed instead of stepped

§ What a mob carries makes a mob's destination a **pure function** of
five fields and the world — nothing accumulates, nothing is remembered.
So an un-tracked mob is not being *simulated coarsely*: **its position is
COMPUTABLE at any time, exactly.**

| | plan 22's LOD | this |
|---|---|---|
| what an un-tracked mob has | a **cheaper approximation** of what it would have done | ⚠⚠ **the same answer**, from the rule |
| the gate | *"the outcome is unchanged"* — weak, per-interaction, unfalsifiable | ⚠⚠ **equality** — materialise at radius `R` and at `2R` and the hexes are identical |
| what follows the player | granularity, i.e. the outcome | **only materialisation**, which changes nothing |

⚠ **So the gate is the same shape plan 22 already wrote for itself** —
`11_f8::test_the_field_a_tick_uses_equals_a_fresh_build`, one system
over: *the cheap path equals the full one, exactly.*  ⚠⚠ And *granularity
must not follow the camera* is satisfied **vacuously**: the rule follows
nothing at all.

### ⚠⚠ A mob is a RULE until something makes it a STATE

**And dryopea already owns the thing that makes one.**  The scrambler
bubble cuts a robot's link (`wave_cutoff`), the change is irreversible,
and from that moment the robot has a history — it is one of the wave.

> ⚠⚠ **The tracked set is: everything the bubble has taken, plus
> everything inside an interaction radius.  Everything else is a rule.**

⚠ The boundary is not chosen, it is **read off the interaction radii**,
which is the list plan 22 already gives for this exact purpose — *tower
range 15, bubble 25, nibble reach 1, salvage reach 1, blocker = the same
hex* — and it is measured from **the core and the player**, the two
things that interact, never from the camera.  The bubble at **25 hexes**
is the largest, so it sets the radius.

⚠ And the arithmetic is comfortable: 25 hexes is **32.5 m** against a
streamed map that may be hundreds of metres across (§ The two scales), so
**the tracked disc is a small fraction of the map at any size it
reaches** — which is what makes growing the scenario cheap rather than
quadratic.

### ⚠ Which is also why DEVIATION only exists inside it

The owner's original ask says it: *"**in the action that is visible to a
player** they can deviate in the moment from their set plans."*

⚠⚠ **Deviation is a property of the materialised BODY, not of the
rule.**  Outside the tracked radius nothing can collide with a mob and
nobody can see it, so the rule's straight answer is not an approximation
of its path — it *is* its path.  Inside, the body exists, other bodies
are in the way, and § Deviation applies.

⚠ **The one thing that must be true for this to hold**: a deviation must
not change where a mob ENDS UP, only how it got there.  A sidestep round
a companion resumes the same destination on the next step (§ What a mob
carries: *deviation is free precisely because there is nothing to
resume*), so it cannot accumulate — and **that is the invariant the gate
should be pointed at**, rather than at the radius.

### ⚠ The proximity query itself

⚠ `crawler` measured this one and did not adopt the fix.
`near_mobs_test.loft:3` records the problem — *"crawler stores enemies as
a flat `vector<Enemy>`, so every proximity query (aggro, threat,
**interest management**) is a LINEAR SCAN"* — and the measurement:
a `spatial<Mob[q,r]>` index gives **22× fewer candidates, 344 ms → 39
ms**, with identical near-sets.  ⚠ dryopea stores its roster the same
way, so the same measurement is available and the same index is the
answer if the roster ever grows past an authored wave list.

## What a mob carries

⚠⚠ **Five fields, and the route is DERIVED from them rather than
stored.**  This is `crawler`'s shape, and it is the right one: its
civilians hold three anchor hexes and a clock function
(`src/sim.loft::npc_route`), and **no waypoint list anywhere**.

```
Errand {
    role:  u8,     // HAUL / GATHER / GUARD / TEND / FORAGE …
    home:  Hex,    // the maintenance point, or the nest
    work:  Hex,    // the face, the picking ground, the post
    alt:   Hex,    // the drop-off, or the second patrol leg
    carry: u8,     // what is in the bag — 0 is empty
}
```

⚠ Off-map anchors are **edge crossings** — where the route leaves the
patch — because § The two scales says both ends usually are off the map.

⚠⚠ **Why derived and not stored.**  A stored path has to be maintained:
invalidated when the world changes, resumed after a deviation, saved and
reloaded.  A destination computed from five fields needs none of that —
**deviation is free precisely because there is nothing to resume.**  It
is also the same discipline `pose.loft` already keeps (*read the sim,
never a second flag*) one system over.

### ⚠⚠ THE BAG STEERS, NOT THE CALENDAR

**The single most valuable thing in `crawler`'s AI, and it was measured
rather than reasoned** (`src/sim.loft:2513`):

> *"This used to alternate by day parity … Measured: the picking ground
> is 50 hexes out, a day carries it 13, and the leg flipped every day —
> so it oscillated between home and 13 hexes out, forever.  In 21 days it
> gathered nothing … Keyed on the bag the trip is self-correcting at any
> distance."*

⚠⚠ **dryopea has exactly this hazard and worse**, because a route here
crosses a 1.5 km cell: **any period you could pick is shorter than the
trip.**  So for every role that carries anything:

```
destination = carry > 0 ? alt : work
```

and the loop closes itself at any distance, with no clock at all.

⚠ **GUARD is the one role a clock may steer**, and only because it has no
bag and its legs are equidistant by construction — the same reason
`crawler` sites its guard master so both legs stay in range of the post
(`src/sim.loft:5522`).

### ⚠ Roles are a TABLE, from the first line

⚠⚠ `crawler` splits here and its own comments say the split hurt:
hostiles are pure data over one code path, while civilians are
hard-coded `role == 7` integer comparisons in **eight** places — and
`src/sim.loft:145` records that the role list *"stopped at 6 while 7-12
were in use, which costs a reader real time."*

dryopea's rule is already the other one — `DESIGN.md` § 10's **ONE AI,
per-class DATA**, restated by `ROBOT_ECONOMY.md` § The governing rule as
*an installation that needs its own movement code has broken it*.  ⚠ So a
role is a **row**: which anchor it works from, whether it carries,
whether a clock or a bag steers it, and what draws it off-route.

⚠ **And a row must not promise behaviour the engine does not have.**
Four of `crawler`'s eleven monster flags — `MF_ERRATIC`, `MF_CASTER`,
`MF_BREEDER` and `MF_GROUP` at runtime — are declared in the table and
read by no AI code at all.  That is `@X112`'s *check the number exists
first*, from the other end.

## ⚠⚠ Deviation — the rule dryopea already has, finally expressible

**`crawler` freezes, and it is documented as a defect source, three
times.**  Its `npc_step` takes only a strictly-improving unoccupied
neighbour and otherwise stands still, so:

> *"one corpse in a one-hex gap freezes a worker for the rest of the
> game"* (`STATE.md:850`)

and a militia picket posted *on* a work site *"strangled the road to
it"* — three deliveries became zero (`src/sim.loft:2990`).

⚠⚠ **dryopea must not copy that, and does not have to.**  Plan 11 F7b
already built the rule for field movers and
[`HARD_WON_RULES.md`](HARD_WON_RULES.md) states it:

> **Blocked by a COMPANION → step beside; blocked by the GROUND → stand
> and attack.**

⚠⚠ **What has been missing is not the rule but the FIELD, and the
mover's own comment says so** (`spawn.loft:1222`):

> *"A heading has no alternatives: there is one hex ahead … and it is all
> approach mode can honestly do, **having no field to say which way
> beside is**."*

**A route supplies it.**  The moment a mob has a DESTINATION rather than
a bearing, *beside* is well defined — the neighbours that do not increase
the distance to it — and F7b's rule becomes expressible for errand
robots with no second steering mode.  ⚠ That is why this document adds
**no mover**: `ROBOT_ECONOMY.md` § What this design does NOT do's third
bullet survives intact.

### ⚠⚠ One actor, ONE occupancy rule

`crawler` gave its civilians an occupancy test and its hostiles none, and
then had to write down why (`src/sim.loft:2608`):

> *"A first cut refused an occupied neighbour, and the raider never took
> a step in any world: it spawns inside its own den, where the ONE
> improving neighbour is a sleeping kinsman … One actor cannot have two
> contradictory occupancy rules across its two states.  **A sleeping
> monster is not terrain.**"*

⚠ dryopea has the same trap waiting: an errand robot and a cut-off robot
are **the same robot**, one bubble crossing apart.  They must ask
`occupancy_taken` the same way or the transition is a behaviour change
nobody authored.

## ⚠⚠ Distraction — and the failure that eats the whole feature

**`crawler` measured this and the measurement is the warning**
(`src/sim.loft:3656`):

> *"⚠ AN INCURSION DOES NOT BREAK FORMATION FOR A HERO IT HAS MERELY
> SEEN … Measured without this: both raiders woke on the way, converged
> on a hero standing near the town and parked there for seven days — the
> picking ground was unsafe for 0 ticks, the raid never arrived, and the
> whole mechanism **silently became 'monsters walk at the player', which
> the game already had**."*

⚠⚠ **That is the failure mode for this entire design.**  If a mob on a
route is drawn to the base easily, the routes are scenery and dryopea is
the game it already is.  So the rule:

> ⚠⚠ **A DISTRACTION MUST BE CAUSED BY SOMETHING THE PLAYER DID OR
> BUILT, NEVER BY THE PLAYER BEING SEEN.**

⚠ dryopea already has exactly one distraction and it is the right shape:
the scrambler bubble cuts a robot's link (`wave_cutoff`) and it never
goes back.  ⚠⚠ **That one is not caused by the player being seen either —
it is caused by the thing they landed.**

### What each role comes for

⚠ Each row is a reason the player can SEE, and each has a counter-play
that costs something — which is `DESIGN.md` § What kind of game this is
at the routine layer.

| role | why it leaves the route | what it does at your base | the counter-play, and its cost |
|---|---|---|---|
| **hauler** | your salvage heap reads as **spilled cargo** on its route | picks it up and carries it home — **it steals your income** | clear the heap early; that is a trip in the worst place |
| **gatherer / miner** | your base is standing on ground it was sent to work | cuts your **wall** as if it were the face | do not build across the seam it came for; the good ground is where it is |
| **builder** | it read the core as a **damaged peer** (`SETTING.md` § They approach to REPAIR) | the nibble — already the game | the jammer switch, which costs the supply (`@X280`) |
| **guard** | something crossed its post | follows it back, and arrives escorted | do not cross the post; the post is where it is |
| **insect** | the **odour-sac alarm** from a tapped tree (`SETTING.md` § The ants) | swarms toward the alarm, and your base is in the way | do not tap that tree yet; the sap is worth taking |

⚠⚠ **The hauler row is the one worth building first.**  It makes
`@X053`'s harvester cargo two-sided — the richest salvage on the field is
now also the thing that *attracts more traffic if you leave it lying* —
and it puts a decision in the player's hands at a moment when acting on
it costs them a trip.  That is the design's own test, met by a mechanic
that needs one field (`carry`) and one rule.

## ⚠⚠ Home is a PLACE, not a despawn

Owner: *"instead of going to sleep the robots get back to a known
maintenance point once in a while, and insects to their nest."*

⚠ Today `errand_depart` **deletes** a robot that can go no further, and
its own comment defends the deletion: it *"REMOVES rather than killing"*
so the wallet is not paid for traffic the player never touched.  ⚠⚠ **The
conservation is right and the PLACE was missing.**  Under a home node the
robot still leaves the roster — it just leaves it *somewhere*, and three
things follow that a deletion cannot give:

- **the player can see where they go**, which is the only way a route is
  legible at all;
- ⚠⚠ **`damage_persistence` becomes visible** —
  [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § 5 already designs it: *"a
  wounded robot that survives the bubble walks home, is fixed, and
  returns whole"*, which is what **makes *kill* and *hurt* different
  verbs for the first time**;
- **a nest is the same node with a different label**, so the insect tier
  costs a row rather than a system.

⚠ And the return trip is what makes a base near a maintenance point
attritional in exactly the way § 5 describes — the same robots come back.

## What this design does NOT do

⚠ Named so a later reader does not think they were forgotten.

- ⚠⚠ **No node inventories, production or depletion inside a sortie.**
  Those are the server's, between sorties (§ The two scales).  A mine on
  your map does not run dry while you watch.
- **No new mover.**  The deviation is F7b's existing rule given a
  destination to be *beside* relative to.
- **No re-planning.**  Everything is recomputed from the five fields
  every step; a deviation ends when the obstacle does, and nothing
  remembers it.  ⚠ `crawler` proves this is enough — its
  safety veto resumes with no state at all — and it is the same reason
  `hex_ground` beats a second painted layer.
- **No de-aggro.**  A robot the bubble has taken never goes back
  (`errand.loft` § The bubble is the whole mechanic), and nothing here
  changes it.
- **No aggro at all in the *seen you* sense.**  See § Distraction.
- **No fear, flee, or morale.**  `crawler` specs them
  (`DESIGN.md:790`) and has never built them; dryopea has `@X119`'s
  refusal of a route to winning by personal power, which points the same
  way.
- ⚠⚠ **No simulation LOD, and § The scenario GROWS is not one.**  An
  un-tracked mob is not simulated coarsely; it is not simulated at all,
  because its answer is computed.  [`plans/22`](../plans/22-the-field-cache/README.md)'s
  refusal stands exactly as written and this design does not ask for an
  exception to it.

## ⚠ Cost, and where it lands

⚠⚠ **A destination per mob is a flow field per destination**, and
[`plans/22`](../plans/22-the-field-cache/README.md) is where that bill
arrives.  `crawler` pays it with a cache keyed by
**(destination, movement class)**, sized `3 * len(enemies) + 16` —
derived from *"an actor can ask for at most three destinations"*
(`src/sim.loft:3216`).  ⚠ dryopea's `Errand` has exactly three anchors,
so the same derivation gives the same cap.

⚠⚠ **And `crawler`'s cap trap transfers verbatim**: a flat
`FLOWD_MAX = 24` against a shipped demand of **38** meant *"nine
townsfolk walked home on a straight line every night"* — **a silent
fallback to greedy, not an error** (`STATE.md:297`).  A cap that is
crossed must go RED.

⚠ **But dryopea may not need a field at all for a first cut.**  Its
errand robots walk a heading with no field today, and a route across a
patch is mostly a corridor.  ⚠⚠ `crawler` measured that **greedy is not
pathing** — *"it walked into the first concave obstacle and stopped
there — permanently … in 21 simulated days delivered nothing"*
(`src/sim.loft:3129`) — so the honest statement is: **greedy plus the
F7b sidestep is fine while a route is roughly straight and is a defect
the moment it is not**, and which of those a real map is, is a
measurement rather than an argument.  That is the probe this work should
open with.

## Open questions — the owner's

1. **Is a route's mob population a POOL or a TAP?**  A fixed set of
   fourteen haulers on this cell that killing depletes, or a rate that
   keeps producing?  ⚠ It decides whether the player can *see* their
   interference during a sortie or only in the delta afterwards.
   *Recommendation: a TAP during the sortie and a POOL on the server* —
   the cell's rate is frozen (§ The two scales), and what the player
   destroyed is in the delta.
2. **What is in the delta a sortie hands back?**  Throughput denied,
   nodes raided, mobs destroyed for good, cargo taken?  ⚠ It is what
   makes a campaign out of a sequence of bases, and it is the same
   question `ROBOT_ECONOMY.md` § Open questions 4 asks about waking the
   military.
3. **Do insects use this system or their own?**  Their pattern is
   foraging around a nest rather than an A→B haul.  *Recommendation: the
   same system, one more role row* — `DESIGN.md` § 10's ONE AI rule, and
   a nest is already a home node.
4. **Does a mob's route persist across a save?**  ⚠ § A mob is a RULE
   until something makes it a STATE mostly answers it: an un-tracked mob
   is re-derived on load and nothing is lost, because there was nothing
   to lose.  ⚠⚠ **What DOES have to be saved is the tracked set** — the
   robots the bubble has taken have a history, and `carry` is state a
   hauler accumulated.  A planet remembers the ground and the markers
   (`persist.loft`) and a RUN is not in it, so this is the first thing
   that would ask it to be.
5. ⚠ **Where does a route's corridor come from inside the patch?**  The
   coarse cell gives an entry bearing and an exit bearing (§ What the
   snapshot actually is); the line between them is a straight one unless
   something authors otherwise.  ⚠ A road painted on the map is the
   obvious answer and dryopea has no road kind — which is `@X112` again:
   check the thing exists before designing against it.

## See also

- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) — the graph, the six
  installation types, and the per-edge parameters this instantiates.
  ⚠ Its § What this design does NOT do's *no economy simulation* bullet
  is superseded by § The two scales: the simulation exists, on the
  server.
- [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) — the mover, unchanged.
- [`SETTING.md`](SETTING.md) § They were on an ERRAND, § The ants are the
  insect tier — the fiction every role row is read out of.
- [`src/errand.loft`](../src/errand.loft) — what exists today: a heading,
  and a deletion.
- `../crawler/src/sim.loft` — the reference implementation this document
  learns from, and the four places it says what NOT to do.
