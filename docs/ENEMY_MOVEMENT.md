<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Enemy movement — the spec

How an enemy decides where to go, what stops it, and what it does
when it is stopped.  Split out of [`DESIGN.md`](DESIGN.md) § 6 on
2026-08-12, when it outgrew a subsection.

**Scope.**  Enemies only.  The player vehicle's movement + input
philosophy is [`DESIGN.md`](DESIGN.md) § 11, and what an enemy
*shoots* — target priority, nibble rates — stays with combat in
[`DESIGN.md`](DESIGN.md) § 7 § Enemy targeting + nibble.  This
document is about getting there.

**Where the pieces live.**  The rules are here; the numbers are
[`NUMBERS.md`](NUMBERS.md); what they cost to build is
[`plans/11-flow-field`](../plans/11-flow-field/README.md).

## Contents

- [Two modes, one passability rule](#two-modes-one-passability-rule)
- [Where `height` comes from](#where-height-comes-from)
- [Wall climbability per enemy type](#wall-climbability-per-enemy-type)
- [Bodies are terrain](#bodies-are-terrain)
- [The tick resolves once](#the-tick-resolves-once)
- [Sealing the perimeter is punished, not forbidden](#sealing-the-perimeter-is-punished-not-forbidden)
- [A wall's HP is structural, not a constant](#a-walls-hp-is-structural-not-a-constant)
- [The siege front is the WALL's width](#the-siege-front-is-the-walls-width)
- [Retaliation — designed, not built](#retaliation--designed-not-built)
- [What a broken wall leaves](#what-a-broken-wall-leaves)

## Two modes, one passability rule

**Steering has two modes** ([`DESIGN.md`](DESIGN.md) § 6
§ Multi-direction spawn markers defines the marker + heading):
outside the scrambler bubble an enemy follows its spawn marker's
heading; inside it, the flow field toward the core.  The bubble
boundary is the handoff.

⚠⚠ **Since BACKLOG C3 the mover asks whether the robot is CUT OFF,
not where it is** — `spawn.loft::enemy_engaged` is
`!errand && enemy_in_bubble`.  While the core is jamming the two are
the same reading, and structurally so: `wave_cutoff` sweeps the roster
at the top of every tick, so anything inside the bubble has already
lost its errand by the time the mover asks.  A core switched OFF
separates them — a robot crossing a dark bubble on its business is
inside it and is not one of the wave, and it walks straight through.

**Passability has none, and it is a HEIGHT STEP — not a material.**
An enemy may enter a neighbouring hex when

```
height(to) - height(from)  <=  climb(class)
```

and that single rule covers everything the wall table used to say
separately.  A `wall` at 3 m stops a robot because the step is 3 m;
an insect's climb limit reaches it (§ Wall climbability, next), so the
per-class table *is* a table of climb limits.  Nothing needs to
know what a wall is made of.

Three behaviours at a step too tall:

| class | at a wall | effect on the world |
|---|---|---|
| robot (normal) | **stops**, and attacks it | none |
| insect | **climbs** — its limit covers the step | none |
| boss | **breaks through** (§ Wall climbability) | the wall is gone, for everyone |

An enemy never halts permanently.  Blocked, it attacks — because
it still wants the core.

### Where `height` comes from

| source | value | state |
|---|---|---|
| structures | `height_override` — `wall` 3.0 m, `wall_high` 5.0 m (`extrusion_kind` pillar / cliff) | in the palette today |
| terrain | the slope solver ([plan 02](../plans/02-solver-validation-viewer/README.md)) — `slope` and `drop` describe terrain SHAPE, not a step | **not built** |
| rubble | accumulated pile height — bodies (§ Bodies are terrain) and broken walls alike | runtime, never saved — the layer is [`src/height.loft`](../src/height.loft) |

Until the solver lands, terrain contributes nothing, so every
impassable thing the map itself puts down is a structure.  A hill too
steep to climb waits on plan 02.

The **runtime** row exists: `HeightLayer` is a sparse map of metres
added to whatever the palette paints, it rides on the wave rather than
on the world, and the step rule reads the sum.  Nothing drops a body
into it yet — combat is what will — so a `.keys` script's `raise <q>
<r> <metres> [source]` is the only thing filling it today.  What that
already buys, with no code beyond the arithmetic: a 3 m pile beside a
5 m `wall_high` leaves a 2 m step, and an insect climbs 3.

A hex carrying a pile stands on **`rubble`** (palette 11), which is a
ground type so that this rule stays one rule — `walk_ground` is read
with no branch for debris.  ⚠ It is a **layer over the map, never a
repaint**: the painted ground underneath is untouched, so clearing a
pile restores exactly what was authored.  That is also what lets a
wall break into a way THROUGH rather than into sea; see
[`docs/GROUND_TYPES.md`](GROUND_TYPES.md) § 11. rubble.

⚠ **`walk_ground` is NOT passability.**  `wall` and `wall_high`
both carry `walk_ground = true`, and that is correct — the
walkable thing about a wall is its *top*, which is how enemies
come to be up there at all.  It describes the **surface**, not
whether anything can get onto it.  An implementation that used it
as the passability predicate would let robots walk straight
through a 3 m wall, which is the exact failure this whole rule
exists to prevent.  Measured 2026-08-12 (plan 11 F0).

## Wall climbability per enemy type

Heights aren't just visual variety:

| Wall type | Stops regular robots | Stops boss robots | Stops insects |
|---|---|---|---|
| `wall` (3 m) | Yes (sheer) | No (2×2 forces gaps or break) | **No — climbs easily** |
| `wall_high` (5 m) | Yes | No (still 2×2) | **Yes — anti-insect barrier** |

`wall_high` is **vital** when insects are part of the threat.
Mixed perimeters (`wall` on robot sides, `wall_high` on
insect-facing sides) become tactical.

## Bodies are terrain

An enemy body stays where it falls until the player collects it
(drive-over pickup, [`DESIGN.md`](DESIGN.md) § 7 § Loot).  **A body
raises its hex's effective height, and bodies accumulate.**
Everything else follows from the height-step rule above, with no
special case:

⚠ **Live since plan 12 B4.**  An enemy has HP, dies when it runs out,
and leaves `numbers.json` § enemy_regular.body_height (0.5 m) of
wreckage — carapace, for an insect — on the hex it fell on.  Death is
two effects and both land: the hex is FREED (a corpse holds nothing)
and the ground is RAISED.  Since B5b a tower is what kills it.

⚠ **And a fourth consequence nobody designed: the pile BLINDS the
tower that made it.**  B5b's line of sight reads `hex_height`, which
adds the rubble layer, so the heap that ramps a kill zone shut also
puts the kill zone out of sight — ten dead robots are 5 m of wreckage,
and a shot three hexes out is only 4.5 m up.  It falls out of reading a
height rather than a material ([`DESIGN.md`](DESIGN.md) § LOS is a
HEIGHT question) and it sharpens point 1 below rather than softening
it: the entrance closes AND the guns covering it go quiet.

⚠⚠ **Measured end to end in plan 12 B7, and it beats a tower.**  Three
scenarios differing only in their defences — same ground, core, spawn
and wave list:

| base | clock | ending |
|---|---|---|
| no defences | 61 ticks | they walked in |
| sealed wall | **104** ticks | the wall broke, at its weak end |
| sealed wall **+ a tower** | **95** ticks | **the pile went over** |

The tower is not weak: it kills eight of thirteen.  What undoes it is
that three of those bodies land on one hex — 1.5 m, inside plan 12 B0's
[1.0, 2.0] ramp band onto a 3.0 m `wall` — so the five survivors climb
their own dead onto a wall that never lost a tenth of its HP.  Every clause of this section fires at once: the
ramp (point 3), the blinding above, and the queue spreading off-axis.
**A kill is a permanent terrain change and nobody can reverse it**, so
until the vehicle's crew exists to collect bodies, a tower without
cleanup shortens the base's life.  `tests/12_b7_the_clock.loft`.

⚠ **The counter-play is a vehicle STANDING there, and since plan 14 H2
it does not have to be the player's.**  A crew — the player, an NPC
helper, or both — clears the ramp as fast as a tower builds it, and the
attackers are put back onto the wall: 95 → 145 ticks on the base above,
77 → 242 on one with two fronts.  ⚠ And a roster buys **coverage, not
throughput**: a second helper beside the first is worth nothing, because
one tower makes 0.03 m of body a tick and one vehicle clears 0.33.  What
a second one is for is a second front.  `plans/14` § Status.

⚠ **A fatal hit is followed by one last step.**  The tick moves before
it resolves deaths, so a body lands one hex down the enemy's route from
where the damage landed.  That is the same "consequences land at the
end of a tick" rule a broken wall follows, and it is worth knowing
before reading point 1 as "they pile exactly where they were shot".

1. **A defended entrance closes itself.**  The chokepoint is where
   enemies die, so it is where bodies pile — the player's own kill
   zone becomes a wall.
2. **A closed entrance triggers the siege** (§ Sealing the
   perimeter), and the enemies spread along the perimeter.
3. **A pile beside a wall shortens the step onto it.**  When it is
   high enough, enemies **climb their own dead onto the wall** —
   the perimeter is breached without the wall ever being broken.

⚠ **"High enough" is a BAND, not a floor**, and plan 12 B0 measured
it.  The pile has to be low enough to step ONTO *and* high enough to
leave less than a climb above it, and one number does both jobs — so
a single-hex ramp onto a structure `H` high needs a climb of `H / 2`,
and the workable pile heights are `[H - c, c]`.  Two consequences
worth knowing before reading point 3 as "more dead is always better":

- **the band is EMPTY below `H / 2`**, however many bodies fall.  A
  robot climbs 2.0 m and a `wall` is 3.0 m, so its band is
  [1.0, 2.0] — **two, three or four dead robots** at 0.5 m a body.  A `wall_high` is 5.0 m, so a
  robot has no single-hex ramp onto one at all, which is what keeps
  the two wall types different for the class § Wall climbability says
  a `wall` stops.
- **a pile can grow PAST being a ramp.**  Five robot bodies on one hex
  is a 2.5 m step and a 2.0 m climber cannot get onto its own ramp any
  more — so a player who farms one chokepoint forever is not building a
  staircase, they are building a second wall.

⚠ **A wreck is not rubble yet, and a big one BLOCKS** (project owner,
2026-08-13; recorded, not built).  The conversion from a broken machine
to a heap of generic rubbish is **not instant**, and the decay clock
drives two things at once:

- **salvage** — a fresh wreck carries components that can be harvested
  off it directly; ignored, they deteriorate into rubbish.  So driving
  in early pays and driving in late does not, which turns "collect the
  bodies" from a spatial counter-play into a timed economic one.
- **passability** — a **big** robot's body stands high enough to seal
  the hex it fell on, and settles under a climb limit as it decays.  A
  **small** one never blocks: others walk straight over it.

⚠ The second is the sharp one, because a plugged chokepoint is not a
win: the wave that cannot get through **attacks the wall instead**
(§ Sealing the perimeter), so the player's own kill zone plugging
itself starts costing them the perimeter.  Shooting the corpse
accelerates its decay and reopens the funnel — at the price of shots
and of the salvage that goes with them.

A tower's damage TYPE decides which way that trade falls: a laser
vaporises, an explosive splashes (and damages the player's OWN walls),
an EMP destroys the high-value electrics while leaving the chassis
nearly intact — maximum obstruction, minimum salvage — and is nearly
useless against insects.

Everything the blocking half needs already exists and is gated —
`height.loft` stores metres per hex, `passable.loft` compares a step
against `climb_limit`, and plan 12 B4 measured the band — so what is
missing is the CLOCK and a per-class body height.  The salvage half
rides on the contents layer
([plan 06](../plans/06-editor-stencil-pipeline/README.md) S1), not on
the source label `height.loft` stores.  See
[`plans/12`](../plans/12-combat-resolution/README.md) § Wreck decay,
blocking, and damage types.

The counter-play is to **collect the bodies**, which means driving
into the kill zone while the wave is still coming.  A player who
farms safely behind a chokepoint is building the ramp that ends
them; the cost of clearing it is exposure at the worst moment.

⚠ This is what makes turtling lose on a timer rather than on a
rule, and it is the same shape as § Sealing the perimeter: no
mechanic forbids the strategy, the strategy defeats itself.

## The tick resolves once

The world is rebuilt **once per tick, not once per event**.  Within
a tick every enemy acts against the same world — the same heights,
the same passability, the same routing.  A body dropped during tick
N raises its hex for tick N+1, never for the enemy that happens to
move two places later in the same tick.

Cost is the obvious reason: a wave reaches 80 enemies and deaths
cluster, so rebuilding per death would rebuild dozens of times
inside one tick for a world that is only read at the end of it.

**Order independence is the real one.**  With a single rebuild per
tick, the outcome of a tick does not depend on which enemy the loop
visited first.  dryopea validates by replaying written-down runs
([plan 08](../plans/08-game-validation/README.md)), and a
simulation whose result depends on roster iteration order cannot be
gated at all — the same script would produce different numbers on a
different day.

⚠ The invariant that holds it: **the same wave, played with the
enemy roster iterated in reverse, produces an identical result.**

## Sealing the perimeter is punished, not forbidden

A wall placement is **never refused**.  The genre convention is to
forbid a full block (the placement greys out); dryopea allows it
and makes it a bad idea instead.

⚠ **Plan 12 B5b added a punishment nobody designed: dead ground.**  A
tower's shot descends from its eye to its target, so a wall past about
three fifths of the way blocks it — which means a tower set two hexes
back from the perimeter **cannot touch the robots chewing the far side
of it**.  Seal the base and the besiegers stand exactly where the guns
cannot reach.  The counter is a placement rule rather than a mechanic:
a tower must OVERLOOK the wall it covers.  Measured in
`tests/12_b5b_los_budget.loft`; the geometry is in
[`DESIGN.md`](DESIGN.md) § LOS is a HEIGHT question.

**Blocked enemies still want the core, and they arrive already
spread.**  So a sealed perimeter is not one fight — each enemy
attacks the wall where *its own* route to the core first meets it,
and enemies from different sides meet it at different hexes.  One
defended chokepoint becomes a siege the player cannot concentrate
fire on, and the whole perimeter becomes the problem.

**They do not queue, either.**  An enemy whose target hex is taken
by a companion does not wait behind it — it moves *beside* them and
attacks its own stretch of wall.  Companions block movement; they
are never targets.  So a single wall face is chewed along its
length rather than at one point, and the more enemies arrive, the
wider the bite.

⚠ **BOTH halves are built** — the spread by APPROACH at
[plan 11](../plans/11-flow-field/README.md) F7, the spread by
OCCUPANCY at F7b.  An enemy with no route follows a *desire field*
(the routing sweep with the climb lifted, so walls are passable) and
attacks where the height rule refuses its next step; and an enemy
whose step is taken by a companion now steps to an **equally distant**
hex instead of waiting.  So enemies from different directions meet the
perimeter at different hexes, AND a wave arriving down one approach
fans out across the face rather than queueing at a point.

⚠ **A VEHICLE in the way is a third case, and it is attacked**
(plan 13 V5).  A companion blocking a step is never a target; a vehicle
blocking the *same* step is, but only where the enemy has no sidestep
either — so in the open it walks round and the vehicle is ignored, and
in a chokepoint it is a liability.  `DESIGN.md` § 8 § Conditional
damage.

⚠ **The whole crew, not just the player** (plan 14 H3).  Who is
standing where on the player's side is one map — `occupancy.loft`'s
`BlockerMap`, built once a tick beside the occupancy — and it answers
WHICH vehicle, because the damage has to land on the one that is in the
way.  Each blocker is charged for the enemies it stopped.  ⚠ And the
two sides part company on death: the player reappears at the core
whole, a helper **wrecks where it stood and stays there** (`DESIGN.md`
§ 9: *"retrieval is the only way back"*).  ⚠ A wreck blocks nothing —
otherwise the first crew member to die in a corridor would be a free
wall with no HP left for anyone to break.

⚠ **A COMPANION, never the GROUND — the condition is the rule.**  An
enemy stopped by the wall must STAND and attack: it is at what it came
to break, and the hex in front of it is what its second of damage is
spent on.  Sidestep on a terrain block instead and a besieger shuffles
along the face for ever, attacking a different hex every tick and
finishing none of them — a jitter, not a spread.  F7b's negative
control measures exactly that: the same enemy attacking the same hex on
six consecutive ticks.

⚠ **F5c was right to reject this shape when it did, and the difference
is the condition.**  F5c's objection was that occupancy should be a
movement constraint and not a way of steering.  It still is: a sidestep
closes NO distance and never increases it, so F5c's own invariant —
every enemy ends a tick one closer or exactly where it was — holds
unchanged, and its test was never edited.  What changed is that
"exactly where it was" now includes a different hex at the same
distance.  It took three phases and a measured balance (plan 12 B7) to
establish that the rule was load-bearing rather than cosmetic.

⚠ **Tuning consequence.**  A spread siege divides wall HP across
many points at once, so a sealed base falls faster than a
single-chokepoint reading of wall HP vs nibble DPS suggests.
`wall HP`, `nibble DPS` and `wave size` are one tuning set, not
three numbers ([`NUMBERS.md`](NUMBERS.md)).

## A trench is the other obstacle a besieger removes

⚠ **Built.**  BACKLOG C9, `@X283`.  A wave stopped at a moat does not
stand there: it **shovels the trench shut**, and the hex is ground again
once the spoil clears the waterline
([`src/moat.loft`](../src/moat.loft) § What a besieger shovels).  So a
moat is a **TIMER** — 130 / 174 / **221** ticks for nothing, a five-hex
wall and a five-hex trench (`@M059`).

⚠⚠ **The rule it added is about the DESIRE FIELD, not about damage**:
*an obstacle the wave can REMOVE is passable in it.*  A wall always was
— a wall's top is walkable, so lifting the climb was enough — and a
trench is not a surface at any height, so the sweep's NODE rule is what
widened ([`src/flow.loft`](../src/flow.loft)`::sweep_ground`).  ⚠ Before
that, a moat hex was not in the desire field at all, so `flow_steps`
never offered it and `enemy_target` named the besieger's own hex: **the
siege could not SEE a moat**, and no amount of code in `wave_damage`
would have changed that.  ⚠ The **sea** is not a moat, so the sweep
still stops at the coast; ROUTING is untouched.

⚠ **A trench is a wall that cannot be UNBRACED**, and that is the rate:
a metre of trench costs a besieger the full `wall_hp` (100), the figure
a wall reaches only closed into a ring, because a hole in the ground has
no ends to unzip from.  The rate is metres per DAMAGE and never per
depth, so a class digs with the tool it chews with and the palette's
`drop` stays the timer.

⚠⚠ **And what a trench is FOR is the kill zone, not the barrier.**  A
besieger has to stand at a fixed distance and dig, which is the most
shootable thing a robot can do — a tower behind a trench is **335 ticks
and nine of thirteen dead** (`@M060`), where the trench alone is 221
with all thirteen alive.  That is `@M058`'s *a wave that cannot reach
you cannot die*, inverted.

## A wall's HP is structural, not a constant

⚠ **Built.**  Plan 12 B2 made a wall breakable and B3 made its HP
structural: `src/damage.loft::brace_of` classifies a hex by its
structure neighbours and `numbers.json` § wall.brace_factor_* scales
the kind's figure by it.  A besieged wall comes down when an enemy with
no route has spent `enemy_regular.damage_to_wall` (1 HP/s) into it for
long enough; it is then removed and leaves a heap of masonry a third of
its own height, which a robot climbs.

⚠ **"Straight" is not what the eye calls straight.**  Two neighbours
brace along one line only when they are OPPOSITE across the hex, and
odd-r offset makes that counter-intuitive: a **row** (constant `r`) is
collinear at every hex, but a **column** (constant `q`) zigzags — row
parity flips which delta each direction carries — so a "vertical" wall
a player drags is a crinkle-crankle wall and is stronger for it.
Measured, not reasoned about; `tests/12_b3_bracing.loft` states it.

`wall.wall_hp` (100) is the *braced* figure.  **A wall hex with no
support from either side is easier to push over, and has less HP
for it** — the same reason a free-standing straight fence topples
and a curved one does not (a crinkle-crankle wall stands one brick
thick because its curvature braces it).

Support comes from a hex's wall neighbours and how they sit:

| the hex | support | HP |
|---|---|---|
| wall neighbours on both sides, **turning** — the perimeter curves | braced both ways | full |
| wall neighbours on both sides, **collinear** — a long straight run | braced along one line only | reduced |
| one wall neighbour — an **end** | cantilevered | low |
| no wall neighbours — an **isolated stub** | nothing to push against | lowest |

This is the structural twin of a rule in
[`DESIGN.md`](DESIGN.md) § 5 § Wall topology — open ends render as
ramps, so *"to actually defend, the player must close the
perimeter (every wall hex has ≥ 2 wall neighbours)"*.  That rule
says an unsupported end is a way **in**; this one says it is also
the place the wall **breaks**.

**It was designed to interact with the siege**: enemies spread along
the perimeter and chew everywhere at once, so they never have to *find*
the weak hex — the perimeter fails at its least-braced point on its
own.  A player who rings the core in a smooth curve is buying HP; one
who runs a straight fence with two loose ends has built the breach for
them.

⚠ **Plan 12 B3 measured that and it did NOT happen; plan 11 F7b then
made it happen — with two conditions.**  A queued wave now arrives at a
wall as a FRONT, because an enemy blocked by a companion steps beside
it, and a five-hex wall it spans **breaks at its 30 HP end** while the
100 HP middle keeps two thirds of its allowance.  Nothing coordinates
it and no enemy knows what bracing is: equal damage across the face,
and the weakest hex under it runs out first.

The two conditions are what a player can actually play against:

- **The front has a WIDTH, and since plan 24 W1 it is the WALL's.**  It
  used to be the fan's — eight robots against a seven-hex wall landed
  nothing on either end — so a perimeter longer than a wave could reach
  across HID its weak hexes.  ⚠ That mechanism is gone: the front grows
  with the wall, and length pays by **dilution** instead (the same wave
  spread over more hexes takes each of them down more slowly).  Bracing
  still rewards length as well as shape; it rewards it continuously
  rather than off a cliff.  § The siege front is the WALL's width.
- **The spread is by occupancy AND by approach.**  A wave thin enough
  never to block itself never sidesteps, and still chews where its
  routes cross.  B3's six robots come from six directions and behave
  exactly as B3 measured; its test is green through F7b for that
  reason.

The rest of this note records what B3 measured on the queueing mover,
because it is why the rule above took three phases to arrive.  Six
robots released across a six-wide slab at a fence spent twelve ticks
chewing the braced MIDDLE and landed **nothing at all** on either end.
The reason was § Sealing the perimeter's own caveat: the spread was by
APPROACH only, so enemies converged onto the hexes
their routes cross — which, with the core behind the middle of a fence,
is its strongest part.  A loose end is only the breach if an enemy's
route happens to meet it.

⚠⚠ **Closing the gap did NOT need the equal-distance sidestep, and that
is worth reading twice** — this paragraph asked for one for three
plans.  [Plan 24](../plans/24-the-siege-front/README.md) W0 measured
what a sidestep offers at a wall face and it steps as readily off the
face as along it (`@M019`); the gap was closed instead by a
PRECEDENCE — *arriving beats queueing*, W1 — which is a smaller change
than the second steering rule everyone had budgeted for.

⚠ B3's tripwire
(`tests/12_b3_bracing.loft::test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak`)
was written to go red the day that steering landed.  **It did not fire**,
and the reason is exact: its six robots come from six directions and
each already touches the fence where its own route meets it, so the
precedence changes nothing for them.  ⚠ A tripwire aimed at the rule
you eventually build is not the same as one aimed at the BEHAVIOUR you
want — this one was aimed at the rule, and the rule turned out to be
the wrong one.

⚠ Curvature is measured on the hex lattice, so "turning" means the
two wall neighbours are not opposite each other across the hex —
a 60° or 120° bend, not a straight-through.  Three or more neighbours
is a junction and takes the full figure; there is no class above full.
The multipliers live in
[`examples/numbers.json`](../examples/numbers.json) §
`wall.brace_factor_*` (1.0 / 0.6 / 0.3 / 0.15); the ordering above is
the design, the numbers are tuning.

⚠ **Bracing is computed from the world, never stored**, so a perimeter
UNZIPS: the hex beside a fresh breach becomes the new end and loses
more than half its allowance.  Nothing implements that — it falls out
of asking the question fresh — and the cascade takes one TICK per link,
because a break is only ever resolved against the state its tick began
with.

⚠ **Terrain does not brace.**  Support comes from a hex's structure
neighbours, so a wall anchored against a cliff is still an end.
Arguable as design; written down rather than assumed.

## The siege front is the WALL's width

⚠⚠ **Plan 24 W1 (`@M020`).  It was THREE, for any wall length, and that
was a defect rather than a balance property.**  A besieger now attacks
the wall hex it is TOUCHING instead of walking on down the desire
gradient to queue behind its minimum — *arriving beats queueing* — so
twelve robots into a five-row sealed band besiege **four** hexes of it
and the same twelve against a SEVEN-row wall besiege **six**.  Widening
the perimeter widens the front.

### ⚠⚠ Why it used to be three, and why the diagnosis matters

⚠ **Five documents named the missing rule *the equal-distance
sidestep*, and dryopea has had one since plan 11 F7b.**  Plan 24 W0
measured what it offers at a wall face and it was not the fix: standing
at `(7,-1)` beside a wall at `q = 6`, its two candidates are `(7,-2)`
along the face and `(8,0)` **back off it**.

The real cause was the DESIRE FIELD's shape (`@M019`).  The field is a
ring around the CORE, so a straight face has exactly one minimum — in
K3's band the column `q = 7` reads 8 / 8 / **7** / 8 / 8 — and a
besieger attacked only when it could not WALK.  So only the minimum and
the two hexes where the lateral step runs out ever attacked: **three,
for any wall length**, because `(7,±3)` steps to `(7,±2)` exactly as
`(7,±1)` steps to `(7,0)`.

⚠ **The tell was that all five face hexes TOUCH the wall.**  Two of
every five besiegers stood beside the thing they came to break and
walked sideways to join a queue.

⚠ So the fix is a PRECEDENCE rather than a steering mode, and it lands
in two places that must agree: `enemy_walk_desire`'s pre-pass and
`enemy_target`'s siege branch.  ⚠ `enemy_target` takes no `Occupancy`,
so the rule is phrased *"a wall is between me and the core"* rather than
*"my closer steps are held"* — which needs no memory and cannot jitter,
because an enemy that stops never moves again.

### ⚠ A longer perimeter still pays — by DILUTION now

The old mechanism was hiding: a wall longer than the fan kept its ends
out of reach entirely.  The new one is spreading — the same wave across
more hexes takes each of them down more slowly, measured directly (the
30 HP end of a five-hex wall kept 14 HP at the tick a three-hex front
had it down to 10).

⚠ **And a wider front makes most bases last LONGER**, which is the
counter-intuitive half: a besieger that stops at the wall is one that is
not walking on to stand on the core, and the wallet is drained by
nibblers rather than by wall damage.  `a-base-on-two-fronts` went
**123 → 132**.

⚠ Everything past the front is still queueing, and an enemy blocked by a
COMPANION attacks nothing — it steps beside if it can and stands if it
cannot (plan 11 F7b).  What it never does is chew over its neighbour's
shoulder.

### ⚠⚠ What that does to a wave's COMPOSITION

Three shipped rules meet here, and the result is the one thing in this
document that reads like a defect and is not:

1. the front is three hexes wide (above);
2. a companion-blocked enemy attacks nothing (F7b);
3. since plan 23 K2b, **speed decides who gets there first**.

So the front rank is filled by whichever class is quickest, and
**a wave is as dangerous as its fastest class and no more**.  Twelve
robots into one band, measured:

| wave of twelve | falls at | pure wave of the FRONT class |
|---|---|---|
| 12 miner | **94** | — |
| 4 builder + 8 miner | **104** | 12 builder = 100 |
| 4 robot + 8 miner | **119** | 12 robot = 115 |
| 4 harvester + 8 miner | **164** | 12 harvester = 161 |
| 4 scout + 8 miner | **never** | 12 scout = never |

⚠ Every mix lands within **four ticks of a PURE wave of its front
class**, and never anywhere near the eight miners that are two thirds
of it.  The miner is the hardest-biting class in the game — 3.0 HP/s
against a harvester's 0.5 — so *a wave two thirds miner performing like
a pure harvester wave* cannot be produced by rate arithmetic at all.
It needs the miners to be doing **nothing**, and the target list says
they are: with four scouts in the wave, the number of miners touching
the wall is zero.

⚠ **It is a cliff, not a ramp.**  The first scout swapped into twelve
miners costs the wave nothing whatever (94 → 94); the fourth costs it
the base.  A wave whose bite were the SUM of its members would lose a
twelfth each time — the step shape is what says the mechanism is
positional.

⚠ **And adding the dangerous class makes the wave WORSE.**  Every mixed
row above is a few ticks slower than the pure wave of its front class,
because the miners take fan hexes early and are then displaced out of
them.

⚠⚠ **The ROSTER order decides nothing.**  Plan 23 K0 measured a mix's
order at 20x on a breach clock, and that reading stands — it was about
enemies PLACED at different distances, in a world where every class
walked at 1.5 hex/s.  A `compose` line sets the DEPARTURE order, and
since K2b the faster class simply overtakes: four scouts first, four
scouts last and four scouts alternated through the miners all land on
the same tick.

### The fix — BUILT, and it was not the rule this section asked for

⚠⚠ **Plan 24 W1 (`@M020`).**  This section asked for *the
equal-distance sidestep* and priced it as a second steering rule.  W0
measured the sidestep dryopea already has and found it steps off a wall
face as readily as along it (`@M019`); what was missing was a
PRECEDENCE — *arriving beats queueing*.

The numbers it moved, same five waves of twelve:

| wave of twelve | was | now |
|---|---|---|
| 12 miner | 94 | 94 |
| 4 builder + 8 miner | 104 | 101 |
| 4 robot + 8 miner | 119 | 116 |
| 4 harvester + 8 miner | 164 | 122 |
| 4 scout + 8 miner | **never** | **126** |

⚠⚠ **So the headline above is retired: a wave is worth its front class
PLUS whatever the front class cannot COVER.**  The front is five hexes
on this band, four screens leak exactly one, and what the leak is worth
depends on how hard the screen bites relative to what gets through it —
a builder screen loses nothing to it, a harvester screen thirty-nine
ticks.  **The screen is arithmetic — bodies against face width** — where
it used to be positional immunity.

⚠ What survived: the ROSTER order is still worth nothing, an enemy
blocked by a companion still attacks nothing, and the cliff is still a
cliff (the first three scouts are worth nothing and the fourth
thirty-two ticks).  ⚠ Refusing to build it inside plan 23 (`@X064`) is
what let plan 24 discover the name was wrong — **pricing a fix you
decline to apply buys the next phase a free diagnosis.**

## Retaliation — designed, not built

⚠ Owner, 2026-08-13.  Enemies attack **towers that hurt them**, and
which towers those are is an **information** question rather than a
threat one: with the scrambler up an enemy knows only its own
injuries, so it retaliates against what hurt *it*; with the scrambler
down they share, and a tower hurting anyone's companions is a target
for all of them.

⚠ **It never overrides the routing above.**  An enemy with no route to
the tower goes on heading for the core — so most of the time nothing
changes, because a tower behind a closed perimeter cannot be reached.
That is what makes "put the towers behind walls" the normal answer and
an outer ridge a real gamble: a ridge tower is exposed because it is
**reachable**, not because it is outside.

⚠ **The one exception under scrambling is the BOSS — and it is not a
different AI.**  *Bosses are not special in their AI; their size and
their options are different, and that is what makes them special
events* (owner, 2026-08-13).  Every class runs the same rules; a boss
is a row in the same table with different data — a 2×2 footprint, so
it cannot fit a one-hex entrance, and the option to share what is
hurting it with the robots around it.

The event is what those two produce unaided: *big footprint → no route
in → stuck at the wall → a tower hits it → the squad is put onto that
tower*.  Nobody writes a boss behaviour.

⚠ This is the same discipline § Two modes already keeps: a class's
climb limit is its WHOLE contribution to passability, which is what
lets one distance field serve several classes.  A boss that needed its
own mover would break that; a boss that is different numbers does not.

⚠ Its footprint is also a routing problem nothing here solves: the
field is built for a ONE-hex unit, so a boss needs a sweep with a
clearance requirement — a second key beside the climb limit that
`wave_fields` already groups on.

Two more consequences: a tower that has never fired has hurt nobody, so
a reserve held in check is safe by the rule; and retaliation gives an
enemy a NEW reason to break a wall — *the thing shooting me is behind
it* — which is the first target priority in
[`DESIGN.md`](DESIGN.md) § Enemy targeting that is about a defence
rather than about the core.

## What a broken wall leaves

Two effects, and they are not the same kind of state.

- The wall is **removed** — persistent, and it really does edit the
  world.  The hex is repainted to a default ground rather than erased:
  the painted layer is sparse and **sea-default**, so an erased breach
  would read as `sea` and be *less* passable than the wall it replaced.
- A heap of **masonry** is deposited into the rubble layer — runtime,
  clearable, never saved (§ Bodies are terrain).  It is a fraction of
  the wall's own height (`numbers.json` § wall.rubble_height_fraction),
  and that fraction has to stay under a robot's climb or the breach is
  not a way in.

⚠ **The break lands at the END of a tick**, after every enemy has
moved, for the same reason a body does (§ The tick resolves once): a
wall that fell mid-loop would open a route for whoever the roster
visited later, and the outcome would depend on iteration order.

⚠ What the ground under a wall *was* is not recoverable — painting the
wall overwrote it, and the save format cannot carry a second kind per
hex.  So the default is a decision made in one place, and the real
answer (walls become their own layer, above the ground rather than in
it) is deferred to plan 06.
