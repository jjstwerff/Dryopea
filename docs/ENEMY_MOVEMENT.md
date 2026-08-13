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
- [What a broken wall leaves](#what-a-broken-wall-leaves)

## Two modes, one passability rule

**Steering has two modes** ([`DESIGN.md`](DESIGN.md) § 6
§ Multi-direction spawn markers defines the marker + heading):
outside the scrambler bubble an enemy follows its spawn marker's
heading; inside it, the flow field toward the core.  The bubble
boundary is the handoff.

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
and the ground is RAISED.  Nothing fires at an enemy yet; the tower is
B5.

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

⚠ **Built as far as "they arrive already spread"; the rest is not.**
The spread by APPROACH is live ([plan 11](../plans/11-flow-field/README.md)
F7): an enemy with no route follows a *desire field* — the routing
sweep with the climb lifted, so walls are passable — and attacks
where the height rule refuses its next step.  Enemies from
different directions therefore meet the perimeter at different
hexes with nothing coordinating them.

What is **not** built is the sideways half of the paragraph above.
Measured at F7: the desire gradient points **at** the wall, not
along it, and a step is only ever taken to a strictly closer hex —
so enemies arriving down one approach still queue behind each
other, and a single face is bitten at one point rather than along
its length.  Chewing it lengthwise needs an enemy to sidestep to an
*equally* distant hex, which is a second steering rule on top of
the field.  It would be a real change, not a fix: plan 11 F5c
rejected exactly that shape once already, because occupancy is
meant to be a movement constraint and not a way of steering.

⚠ **Tuning consequence.**  A spread siege divides wall HP across
many points at once, so a sealed base falls faster than a
single-chokepoint reading of wall HP vs nibble DPS suggests.
`wall HP`, `nibble DPS` and `wave size` are one tuning set, not
three numbers ([`NUMBERS.md`](NUMBERS.md)).

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

**It was designed to interact with the siege, and that half is NOT
built.**  The intention: enemies spread along the perimeter and chew
everywhere at once, so they never have to *find* the weak hex — the
perimeter fails at its least-braced point on its own.  A player who
rings the core in a smooth curve is buying HP; one who runs a straight
fence with two loose ends has built the breach for them.

⚠ **Plan 12 B3 measured that, and it does not happen.**  Six robots
released across a six-wide slab at a fence spend twelve ticks chewing
the braced MIDDLE and land **nothing at all** on either end.  The
reason is § Sealing the perimeter's own caveat: the spread is by
APPROACH and never by sidestepping, so enemies converge onto the hexes
their routes cross — which, with the core behind the middle of a fence,
is its strongest part.  A loose end is only the breach if an enemy's
route happens to meet it.

Closing the gap needs the equal-distance sidestep that
[plan 11](../plans/11-flow-field/README.md) F7 explicitly did not
build: a second steering rule, not a fix.  Until then the bracing rule
is exact and its consequence is latent —
`tests/12_b3_bracing.loft::test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak`
asserts today's behaviour, so the day somebody builds that steering it
goes red and points at this paragraph.

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
