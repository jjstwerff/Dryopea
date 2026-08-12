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
- [Wall climbability per enemy type](#wall-climbability-per-enemy-type)
- [Bodies are terrain](#bodies-are-terrain)
- [The tick resolves once](#the-tick-resolves-once)
- [Sealing the perimeter is punished, not forbidden](#sealing-the-perimeter-is-punished-not-forbidden)
- [A wall's HP is structural, not a constant](#a-walls-hp-is-structural-not-a-constant)

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

1. **A defended entrance closes itself.**  The chokepoint is where
   enemies die, so it is where bodies pile — the player's own kill
   zone becomes a wall.
2. **A closed entrance triggers the siege** (§ Sealing the
   perimeter), and the enemies spread along the perimeter.
3. **A pile beside a wall shortens the step onto it.**  When it is
   high enough, enemies **climb their own dead onto the wall** —
   the perimeter is breached without the wall ever being broken.

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

⚠ **Tuning consequence.**  A spread siege divides wall HP across
many points at once, so a sealed base falls faster than a
single-chokepoint reading of wall HP vs nibble DPS suggests.
`wall HP`, `nibble DPS` and `wave size` are one tuning set, not
three numbers ([`NUMBERS.md`](NUMBERS.md)).

## A wall's HP is structural, not a constant

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

**It interacts with the siege, and the interaction is the point.**
Enemies spread along the perimeter and chew everywhere at once, so
they do not need to *find* the weak hex — the perimeter fails at
its least-braced point on its own.  A player who rings the core in
a smooth curve is buying HP; one who runs a straight fence with
two loose ends has built the breach for them.

⚠ Curvature is measured on the hex lattice, so "turning" means the
two wall neighbours are not opposite each other across the hex —
a 60° or 120° bend, not a straight-through.  The exact multipliers
belong in [`NUMBERS.md`](NUMBERS.md) § `wall`; the ordering above
is the design, the numbers are tuning.
