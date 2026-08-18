<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea — design (canonical)

The current master design for dryopea.  Originated as
@PLAN46 in the loft tracker (2026-05-21); refined and made
canonical here.  The original @PLAN46 draft is preserved as
an [appendix](#appendix--original-plan46-2026-05-21) at the
bottom of this file for traceability.

**Companion docs:**

- [`SETTING.md`](SETTING.md) — fiction (haywire robots, cordon,
  station, insects + sap, elementals + stones, history)
- [`GROUND_TYPES.md`](GROUND_TYPES.md) — 11-type ground palette
- [`PROXY_ART.md`](PROXY_ART.md) — placeholder geometry
- [`NUMBERS.md`](NUMBERS.md) +
  [`../examples/numbers.json`](../examples/numbers.json) —
  runtime parameters

**Plans** in `plans/`: 01 ground-editor, 02 viewer,
03 spawns, 04 maps, 05 validation-scenario.

---

## Contents

- [1. Status + scope](#1-status--scope)
- [2. The pitch](#2-the-pitch)
- [3. World](#3-world)
- [4. The core — the scrambling tower](#4-the-core--the-scrambling-tower)
- [5. Ground + walls](#5-ground--walls)
- [6. Spawn system + waves](#6-spawn-system--waves)
- [7. Combat dynamics](#7-combat-dynamics)
- [8. Player vehicle](#8-player-vehicle)
- [9. Helpers](#9-helpers)
- [10. Three enemy tiers](#10-three-enemy-tiers)
- [11. Movement + input philosophy](#11-movement--input-philosophy)
- [12. Camera + HUD](#12-camera--hud)
- [13. Economy + progression](#13-economy--progression)
- [14. Run structure](#14-run-structure)
- [15. Landing flow](#15-landing-flow)
- [16. Meta-game hub](#16-meta-game-hub)
- [17. Moddability](#17-moddability)
- [18. Numbers](#18-numbers)
- [19. Validation tier scope](#19-validation-tier-scope)
- [20. Future expansion](#20-future-expansion)
- [21. Open questions](#21-open-questions)
- [17a. Library evolution — dryopea trail-blazes](#17a-library-evolution--dryopea-trail-blazes)
- [17b. Loft idiom alignment](#17b-loft-idiom-alignment)
- [Appendix — Original @PLAN46 (2026-05-21)](#appendix--original-plan46-2026-05-21)

---

## 1. Status + scope

**Pre-alpha, under active implementation.**  A working hex
editor, a wave engine that routes enemies round walls per
class, and combat that resolves — towers fire, enemies die and
leave bodies, walls take structural damage and break into
climbable rubble.  No player vehicle yet, and no game loop
around the base.

⚠ **This document describes the design INCLUDING its future**,
so most of what follows is not built.  What exists is
[`../plans/README.md`](../plans/README.md), where each plan's
own `## Status` is the source of truth — never this file.  The
runtime parameters live in
[`../examples/numbers.json`](../examples/numbers.json); the
fiction in [`SETTING.md`](SETTING.md).

**Validation tier** = the buildable goal.  One base, one
mission, robots only, one tower type, one enemy type.  Targets
a single base session of **~15-25 minutes**: ~45 s pre-wave
commitment → 7 waves with ~15 s gaps → ~5-6 min wave phase →
free scramble or earlier exit.  Validation passes when a cold
player can play one base end-to-end with no critical
contradiction in 30 minutes of play.  Full scope in
[`plans/05-validation-scenario/README.md`](../plans/05-validation-scenario/README.md).

What is in validation tier vs deferred is called out in
[§ Validation tier scope](#19-validation-tier-scope).

## 2. The pitch

A non-standard sci-fi tower-defence.  The player is a
**field-head of a small mining cooperative**, hired on a
**permit-bound sortie** into a planet sealed by a military
cordon.  In each sortie they drop into a base, paint walls,
order towers + helpers, defend against waves of haywire robots
(and eventually insects and elementals), and **scramble out**
when the time is right — launching the central building as a
rocket, carrying whatever they managed to grab.

The signature mechanic: **scramble-and-salvage**.  A base is
not win-or-lose-forever; it is one round of a longer **run**.
Evacuating a tower-top takes it with you but **disables the
tower it came from**, so grabbing salvage *hastens* the
overrun.  Hold longer for more haul; launch now to keep what
you already have.  That tension is the core decision of the
whole game.

### What kind of game this is

Owner, 2026-08-13, and it is the frame the rest of this document
turns out to have been written against: **a real strategy game
built out of tower-defence mechanisms, in which the player
cannot lean back the way a normal tower defence lets them.**

In a classic tower defence the player spends, places, and then
watches.  Here almost every advantage has to be **collected in
person, at a moment when collecting it costs something**:

| The advantage | What it costs to keep |
|---|---|
| a tower's shot budget | decay is per **shot**, and only a player standing at it can refill it (§ 7) |
| repairing one at all | a **firing** tower cannot be repaired — stop it first (§ 7) |
| aiming one at a corpse | presence-locked: the player has to be there (§ 7 § Damage TYPE) |
| a kill zone that keeps working | bodies pile into a ramp, so someone must drive **into** it and clear them ([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Bodies are terrain) |
| salvage worth having | a wreck decays, so it must be collected **early**, which is the worst moment |
| a chokepoint that stays open | a big body plugs it, and unplugging it spends shots and salvage |
| a sniper's range | it wants the outer ridge, which means driving **outside the wall** mid-wave to service it |
| the whole run | scramble-and-salvage: every tower-top taken disables the tower it came from |

⚠ **That pattern is systematic, not accumulated**, and it is
worth stating as a test rather than a description.  A mechanic
earns its place here when it answers yes to: *does this put
something in the player's hands at a moment when using it costs
them something?*  A mechanic that hands out a permanent
advantage for a one-time placement decision is the thing this
design is avoiding.

#### ⚠⚠ And the DEEP layers are what keep it a tower defence

Owner, 2026-08-14: **the deeper layers of this game are also the
ones that keep the game a tower-defence game.**

That reads backwards at first — cosmic horror, hive minds and
ancient cities sound like the material that turns a defence game
into something else.  It is the opposite, and the mechanism is
worth stating because it is what a future contributor would
otherwise erode:

**1. Every deep layer is unbeatable by personal power.**  Look at
what [`SETTING.md`](SETTING.md) actually forbids: a solo player
can never wake an old one, understanding them is the *failed*
defence, resistance cannot be had without ceasing to be a person,
and the portal's warriors answer the threat rather than the
player.  ⚠ **Not one of those offers "become strong enough and
win"** — which is the drift that would turn this into an action
RPG.  With that route closed at every depth, the player's toolkit
stays what it started as: position, terrain, towers, walls, crew,
timing.

**2. They keep the player permanently SMALL.**  A tower defence
only makes sense while defending a place is the right size of
activity.  The moment a player is the protagonist of the cosmos,
holding a perimeter is beneath them.  Indifference on that scale
is what guarantees it never becomes beneath them.

**3. ⚠ Every deep element resolves into a fact about PLACE** —
which is precisely what a tower defence is made of:

| deep-layer element | what it actually is, mechanically |
|---|---|
| elementals | **spatially keyed** to stones |
| an old one commanding them | ⚠ it **deletes that geography** — it attacks the spatial rule itself, which is the sharpest possible tower-defence threat |
| an insect-guarded wound | a **place** held for a **duration** |
| crystal, trees, shafts, caverns | places, and the routes between them |
| the robot economy | a **map**: nodes and edges |
| the portal's warriors | **anchors** — a position where the line holds |

So the lore is not decorating the genre.  **It generates more of
the genre's own material**, one layer up.

⚠ **The test, extended.**  The § What kind of game this is
question stays as written for mechanics.  For deep-layer content
there is a second: *does it resolve into a statement about
position, terrain or timing?*  If it resolves into a statement
about the player's **stats or abilities**, it is off-genre —
however good the story is.

#### ⚠⚠ The end game, and why it is still this game

Owner, 2026-08-14: **in the end game the types of enemies change
and the player's defence is still the same — build strong bases
to overcome the challenge.  And the robots and insects are part
of the solution: the humans cannot attack an old one, but both
the other factions inherently can.**

**Only two things change, and neither is the player's verb.**

| | changes | stays |
|---|---|---|
| **who attacks** | the old one's other servants — the ones under the sheets in the buried city ([`SETTING.md`](SETTING.md) § "Not all are elementals") | — |
| **who the robots and insects are** | co-belligerents rather than the threat | — |
| **what the player does** | — | **builds strong bases** |

⚠ **The player's inability is the design, not a limitation.**
`SETTING.md` § The responses to the incomprehensible: resistance
is the absence of the compulsion to comprehend, so a robot (which
has a task) and an insect (which has a wound to guard) were never
vulnerable, while a human is vulnerable *by being human*.  The
player therefore **cannot be the one who fights it — ever** —
and what is left for them is the thing they have been doing all
along: **hold a place, and keep the ones who can fight supplied
and alive.**

⚠⚠ **So the human contribution to a cosmic war is logistics and
architecture** — which is exactly what a tower-defence player
does.  The end game does not graduate out of the genre; it is the
genre's strongest possible justification, arriving last.

**And the map inverts without changing shape.**  Everything
[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) builds — routes, mines,
factories, repair points — keeps its geometry and reverses its
meaning: the road that delivered waves now delivers allies, and a
repair point behind your line is an asset.  ⚠ A player's whole
mental model of the map is re-used rather than discarded, which
is the cheapest possible way to make an end game feel different.

#### ⚠⚠ Shutting down the scrambler — the end-game move

**Confirmed by the owner, 2026-08-14.**  The core's entire
function is to cut robot coordination (§ 4); against an old one
the player wants that swarm *coordinated*.  So the end-game move
is to **switch off your own scrambler**.

**What it gains.**  Two things at once, and they are the same
act:

- **The robots become effective.**  `SETTING.md` § The core is a
  scrambling tower: *without the scrambler, robot waves would
  arrive as coordinated swarms running on full AI coordination*.
  That is exactly what you now want pointed at the servants.
- **The robots stop coming for you at all.**  The bubble IS the
  wave system — comm-cut robots turning toward the interference
  ([`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md)).  No bubble, no
  deafened robots, no waves.

⚠⚠ **And it is the alliance mechanism, with no diplomacy in
it.**  The robots have spent the entire game walking to your core
to fix a malfunction they cannot identify
([`SETTING.md`](SETTING.md) § They were on an ERRAND).  Shutting
the scrambler down is **the player finally granting the request**
— every wave in the run was asking for this.  No faction system,
no dialogue tree, no reputation bar: you stop doing the thing
they were objecting to, and they were never your enemy in the
first place.

**What it costs, and the number is already written.**  The same
sentence finishes: *…and **no perimeter could hold**.*  If the
swarm ever turns on you again — when the crisis passes, or
because your base is in the way — the one thing that made a
perimeter viable is not running.  It is the classic bargain of
arming your enemy against a worse one, and dryopea can price it
exactly.

⚠ **It also buys nothing against the actual enemy.**  Elementals
and their fellow servants *"answer to stones, not radio"*
(`SETTING.md`), so the scrambler was never any use against
tier 3.  The entire cost falls on your relationship with the
faction you are choosing to help.

##### ⚠⚠ But it is not an off-switch — the scrambler changes JOB

Owner, 2026-08-14: *"players can still use their scramblers, for
example to steer the robots in the right direction."*

The bubble is what robots **converge on** — that has been the
whole wave system.  So in the end game the same device stops
being a shield and becomes a **lure**: switch it on and the swarm
comes *here*.  Where you put a scrambler decides where the swarm
fights.

⚠⚠ **And that creates the end game's central tension, out of two
rules that already ship:**

> **The bubble ATTRACTS and DEGRADES with one act.**  Robots
> converge on it *because* it deafened them — and a deafened
> robot is the degraded per-unit thing the whole wave system
> describes.  So you can have them **coordinated**, or you can
> have them **where you want them**, and never both at once.

Which makes the end-game scrambler a **rhythm rather than a
setting**: pulse it on to gather and redirect, off to let them
fight properly.  ⚠ That is § What kind of game this is in its
purest form — an advantage that only works while you are
actively working it, and that costs the thing you want most at
the moment you use it.

⚠ **So the earlier "permanently disarm yourself" reading was too
strong** and is corrected here: the capability stays, its *job*
changes.  What is genuinely given up is the scrambler as a
**passive defence** — it can no longer be left on to keep a
perimeter survivable, because leaving it on is what stops your
allies working.

##### ⚠ And you must FEED the economy you spent the game strangling

Owner, same statement: *"they will also need to aid the robots in
their war economy instead of the mid-game hampering of the
robots."*

Every lever in [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) **inverts
its sign, and not one of them changes shape** — see that file's
§ The whole graph inverts in the end game for the table.  The
player who learned to strangle a supply line knows exactly how to
protect one.

⚠⚠ **And that is what "build strong bases" means in the end
game: your base stops defending your core and starts defending
THEIR ROAD.**  Same verb, same walls, same towers, same crew —
a different thing behind them.  It is the largest possible change
in what the game is about, bought with no new mechanic at all.

**And it is what buys the variety.**  Because so many of the
counters are **architectural** — decided by where the walls
went rather than by how much damage the player brought — an
author can pose genuinely different problems out of one rule
set:

- **bracing** decides where a perimeter fails
  ([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § A wall's HP is
  structural);
- **funnel width** decides whether a boss fits, and whether its
  builders have room to reach it (§ 7);
- **elevation and wall height** decide what a tower can see
  (§ 7 § LOS is a HEIGHT question);
- **entrance placement** decides where bodies pile, and
  therefore where the ramp forms;
- **wave composition** decides whether the wall or the wallet is
  the thing under threat (§ 10 § Small robots).

⚠ **The risk this frame carries, stated so it is not
rediscovered in play:** a game that never lets the player rest
is exhausting rather than tense.  The design already has its
pressure valves and they should be treated as load-bearing, not
as slack — *an idle tower in a quiet corner never decays*
(§ 7), *a tower that has never fired is never retaliated
against* (§ 7 § Retaliation), and the inter-wave delay is
fifteen real seconds of nothing happening ([§ 6](#6-spawn-system--waves)).
The lean-back is meant to be **earned and brief**, not absent.

Setting + tone in [`SETTING.md`](SETTING.md).

## 3. World

### Sea-default infinite world

Worlds start as an **endless flat sea**; only painted hexes
occupy storage (`hash<GroundType[q,r]>`, miss = sea).  Editor
and runtime share this data model — the in-game editor (plan
01) is a mode of the dryopea executable, not a separate
binary.  Painting walls + terrain happens in the same render
pipeline the game uses.

### Hex layout + scale

- **Pointy-top, odd-r offset** — `hex_grid`'s convention, which
  every `hex_*` library and moros speak.  ⚠ **This line said
  "axial flat-top" until 2026-08-14 and was stale**: plan 09
  converted the whole lattice and C6 deleted the axial layer,
  so the code has been pointy-top odd-r since 2026-08-13.  See
  `CLAUDE.md` § Hex convention, which is the live statement.
- **Hex diameter ~1.5 m** vertex-to-vertex (side ≈ 0.75 m,
  flat-to-flat ≈ 1.30 m).  Small enough that the painted
  resolution feels tactical (one hex ≈ "where one person
  stands"); large enough that authoring doesn't drown in
  count.

Implications: vehicle 2-3 hexes; tower 7 hexes; wall section
1 hex wide and several long; sniping ≈ 15-30 hexes; chunk
(32×32) ≈ 48 m across.

### Trees as terrain — stems, tops and spans

Owner, 2026-08-14.  The huge trees (SETTING.md § A tree is also
a drill) are not props: **a stem is about 10 hexes wide**,
tapering to less at the top, *"so it might be possible to create
a base in/on them"*, and the limbs **span caverns**.

**The scale is the design.**  Ten hexes flat-to-flat is ~13 m.
Against § Hex layout's own comparisons — vehicle 2-3 hexes,
tower 7 — a tapered top lands at roughly **5-7 hexes**, which is
one core footprint (`core.footprint_layout` is a radius-1 disc)
plus standing room and essentially nothing else.  ⚠ **A stem top
is a base site by construction, and a cramped one.**

#### ⚠ It is terrain, and the model already expresses it

`GroundType.height_override` is a per-palette-entry number —
`wall` 3.0, `wall_high` 5.0 — so a stem is that idea with a
bigger number, and every consequence below comes from rules that
already ship:

- **Robots climb 2.0 m** (`CLIMB_REGULAR`), so a sheer stem is
  unclimbable.  **The perimeter IS the terrain**; a stem base
  needs no wall.
- **The player's hover climbs 0.4 m, or 3.0 m boosting** (plan
  13 V4).  So the player cannot get up or down freely either —
  and the boost was built for exactly this shape of problem: *a
  crew inside a sealed base can only reach the ramp by boosting
  out*.
- **Bodies pile 0.5 m each and ramp** (plan 12 B4).
- ⚠ **The bark is very hard** (SETTING.md § It is a MUTUALISM), so a
  stem is not chewable the way a wall is.  `structure_max_hp` is keyed
  on the palette NAME, so this is a row rather than a rule — but it is
  what makes the ramp the *only* way up rather than the easiest one.

#### ⚠⚠ The payoff: your own kills build the staircase

Put those three together and a stem base is **impregnable until
you start killing** — after which the only route up is the one
your kills are building.  Plan 12 B7's *a tower buries its own
wall* stops being a liability to design around and becomes **the
whole loop of the map type**, with the crew's clearing (plan 13
V2) as the only answer.

**Design test** (§ What kind of game this is — *does this put
something in the player's hands at a moment when using it costs
them something?*): ✓✓, and it clears the harder bar too — the
terrain advantage **decays with use**, so it is not a permanent
advantage bought with a one-time placement decision.

⚠ **A stem base is not empty of enemies, only of ROBOTS.**  Insects
live in and around the trees, and they ignore the scrambler — so
building on a stem trades tier 1 for tier 2 and puts the base in the
one biome where the core's defining mechanic does nothing.

⚠ **And you cannot leave.**  Your salvage is at the bottom of a
drop you have to boost off and cannot boost back up until the
ramp exists — so the loot and the breach are the same object.

#### Inside a stem, and spans across caverns

- **"In" as well as "on"** — a hollow stem is an interior base
  with one or two entrances.  That is the chokepoint case, and
  plan 13 V5's rule already says what it means: *blocking is a
  property of the map*, so an interior base is precisely where
  the player's own parking becomes a liability.
- **Limbs span caverns.**  A cavern is a **hole in the surface
  map** — a non-walkable kind, the way sea already is — and a
  limb across it is a walkable strip at height: a bridge one or
  two hexes wide.  A span is the one chokepoint that **cannot be
  walled and cannot be flanked**, which makes it the place where
  a wall is pointless and a tower is everything.
- ⚠ **Caverns give the underground geography without a second
  level.**  The braided shafts a withered tree leaves
  ([`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The vertical
  dimension) lead into them.  Whether the player can descend
  stays undesigned on purpose — see that file's § What this
  deliberately does NOT design.

⚠ **Gating note for whoever builds it:** a span is a 1-hex
corridor, and `CLAUDE.md` § Testing something that moves records
that *a 1-hex-wide corridor cannot tell a flow field from a
fixed heading*.  A span scenario proves nothing about routing
unless the approach to it bends.

### Atmosphere — bounded view distance

The planet's air is **dense / hazy**.  Long visual sight lines
are physically blocked beyond ~40 hex (`atmosphere_haze_radius`
in NUMBERS.md).  Players must **scout** to learn what's beyond
the haze.  Also explains the prior humans' retreat to mountain
caves for breathing (SETTING.md § History); rendering bonus —
the engine never draws the whole map, just the haze radius
around the player.

## 4. The core — the scrambling tower

The central building is the **design hinge** — every other
mechanic attaches to or is gated by it.

### What it is

In the fiction, the core is a **signal-scrambling tower** (see
SETTING.md § The core is a scrambling tower).  It carves a
**bubble of broken comms** around the base in which robots
cannot reach the controlling AI that drives them and fall
back to local per-unit behaviour.  Outside the bubble, robots
regain coordination; scouting beyond it is a risk.

Etymological convergence: the player's force-launch is called
**scramble**; the core is the **scrambler**.  Same word, same
thing — the scrambling tower the player ultimately rides home.

### Geometry — hexagonal prism, 6 sides

The core is a **black hexagonal prism**:

- **7-hex footprint** (centre + 6 neighbours).
- **~3.9 m diameter** flat-to-flat, **~8 m tall** (taller than
  a max-decay tower so silhouettes stay distinct).
- **6 distinct flat sides**, one per outer hex of the
  footprint.  **Three are functional, three are plain.**

Functional sides + their meaning:

| Face | Icon | Player interaction |
|---|---|---|
| **Output / lift-off (the opening)** | red flame/chevron | Drive **through** this opening to enter the core's interior and trigger launch.  Also where landers (helpers / supplies) emerge.  The only side the vehicle can pass through. |
| **Tower-core retrieval** | red disc | Drive next to it + pickup key → tower beacon spawns above the vehicle (cost in points). |
| **NPC ordering** | silver-grey rectangle | Drive next to it + pickup key → helper order placed (cost in points; lander touches down at the opening face shortly after). |

Plain sides are uniform black, no markings, no interaction.

### Two surface signals

- **Top colour** signals NPC-order status (validates pending
  orders at any zoom): black (no order) → red → amber → green
  → white flash on landing.  Colour interpolates smoothly
  across `lander_delivery_time` (default 20 s).  Diegetic; no
  HUD.
- **Bottom pulse** activates when the player enters the core's
  interior → orange-red ring at the base brightens and beats
  faster as the launch countdown elapses (default 6 s).  White
  flash at liftoff; fade to dark on cancel.

### Invulnerability + nibble → points

The core **cannot be destroyed**.  Enemies that reach the core
and "nibble" it do **not** damage its structure; each tick
**drains the player's point wallet** instead.  This retires
the @PLAN46-original "core destroyed = run ends" framing.
The player is never *forced* out by structural collapse; they
choose when to scramble.  The cost of staying too long is
bleeding points to nibbles.

### Force-launch — drive in, hold, exit to cancel

The only way to leave a base is to launch the rocket.  The
sequence:

1. Player drives **through the opening** into the core's
   interior.
2. Bottom pulse activates (orange-red ring at the base lights
   up + beats slowly).
3. Pulse rate accelerates as the countdown elapses.
4. At T = `launch_countdown_duration` (default 6 s), white
   flash → liftoff fires with whatever is currently onboard
   (carried items, deposited scramble inventory, wallet, any
   helpers that have made it back).
5. **Exiting the opening at any time before liftoff cancels**
   the sequence — pulse fades over 0.3 s, countdown resets.

NPC helpers normally board the rocket on their own when their
work list is empty.  Entering as the player **forces** launch
right then; any helpers not yet onboard, loot not yet
delivered, and stranded helpers not yet rescued are **left
behind**.

The countdown is also a **hazard window** during a wave —
enemies that reach the core keep nibbling (draining points)
while the player sits inside.  Stay longer for more carried
items + helpers boarding; launch sooner to keep what you
have.

### Vehicle respawn at core

When the player vehicle is destroyed (blocker-damage edge
case is the only damage path; see [§ Player vehicle](#8-player-vehicle)),
the player **respawns inside the core**.  This **starts the
launch countdown automatically** — the player must drive out
the opening to cancel (vehicle restored, return to base) or
stay to ratify the scramble.  Vehicle "death" is never a
game-loss; it's a forced return-to-base + a free "ready to
leave?" prompt.

## 5. Ground + walls

The painted hex layer is the substrate; full design in
[`GROUND_TYPES.md`](GROUND_TYPES.md).

### Palette

**Eleven ground types** in three sub-palettes:

- **Water** (4): sea / water / rapids / waterfall —
  drainage seeds with progressive drop.
- **Land** (5): sand / grass / hill / rock / steep_rock —
  progressive slope.
- **Structure** (2): wall / wall_high — height-override
  structures, placeholder red colours (the chosen colour will
  change; the placeholder is meant to stand out during
  development).

Loadable form in
[`../examples/palette.json`](../examples/palette.json).

### Walls — economy + topology

Walls (both heights) are **free in points** but **not
instant** — an NPC helper must spend construction time at the
build site (default 10 s for `wall`, 20 s for `wall_high`).
Bridges between walls (the `cy`-layer deck mechanic from
@PLAN46 Systems #3 + #4) are a **second-phase feature**; same
free-but-timed economics when they ship.

### Wall topology — drivable ends + recognised entrances

A wall hex with **exactly one wall neighbour** is a wall
**end**.  The face of the wall hex opposite that neighbour
renders as a **ramp** (drivable, slope-value of `hill`); the
other non-wall faces remain sheer.  Open ends therefore let
enemies roll up onto the wall — to actually defend, the
player must close the perimeter (every wall hex has ≥ 2 wall
neighbours).

**Two wall ends within 1-3 hexes of each other form a
recognised ENTRANCE.**  The non-wall hex(es) between them
become a preferred entry point — the flow field routes enemies
through; the player concentrates defensive fire there.  A
fully closed perimeter has **no entrance**, so enemies have no
path to the core and fall back to **nibbling the nearest
wall** (slow attrition, but the wall *will* fall).

⚠ **A GATE IS NOT A DEFENCE — measured, plan 12 B7.**  A sealed
wall buys 70% more time for an unattended base (61 ticks bare,
**104** sealed).  The *same* wall with its middle hex left open
falls in **62** — one tick over the undefended clock, against
the seal's forty-three.  Walking through an entrance costs an attacker nothing
whatever, so **a wall buys time only where it has to be
chewed**.

That does not make entrances a mistake; it prices them.  An
entrance is a decision about the *player's own* convenience —
their vehicle, their helpers, their salvage runs — paid for in
full, and its defensive value is entirely in what the player
puts BEHIND it.  A closed perimeter is what costs the attacker
time; a recognised entrance is what costs the player nothing to
drive through.

### Wall climbability per enemy type

Moved to [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Wall
climbability per enemy type, where it is one table with the
height-step rule that reads it — a climb limit is meaningless
apart from the step it is compared against.

### Swap pits — hot-swap bay pattern (planned)

For skilled tower play across the boost / overload / type-
swap spectrum (§ 7 Tower overload + hot-swap), a standard
closed perimeter doesn't leave room near each tower for a
parked spare top.  Skilled wall authoring carves a **swap
pit** — a short inward indentation beside each swap-ready
tower, sized to hold a **full-wall-width spare top** and
still leave the player vehicle's path clear for in/out swap
traffic.  Without the pit, the spare top blocks the
corridor and the swap loop can't run.

A single pit supports **three patterns** at increasing
skill levels:

1. **Boost-cooldown mitigation** (mildest).  Park a same-
   type spare; between engagements, swap the just-boosted
   top out for the spare while the tired one repairs on
   the ground.  Doubles effective boost frequency without
   touching overload.
2. **Overload strain-cycle.**  Same same-type-spare setup,
   but used to sustain the more aggressive overload mode.
3. **Tactical type-swap** (when future variants ship).
   Park a different-type spare; mid-combat swap to match
   the current threat profile.

Maps with multiple expected threat types may carve **multi-
stall pits** — wider indentations holding several spares
(e.g. one same-type for strain-cycle + one different-type
for weapon profile).

**The pit also holds the player.**  Overload is presence-
locked (§ 7 Tower overload), so the player vehicle sits
parked at the tower for as long as overload runs.  A
well-authored swap pit places the **player's parking hex
behind the wall line** — out of enemy reach — so the player
isn't simultaneously a blocker (§ 8 conditional damage)
during the overload session.  Pit geometry therefore has
three constraints: hold the spare top, leave the swap path
clear, AND give the player a safe parking hex.  Tightly
packed pits sacrifice one of the three; truly well-
designed pits get all three.

The wall toolset already supports the pattern as geometry
(it's just authored shape, no new mechanics needed).  The
tactical payoff arrives when overload + type-swap (§ 7)
ship — until then, the pit is just an unused widening.
Maps may still ship pits today as **affordance hints** for
forward-compatible base layouts.

## 6. Spawn system + waves

Plan 03 designs the spawn marker layer; plan 04 places markers
in maps.

### Multi-direction spawn markers

A second sparse data layer (`hash<Marker[q,r]>`) parallel to
the painted ground.  First marker variant: **spawn point** =
hex + one of 6 hex directions (the approach heading).  A base
typically has multiple spawn markers; enemies appear at each
and head along the marker's direction until they enter the
scrambler bubble, at which point they pivot to engage mode
(flow field toward the core).

**Close-spawn auto-disable.**  At landing, markers within
`close_spawn_disable_radius` (default 10 hex) of the core are
**silenced for the mission** — the visible marker remains as
map atmosphere but produces no enemies.  Map authors place
enough markers (~4-6 spread across a starter map) that any
reasonable landing leaves ≥ 2 active.

### Wave list (validation placeholder)

Authored as a flat sequence of integers in
[`../examples/waves.json`](../examples/waves.json) — each
wave's enemy **count**, with a fixed `inter_wave_delay`
between waves.  Per-enemy spawn-marker selection is **random**
at spawn time among active markers; direction comes from the
picked marker.  Default: `[5, 8, 12, 20, 30, 50, 80]` —
seven progressively-larger waves.

This is a **placeholder** for the eventual economy-driven
model (see § Future expansion): waves stop being authored and
become *output of* the robot economy state — supply lines
deliver, factories fabricate, mines fuel.  Players can alter
the economy to thin the waves.

### Wave-1 triggers — walls or provocation

Wave 1 fires when **either**:

(a) The player has built **N walls** (`wave_1_wall_trigger`,
default 8) — the act of laying perimeter is the commitment.

(b) The player has driven onto a spawn marker that sits
≥ `wave_1_provocation_distance` hex (default 12) from the
core — touching it = poking the enemy.

Markers **very close** to the core never trigger; map authors
shape pacing by close (safe) vs far (provocation) placement.
Once activated, the wave list proceeds via its scheduler.

### Pre-walk visibility — the scramble decision window

When a wave begins, enemies **appear at their picked spawn
markers and stand visible** for `pre_walk_visibility_interval`
(default 5 s) before walking.  Active markers pulse during
this window.  This is the player's window to see what's
coming (how many, from which directions) and decide whether
to stand or scramble before any enemy has moved.

### No wave HUD

There is **no on-screen wave indicator at all** in validation
— no wave-number display, no inter-wave countdown, no banners.
The player discovers wave shape and timing by **moving around**
(scouting active spawn markers, watching which pulse) and by
**internally learning the rhythm** across plays.  Diegetic
principle applied uncompromisingly to waves: the in-world
signals (marker pulse + pre-walk visibility) are the entire
wave UI.

### Wave 7 cleared → free scramble

If the player clears the final wave with the core alive, they
enter a **free scramble phase**: no more enemies, so they can
ferry tower-tops at leisure and launch with full carry.  Most
plays won't reach the final wave — the curve is designed so
the scramble decision usually has to be made mid-list.  The
final wave being cleared is the "perfect run" outcome.

### Enemy movement, and what stops it

Moved to its own document — it outgrew a subsection of this
chapter.  [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) is the spec:
the two steering modes and the handoff between them, passability
as a height step, bodies as terrain, why sealing the perimeter is
punished rather than forbidden, structural wall HP, and the rule
that the tick resolves once.

## 7. Combat dynamics

### Towers — pulsed laser, attack-count decay

All towers ship validation as a single type: **pulsed laser**.

- **Range** 15 hex (~20 m); LOS is the sight line, and what
  blocks it depends on WHERE the obstacle stands as well as how
  tall it is — see § LOS is a HEIGHT question below, which plan
  12 B5b built and which corrects the *"`wall_high` blocks,
  `wall` does not"* shorthand this line used to carry.
- **Fire interval** 1 shot/s; **damage** 10/shot.
- **Shot budget** 30 shots per charge; once spent, the tower
  **goes black** and stops firing — *decay is per-attack, not
  per-time*.  An idle tower in a quiet corner never decays.
  A tower covering a busy entrance burns through its budget
  fast.  Player repairs to refill the budget.

⚠⚠ **A tower without a cleanup crew makes a base fall SOONER
— measured, plan 12 B7.**  Same base, same waves, the only
difference a single tower behind a sealed wall: **104 ticks
without it, 95 with**.  The tower is not weak; it kills eight
of thirteen attackers.  What undoes it is that **a kill is a
permanent terrain change and nobody can reverse it**: three
bodies on one hex is 1.5 m, a robot climbs 2.0, and a 3.0 m
wall needs a 1.0-2.0 m step — so the five survivors walked over
their own dead onto a wall that never lost a tenth of its HP.  The same heap also blinds the tower
(§ LOS is a HEIGHT question) and pushes the attacking column
off its axis, where more of it can reach the core.

This is not a balance bug to tune away.  It is the design's own
loop demanding the piece plan 12 did not build: **salvage
decays, so bodies have to be collected, and collecting them
means driving into the kill zone at the worst possible
moment.**  That is exactly the test in
[`CLAUDE.md`](../CLAUDE.md) § What dryopea is — *does this put
something in the player's hands at a moment when using it costs
them something?*  A tower is the strongest argument the game
has for why the player cannot lean back.
- **Repair rule.**  A **firing (red) tower cannot be
  repaired** — repair only applies to a tower whose top is
  either **black** (decayed in place, no longer firing) or
  **detached** (sitting on the ground, off the tower).  The
  player walks up to a black-in-place tower and refills the
  budget, or up to a detached top on the ground and heals
  it.  An actively-firing tower must be **stopped first**
  (decay finishes, or player detaches the top to ground it)
  before repair becomes possible.  This isn't arbitrary game
  balance — engineering-wise it's just **bad practice to
  fiddle with a heavy energy weapon while it's powered on**.
  The mount cools, the capacitors discharge, the safety
  interlocks unlatch — *then* maintenance is safe.  This is
  the rule that makes Tower overload (§ below) a *swap-
  mandatory* cycle.

Tower top colour signals state at any zoom:

- **Red** = healthy, firing.
- **Black** = decayed or salvaged (uniform with body — instantly
  readable as "spent" from across the base).
- **Pink** = boosted — drive to a tower and press boost; the
  laser pulses harder (higher fire rate / damage / range) for
  a **fixed timed duration**, then reverts to red on its own.
  Boost is **fire-and-forget for the player** — once engaged,
  the player can drive away and the boost runs out on its
  own.  No points cost, no carried-top consumed.  In the
  **full mechanic** boost has a **small enduring cost**
  attached, but the cost is **mostly proportional to shots
  fired**, not to boost time: a boost engaged on an idle
  tower with no targets in range costs almost nothing, while
  a boost during heavy wave pressure burns shots fast and
  brings the tower's maintenance window forward.  The wear
  is the same kind of strain that the more aggressive
  **overload** mode (§ Tower overload) accumulates rapidly
  — boost just sits at the mild end of the same spectrum.
- **Boost cooldown + active-maintenance mitigation.**  After
  a boost ends, the tower needs **a window of normal-output
  operation** before another boost is allowed (the rated-
  output cool-off — capacitor recharge, optics realign).
  Lazy play waits the cooldown out.  Skilled play
  **mitigates it via active maintenance**: pull the top off
  (it stops firing → repair allowed by the rule above) →
  repair on the ground (resets strain *and* clears the
  cooldown) → re-mount → boost again.  Net effect:
  maintenance-effort caps boost frequency, not pure timer.
  A player willing to do the pickup-drop-repair cycle
  between engagements chains boosts at roughly twice the
  lazy rate — the same swap-pit infrastructure that supports
  overload also supports this milder loop.

**Validation tier ships boost with strain disabled** as a
simplification; strain + cooldown + overload + the hot-swap
cycle arrive together in a later phase.

### Tower-top salvage — the scramble mechanic lived tactically

A tower's red top is a **detachable carry object**.

1. Player drives to a healthy tower, presses pickup → top
   detaches, tower goes black (stops firing); the red disc
   floats above the player vehicle.
2. Player drives to a destination, presses pickup again.
   Two valid destinations:

   - **Another black tower** → top installs there, that tower
     goes red instantly (**fast repair**, consumed).
   - **The core building** → top added to **scramble
     inventory** for the next base (per Q4 closure: future
     model has specialised tops + a limited-loadout pre-mission
     pick from the persistent station inventory; validation
     places no effect yet).

The same disc, two mutually exclusive uses, decided every
sortie.  The scramble decision lived inside every combat run.

### Tower overload + hot-swap — high-skill upkeep loop (planned)

Boost and overload are **two points on the same strain-vs-
output curve**, but they have **opposite input models**:

- **Boost** (§ above) is **timed and fire-and-forget**.  Tap
  the boost on, the timer runs, the player is free to drive
  off and deal with other things.  Strain is small and tied
  to actual shots fired — boost in a quiet moment costs
  almost nothing; boost during heavy fire brings the next
  maintenance forward.
- **Overload** is **player-presence-locked**.  The player
  must remain at the tower for the entire duration —
  vehicle parked on the engagement hex, holding the overload
  key.  Leave the hex or release the key and overload ends.
  Output is much higher than boost, **strain per shot is
  also higher** (the laser runs even harder above rated
  limits), and the player is *committed*: they can't ferry
  tower-tops, can't escort helpers, can't reposition for a
  different threat.  The player's own attention and position
  are part of the cost.

The strain mechanic is uniform across both modes — every
shot fired adds a small amount of strain to the top, scaled
by the output level (normal < boost < overload).  What
changes between modes is **how fast strain accumulates per
unit time of combat**.  Where boost-tier strain is
recoverable by simply letting the tower idle for a bit,
overload accumulates strain fast enough that the only way
to sustain it is the hot-swap cycle below.
Skilled play turns the strain into a manageable upkeep
loop, and the same infrastructure also enables **tactical
type-swapping** between different tower-top weapons.

(Side-effect of the presence rule: a player parked at an
overloading tower is also a *blocker* — see § 8 Player
vehicle.  If their parked hex sits on an enemy path to the
core, they take nibble damage for as long as they hold the
overload.  Overload + safe parking = sustainable;
overload + bad parking = quickly fatal.  Yet another reason
swap pits (§ 5) are authored to keep the player's parking
hex *behind* the wall line.)

**The strain-cycle loop (swap-mandatory).**

Because a firing tower **cannot be repaired in-place** (see
§ Towers — Repair rule), the player can't shortcut the
strain by standing at the tower and healing it.  Strain
accumulates while the top is mounted and firing.  The only
relief is to *get the top off the tower*:

- **Hot-swap when strain reaches the player's chosen
  threshold.**  Before strain burns the top out, the player
  swaps in a **second tower-top parked in a swap pit
  nearby** (same single pickup-drop verb as Tower-top
  salvage).  Strained top drops to the ground; spare goes
  onto the tower; firing resumes red instantly.  No mid-
  combat stand-around rebuild.
- **Repair-on-the-ground.**  Once the strained top is on
  the ground, it's no longer firing, so repair *now*
  applies.  The player (or helpers) heal it back to ready
  while the active spare runs.
- **Cycle.**  When the active spare reaches its own strain
  threshold, swap back to the now-repaired original.  Two
  tops alternating between *mounted-and-overloading* and
  *grounded-and-repairing* keep overload-grade firepower
  running indefinitely — the high-skill ceiling.

The cadence the player must learn: swap **before** strain
peaks, not after.  Mistime the swap and the top burns out
mid-mount (forced black state) — still recoverable, but
now you've lost the overload window and start the recovery
behind the strain curve.

**Tactical type-swap (when future tower variants ship).**

The same swap-pit setup lets the player **switch weapon
profiles mid-combat**: park a non-laser top (anti-insect
pulse, area splash, anti-elemental dampener — see § Future
tower types) in the pit instead of (or alongside) a same-
type spare.  Mid-wave the player swaps the active top for
whichever type the current threat calls for — no rebuild,
no beacon-ferry, just a pickup-drop cycle.

Cross-type swapping adds **ammo bookkeeping**: validation-
era laser tops use attack-count decay only, but several of
the future-variant weapons consume **ammo** (per-shot
consumable, distinct from decay), so the swap workflow
includes pre-loading the spare top and reloading on
recovery.  More steps, more planning, more reward.

**The opportunity-cost layer.**

A spare top sitting in a swap pit **is a top that is NOT
firing on a different tower**.  Every reserve top in the
base is a slot of tower-firepower the player chose to keep
in reserve instead of mounted active elsewhere.  Skilled
play is a balance:

- **Many active tops, no spares** — maximum firepower per
  second, no type flex, no strain-cycle.  Vulnerable to
  type-shifting threats and to overload-only kill windows.
- **Many spares, fewer active** — fewer towers firing at
  any moment, but every active tower can overload-cycle
  indefinitely and switch type to match incoming threats.
  Vulnerable in the cold-start phase before swapping pays
  off.
- The right ratio is **per-map, per-wave-composition**: a
  map with mixed enemy types (insects + robots) rewards
  type-swap; a map with mass-robot pressure rewards strain-
  cycle; a map with thin-but-constant pressure rewards
  active-firepower.

**The bottleneck is physical space.**  A spare top parked
beside a tower occupies roughly a full wall-section's width;
a standard closed perimeter has no room to stash it without
blocking the player vehicle's swap traffic.  Overload + type-
swap therefore require walls authored with a **swap pit**
(see § 5 Swap pits) — an indentation that holds the spare +
keeps the swap path clear.  This pushes the strategic
decision *back to base design time*: skilled players plan
overload-ready and swap-ready towers as a wall-layout choice,
not just an in-combat input.

Validation tier: **deferred**.  Boost (pink, time-limited,
no strain) ships at validation; overload + strain + spare-
top swap + type-swap + ammo bookkeeping + swap-pit
authoring arrive in a later phase, once the base tower
model is stable, attack-count decay is tuned, and the
future tower variants (with ammo) have landed.

### New towers via beacon ferry

To order a new tower, the player **carries a tower beacon
from the core to the chosen build site**:

1. Drive to the core's **tower-core retrieval face**, press
   pickup → points debited from wallet, beacon spawns above
   vehicle.
2. Drive to the chosen 7-hex centre, press pickup → beacon
   placed; a lander touches down on it; helpers handle any
   remaining construction time.

The single pickup/drop key handles all intentional carries
(beacon at core, tower-top at healthy tower, deposit at
target).  Loot drops are auto-pickup on drive-over (too cheap
a decision to need an explicit press).

### Future tower types (deferred)

Validation ships the placeholder laser only.  Future tower
variants are **unlocked content** — found on the map through
scouting, brought back to the core, become orderable from
then on.  Different types specialise (anti-insect pulse,
anti-elemental dampener, anti-comms-priority disruptor,
area-effect splash, …; exact catalogue TBD).  See
[§ Economy + progression](#13-economy--progression).

#### Damage TYPE is the axis, and it is a triangle rather than a ladder

Project owner, 2026-08-13.  Designed, **not built** — plan 12
B5a ships one untyped weapon.  What makes the catalogue worth
having is that no type dominates: each is bought with a real
cost, and two of the costs are things the player does to
*themselves*.

| Type | Against armour | Targets | Salvage left | How long the wreck blocks | Against insects | Its own cost |
|---|---|---|---|---|---|---|
| **laser** | poor | single | little — it vaporises | short | good | none; instant, so it cannot miss |
| **artillery** | good | single | — | — | — | travel time: a moving target can be missed, and a miss is a wasted shot |
| **explosive / splash** | — | several | — | — | — | **damages the player's own walls** |
| **EMP** | — | — | worst: destroys the high-value electrics (brain, wiring, motors) and leaves the chassis nearly whole | **longest** | nearly useless — it only lightly burns them | maximum obstruction, minimum salvage |
| **flame thrower** | — | several, but only SMALL ones | — | — | **excellent** against a swarm | markedly **shorter range**, so it has to be placed where things get close |
| **sniper** | heavy gun | single | — | — | — | **slowest to aim**, and it is *especially* bad at very short range — the best weapon there is at long range and nearly useless up close |

⚠ **The sniper adds an axis the others did not need: range is a
PROFILE, not a number.**  Every type above is "effective out to
N hexes"; a sniper is bad below a minimum and best at the far
end, so the thing a tower answers is *how well does this shot
land at this distance* rather than *is this in range*.  The
flame thrower is the same curve inverted — best up close,
nothing at distance — so the two bookend both axes at once, on
range and on target size.  A catalogue whose extremes are
opposites in two dimensions is a coherent one.

⚠ **Two enemy properties fall out of that table, and neither is
invented for it.**  **Armour** decides laser-vs-artillery, and
**size** decides the flame thrower — and size is already
load-bearing for a different reason: a *big* robot's body seals
the hex it falls on and a small one does not
([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Bodies are terrain).
A property that two unrelated mechanics both need is one worth
having.

Three of those columns are the same three-way tension, chosen
per shot: **kill speed vs salvage vs how long the corpse plugs
the gap** (see [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) §
Bodies are terrain — a fresh big wreck seals its hex, and a
plugged chokepoint makes the wave attack the *wall* instead).

#### Aiming costs time, so speed is a defence

A tower **turns toward what it is shooting, and that takes
time.**  Three consequences, and none of them is an ability
anybody designed for an enemy:

- **Switching targets is expensive.**  Time spent traversing
  is time not firing, so a tower harassed by scattered
  approaches does markedly less damage than one watching a
  single lane.  ⚠ The **sniper is the extreme case** — slowest
  to aim, so it is the type that most needs a funnel to look
  down and the one a spread approach punishes hardest.  That is
  a placement decision the player makes with walls, which is
  the loop this whole design is built on.  ⚠ This cuts against the "chew the whole
  perimeter" steering (`ENEMY_MOVEMENT.md` § Sealing the
  perimeter): enemies converging on one route are *easier*
  for a tower, which is a tension worth keeping rather than
  balancing away.
- **A shot that has become impossible is not fired.**  If the
  line is blocked when the trigger comes, the tower holds —
  it does not spend the shot into a wall.  That is a decision
  at fire time, and it is what keeps the per-shot budget
  honest.
- **A shot already in flight can still be wasted.**  Artillery
  has travel time, so a target that moves behind a wall after
  the shot is away is simply missed.  **Faster enemies
  therefore dodge without trying to**, which gives enemy speed
  a defensive role nothing had to grant it.

#### Where a tower goes: inside the base, or out on the ridge

Owner, same session, and it is the pay-off of the two tables
above.  A **sniper or artillery piece inside the base is badly
placed** — everything reaches it at the range it is worst at —
and the same gun **on an outer ridge is excellent**, covering
the whole approach at the distance it is built for.  A flame
thrower is the exact opposite: it wants the entrance, where
things arrive close and in numbers.

So the catalogue turns *where* into a decision rather than a
formality, and it is one the player makes with the same walls
they defend with — funnel the approach, then look down it.

The ridge is paid for twice.

**In combat: enemies DO attack towers that hurt them** — see
§ Retaliation is an information rule, below.  A tower inside
the base is usually behind a wall, so retaliation has to chew
through the perimeter to reach it; a tower on an outer ridge is
standing in the open.

**In logistics:** a tower's budget is spent per shot and only a
player standing at it can repair it, boost it, hot-swap its top
or (§ Damage TYPE) aim it at a corpse.  So an outer-ridge tower
means driving out through a live wave, mid-fight, to service
the thing that is doing the most work.

#### Retaliation is an INFORMATION rule, not a threat rule

Owner, 2026-08-13, and it is the scrambler's fiction made
mechanical.  Robots do not evaluate which tower is most
dangerous.  They respond to damage they have **information
about**, and the scrambler is what decides how much that is:

- **Scrambler up** (the normal case): an enemy targets a tower
  that has hurt **it, personally**.  Comms are cut, so each
  robot knows only its own injuries — retaliation is
  individual, late and easy to dilute.
- **Scrambler down**: they share what they know, so a tower
  hurting **anyone's companions** becomes a target for all of
  them.  Fire gets focused, and the defences that were quietly
  doing the work become the thing the wave is aimed at.

⚠ **And retaliation never overrides ROUTING, which is what stops
it dominating.**  Wanting to reach a tower is not the same as
being able to: an enemy that has no route to the thing shooting
it goes on doing what it was doing — heading for the heart of
the base.  So **most of the time nothing changes**, because a
tower inside a closed perimeter simply cannot be reached, and
the wave keeps its attention on the core (chewing the wall in
its way, by the existing rule, rather than the wall nearest the
tower).

That is exactly why *"towers can often be placed behind walls"*
is the normal answer, and it is what makes the outer ridge a
real gamble rather than a free upgrade: a ridge tower is exposed
because it is **reachable**, not because it is outside.

⚠ **That is a real second phase, not a modifier.**  The core is
the scrambling tower ([§ 4](#4-the-core--the-scrambling-tower)),
so whatever ends the scrambling — and *when* it ends is the one
thing this note does not settle — flips the wave from a crowd
into a coordinated force at exactly the moment the player has
least to spare.

⚠ **The ONE exception under scrambling is the boss — and it is
NOT a different AI.**  Owner, 2026-08-13, and the framing is
the point: *bosses are not special in their AI; their size and
their options are different, and that is what makes them
special events.*

Every class runs the same rules — the same routing, the same
targeting, the same retaliation.  What a boss has is different
**data**: a 2×2 footprint ([§ 10](#10-three-enemy-tiers)), so
it cannot fit a one-hex entrance; and the option to **share
what is hurting it** with the robots around it, being the
engineering / command platform of robot society (§ Boss =
mobile REPAIR PLATFORM) and carrying enough power to reach them
through the jamming.

The event is what those two produce on their own: **big
footprint → no route in → stuck at the wall → a tower shoots it
→ the squad is put onto that tower.**  Nobody wrote a boss
behaviour; a boss is a row in the same table with different
numbers, and the fight around it is emergent.

⚠ That is the same discipline the movement rules already keep —
a class's climb limit is *"its whole contribution to
passability"*, which is why one distance field serves several
classes ([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md)).  A boss
that needed its own mover would break that, and the properties
version does not.

Two consequences worth writing down before anybody builds it:

- **A tower that has never fired has hurt nobody, so nothing is
  looking for it.**  A reserve held in check is safe by the
  rule rather than by a special case.
- **It gives an enemy a NEW reason to break a wall.**  Today
  § Enemy targeting attacks a wall only when no path to the
  core exists; retaliation adds "because the thing shooting me
  is behind it", which is a second entry in that priority list
  and the first one that is about a *defence* rather than the
  core.

#### LOS is a HEIGHT question, not a table of materials

**Built by plan 12 B5b**, and the rule is one sentence: *a shot
travels in a straight line from the tower's eye to the target's
body, and anything the line does not clear stops it.*  The eye
is the tower's hex plus 6.0 m, the aim is the target's hex plus
the 1.0 m robot standing on it, and an obstacle is whatever
`hex_height` says is standing on a hex in between — a painted
structure plus any rubble piled on it.

⚠ **Why it is not a lookup.**  A *`wall_high` blocks, `wall`
does not* table duplicates heights the palette already carries
and disagrees with them the moment either moves; dryopea has
caught that shape twice already (`walk_ground` versus the height
rule; the painted kind versus the surface).  Reading the height
also makes the ridge work for free: a tower standing on 3 m of
rock is at 9 m and sees over things a ground-level one cannot,
with nothing written for elevation at all — which is the whole
reason the outer ridge is a place worth putting a sniper.

##### ⚠ What building it falsified

Two things this document used to say are **wrong**, and the
measurements are in `tests/12_b5b_los_budget.loft`:

- **A `wall` DOES block**, once it stands past roughly three
  fifths of the way to the target: the shot has descended below
  3 m by then.  So the rule a player learns is **a tower must
  overlook the wall it covers** — directly behind it, the robots
  chewing the far side are targets; two hexes back they are in
  dead ground.  This is the *sealing is punished rather than
  forbidden* rule arriving from a direction nobody planned:
  seal the perimeter and the besiegers stand where the towers
  cannot help.
- **`steep_rock` blocks nothing at all**, because dryopea has no
  terrain elevation: `palette.json` writes `height_override:
  null` for every terrain kind, so a cliff is as flat as sand to
  a sight line.  Nothing has to change when plan 02's slope
  solver lands — the rule already reads the height — and the
  test asserting today's answer is what goes red on that day.

And one consequence nobody wrote down: **a pile of bodies blinds
the tower that made it.**  Rubble is a height, so the heap that
ramps a kill zone shut (§ Bodies are terrain) also puts the kill
zone out of sight — ten dead robots are 5 m of wreckage.

⚠ Implementation note for whoever builds traverse time: plan 12
B5a's `tower_pick` re-chooses the nearest visible enemy every
shot with no cost to switching, which is exactly the placeholder
traverse time replaces — it will want hysteresis (stay on
target) and will still have to be deterministic, because dryopea
gates itself by replaying written-down runs.

### Enemy targeting + nibble

Enemies have **nibble** (damage-per-second melee) when they
reach a target.  Target priority (highest first):

1. **A player or NPC physically blocking the path to the
   core** (conditional damage — see [§ Player vehicle](#8-player-vehicle)).
   ⚠ Built in plan 13 V5.  *"Blocking"* means the enemy has no
   way forward AND no way round — in the open it goes round and
   the player is ignored, exactly as this list's closing line
   says.
2. **The core itself** (via entrance / broken wall) —
   drains the player's wallet, not the core's HP.
3. **The nearest wall hex** when no path through exists —
   slow attrition that eventually breaks the wall.

**Nibble reach is the core's own footprint** (built in plan 12
B6).  Priority 2 fires for an enemy within **one hex** of the
core centre — and that number is read off
`numbers.json § core.footprint_layout`, which is a radius-1
disc, rather than picked as a melee range.  An enemy at
distance 1 is not *near* the core, it is standing on it, and
the seven hexes it names are exactly the seven an arriving
wave ends up queued on.

⚠ **It is a straight-line `lat_distance`, and it is the whole
rule.**  A nibbler is a POSITION, not a target: `enemy_target`
deliberately answers an arrived enemy's own hex, because it
names what is in the WAY and nothing is.  The tempting
alternative — drain for every live enemy — passes every
arithmetic check about rates and floors while making walls and
towers pointless, since a base under siege would bleed at the
rate of a base that had been overrun.

In the absence of a blocker, the player and NPCs are
**ignored**.

### ⚠ There are TWO boss kinds, and they are opposites

Owner, 2026-08-14: **the big combat robots are bosses of a
different kind than the repair platforms.  They need crystals for
their core and their power weapons.**

| | **repair platform** | **big combat robot** |
|---|---|---|
| origin | the **engineering role** — industrial machinery repurposed (`SETTING.md` § Robot diversity) | the **defence / security role** — built for the AI-vs-AI wars and mothballed (`SETTING.md` § Combat bots are dormant) |
| built to fight? | **no** — never attacks towers directly | ⚠ **yes** — it is the only thing in the game that was |
| what it does | heals nearby regulars; marks towers for others | **power weapons** |
| crystal demand | its **energy core** | ⚠ its energy core **and its weapons** — so it costs more |
| where it comes from | a factory's boss-production machine | ⚠ a **military stockpile**, woken — it is the top of that escalation ladder |

⚠⚠ **So crystal gates MILITARY capability specifically**, and
that gives the run a long arc nothing else does:

- **Mid-game**, cutting the crystal line
  ([`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § 1a) is insurance
  against ever meeting a combat boss.  It is a good decision and
  a player will make it for good reasons.
- **End-game**, the combat bosses are the units that can actually
  hurt an old one's servants — and ⚠ **the player who strangled
  the crystal line hours ago has crippled their own allies.**
  A correct decision, paid for much later, by the same person.
  It needs per-planet persistence to express
  ([`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § Open questions 1),
  which is one more argument for it.

#### ⚠⚠ It shoots TOWERS — the first real challenge

**Confirmed by the owner, 2026-08-14:** the combat boss shoots
towers, *"it is the first real challenge a player will face
beside being rushed earlier on.  Big chunks of their base can
fall easily then, and their layout should be changed to have a
chance to face a combat robot."*

**Why it is the first REAL one.**  Everything before it threatens
the **core** — the wallet drains, the run ends, and the answer is
always more of the same defence.  A combat boss threatens the
**investment**: towers cost 100 points each, carry a 30-shot
budget only a player standing at them can refill (§ 7), and are
the entire reason a base can hold anything.  ⚠ It is the first
enemy that makes you **poorer** rather than merely closer to
losing.

⚠ **And the failure compounds, out of rules that already ship.**
A felled structure leaves rubble
([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Bodies are terrain,
`damage.loft::break_structure`), rubble raises its hex, and a
raised hex is a ramp.  So a base losing towers is simultaneously
a base **growing the staircase** its attackers climb.  Nothing
needs adding for "big chunks fall easily" to be true; it already
is, as soon as something can fell them.

##### ⚠⚠ What it invalidates: the narrow funnel

This is the precise sense in which *"their layout should be
changed"*, and it is sharp rather than vague:

> § 7 already makes **funnel width** decide *whether a boss fits,
> and whether its builders have room to reach it*.  Against the
> **repair platform** that is a complete answer — deny the 2×2
> entry and it cannot act.
>
> ⚠⚠ **A boss that shoots does not need to come in.**  Every base
> built on the tight-entrance answer is defeated without the boss
> ever entering it.

That is a genuine invalidation of a *learned optimum*, which is
the most valuable kind of difficulty step: the player is not
beaten by a bigger number, they are beaten by a design decision
that was correct until now.

##### What the new layout wants — inferred, not decided

⚠ **Reasoning from shipped mechanics, flagged as such.**  The
counters the existing rule set already offers:

- **Depth instead of a wall** — layers to cross while towers
  whittle, since a single line it can shoot from outside is worth
  nothing.
- **Dispersal instead of concentration** — clustered towers give
  overlapping fire *and* a single volley that removes a battery.
  The trade becomes real for the first time.
- **LOS breaking** — `wall_high` at 5.0 m and terrain elevation
  decide what a 6 m tower eye can see (§ 7 § LOS is a HEIGHT
  question).  ⚠ Today `tower_sees` is one symmetric line, so a
  layout cannot give a tower sight the boss lacks.  What *does*
  give it an edge is **range** — see § The dead band below.
- ⚠⚠ **Evacuation** — and this is the good one.  Tower-tops are
  **carryable** (§ Tower-top salvage, § New towers via beacon
  ferry).  Against an enemy that destroys towers, pulling the top
  off one before it dies is the game's *signature* mechanic —
  scramble-and-salvage — arriving as a **tactical** move inside a
  single fight rather than as the end of a run.

**Design test:** ✓✓ — the answer to the first real challenge is
to drive into the fire and carry something out.

##### ⚠⚠ The DEAD BAND — boss range 10 against tower range 15

Owner, 2026-08-14: **the default boss weapon range is 10, shorter
than a tower's 15.**

Those two numbers do the whole job, because the gap between them
is a **five-hex band in which a tower can shoot and the boss
cannot answer**.  Everything below is arithmetic on the numbers
already in `numbers.json`:

| | |
|---|---|
| tower range | **15** |
| boss weapon range | **10** |
| the dead band | **5 hexes** |
| boss speed (phase-3 figure) | 1.0 hex/s → **5.0 s to cross it** |
| tower fire interval | 1.0 s → **5 shots**, 50 HP, per tower |
| against 200 HP | ⚠ **four towers' worth of approach fire** |

⚠⚠ **And it yields a layout rule a player can actually learn:**

> **Put the perimeter in the dead band.**  A tower set back a
> distance `D` from the wall can hit a boss stopped at that wall
> when `D ≤ 15`, and is safe from it when `D > 10`.  So the
> tower placement window is **10 < D ≤ 15** — five hexes wide,
> and outside it you are either out of the fight or in reach.

**That is the layout change, made concrete.**  Before the combat
boss the right answer was towers *close* to the wall — better
coverage of it, a shorter drive to service them.  Now they must
come back ten hexes, and ⚠ **the cost is exactly the thing the
game is about**: a tower that far back covers less wall and is a
much longer drive to refill, while its 30-shot budget is
presence-locked (§ 7).  Surviving the boss makes your own battery
harder to keep loaded.

##### ⚠ Longer-ranged bosses must PAY for it

Owner, same statement: there will be different bosses — *"some
with far smaller range weapons and some with perhaps even more
than 15, but they need to have mitigations like very slow speeds
and a wind-up time for their weapon."*

**So range above 15 is not forbidden, it is priced**, and the
currency is named: **speed** and **wind-up**.  That makes the
boss family per-type DATA in the sense § 10 already demands —
three numbers per boss, no new code path.

| boss range | what beats it | what the player does |
|---|---|---|
| **under 10** | depth alone; it dies crossing a wider band | build, and stand back |
| **10** (default) | the placement window above | **architecture** |
| **over 15** | ⚠ nothing you can build — it outranges every tower you own | ⚠⚠ **go out and deal with it** |

⚠⚠ **That last row is the good one.**  A boss that outranges your
towers cannot be answered by a base at all, so its mitigations
have to make it *reachable*: **very slow** means the player's
vehicle (3.0 hex/s, twice an enemy) can close on it, and a
**wind-up** means there is a telegraphed window to move, evade,
or pull a tower-top off before the shot lands.

So the boss family produces a genuine progression of *kinds* of
answer rather than of numbers:

- short range → **you build**;
- default range → **you build differently**;
- long range → **you leave the base during the fight.**

⚠ And the third is § What kind of game this is again: the answer
to the biggest gun on the map is to drive at it personally, at
the moment your base most needs you standing in it.

### Boss = mobile REPAIR PLATFORM (phase 3)

The boss class is, in the fiction, the **engineering / repair
role** in robot society (SETTING.md § Robot diversity) —
heavy industrial machinery, 2×2 footprint, **not a combat
unit**.

Its primary phase-3 mechanic: **heals nearby damaged regulars
over time** (range 3 hex, default 5 HP/s per repaired unit).
Killing the boss stops the heal pool — high-priority target.

Secondary phase-3 behaviours:

- **"Guard me" command** to nearby regulars (formation play).
- **Localised tower-attack retaliation:** towers that fire on
  the boss are *marked*; regulars in the boss's immediate
  communication vicinity (short range — the boss locally
  overcomes the scrambler) re-target marked towers.  Boss
  itself never attacks towers; stays focused on the core.

#### Builders repair the BOSS — the loop closes both ways

Owner, 2026-08-13.  A **builder** (§ 10 § Small robots) can
repair a damaged boss, under three conditions, and every one of
them is a counter the player can play against:

1. **Near it** — separate them and the repair stops.
2. **Room to reach it** — the boss is 2×2 and needs an adjacent
   free hex for a builder to work from, so **tight terrain and
   a narrow approach can deny the repair outright**.
3. **Left alone by the towers** — a builder that is being shot
   is not repairing.

⚠ **Together with the heal above this is a MUTUAL-repair knot**,
and that is the interesting part: the boss heals nearby damaged
regulars, and the builders among those regulars heal the boss
back.  A boss escorted by builders is a self-sustaining group,
and the player's question stops being *how much damage can I
do* and becomes **where do I break the loop** — kill the small
cheap unit, or the big expensive one?

⚠ **Two of the three counters are ARCHITECTURAL**, which is the
best thing about it: "near" and "room to reach" are decided by
where the player put their walls, not by how much DPS they
brought.  A funnel that leaves a boss no shoulder-room is a
counter built before the wave arrived.

⚠ **Worth checking in play before it ships:** the arithmetic can
make the knot unbreakable.  A tower is 10 HP/s and a boss heals
5 HP/s per unit, so a couple of builders repairing at a similar
rate out-heal a single laser.  That is a fine tension — it is
what splash and artillery are FOR (§ Damage TYPE) — but it
needs the rates chosen deliberately rather than inherited.

Player tactics: isolate the boss from escorts to cut the
order chain; salvage tower tops to silence specific towers
and keep them unmarked; **and pick which end of the repair
loop to break.**

2×2 footprint **cannot fit through 1-hex entrance gaps** —
boss must use a 2-hex+ gate or **break the wall** to make
its own path.  Wall topology becomes a tactical lever against
bosses specifically.

**And it is quite a bit slower than everything else**
(§ 10 § Speed must NOT be tied to the tick) — deliberately, so
the player has time to strategize against it rather than react
to it.  ⚠ Three things already written down compound with that,
and none was designed for it:

- it stays in a tower's field of fire far longer, so it takes
  the fire that makes it start marking towers (§ Retaliation);
- **artillery cannot miss it**, where a fast scout dodges the
  same shell without trying (§ Damage TYPE) — the two ends of
  the speed spectrum are the two ends of the weapon matching
  problem;
- slow, 2×2 and badly routed is the same unit three times over:
  it is the one left standing outside, which is exactly where
  its builders have room to reach it.

Until phase 3 ships, towers cannot be damaged by enemies at
all.

## 8. Player vehicle

### Role — noncombatant manager

The player vehicle **cannot harm enemies** (no weapon) and is
**not hunted by them** in the general case.  Combat is
entirely between **towers and enemies**; the player
choreographs.  Their actions: positioning, timing of repairs /
boosts, ordering towers + helpers, salvaging tops, force-
launching.

This makes the player a **noncombatant base manager**, not a
gunner.

### Conditional damage — blocker exception

The only exception: if a player vehicle (or NPC helper) is
**physically blocking an enemy's path to the core**, the enemy
attacks the blocker instead.  The blocker takes nibble damage
until it moves out of the way (or is destroyed).

Consequences:

- The vehicle **cannot tank** for the core — parking in front
  of the core just makes enemies attack the player en route.
- Genuine accidental obstruction (helper standing in a narrow
  entrance, idle player parked across a kill funnel) becomes
  a *liability*, not a defence.
- The vehicle has a minimal damage model that **activates
  only by positioning choices**.

⚠ **Built in [plan 13](../plans/13-the-vehicle/README.md) V5,
and "blocking" turned out to be a property of the MAP rather
than of parking.**  An enemy attacks the vehicle only when the
vehicle is on one of its closer steps AND every other closer
step is taken AND **every sidestep is taken too**.  In the open
it simply walks round — which is this section's own *"in the
absence of a blocker, the player is ignored"* arriving as a
consequence instead of a second rule.  Both of the examples
above are chokepoints, and that is exactly where the rule
fires.

⚠ **A companion is never attacked for the same obstruction.**
Robots jam nose-to-tail without touching each other; they
attack the thing that does not belong.

⚠ **The "cannot tank" clause is arithmetic**: 100 HP at 5 HP/s
is twenty seconds for one attacker and five for four —
measured at 20.7 s.  Blocking buys a moment and costs the trip
home.

### Hover + boost

Vehicle hovers at `hover_clearance_idle` (default 0.4 m) above
the local terrain max under its footprint — rides over
terraced cliffs without clipping.

**Boost** (held key) lifts to `hover_clearance_boost` (default
3 m) for a fixed duration (default 2 s).  While boosting,
the vehicle ignores ground-slope constraints (can cross
`steep_rock`, walls, closed perimeters).  Landing on
descent is **automatically softened** — no damage from the
height drop.  Cooldown ~5 s before next boost.

### Paint-mode tint

When wall-paint mode is **on**, the vehicle body tints
red-near-white (placeholder `#e09090`); off, near-white
`#f0f0f0`.  Diegetic indicator — no HUD icon needed.  The
appearance of wall outlines while driving confirms.

### Respawn at core

When the vehicle's blocker-damage HP reaches 0, the player
**respawns inside the core** — which immediately starts the
launch countdown (see [§ The core § Force-launch](#force-launch--drive-in-hold-exit-to-cancel)).
Drive out the opening to cancel and continue; stay to ratify
the scramble.  Vehicle "death" is never a game-loss.

## 9. Helpers

NPC vehicles that do the cooperative's actual work.  Same
chassis as the player, **silver-grey** body, black front
(same facing convention).  No combat role.

### Roster

**2 starting** (emerging from the core's lift-off face on
landing), **6 maximum** (hard cap).  Helpers can be ordered
mid-mission at the core's NPC-ordering face (cost 100 points
per helper); a lander touches down at the lift-off face
~20 s later.

### Future skills

Eventually each helper carries a **per-character skill
profile** (validation: interchangeable, opaque id):

- **Building** — faster wall / tower construction.
- **Mining** — gather raw materials from terrain hexes.
- **Scouting** — faster movement / wider visibility.
- **Hacking** — (a) subvert enemy structures (planet meta);
  (b) hack enemy **robots directly** in combat (disable /
  redirect / convert).  Robot enemies are hackable; insects
  / elementals are not.
- **Engineering** — faster repair + boost on towers.
- **Crafting** — produce items from gathered materials.

Data model carries the per-helper id today so future skills
hang off existing characters without re-engineering.

### Damage → wreck → retrieve → recover

Helpers take damage in the same edge cases as the player
(blocking + phase-3 boss consequences).  When a helper
vehicle is too damaged:

1. Helper vehicle **wrecks at its hex**.  Helper inside is
   **downed but alive**.  Visible as a damaged silver-grey
   cuboid; mid-task work (carried loot, partial structure)
   freezes for another helper to resume / pick up.
2. **Retrieval** — player or another helper drives to the
   wreck + presses pickup → downed helper becomes a carry
   object floating above the carrier.  Deliver to the core →
   recovery state for ~60 s → helper rejoins the roster.
3. **No automatic respawn** — retrieval is the only way back.

### Stranded helpers — future rescue quests

A downed helper **not retrieved by force-launch** is
**stranded** at their wreck hex.  They are not lost
permanently — they become a **rescue-quest target** for the
same player on a later run, or (multiplayer) for a different
player who lands nearby.  Persists with the abandoned-bases
mechanic (§ Future expansion).  For validation: stranded is a
data state only; the rescue-quest UI is deferred.

### Carry visibility — universal rule

Anything a helper (or the player) is carrying — loot cube,
tower-top, beacon, downed helper — is **rendered above the
carrier as part of its geometry**.  No HUD; the world reads
loaded vs idle at a glance.

## 10. Three enemy tiers

Tiers **stack** rather than replace.  Each is a distinct
interaction loop the player can engage with or avoid.  Full
fiction + per-tier behaviour in
[`SETTING.md`](SETTING.md).

| Tier | Kind | Default state | Trigger to engage | Counter |
|---|---|---|---|---|
| 1 | Robots (haywire) | Territorial — react to encroachment | Encroach on their factories / mines / supply lines (future) | Walls + towers; hackable by helpers (future) |
| 2 | Insects | Passive (fly among trees) | Gather sap (smell tracking) | `wall_high` blocks; outrun delivery; or skip sap |
| 3 | Elementals (4 kinds: water / fire / wind / earth) | Dormant — keyed to stone proximity | Author-placed stones near them; disturb a gem | TBD (deferred); player-stone interactions open by design |

⚠ **All three tiers escalate, and on different axes** (owner,
2026-08-14; [`SETTING.md`](SETTING.md) § Both factions have
warriors, § Elementals escalate on a different axis).  Robots
and insects each open as *maintenance* behaviour and escalate by
gaining **units** — mothballed combat bots woken by authority (a
**step**), insect soldiers grown by brood time (a **ramp**, and
the brood does not un-hatch).  Elementals have no combat version
because **every one of them is already combat-ready**; what they
lack is a drive, and what escalation gives them is **command**,
from a woken *old one*, on rare authored occasions.

⚠ **Elementals are a SERVITOR RACE** — direct servants of the old
ones, and *not the only kind* (`SETTING.md` § "Not all are
elementals").  The other servants are **awakening-scale content
only** and must not become a fourth tier: three is the settled
taxonomy.

⚠⚠ **So two of the three escalations are quantity and the third
is INTENT** — nothing about an elemental gets stronger, it simply
starts wanting something.  Keeping that distinction is what stops
tier 3 becoming a re-skinned tier 1.

**Bosses** sit within tier 1, and there are **two kinds**
(owner, 2026-08-14): the **repair platform** — an economic unit
repurposed, which never attacks towers — and the **big combat
robot**, which was actually built to fight and needs crystal for
its core *and* its power weapons.  See § There are TWO boss
kinds, and they are opposites.

Robot wave behaviour has a *lore* explanation that retroactively
explains the wave system: **robots in the bubble are
comm-cut and individually drawn to the scrambler tower trying
to "find their way home"**.  Waves are accumulating cut-off
units, not coordinated attacks.  The approach→engage handoff
is the **bubble boundary itself**.  Full explanation in
SETTING.md § Why waves happen.

### Small robots: four roles, one AI

Owner, 2026-08-13.  The small robots are **scouts, harvesters,
builders and miners** — economic roles from the colonisation
programme, hostile only because their command links broke
([`SETTING.md`](SETTING.md) § Robot diversity).

**What differs between them is their effectiveness against a
wall, and it differs a lot.**  A miner cuts rock for a living;
a scout has no real weapon at all.  So the same wave arriving
at the same perimeter is a very different problem depending on
what is in it.

**And the scout is quite a bit faster than the others — but
lacks a good weapon of its own.**  It is the surveyor, the eyes
of the swarm ([`SETTING.md`](SETTING.md) § Robot diversity),
and it is built to look rather than to fight: it arrives first
and does almost nothing when it gets there.

⚠ **BUILT, and measured** (plan 23 K2b): the scout walks at
**2.5 hex/s** and the miner at **1.0** against the robot's 1.5,
so the same nine hexes of corridor take **6 / 9 / 14** ticks
(`@M016`).  Read that beside K0's damage table and the pairing
is the whole design in two numbers — the class that arrives
first is the class that needs 454 ticks to open a wall the
miner opens in 23.  ⚠ **The other two roles took no speed row**
(`@X062`): harvester and builder walk at the regular's pace,
because `numbers.json` § enemy_small_robots is a DELTAS section
and inventing two tunables to say *unchanged* is not free.

⚠ That pairing is what makes it interesting rather than
filler.  Speed is not just "arrives sooner" — § Damage TYPE
already gives it a defensive role, because artillery has travel
time and a fast target that steps behind a wall after the shot
is away is simply **missed**.  So the scout is the unit
artillery cannot hit *and* the unit not worth hitting, while
the miner is slow, deadly to a perimeter, and exactly what
artillery is for.  **Role composition and tower composition
become a matching problem**, which is two design threads
meeting rather than one bolted onto the other.

**The harvester's axis is what it CARRIES** — owner, 2026-08-15
(`@X053`), filling the one hole this matrix had.  Scout has
speed, miner has bite, builder has repair, and a hauler is the
richest salvage on the field: **the one worth letting through
the kill zone to collect later.**

⚠ **It cost no new mechanism, which is why it is the right
fill.**  A harvester's body is `RUBBLE_CARGO` rather than
`RUBBLE_WRECKAGE`, and `wallet.loft::loot_rate` already prices
a rubble source — two lookups written phases before anything
varied in them (plan 23 K0).  At **3×**, one body is 90 points
against a robot's 30, which is most of a helper order.

⚠ **The multiplier is chosen against § What kind of game this
is, not by feel.**  At 2× a player who was going to clear that
hex anyway barely notices, and the role says nothing; at 3× the
kill and the collection are a real decision with a cost on both
sides — and the cost is the trip, at the moment a kill zone is
the worst place to be.

⚠ **Their AI is not different.**  This is the first real payoff
of the rule in § Combat dynamics § Retaliation: they route the
same way, target the same way and retaliate the same way, and
the whole difference is **numbers per role** — damage to a wall,
and speed.  Four enemy types, no new behaviour, no new code
path.

##### ⚠⚠ And a MIX is worth its fastest member, which the roles need fixing

*"The same wave arriving at the same perimeter is a very
different problem depending on what is in it"* is true, and plan
23 K3 measured how much: three waves of twelve fall at **94 /
126 / never** (`@M018`).  But it also measured the shape, and the
shape is a problem for this design rather than for the engine:

**every mixed wave behaves like a PURE wave of its fastest
class.**  Four harvesters in front of eight miners takes the
base at 164 ticks where twelve harvesters take it at 161 and
twelve miners at 94 — so the eight miners, two thirds of the
wave and the hardest-biting class in the game, contribute
nothing at all.

The cause is positional and every rule in it is one this
document already asked for: only **three** hexes of a wall are
ever attacked (the approach fan's width — a longer wall does not
widen it, § Combat dynamics), a robot blocked by a companion
attacks nothing, and the fast class arrives first.  So four
robots hold the whole front.

⚠ **What this costs the design is the interesting half.**  The
matching problem two paragraphs up — role composition against
tower composition — needs a mix to be a mix when it arrives.
Today a convoy of miners screened by scouts is a *scout wave*,
so [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md)'s traffic cannot
express itself at the wall however carefully it is authored.

⚠ **The fix is one already-designed rule**: the equal-distance
sidestep, so a besieger spreads ALONG a face instead of queueing
behind the hex its route landed on
([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § The siege front is
three hexes wide).  ⚠ It is **not** another class property —
which is the rule holding: the roles stay four rows of numbers,
and what needs building is the steering they are read through.

#### ⚠ Speed must NOT be tied to the tick

Owner, 2026-08-13, and it is a direct instruction rather than a
consequence: **there will be a variety of speeds, so do not
link speed too closely to a tick.**  Three sources of variety,
and the second is the one that settles it:

- **per role** — the scout is quite a bit faster than the rest,
  and the **boss quite a bit slower**;
- **per CONDITION** — *a damaged robot moves slower*, so speed
  is a running quantity that changes during a life, not a
  constant read off a class table;
- **per tier** — insects will be quite different again.

⚠ **Speed is a PACING tool, not just a stat.**  The boss is slow
*"for effect on the player, but also to allow them to strategize
against them"* — which is the same principle
[§ 6](#6-spawn-system--waves) § Pre-walk visibility already
uses, where enemies stand at their spawn markers for five
seconds so the scramble decision has room to happen.  A slow
boss is the same idea inside the fight: time to read the threat
and answer it.  The scout is that principle inverted — it is
fast *and* harmless, so it costs the player attention without
costing them time.

⚠ **This breaks a derivation the engine currently rests on**, on
purpose.  A tick is defined today as the time an enemy takes to
cross one hex, so the timestep is `1 / 1.5 s` and the mover
advances exactly one hex per tick with no arithmetic at all.
That is only available while every enemy moves at one speed.

What replaces it: **the tick becomes a simulation timestep
chosen on its own merits**, and each enemy banks movement
progress — `speed × timestep`, stepping a hex whenever a whole
one is due.  The codebase already has that pattern, built and
tested: a tower banks its fire interval exactly this way
([`plans/12`](../plans/12-combat-resolution/README.md) B5a),
float-rounding trap included.

⚠ **BUILT** — [`plans/23`](../plans/23-the-small-robots/README.md)
K2a built the banking with every number held (the corpus was
the gate: 569 measurements unchanged, `@M015`) and K2b then
moved two of them.  `TICK_SECONDS` survives as the expression
that HOLDS the timestep at one regular's hex rather than the
one that forces it (`@X058`).

⚠⚠ **And the float-rounding trap was not a footnote.**  1.5
hex/s over a 1/1.5 s tick is exactly 1.0 to the bit, so the
guard against it could not fire at all — measured, by deleting
it and watching 1128 tests stay green (`@M014`).  Two of the
five speeds that would have read as *"quite a bit faster"* have
that same property, so the scout's 2.5 was chosen partly
because it does NOT (`@X063`); zero the epsilon today and the
suite goes red (`@M017`).  **The number a design picks decides
whether its own guards are testable**, which is not a thing a
design document usually has to say.

⚠ **And it has a COST consequence nobody would look for.**  The
tick's length is what the simulation's per-tick budget is
measured against, and today that budget is generous *because* a
tick is two thirds of a second.  Decouple it and the timestep
becomes a free variable — and a shorter one, chosen for smooth
varied speeds, shrinks the budget in direct proportion.  Plan 11
deliberately did **not** build an incremental route rebuild
because a from-scratch rebuild fits comfortably at 667 ms per
tick; at 100 ms it does not.  So *"the tick got shorter"* is a
third trigger for that work, beside the two already recorded.

Two consequences worth having:

- **Wave composition becomes something the player can READ.**
  Spotting miners in an approaching wave means the perimeter
  will not hold, which is exactly the kind of early signal the
  scramble decision needs ([§ 6](#6-spawn-system--waves) §
  Pre-walk visibility) — and it is legible without a HUD,
  because the robots look like what they do.
- **It sharpens the bracing rule.**  A wall's HP already depends
  on how it is braced ([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md)
  § A wall's HP is structural); a strong attacker arriving at a
  weak hex is the case that decides a perimeter, and role plus
  bracing is what makes that a real spread of outcomes rather
  than one number against another.

Different roles should also carry **different salvage** — a
harvester and a miner are not made of the same parts — which is
the contents axis § Future tower types already needs for wreck
decay.

⚠ **The builder has a second job**, and it is the one that makes
role composition matter beyond wall-chewing: it can **repair a
damaged boss** (§ Combat dynamics § Builders repair the BOSS).
Still no new AI — being able to repair is an option on the role,
exactly as a miner's bite against a wall is a number on it.

**Within tier 1, early-vs-late escalation is lore-driven.**
The first waves a player meets are **economic units** —
workers, haulers, scouts, repair platforms — built for
non-combat roles in the original colonisation programme, only
hostile because their command links broke.  **Combat-purposed
bots** (defense / security units) exist but are *largely
abandoned by their AI* after the underground faction wars;
under sustained pressure the AI reactivates them and they
arrive in later waves.  Full fiction in
[`SETTING.md`](SETTING.md) § Combat bots are dormant.  The
*mechanical* split between economic-bot and combat-bot waves
is **not yet shipped** — all tier-1 enemies render as the
same placeholder for validation — but the wave-list format
(§ 6) is expected to extend to typed mixes once distinct
stats + visuals + an audible activation cue arrive.

## 11. Movement + input philosophy

### Position triggers, not key presses

The player should *feel* they activated something through
**motion**, not by typing.  Most actions are **bumping into
things** (drive into the core = force-launch; drive over loot
= auto-pickup; drive to a tower = pickup / deposit / repair /
boost) or **leaving trails** (driving with wall-paint mode on
marks the trail as walls for helpers to construct).

Key presses are reserved for *mode toggles* and the rare
intent that has no spatial form.  Design new mechanics in
spatial terms first.

### The handful of keys

| Key | Action |
|---|---|
| **WASD** | Move |
| **E** | Pickup / drop (single key, context-resolved: empty hands = pickup, carrying = deposit) |
| **Q** | Wall-paint mode toggle (acknowledged exception to the spatial principle; no clean spatial alternative surfaced — the wall-trail mode flip is keyed) |
| **Shift** | Boost (held; context: vehicle while moving, tower while adjacent) |
| **Tab** | Editor mode toggle (ground ↔ marker) |
| **1-0, -** | Palette select (editor) |
| **Esc** | Cancel / menu |
| **Left click / drag** | Placement and UI — landing-spot pick, editor click, map markers.  ⚠ **Unchanged**: this is the half that *"the mouse is free for placement"* was protecting, and it keeps doing it |
| **Right drag** | Orbit the camera (azimuth + elevation) |
| **Wheel** | Boom length |

⚠ **§ 12's *"NOT camera orbit"* is retired** (2026-08-17, plan 21 R1).
The camera orbits, so the mouse has to reach it — but the button that
was doing placement work still does only placement.  Orbit went to the
**other** button rather than sharing the left one, because a click that
sometimes places and sometimes swings the view is the ambiguity § 11's
spatial principle exists to avoid.

⚠ **No binding is built yet.**  Plan 21 R1 shipped the camera and its
verbs (`camera_orbit`, `camera_zoom`); wiring them to the mouse is a row
in `src/bindings.loft`'s ONE key table and belongs with the phase that
gives the player something to orbit *around* — nothing of the game is
drawn yet.

Full mapping in
[`../examples/numbers.json`](../examples/numbers.json) §
`input`.

### Wall paint — trail outline + erasable

While paint mode is on, each hex the vehicle traverses gets a
flat red wall outline.  Re-driving over an outlined hex erases
the outline (only valid while no helper has started building —
construction commits the order).  The vehicle's body tints red
while paint mode is on (diegetic indicator).

## 12. Camera + HUD

> ⚠ **REWRITTEN 2026-08-17, by plan 21 R1, in the commit that landed the
> camera** — as the previous banner said it would be.  The camera is
> moros's `RenderCamera`, ported: an orbit camera in spherical
> coordinates, with **elevation and distance under the player's
> control**.  *"Locked in pose — no mouse orbit"* is gone, and so is
> § 11's *"Mouse … **NOT** camera orbit"*.
>
> [`RENDERER.md`](RENDERER.md) § R1 carries the decisions;
> [`DECISIONS.md`](DECISIONS.md) `@X065`-`@X067` carry what R1 settled
> while building it.

### Camera — over-the-shoulder, orbitable, auto-reframe

The camera orbits the vehicle: a **target** it looks at, plus an
azimuth, an elevation and a boom length.  Its resting pose is
**~3 m above and ~5 m behind** the vehicle — which is the pair
`elevation 30.96°, distance 5.83 m`, and
`src/render_camera.loft` derives the constants from these two
numbers rather than picking them.  ⚠ `tests/21_r1_the_camera.loft`
§ The default pose is the design's pose asserts them back **in
metres**, so this paragraph is what the constants answer to.

**Azimuth is the game's, elevation and distance are the player's.**
The azimuth is derived from the vehicle's **velocity** — a hover unit
slides sideways and keeps its nose forward, so there is no stored
facing to disagree with where it is going (`@X067`).  ⚠ A **parked**
vehicle has no velocity and therefore no bearing, so the camera keeps
the azimuth it had; swinging to a default on every key release is the
failure this avoids.

Auto-reframes on two triggers:

- **Sudden vehicle movement** (sharp turn, boost start) —
  smooth swing to maintain framing.
- **Terrain blocks line-of-sight to the vehicle** (wall,
  `wall_high`, `steep_rock`) — smooth swing to a position
  that can see the vehicle.  ⚠ This is moros's occlusion sweep and
  the query it asks is `tower.loft::tower_sees`, not a second
  line-walker that agrees with it today.

⚠ **The editor's top-down view is a PRESET of this camera, not a second
camera** — elevation 89°, azimuth 270°.  Measured against the software
rasteriser at 0.08° of bearing and 0.56% of scale (`@M022`), which is
what makes eventually collapsing the two rasterisers a migration rather
than a rewrite ([`RENDERER.md`](RENDERER.md) § R2).

Swing easing ~0.5 s — reads as "the camera adjusted," not
teleported.

Visible radius bounded by `atmosphere_haze_radius` (see
[§ World](#3-world)).

### HUD — diegetic + minimal numeric

Most game state is shown in the world:

- Tower state (red / black / pink top), NPC order status
  (core's top colour), launch countdown (core's bottom
  pulse), wave incoming (spawn markers pulse), what's carried
  (object floats above carrier), construction progress (wall /
  tower grows out of the ground), wall outline (red outline
  on hex), stranded helper (damaged silver-grey cuboid at
  wreck), paint mode on (vehicle body tinted red).

Numeric / state HUD reduced to the bare minimum:

- **Wallet** (points) — one corner number.  The only number
  the player must see to make build decisions.
- **Active palette entry** (editor only) — swatch + name
  highlight.
- **Paint-mode on/off** — *vehicle tint is the primary signal;*
  small icon optional.

That's the entire HUD.  No wave-number, no inter-wave
countdown, no minimap, no boost cooldown bar.

⚠⚠ **BUILT 2026-08-18** ([`plans/19`](../plans/19-the-interactive-loop/README.md)
§ P7, `@X097`): the wallet is a seven-segment number in the play frame's
top-left corner, and **nothing else was built, because this section names
what it refuses**.  ⚠ The digits are RECTANGLES rather than text —
`graphics::draw_text` rasterises through a `#native` call unavailable under
`loft test` and needs a font file dryopea does not have, so a text HUD would be
one no test and no `snap` could see.  ⚠ The *active palette entry* half is the
editor's `picker.loft`, built since plan 01; **paint mode is not built at all**,
and its *vehicle tint is the primary signal* rule is waiting on building.

⚠⚠ **ONE ADDITION BEYOND THIS SECTION** (project owner, 2026-08-18, `@X098`):
the number **ramps amber to red as the wallet drains**.  It adds no HUD
element — it is the same one number — and it makes the run's ONLY end state
(`wallet_broke`, § 14) legible at a glance.  ⚠ The obvious threshold — *warm
when you can no longer buy the cheapest thing*, i.e. below the 100-point order
cost above — is deliberately NOT built: that number lives only in
`examples/numbers.json` because building does not exist yet, and a simulation
constant nobody spends is one that drifts.  The trigger is building.

## 13. Economy + progression

### Currency — points

**Points** are the wallet currency, earned from enemy salvage
(loot drops on death; the player picks up by driving over, or
helpers carry to the core).  Loot value: 10/regular,
50/boss.

Points spend on **two things**:

- **Tower orders** (100 pts each) — at the core's tower-core
  retrieval face.
- **Helper orders** (100 pts each) — at the core's NPC-order
  face; hard cap of 6.

Walls (both heights) and **bridges between walls (future,
phase 2)** are **free in points** — helper-seconds is the
bottleneck.  Same economics for both wall heights.

### Starting budget + 1:1 carryover

Every base begins with a **points budget** (default 200) the
player can spend before / during wave 1.  On subsequent bases
(post-scramble), the budget = baseline + **the player's
unspent wallet at scramble time** (1:1 carry).  Unspent points
that the player did NOT manage to bring onboard at force-
launch are forfeit (mirrors the abandoned cargo rule).

### Tower-top loadout (future — Q4 closure)

Tower-tops carried to the core at scramble accumulate in a
**persistent between-mission inventory** at the cooperative's
rented spot on the station (see [§ Meta-game hub](#16-meta-game-hub)).
Each top is **specialised** by gun / ammunition type (anti-
insect pulse, anti-elemental dampener, anti-comms-priority
disruptor, etc.; future catalogue).

Before each next mission the player **selects a limited
number** of tops from inventory to load into the rocket.  That
selection is the mission's available top pool.  Tops not
selected stay at home for future missions.  Picking the
loadout is the meta-game.

Validation tier: mechanic carries (tops collected, parked at
core, survive launch), but the **effect on the next base is
placeholder** — no in-mission effect yet.

### Scouting — the primary discovery loop

The player's main path to **special materials, upgrades, and
new tower types** is **scouting**: driving out beyond the
haze radius into the unknown to find them.  This makes
scouting *the* progression activity (not building, not combat)
and motivates the helper scout skill.

**Every reward has its own pressure:**

- **Sap** (from huge trees) → **invites insect chase by
  smell**.
- **Special gems** (associated with elementals) → **awakens
  matching elementals**.
- **Future kinds** carry their own triggers (each authored
  per map).

Scouting is a **bet**: every find is high-value AND opens a
fight.  Stay near the core (no gains, low risk) vs push
outward (real rewards, real consequences).  Combined with the
bounded haze radius, every venture is a small commitment with
a known shape.

**New tower types via scouting.**  Validation ships only the
placeholder laser.  Future tower variants are *unlocked
content* — found on the map, brought back to the core, become
orderable from then on.

### Future expansion — orbital banking

Eventually the player will be able to **launch cargo pods
during play** (separate from the scramble rocket) — sending
materials / points to orbit and instantly banking them.
Distinct from the scramble: scramble takes the player + their
carry; the cargo pod takes resources, no exit.  A counter to
"stay too long and lose what you gathered."  Deferred.

## 14. Run structure

### No hard run-loss — the player decides

dryopea **does not have a fail screen.**  Every base ends with
the player launching the rocket — sometimes with full carry,
sometimes with almost nothing.  The next base always starts.
A run the player feels was *bad* is simply a run that produced
meagre carryover.  A *good* run produced a lot.  The
difference is felt across the sequence, not announced by the
game.

Consequences:

- No "Game Over" screen, no forced exit from a run.
- The run is the **continuous sequence of bases the player
  chooses to play**; it ends only when they stop playing.
- Bad performance still produces *some* carryover (an empty
  wallet, no tops; the next base falls back to the fixed
  baseline starter loadout).

This retires the @PLAN46-original "core destroyed = run ends"
framing.  A "lost run" is the player's own judgment, not a
game state.

### Base sequence

A run is a **sequence of bases**, chained by what the player
carries out.  Each base is a permitted sortie down to the
planet; between bases, the cooperative returns to the central
space station (see [§ Meta-game hub](#16-meta-game-hub)) to
pick the next sortie + loadout.

### Bounded session

A single base must be a **bounded, one-sitting mission**, and
the boundaries between bases are **clean save-and-quit
points**.  The session target is ~15-25 minutes; this is the
*permit duration* in the fiction (SETTING.md § The premise),
not a soft preference.  The scramble exit is what gives the
player the graceful opt-out — bail out of a failing base, keep
your salvage, stop; resume the run later.

### Scramble exit + cargo manifest

The scramble (force-launch) is the **only confirmed exit**
through the cordon (SETTING.md § The quarantine).  At launch,
the cargo manifest is *whatever made it onboard*:

- The player's wallet (unspent points, 1:1 carry).
- Tower-tops the player deposited at the core.
- Helpers who'd boarded by liftoff (others left behind →
  stranded).
- Loot helpers had delivered to the core (not the carried-but-
  not-delivered ones).

Force-launch leaves stragglers behind — by design.  The cost
of haste.

## 15. Landing flow

The complete landing sequence:

1. **Map selection.**  Player sees a **static planet view**
   (validation teaser of the eventual rotating-planet hub —
   see [§ Meta-game hub](#16-meta-game-hub)) with one
   clickable marker per available map.  Clicks one.
2. **Landing-spot pick.**  Player clicks ANY hex on the
   selected map (within `landing_pick_edge_buffer` from the
   map's playable-area boundary).  Picking a lake centre or
   mountain summit is *allowed* — no up-front rejection.
3. **Rocket descent — auto-steers off invalid hexes.**  The
   rocket lands at a random hex within
   `core_landing_area_radius` (default 3 hex) around the
   pick.  If the candidate hex is invalid (water,
   `steep_rock`, painted-impassable) OR fails the 7-hex
   footprint + `obstruction_clearance_buffer` (default 2 hex)
   test, the algorithm searches outward for a valid landing.
   Visually diegetic — the rocket appears to choose safe
   ground.
4. **Random rotation.**  The core's six faces (lift-off /
   tower / NPC / 3 plain) point at arbitrary hex directions.
   Acceptable because no walls exist yet at landing; player
   adapts.
5. **Close-spawn auto-disable.**  Spawn markers within
   `close_spawn_disable_radius` (default 10 hex) of the
   landed core are **silenced for the mission** — still
   visible as map atmosphere; produce no enemies.  Map
   authors guarantee enough markers survive (~4-6 spread
   across a starter map).
6. **Starter tower lands.**  A separate lander touches down
   5-10 hex from the core in a random direction, **already
   standing + firing-ready.**  This is the free defensive
   guarantee that prevents wave-1 deadlock.
7. **Helpers emerge.**  Two NPC helpers exit through the
   lift-off face within 2-3 s of landing.
8. **Player gains control.**  Wallet = starting budget +
   carried-over points from previous run.  Pre-wave window
   begins.

Wave 1 fires when either trigger satisfies (see [§ Spawn
system + waves § Wave-1 triggers](#wave-1-triggers--walls-or-provocation)).

## 16. Meta-game hub

### The central space station

Between sorties, the player's cooperative lives at a **shared
orbital space station** outside the planet's cordon.  The
cooperative **rents a spot**.  The rented spot holds:

- **Persistent inventory** — tower-tops carried out of past
  missions, points unspent, blueprints recovered through
  scouting, materials brought back.  Stranded helpers
  awaiting rescue are listed here too.
- **Pre-mission loadout selection** — the player picks from
  inventory which specialised tops to take down (the Q4
  limited-loadout pick).
- **Future shopping** — vendors / fabricators / brokers sell
  gear that doesn't exist in the cooperative's existing
  inventory.  Carry-out points become spending power.
- **Future shipping outward** — selling carry-outs, sending
  cargo to other clients, traveling to other quarantined
  sites or unrelated jobs.  The cooperative's business is
  *not* limited to this one planet.
- **Permit administration** runs through the station.

Tone: shared / working-class orbital, not the player's; other
operators visible; cordon battleships visible from observation
viewports.

### The rotating planet UI (future)

The long-term replacement for the abstract map-selection menu:
a **diegetic view of the planet** from the station's
observation deck.  Planet rotates below; day / night
terminator moves; information projected on the surface
(previous landing sites, abandoned bases fading
green-to-red over time, intel hotspots, faction territory
hints).  Player **clicks directly on the rotating planet** to
pick a sortie destination.

**Validation already ships a static teaser** — placeholder
sphere with one clickable marker per available map (plan 04
L3).  No rotation, no projected data — just planet + markers
+ click.  Sets the visual tone before the meta-game state is
implemented.

The full rotating version ships when multi-mission run state,
persistent surface, and the planet-scale meta from §
[Future expansion](#20-future-expansion) are in place.

## 17. Moddability

dryopea is open source (LGPL-3.0-or-later) and aims to let
other developers and players **mod the game immediately**
without rebuilds.  This shapes design + data choices
throughout:

- **All numerical values live in runtime config files** — see
  [`../examples/numbers.json`](../examples/numbers.json).
  Tuning damage / range / fire rate / budget / build time /
  scrambler radius / landing parameters is a config edit,
  never a code change.
- **All content lives in editable data files** —
  [`palette.json`](../examples/palette.json) (ground types),
  [`waves.json`](../examples/waves.json) (wave schedules),
  maps as JSON under `maps/` (plan 04), the future tower-type
  catalogue, etc.  Adding a new ground type, weapon variant,
  enemy stat block, or starter map is a data edit.
- **The in-game editor IS the modding tool.**  Players who
  want to create new maps do so in the same binary they play
  in.  Authoring is end-user; no separate developer
  toolchain.
- **Anti-mod choices are out of bounds** — no obfuscation, no
  signed-only content, no first-party content gates.  Save
  formats are stable and documented; data formats are
  text-first (JSON / loft literals) over binary blobs.

Net: a developer downloading the repo can change "tower fire
rate" by editing one line in `numbers.json` and re-launching.
A player can author a new starter map in the in-game editor
and share the resulting JSON.  Both are first-day-of-shipping
behaviours, not aspirational features.

## 17a. Library evolution — dryopea trail-blazes (loft proper off-limits)

dryopea is the **first real consumer** of several loft library
plans (lib-plan 19 gridmesh, lib-plan 20 terrain-heightmap, the
loft-libs-world chunk in plan-12, etc.).  As such it has
explicit licence to:

- **Modify** those libraries when their current shape doesn't
  fit.  If gridmesh's API gets in the way, change it; the
  library is for us as much as we're for it.
- **Extract** new libraries when a shape becomes reusable.
  If dryopea's marker layer, wave engine, hex-input handling,
  diegetic HUD primitives — anything — turns out to be useful
  to a second consumer (moros, audience-demo, a future game),
  promote it to `lib/<name>` in loft (or its own chunk in the
  library-extraction plan).
- **Drive** the API of shared primitives.  The validation
  scenario is the integration test for the *libraries* as
  well as the game.  When a library fails to fit, it's the
  library that adjusts.

This is the loft project's **"consumer drives the library"**
principle (see lib_plans § "toolkit not framework"
discipline).  dryopea is the trail-blazer consumer; do not be
afraid to change gridmesh or to add new `lib/*` directories
as the implementation reveals what's actually shared.

### Boundary — loft itself is off-limits

**The loft compiler, language, runtime, and stdlib (the
`default/*.loft` files + everything in `src/` of the loft
crate) are NOT in dryopea's scope.**  Loft has its own
dedicated agents — its complexity warrants focused attention
that the dryopea agent does not duplicate.  Each agent
focuses on its own properties.

The rule:

- **Library code (`lib/*`)** — fair game for dryopea to
  modify, extend, or trail-blaze.  These are extractable per
  the loft library-extraction plan; eventually they live in
  their own repos.
- **Loft itself** — the compiler, language semantics,
  built-in types, stdlib (`default/*.loft`), runtime —
  **off-limits from this repo**.  When dryopea surfaces a
  need from loft (a language feature, a stdlib gap, a
  runtime bug), file it in
  [`../QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) —
  the outbound queue — and let loft's own agents address
  it.  CLAUDE.md already enforces this; this section
  records the *design rationale* for the boundary.

## 17b. Loft idiom alignment

Two loft language features the design relies on were verified
in the loft project on 2026-05-27 (read-only check;
[`LOFT.md`](https://github.com/jjstwerff/loft/blob/main/doc/claude/LOFT.md) + `lib/gridmesh/src/gridmesh.loft`):

- **Multi-field hash keys are first-class.**
  `hash<T[field1, field2]>` is a loft language feature, not a
  workaround.  The dryopea data layers use it directly:
  - Painted ground: `pub struct PaintedHex { q, r, type }`
    + `hash<PaintedHex[q, r]>`.
  - Spawn markers: `pub struct MarkerEntry { q, r, marker }`
    + `hash<MarkerEntry[q, r]>`.
  The packed-key idiom in `lib/gridmesh` (`enc_coord` →
  `hash<CellRef[ck]>`) is one library's *choice*; not required
  by the language.
- **Polymorphic enums with named-field per-variant payloads
  are supported.**
  `enum Marker { Spawn { direction: u8 } }` follows the
  documented pattern (LOFT.md § Enum types) — same shape as
  loft stdlib's `enum Shape { Circle { radius: float }, Rectangle { width, height } }`.

Plans 01 + 03 use these forms in their Implementation +
testing sections.

## 18. Numbers

A coherent first-pass set covering every parameter is in
[`../examples/numbers.json`](../examples/numbers.json) (the
runtime config the game loads at startup) with
[`NUMBERS.md`](NUMBERS.md) as the overview + design-target
rationale.

Every value is **tunable** by the principle in [§
Moddability](#17-moddability).

## 19. Validation tier scope

The buildable goal.  In:

- **One base, one mission.**  No multi-mission run state
  beyond the wallet carryover.
- **Robots only** as enemies.  No bosses, no insects, no
  elementals.
- **One tower type** (placeholder laser).
- **One enemy type** (placeholder magenta cuboid regular).
- **One starter map** (hand-authored; plan 04 L4).
- **All systems wired:** editor + landing + waves +
  tower lifecycle + tower-top salvage + beacon ferry +
  helper roster + force-launch.

Out:

- Bosses (phase 3) and their repair / retaliation mechanics.
- Insects + sap (mechanics deferred; insects can appear
  visually as passive wildlife if useful).
- Elementals + gems (deferred).
- Tower variants (only placeholder laser).
- Robot diversity (all enemies identical).
- Helper skills (interchangeable).
- Tower-top carryover effect (mechanic carries; effect
  deferred per Q4).
- Stranded-helper rescue quests (data state only).
- Sap harvesting, gem collection.
- Multi-mission run state beyond wallet.
- Orbital banking, planet meta, multiplayer.
- Abandoned-base persistence.
- Rotating planet UI (static teaser only).
- Sound, animations beyond construction-rise.

These are tracked future-design items, not bugs to fix during
validation.  Full integration plan in
[`plans/05-validation-scenario/README.md`](../plans/05-validation-scenario/README.md).

## 20. Future expansion

The base-hopping run is the foundation.  The long-horizon
vision (recorded so the core design doesn't foreclose it):

- **Robot economy as wave source.**  Waves stop being authored
  set-pieces and become **output of** the robot economy
  state — supply lines deliver, factories fabricate, mines
  fuel.  Players push outward to **disrupt that economy**;
  fewer / weaker / later enemies are *consequences* of
  disruption.  This is also the wave-list retirement plan —
  waves.json is the placeholder.
- **Persistent planet map.**  Bases sit on a map of the
  planet.  Early bases are rural (easier); missions get
  much harder as players push toward the industrial
  heartland.  The escalating difficulty has an in-world
  cause.  The rotating planet UI (§ Meta-game hub) is the
  surface form.
- **Persistent abandoned bases.**  Scramble-and-leave bases
  *persist* on the planet with whatever wasn't evacuated.
  Mobs encroach over time.  Revisiting (same player or
  another) inherits the leftover resources AND the gathered
  threat.  Same risk/reward tension as the scramble, scaled
  to the whole world.
- **Multiplayer.**  Multiple players operate on the same
  planet — coordinated economy disruption + abandoned-base
  rescue + shared-territory missions.  Reuses loft's shipped
  netcode (`lib/server` + `lib/web`).
- **Three concentric truths about the cordon** (SETTING.md):
  public AI-contagion story, military naval-blockade
  reality, hidden faction-escape-rocket fact.  A player who
  discovers the truth holds future-content leverage.

Architectural notes (so the core build leaves room): keep
base state self-contained (already true: terrain + structure
+ run state); the per-base game is the **unit** the planet
campaign composes; abandoned-base derivation is cheap (state
+ timestamp + on-demand encroachment compute), no live
simulation needed.

## 21. Open questions

The design has settled almost everything.  A small residue:

1. **Liftoff visual transition.**  After T = 0, does the
   camera follow the rocket up + fade to the inter-mission
   screen, or a clean cut?  Settle in build.
2. **Multi-level pathing representation** (from @PLAN46
   open Q #1).  How the ground + wall-top + bridge-deck
   graph is stored and queried — per-hex walkable-surface
   list, the `cy`-layer model directly, or something else.
   Resolve when D2 / plan-level pathing lands.
3. **Lib vs game boundary** for the override layer +
   multi-level pathing (from @PLAN46 open Q #5).  Stays in
   dryopea until a second consumer appears.

Everything else routes back to a settled rule or a defaulted
value in `numbers.json`.

## See also

- [`SETTING.md`](SETTING.md) — fiction.
- [`GROUND_TYPES.md`](GROUND_TYPES.md) — palette.
- [`PROXY_ART.md`](PROXY_ART.md) — proxy geometry.
- [`NUMBERS.md`](NUMBERS.md) +
  [`../examples/numbers.json`](../examples/numbers.json) —
  runtime parameters.
- [`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) — design seeds
  from the 2023 prototype.
- Plans 01-05 in `plans/`.

---

## Appendix — Original @PLAN46 (2026-05-21)

The original design draft, preserved for traceability.
Decisions that have evolved since are recorded in the body of
this file above; this appendix is the *origin* document.

> **Note.** The "moros editor" framing in the editor/game-split
> table is **superseded** — dryopea owns its own in-game
> editor.  The "core destroyed = run ends" framing in
> § Scramble is **retired** — the core is invulnerable and
> there is no hard run-loss.  Other content remains broadly
> accurate; the body above is the canonical reference.

### Goal

A non-standard sci-fi tower-defence.  At the **start of a
match the player places their core building** — the thing
the enemies attack and the player must defend (lose it =
lose).  The player rides a **semi-floating vehicle** (over-
the-shoulder 3rd-person camera; it hovers above terrain to
avoid clipping cliff edges), and rather than placing
structures directly, **issues build ORDERS** — towers, walls,
bridges — that **NPC workers** then construct over time.
The player reacts in real time: repairing and buffing towers
as enemy waves and a boss approach, and **travelling the
landscape** to find hidden treasures that speed up upgrades.
Walls are **≥1 hex wide and walkable**, so the vehicle can
drive along them to reach the core under attack; **bosses
can break walls**, severing those routes and re-opening the
enemy path.

What sets dryopea apart from every other tower-defence is
the **scramble phase**: a base is never a simple
win-or-lose.  When it's about to be overrun, the player
fires a rocket out of the core building, **evacuating key
components** to start the *next* base with an advantage.
The game is a **run** of bases, strung together by what you
manage to carry out.

### The scramble phase — the signature mechanic

dryopea's identity.  A base is **not** win-or-lose-forever;
it's one round of a longer **run**.  When a base is about to
be overrun, the player can **scramble** — fire a rocket out
of the core building and evacuate to the next base.

- The core building is also the escape rocket.  *(Retired:
  the core is invulnerable; the run does not end on its
  destruction.)*
- Salvage is a live tradeoff.  Evacuating a key component
  takes it with you but **disables the tower it came from**.
- Carry-over → the next base starts ahead.  A run is a
  **sequence of bases**, each a TD round, chained by what
  you carry out — a roguelike structure rather than a single
  defended base.

### Design principle — bounded sessions (the rogue-lite opt-out)

A single base must be a **bounded, one-sitting mission**, and
the boundaries between bases must be **clean save-and-quit
points**.  This is a first-class design constraint — and a
deliberate strength — see [§ Run structure](#14-run-structure)
for the canonical version.

### The editor / game split (architectural spine)

The original framing was: editor authors only TERRAIN; the
running game places everything else.  The editor was meant
to be the moros editor.

**Superseded by 2026-05-26:** dryopea owns its own in-game
editor (plan 01), and structures (walls, towers, the core)
are no longer placed solely by build orders — walls are
painted by the player via a trail mechanic, towers via the
beacon ferry, the core by the rocket landing.

### Systems (game-specific scope)

The original numbered system list is reorganised in the body
above (sections 4-14).  See § The core, § Spawn system + waves,
§ Combat dynamics, § Player vehicle, § Helpers.

### Phases (vertical-slice first)

D0-D5 from the original phase plan are subsumed into the
validation tier (D0+D1+D2+D3 minimal) and § Future expansion
(D4 economy, D5 scramble).  Validation success criteria are
in [`plans/05-validation-scenario/README.md`](../plans/05-validation-scenario/README.md).

### Dependencies + shared primitives

- **lib-plan 20 terrain height-map** — currently a loft
  tracker plan; will migrate to its own repo when loft drops
  outside-project references.
- **lib-plan 19 gridmesh Phase C** — same.
- *moros editor — superseded by dryopea's in-game editor.*
- **Likely needs:** A*/flow-field pathfinding over the
  multi-level hex graph; an entity/update loop.

### Open questions (from the original draft)

The original list of five open questions is largely closed
or migrated to [§ Open questions](#21-open-questions) above.
The two surviving items (multi-level pathing representation,
lib vs game boundary) appear there.

### Future expansion — planet-scale enemy economy + multiplayer

Documented above in [§ Future expansion](#20-future-expansion).
