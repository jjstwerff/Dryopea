<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# PROGRESSION — the player gets better, the vehicle does not

⚠ **Split out of [`EXPLORATION.md`](EXPLORATION.md) § X0** (2026-08-15), because
it is not about exploring: it is about what a player *becomes*.  Exploration is
where the skill is learned; this file is what the skill IS and where it is spent.

Codes: `@X016`–`@X019` in [`DECISIONS.md`](DECISIONS.md).

*(project owner, 2026-08-15: "the player skill progression should be that it is
fun to glide around the landscape finding things, possibly looking at
robot/insect activities around and when the controls are learned using that
inside the own base to create an efficient layout to get to towers quickly when
needed")*

⚠⚠ **This is the WHY the rest of this document was missing, and it changes what
exploration is FOR.**  Scouting is not primarily a way to get things.  It is the
low-stakes place where the player becomes good at flying — and the base is where
that competence cashes out.

**The progression lives in the player's hands, not in the vehicle.**  No XP, no
upgrade tree, no faster chassis.  What improves over a run is the person at the
controls.

## G1 — the progression is SKILL, not stats  `@X016` `@X017`

### ⚠ The genre test, passed in its purest form

`DESIGN.md` § And the DEEP layers are what keep it a tower defence sets the bar
for any progression:

> *does this resolve into a statement about **position, terrain or timing**?  If
> it resolves into the player's **STATS or ABILITIES** it is off-genre, however
> good the story is.*

A skill progression that lives in the player's hands cannot fail that test —
there are no stats to resolve into.  It resolves into where you put your walls
and how fast you reach a tower, which is position and timing exactly.

⚠ **And it constrains future design in one specific way, worth writing down
before someone proposes it kindly: the player's vehicle must not get faster.**
The moment speed is a purchase, the skill stops being the thing that separates a
good run from a bad one.  (`DESIGN.md` § 9's *"Scouting — faster movement"* is a
**helper** skill and is unaffected; helpers are not the player's hands.)

## G2 — the landscape is a SCHOOL, and the fiction already made it safe  `@X018`

⚠ The reason gliding around watching robots and insects is *pleasant* rather
than tense is already designed, in two places:

- [`SETTING.md`](SETTING.md) § Nobody is attacking anybody — yet: **both
  non-human tiers open as MAINTENANCE.**  Robots think they are repairing;
  insects guard a wound.
- Robots on the map are on **errands** — they approach because their route
  crossed the jammer and they think a peer is broken, never to attack.

**So the world is safe to watch until the player interferes.**  That is what
makes it a school rather than a gauntlet, and nothing has to be built to make it
so — it is the setting's existing posture.

⚠ And watching is not idle: `ROBOT_ECONOMY.md` § How the player ever learns any
of this makes wave composition a **readout** — *"miners at the wall means a mine
upwind"*.  The player who watched traffic knows what is coming; the one who did
not, does not.  Diegetic, no UI
([`EXPLORATION.md`](EXPLORATION.md) § X8, layer 3).

## G3 — the exam is the BASE, and the numbers already say what it examines  `@X019` `@M004` `@M007`

This is the part that makes the whole idea land, because dryopea has **already
measured** that the best base is one only a good pilot can live in:

| measured | where |
|---|---|
| a **sealed** wall nearly **doubles** the fall clock | [`plans/12`](../plans/12-combat-resolution/README.md) § B7 |
| a wall with a **GATE buys nothing at all** | plan 12 § B7 |
| **boost is the only way out of a sealed base** — 3.0 m climb for three ticks, where an idle vehicle climbs 0.4 m | [`plans/13`](../plans/13-the-vehicle/README.md) § V4 |
| **upkeep is a POSITIONING problem, not a resource** — two *shuttling* helpers clear all 205 robots; the same two *parked* reach 5/7 and the base falls | [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 |
| a black tower needs **20 s of standing within one hex** to come back | plan 17 § T1 |

⚠⚠ **Read together: the defensive layout the numbers already favour is one you
cannot get out of unless you can fly.**  Sealing your perimeter is a bet on your
own boost management — and the alternative, cutting a gate, is measured at
*worth nothing*.  Nobody designed that as a skill gate; it fell out of two
independent measurements and it is exactly the mechanic the project owner is
describing.

### The layout is a racetrack the player designs for themselves

So "an efficient layout to get to towers quickly" is not flavour — it is the
optimisation the game is actually about, and it has a genuine tension in it:

- a **compact** base is fast to service and concentrates the approach fan onto
  fewer wall hexes — which plan 12 B3 says is where a wall *breaks*;
- a **spread** base covers more ground and braces more wall, and costs travel
  time between towers on a 20-second repair clock.

⚠ **That is the tower-defence version of a racing line**, and the player's own
flying is what moves the optimum.  A better pilot can afford a more spread base;
a worse one must build tight and accept the funnel.

⚠ **What it needs from a map author** is height — boostable ledges, a 3.0 m step
that a good pilot crosses and a bad one goes round.  `DESIGN.md` § Trees as
terrain already supplies the extreme case (a 10-hex tree stem is a *plateau*, so
the perimeter IS the terrain).

⚠ **What it needs from this project** is measurement, and there is none yet: no
scenario in the corpus varies the *layout* while holding the defences equal.
That is the second scenario to write after [`EXPLORATION.md`](EXPLORATION.md)
§ X7's, and it is the one that would say whether the racing line exists.

---


---

## See also

- [`EXPLORATION.md`](EXPLORATION.md) — where the skill is learned, and what a
  sortie is for.
- [`DESIGN.md`](DESIGN.md) § And the DEEP layers are what keep it a tower
  defence — the genre test `@X016` passes.
- [`DECISIONS.md`](DECISIONS.md) — the index.
