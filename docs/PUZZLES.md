<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# PUZZLES — they come from the WORLD MODEL, never from obstacles

⚠⚠ **The owner's framing, 2026-08-30**: *"dryopea is on the surface no puzzle
game but I want it to have puzzle elements, those arrive not from obstacles but
from the world model."*

That distinction is the whole document.  An **obstacle** is a thing placed to
be solved — a door with a key, a gap with a bridge.  A **world model** puzzle
is one nobody placed: it exists because matter has to be somewhere, somebody
has to move it, and the arrangement you chose an hour ago decides whether that
is easy now.

⚠ **This page owns the thesis.**  The mechanisms it argues for live in their
own documents — [`MATERIALS.md`](MATERIALS.md) for what gets moved,
[`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) for what blocks it,
[`PROGRESSION.md`](PROGRESSION.md) § 9 for who moves it — and this one is where
the reason they hang together is written down.

---

## ⚠⚠ THE THESIS — no instant movement for matter  `@X351`

> **Things need to be transported before they can be built or used, and to get
> transported you need ways to do that, or to aid the helpers who do it for
> you.  Most of the time that is easy.  It can become complex quickly.**

```
  (T-No-Instant-Path)
                     matter has no instant path.  A thing is where it is
                     until somebody moves it, and moving it costs the
                     mover's time — so a mechanic that makes matter
                     appear where it is needed has deleted a trip, and
                     the trip is the game.  ⚠ A SOURCE with a position
                     is fine; a CHANNEL between two positions is not.
```

⚠⚠ **It is the citable form of a refusal `MATERIALS.md` § Power already
makes** — *"a grid replaces a trip with a wire"* — generalised past power so
that the next conveyor, pipeline, teleport or autopilot is judged without
reopening the argument.  ⚠ The test it gives is one line: **does this make
matter arrive without anybody going?**

### ⚠⚠ It is not an aspiration — it is what the shipped game already measures

| the trip | what it is worth |
|---|---|
| a crew member repairing a black tower is one who was NOT clearing bodies for points | `@M095` — **upkeep is paid in the currency of INCOME** |
| two helpers shuttling clear all 205 robots; the same two parked reach 5 of 7 and the base falls | [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 |
| a tower does not exist until a beacon has been carried from the core and a crew member has stood on the site for 30 s | `plans/27` C4 |
| a heap you left on a robot road is picked up and carried off | `@M084` — **230.0 against 200.0** |
| a find pays nothing until it is driven home | `@M095` — **+30 ticks, flat across seven fetch ticks** |

⚠ So the thesis needs no new pillar.  What it needs is **depth**: today the
transport layer is a tax, and the owner's ask is that it sometimes be a
*problem*.

---

## ⚠⚠ The pattern that makes a world-model puzzle teachable  `@X352`

`DESIGN.md` § There is NO TUTORIAL is categorical, and `DESIGN.md` § HUD allows
exactly one number on screen.  So a puzzle whose failure needs explaining
cannot ship.  The ones that CAN share a shape, and it is worth naming because
three separate mechanics have now arrived at it independently:

> ⚠⚠ **THE COST OF THE WORKAROUND IS THE MEASUREMENT OF THE MISTAKE.**

- **bodies ramping a kill zone shut** — how often you must drive in and clear
  is the readout of how badly the kill zone is placed;
- **upkeep** — how often you must drive out to a black tower is the readout of
  how far apart you built them (`plans/17` § T3);
- **a jammed rail** (below) — how often you must drive out to unstick somebody
  is the readout of how badly the track is laid.

⚠ A mechanic with this shape teaches itself, needs no UI, and is **permeable to
knowledge** in `PROGRESSION.md` § P0c's sense: a returning player builds it
right the first time, and that advantage cannot be bought or ground for.  ⚠ A
mechanic WITHOUT it needs a tutorial, and there is none.

---

## Rails and trains — the first worked example  `@X353`

⚠⚠ **`MATERIALS.md` § What this design deliberately does NOT do refused these**
— *"No rails, no trains, no lifts — yet … automating transport removes the
trip"* — and the owner has now ruled the other way, on a reframe that survives
the refusal:

| | |
|---|---|
| what was refused | a **conveyor**: matter arrives without anybody going |
| what is ruled in | a **rail**: matter still has to be loaded, still occupies hexes, still can be blocked, and the track itself is built by somebody standing on it |

⚠ The first deletes a trip; the second **constrains** one.  `(T-No-Instant-Path)`
forbids only the first.

### The design, as ruled

- **A train cannot leave its rails.**  The efficiency is bought with
  *commitment*: you chose the topology before you knew where the pressure
  would come from.
- **A helper standing on the rails blocks it.**  ⚠⚠ This is the part with no
  precedent in the game: today the crew interfere with each other only through
  ATTENTION (`@M095`).  A rail makes them interfere through **space**.
- **Rails are very efficient but take care to build.**
- **Trains have priority; helpers yield.**  ⚠ The opposite rule — trains wait
  for people — makes deadlock impossible and rails unreliable, which is a
  different mechanic with a different feel.  It is the imposition on your own
  base that keeps a rail from being the conveyor the design refused.

### ⚠⚠ THE CURVE IS THE KEYSTONE — one geometry, three returns, one cost  `@X356`

**Owner, 2026-08-30**: *"rails are even more complex, a helper or the player
can take sharp turns, but a train/rails can't — they need the curved walls
(that are strong against push/damage) … those curved walls should also allow
the player to move faster, because sharp turns make them almost halt, because
otherwise their momentum would let them fall/float from the wall."*

⚠⚠ **A hex lattice has no gentle turn.**  The six directions are 60° apart, so
*every* single-hex turn is a sharp one.  A curve is therefore not a hex — it is
a **run of hexes whose direction changes at most once every N**, which is a
radius, and a radius costs **space**.  ⚠ That is the geometry `hex_way` and
`hex_shape` exist for, and the shape of a shallow hex line is a staircase of
alternating directions; **do not invent a second one** (`@X322` — the library
never asks for a thing by name, it asks the catalogue for one that fits).

⚠⚠ **And the curve pays in THREE currencies while costing exactly one:**

| the return | why it matters |
|---|---|
| **a train can pass it at all** | the hard constraint — without radius there is no route |
| **it is strong against push and damage** | so the corners of a base are its tough points, which is what real fortification does and for the same reason |
| **the player moves FAST along it** | ⚠⚠ and this is the big one — see below |

⚠ The cost is **space**, and a compact base has none.

#### ⚠⚠ The third return is what gives the WALL a positive reason to exist

A curved wall the player can drive fast along is not a barrier — it is a
**ROAD**, and the road runs exactly where the servicing has to happen.
[`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 measured that
difference as the whole game: two helpers **shuttling** clear all 205 robots,
the same two **parked** reach 5 of 7 and the base falls.

⚠⚠ **So this is the missing positive term in `@M094`.**  That measurement reads
a wall at **−80 / −117 ticks** in front of a tower and the leading hypothesis is
SIGHT.  A wall that is also the fastest route around your own base has a credit
column no fixture in the corpus has ever given it — and together with § the
FUNNEL below, *the wall's sign may be entirely a question of what shape it is
built in*.

⚠ **And a sharp turn is a real cost with numbers that already exist**: leaving
the wall sideways drops the vehicle off it, a drop is free
(`can_travel`) and the chassis HOVERS (`@D006`), so nothing is destroyed — you
are simply **outside your own wall**.  ⚠⚠ Getting back is where the palette
already bites: `VEHICLE_BOOST_CLIMB_METRES` is **3.0 m, exactly a `wall`** —
and a `wall_high` is **5.0**.  ***Fall off the high wall and you are walking to
the gate.***

#### ⚠ What is REUSE and what is genuinely new

| piece | status |
|---|---|
| the track's shape, radius, staircase | `hex_way` / `hex_shape` — ⚠ look before writing |
| a curved section being tougher | `damage.loft` already scales HP by CONTEXT — bracing (`plans/12` B3) and footing (`@X284`).  A curve is a third column, not a third mechanism |
| falling off, and hovering back | `can_travel` / `can_hover` / the boost — all shipped |
| **momentum, and a cost for turning** | ⚠⚠ **NEW.**  `VEHICLE_SPEED_HEX_PER_SECOND` is a flat 3.0 and there is **no heading-change cost anywhere** in `vehicle.loft`.  This is the first thing in this design that is not a reframe of an existing door |

⚠⚠ **And a turn cost lands on the CREW as well as the player**, because
`drive_along` is one implementation with two doors (`DESIGN.md` § 9 — *same
chassis as the player*).  ⚠ That is probably right rather than a problem: a
crew shuttling along a curved wall is exactly the mechanic § T3 priced.  But it
means a turn cost is not a player-feel tweak — **it re-prices every servicing
trip in the game**, and it must be measured as such.

⚠ **Two numbers nobody has**: the minimum radius, and whether the sharp-turn
penalty is a **speed cap** (you slow, you keep the wall) or a **fall risk** (you
keep speed, you leave the wall).  ⚠ The first is a tunable; the second is a
different game, because a fall is a positional punishment and this design's
whole currency is position.

#### ⚠⚠ The vehicle model: NO instant speed-up and NO instant slowing  `@X357`

**Owner, 2026-08-30**: *"I do not want instant speed-up or slowing down by the
player vehicle."*

⚠ So the momentum above is not a turn penalty bolted onto a flat speed — it is
a **speed that takes time to reach and time to lose**, and the sharp-turn
behaviour falls out of it rather than being a rule of its own.  A vehicle that
has to slow before a corner is one that must *decide to* several hexes early,
which is the same thing the rail asks of the track and the siding asks of the
base.

⚠⚠ **THE CONTROL MODEL, as ruled** (owner, 2026-08-30) — a THROTTLE, not a
speed switch:

| input | effect |
|---|---|
| push forward | the vehicle moves; **holding it keeps increasing the speed** |
| release | it **slowly** loses speed |
| push backward | it loses speed **quicker** — braking is the only fast way down |
| turn | changes direction and **does NOT slow the vehicle** |

⚠ Three rates where there is one constant today: **accelerate**, **coast**,
**brake**.  ⚠⚠ And the fourth line is the design: *"turning changes the
direction but doesn't slow the vehicle down, **so a sharp turn needs a slow
speed**"* — there is **no turn penalty at all**.  Nothing stops you turning;
what stops you is where you end up.

⚠⚠ **THAT RESOLVES THE OPEN QUESTION ABOVE, AND NEITHER WAY IT WAS POSED.**
It is not a speed cap and it is not a fall *risk* bolted on: **momentum carries
the vehicle, and off a one-hex-wide wall the hex it carries you to is not on
the wall.**  ⚠ The punishment is POSITIONAL — which is this design's whole
currency — and it is emergent rather than authored: no rule says *you fell*,
only that you went where you were going.

⚠⚠ **And it unifies the vehicle with the train.**  A rail's radius is
permanent and geometric; a fast vehicle's is **dynamic and speed-dependent** —
the quicker you are, the more your next hex follows your previous heading
rather than your input.  ***So a fast vehicle on a wall behaves exactly like a
train, and wants the same curves.***  ⚠ That is why a curved wall is worth
building once and paying for three times, and it is the cleanest argument in
this document for the whole mechanic.

⚠⚠ **This replaces a constant with a state.**  `VEHICLE_SPEED_HEX_PER_SECOND`
is a flat 3.0 today and `enemy_bank` / `helper_bank` / `drive_along` all take a
speed as an ARGUMENT rather than reading a constant — which
`spawn.loft::enemy_bank`'s header says was deliberate, *"`DESIGN.md` § Speed
must NOT be tied to the tick wants speed to vary within a life"*.  ⚠ So the
seam already exists; what is missing is the thing that carries the current
speed between ticks, and `fixstep`'s `Bank` is the shape for it.

#### ⚠⚠ The FEEL target: quick to a useful speed, and less precise once there

**Owner, 2026-08-30**: *"It should feel good to drive around and get to a
reasonable quick speed to get somewhere quick but also less precise."*

⚠ That is a target for the three rates rather than a fourth rule: **time to
useful speed must be short** — the throttle is a travel tool, not a chore — and
**precision is what speed costs**.

⚠⚠ **AND THE TRADE MAPS ONTO THE SHIPPED GAME WITH NOTHING INVENTED.**
Everything dryopea asks the player to do precisely happens at **reach one**:

| the job | the reach |
|---|---|
| repair or rearm a tower | 1 hex, and presence-locked (`DESIGN.md` § 7) |
| give a crew member an order | `CARGO_REACH_HEXES` — `wave_direct_nearest` |
| pick up, deliver, or buy a beacon | `CARGO_REACH_HEXES`, all three |
| clear rubble | by POSITION, one hex |
| the scramble | the core's own hex, held for six seconds |

⚠ So the rhythm is already there: **travel is long and jobs are reach-one**.
Speed pays on the way and costs at the destination, which is exactly the
shuttling loop [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 priced
at the difference between clearing 205 robots and falling at 5 of 7.

⚠⚠ **The risk GRADIENT is the part to get right.**  The same imprecision costs
almost nothing in the open — overshoot a tower, come back — and costs a walk to
the gate on a one-hex wall.  ***That gradient is the mechanic***, but it only
reads as the player's fault if the cheap case is genuinely cheap.  ⚠ An
overshoot that is merely annoying everywhere will be read as bad controls; one
that is free in the field and expensive on the wall is read as a place you
should have slowed down.

#### ⚠⚠ OPEN — the BOOST contradicts *no instant speed-up* as it stands

`VEHICLE_BOOST_HEX_PER_SECOND` is **6.0 against a base 3.0**, applied for 2 s
on a 5 s cooldown — **an instant doubling**, which is precisely what `@X357`
refuses.  ⚠ So boost cannot survive the throttle model unchanged, and the
choice has not been made:

- a **higher ceiling** — boost raises the top speed and you still have to
  accelerate into it, so it rewards a run-up and is useless from standstill;
- a **higher acceleration** — boost is the fast way *to* speed, which makes it
  an escape tool and keeps its current feel;
- or **both**, which is probably what a jump jet should be.

⚠ Its **3.0 m climb** is a separate property and is untouched either way —
`VEHICLE_BOOST_CLIMB_METRES` is exactly a `wall`, which is what lets a boost put
you back on one.

⚠⚠ **DEFERRED — owner, 2026-08-30**: *"we will concentrate on that as soon as
we have a playable version."*  ⚠ The gate is
[`plans/ROADMAP.md`](../plans/ROADMAP.md) § THE SESSION IS THE GAP NOW — the
best base plays **four of seven waves and falls at 3.6 minutes** against
`numbers.json`'s 15-25 minute target.  **Nothing here is built until that
closes**, and this section exists so the ruling is not re-derived when it
does.

### ⚠⚠ The jam, and its three outs — a LADDER, each rung a different currency

Owner, 2026-08-30.  A jam is never terminal; it is expensive, and it costs
something different at each rung:

| rung | what it costs | notes |
|---|---|---|
| **reassign the helper** — drive to them, give a different job, they walk off the rails for it | a TRIP, plus whatever they stop doing | ⚠ It is `helper_direct` (key **G**), which is **the only door in the codebase that clears `ordered`** — and `@X289` already prices widening a remit at *what narrowing cost: a trip* |
| **build a passing place** — somewhere a helper can step off to let a train through | wall/track budget, and space the base may not have | ⚠ the retrofit is the hard case, and that is the point |
| **clear the track** — demolish a rail hex to break the jam | the track | ⚠ `@M059` measured a clearer as taking the WHOLE pile, an off-switch rather than a dial |

⚠⚠ **And the last resort is the run's own**: a base you have deadlocked
yourself out of is a base you leave early, carrying what you learned.
`@X292` — the wallet at zero is POVERTY and not an ending — is the same
refusal one layer down: **dryopea has no unrecoverable state.**

### ⚠⚠ Why the passing place is the good answer

It converts a deadlock from a **bug** into **a decision the player got wrong
earlier, in a place they can point at**.  That is the difference between a
puzzle and a soft-lock, and it inverts the usual shape of this game's pressure:

⚠ Everywhere else the design charges you **at the moment of use**
(`DESIGN.md` § What kind of game this is).  Here the mistake is **free and
invisible** and the fix is **expensive and late**.  ⚠⚠ That is a new shape and
it is worth knowing it was added deliberately — it is only survivable because
the ladder above guarantees an out at every rung.

### ⚠ The retrofit problem is the best part of the idea

Right now a dryopea base's **interior has no reason to have a shape**.
`@M093` measured layout at **+37 ticks** and the whole effect was *how far from
the core the fight happens* — a PERIMETER property.  Sidings would give the
interior its first load-bearing job, for an orthogonal reason: not distance
but **width**.

⚠ Hold it to `@X305`'s test, which is the one this repo applies to any place:
**it earns its keep only if REMOVING it moves the clock.**  A rail network that
measures at zero is scenery with a build cost, and this plan has produced three
nulls in a row.

---

## ⚠⚠ Rubble on the rails — decided, and currently a NON-ISSUE  `@M097`

**Owner's ruling: rubble DOES block a train**, and the exposure is expected to
be rare — because mobs do not walk on walls, and reach wall height only when
bodies pile high enough to ramp, which is already a bigger problem than a
stopped train.

⚠⚠ **The model is right and the frequency is currently ZERO.**  Four bases
swept (`@M097`): the wall face and its whole approach read **0.0 rubble at 24
of 25, 29 of 30, 24 of 25 and 12 of 13 hexes**, with the only rubble sitting
three or more hexes OUTSIDE the wall.  So the body ramp — which `CLAUDE.md`
lists among the pressures that stop the player leaning back — **does not fire
in any scenario the corpus produces.**

⚠ So the rubble question must not constrain the rail design today.  ⚠⚠ And it
means `wall_high` is NOT the rail material *for that reason* — see the next
section for why it must not be for any reason.

---

## ⚠⚠ No height may be IMMUNE to a body ramp  `@X354`

**Owner's ruling, 2026-08-30**: *"I do not want walls that are totally immune
to the piling up of the mobs.  Just perhaps a wall that is less easy for it to
happen … it can be that you perhaps need 12 dead mobs to reach wall height but
it should still be possible."*

⚠⚠ **It is `DESIGN.md` § What kind of game this is enforcing itself**: a wall
that can never be ramped is *a permanent advantage bought with a one-time
placement decision*, which is the thing that section exists to refuse.

⚠ **Defined as `@FR-M-Ramp-Reachable` in
[`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md)**, with the rest of the `M-` family —
a rule belongs with its family so `scripts/rules.sh sites` finds it where a
reader would look.

⚠ **The rule is currently satisfied by construction rather than by intent**,
which is why it is written down: `can_step` resolves to
`can_travel(pal, pw, hl, climb, from, to)`, a pairwise comparison of two
adjacent surfaces.  Nothing asks *how high is this mover*, and nothing may
start asking.

⚠ **The cost it produces, at `CLIMB_REGULAR` 2.0 m and 0.5 m per body:**

| wall | piles needed | bodies, minimum |
|---|---|---|
| `wall` — 3.0 m | one hex at 1.0 | **2** |
| `wall_high` — 5.0 m | 1.0 + 3.0 | **8**, across two hexes |
| a hypothetical 7 m | 1.0 + 3.0 + 5.0 | **18**, across three |

⚠⚠ **The single-hex reading is a TRAP and it was published wrong once in
conversation**: a single-hex ramp onto a `wall_high` is impossible (it needs
six bodies of pile, and five is already too tall to climb onto from flat
ground), which looks exactly like immunity until you remember the rule is
pairwise.  ***Never compute a ramp on one hex and generalise it.***

---

## ⚠⚠ What the tower numbers actually are — and the FUNNEL nobody has built  `@X355`

The owner's reading was *"towers are too strong; if they kill each mob in a
single hit they should be less strong"*.  ⚠ **They do not one-shot, and they
are not strong**:

| | value | consequence |
|---|---|---|
| `ENEMY_REGULAR_HP` | 30.0 | |
| `TOWER_DAMAGE_PER_SHOT` | 10.0 | **three shots per kill** |
| `TOWER_SHOT_BUDGET` | 30 | ⚠⚠ **ten kills, then the tower is BLACK** |

⚠ So a single tower already cannot hold a front — it kills ten and goes dark
until somebody walks over and repairs it.  Against the authored 162-robot list,
two towers are twenty kills a charge.

⚠⚠ **AND THAT EXPLAINS THE MISSING RAMP BETTER THAN TOWER STRENGTH DOES**:
barely anything dies, and the twenty-odd that do are spread along a
fifteen-hex approach — one or two corpses a hex, which is exactly what the
sweeps read and nowhere near the 1.0 m *on the right hex* a `wall` ramp needs.

### ⚠⚠ The owner's second point is the one that is missing

> *"the wall structure leading the mobs specifically into their range can make
> them kill mobs reliably"*

⚠⚠ **NO SCENARIO IN THE CORPUS HAS EVER BUILT A FUNNEL.**  Every base in
`tests/scripts/` is one shape: a wall straight across the front with a tower
behind it — a **barrier**, not a lead-in.  Under this reading that base is
built wrong, and three separate findings collapse into one cause:

| finding | under the funnel reading |
|---|---|
| `@M094` — a wall in front of a tower costs **−80 / −117 ticks** | a wall across the front is a wall across the tower's SIGHT |
| `@M097` — bodies never pile | kills are spread along an approach instead of concentrated in a corridor |
| a tower's ten kills are spent at long range | nothing forces the stream through its best ground |

⚠⚠ **So the funnel is the single most valuable thing left to measure**, and it
is cheap: `@M094`'s factorial with one new cell — same tower, same list, same
wall BUDGET, walls arranged to LEAD rather than to BLOCK.  ⚠ If a funnel reads
positive where a barrier reads −80, then *the wall's sign is a question of
SHAPE and not of walls*, and `@M094`'s ungated SIGHT hypothesis is answered at
the same time.

⚠ **One number is missing before anyone tunes a tower**: the firing CADENCE.
Damage and magazine are constants; the charge is a hand-rolled `bank_gain` and
there is no shots-per-second in `numbers.json`.  *Ten kills then dark* feels
very different at one shot a second than at one every three.

---

## ⚠⚠ WHY IT IS WORTH THE COMPLEXITY — the base is the curriculum  `@X358`

**Owner, 2026-08-30**: *"it makes designing a base far more complex than just
jotting down walls that a normal tower defence game teaches the players.  But
only after playing longer the player learns the more complex but optimal shapes
naturally."*

⚠⚠ **This is the payoff argument for everything above, and it completes a
triangle the design already had two corners of:**

| corner | where it already lives |
|---|---|
| **the curriculum** — geometry that rewards being understood | ⚠ NEW: this document |
| **the feedback** — how a mistake announces itself without a UI | `@X352` — *the cost of the workaround is the measurement of the mistake* |
| **the fairness** — why learning it is a legitimate advantage | `PROGRESSION.md` § P0c — **permeable to knowledge**: *a returning veteran collects on day one what a first-timer stumbles on in week three* |

⚠ Base shape is the purest thing that permeability rule can apply to: it
**cannot be bought, cannot be ground for, and transfers instantly** — between
runs, between bases, and between players.  ⚠⚠ And it is why
`DESIGN.md` § There is NO TUTORIAL can hold: *the geometry is the lesson, the
workaround cost is the mark, and the next base is the retake.*

⚠ **The RUN is the unit of learning**, and it already exists: `DESIGN.md` § 14
makes a run *a sequence of bases chained by what you carry out*, and
[`plans/31`](../plans/31-carryover/README.md) built the chain.  A curriculum
spread over bases has a vehicle already.

### ⚠⚠ AND THE PLAYER DRAWS WALLS BY DRIVING, SO THE TOOL AND THE PRODUCT SHARE ONE PHYSICS  `@X359`

**Owner, 2026-08-30**: *"because the player draws walls by moving, they
determine the speed of their moving on the wall by their drive that they do
while drawing."*

⚠⚠ **This is the keystone of the whole document.**  [`plans/27`](../plans/27-building/README.md)
built walls as a DRIVE — press **Q** and every hex you drive over is ordered as
a wall your crew raise (`@M050`, +44 ticks) — so **the shape of a wall is the
shape of the drive that made it.**  Put that under `@X357`'s momentum and the
consequences are not designed, they are forced:

- **You cannot draw a wall you are unable to drive on**, because you drew it by
  driving on it.  ⚠ The route is proven at construction time, by construction.
- **You cannot draw a fast wall slowly.**  A crawling drive makes tight corners;
  only a fast drive sweeps, and on a hex lattice a sweep IS the staircase curve
  a rail needs.  ***So driving fast while drawing produces rail-legal geometry
  automatically, and driving slowly produces a shape no train will ever take.***
- **The throttle is the dial between the two base archetypes.**  Compact and
  angular at one end, large and sweeping at the other — which is exactly the
  trade § The risk sets up between `@M020`'s siege front and rail geometry, and
  the player expresses it *with one continuous physical control* rather than by
  understanding a rule.

⚠⚠ **AND IT RETRACTS A CONCERN RAISED EARLIER IN THIS DESIGN.**  § Why the
passing place is the good answer says the rail inverts this game's usual shape
— *the mistake is free and invisible, the fix is expensive and late*.  **The
mistake is not invisible.**  A wall you had to crawl around to draw is a wall
you will crawl around for the rest of the base, and **you felt it while
drawing it**.  ⚠ So it is `@X352` at its tightest: *the cost of the workaround
is the measurement of the mistake*, with the measurement delivered **at the
moment of the mistake** rather than an hour later.

⚠ **RULED, 2026-08-30** (`@X360`): the question of an authored base holding a
shape no drive could produce **does not arise in the shipped game**, because
*every base that is seen is built by a player*.  An authored base is
scaffolding, not content.

### ⚠⚠ AND THE TWO PASSES ARE A RACE AGAINST YOUR OWN CREW  `@X361`

**Owner, 2026-08-30**: *"the player has to draw a curved wall and then get back
to slowly draw the places where the helper can stand beside the track — before
the helpers have built the wall, because then this becomes even more
problematic."*

⚠⚠ **THIS IS WHERE THE COMPLEXITY ACTUALLY LIVES, AND IT IS A CLOCK.**  The two
things a good rail wall needs are drawn by **contradictory drives**:

| what you are drawing | the drive it takes |
|---|---|
| the **curve** — so a train can pass and you can move fast | **FAST**, because only a fast drive sweeps (`@X359`) |
| the **passing places** — so a helper can step off the track | **SLOW**, because a siding is a precise hex beside a precise hex |

⚠ You cannot do both in one pass.  ⚠⚠ **And the second pass is on the BUILD
CLOCK**: `numbers.json` § construction_tick_hp_per_second is 10 HP/s — *"net
10 s per wall, 20 s per `wall_high`"* — so at a 2/3 s tick a wall hex is solid
in **15 ticks** and a high one in **30**.  The crew start raising what you drew
while you are still drawing it.

> ***You are racing your own helpers to change your mind.***

⚠⚠ **AND THE PENALTY FOR LOSING THE RACE IS ALREADY PRICED, BY TWO NUMBERS
THAT WERE SET FOR OTHER REASONS.**  Once a hex is a standing wall you can no
longer drive it to draw beside it — you have to get **on top**, and
`VEHICLE_BOOST_CLIMB_METRES` is **3.0 m**:

| wall | height | can a boost still get you up to fix it? |
|---|---|---|
| `wall` | 3.0 m | ⚠ **yes, exactly** — a late siding costs a boost, a 2 s window and a 5 s cooldown |
| `wall_high` | 5.0 m | ⚠⚠ **NO.**  3.0 m of boost does not reach 5.0 m of wall |

⚠⚠ ***So a `wall_high` rail must have its sidings right the FIRST time, for
ever.***  That is the risk half of the material choice `@X354` gives its reward
half — tougher, harder to ramp, and **unforgiving of a planning error**.  ⚠ It
is `DESIGN.md` § What kind of game this is in its purest form: the advantage is
real, and the moment of paying for it is the moment you can least afford to.

⚠ **And the crew's own autonomy becomes a lever rather than a nuisance.**
`plans/29`'s search takes the NEAREST job, so the crew raise the end of the
wall they are standing at — which means **where you leave them, and whether you
order one away, chooses which end of your wall goes solid first**.  ⚠⚠ That is
a real strategic use for `helper_drive` and `helper_direct` that nothing in the
game has needed until now: *buy time at the end you have not finished
thinking about.*

### ⚠⚠ The risk, and the thing that prevents it

**A skill curve made of shapes fails if there is ONE optimal shape.**  Then
"learning naturally" ends with every veteran building the same base, and the
depth collapses into a solved puzzle.  ⚠ `ROBOT_ECONOMY.md` § The spreadsheet
test states the requirement in its own domain — *every reward has its own
pressure, so there is **no single number to maximise**, which is what keeps the
economy unsolvable even by somebody reading the source* — and the shape layer
needs exactly the same property.

⚠⚠ **It has it, and the conflict is already MEASURED.**  Rail geometry wants
**space**: curves need radius, sidings need width, and a sweeping run needs
length.  But a bigger perimeter is a **wider wall**, and `@M020` is categorical
— ***the siege front is the wall's WIDTH***, 4 hexes on a five-row wall and 6
on a seven-row one.  So:

> **A base big enough for good rail geometry hands the enemy a wider siege
> front.  Transport and concentration pull against each other, and neither is
> free.**

⚠ That is a genuine conflicting pressure rather than a tuning knob, which is
what makes *optimal* depend on the map, the wave list and what the player
intends to do — and therefore unsolvable in the way `@X200` asks for.

### ⚠⚠ EVERY BASE THAT IS SEEN IS BUILT BY A PLAYER  `@X360`

**Owner, 2026-08-30**: *"In the eventual game all bases that are seen are built
by players.  So your set is useful for your logic only.  That is not an
invalidation — we have to test this with a real player in any case.  And when
we have tested a few situations we can record those bases as new test
scenarios."*

⚠⚠ **This is the content model, and it settles what the corpus IS.**
`tests/scripts/*.keys` is an **instrument** — it checks mechanisms, and every
number this repo has measured (`@M093`'s 170/207, `@M095`'s 209/239/245,
`@M096`'s 240/211) is a claim about a *mechanism on a synthetic base*, never
about what a real player experiences.  ⚠ That is not a fault in the fixtures;
it is what they are for.  **Balance is a player question and cannot be answered
here.**

⚠⚠ **AND THE RECORDING LOOP IS ALREADY BUILT.**
[`plans/18`](../plans/18-scenario-capture/README.md) is **COMPLETE** — capture
(`state_diff`), write down (`emit_keys`), cut to size (`crop_keys`) and cut to
the bone (`reduce_keys`) all exist and all round-trip.  Its own § Status names
the one thing missing: *"what it is still waiting for is an interactive loop to
capture FROM."*

> ⚠⚠ **So *play → find a situation → record it as a scenario* is one KEY PRESS
> away from existing**, and that key is the highest-value unbuilt thing this
> document touches.  ⚠ `@X139` says the key table is a BUDGET and a new row
> needs an argument; this is the argument.

### ⚠ What CANNOT be measured yet, and it is a ROOM gap

⚠⚠ **No base in `tests/scripts/` has a corner.**  Every fixture is a wall
straight across a five-row strip, and `@M093`'s +37 tick layout finding came
from sliding that straight line back and forth — *the simplest possible shape
variation there is*.  A five-row strip cannot hold a bastion, a curve, a siding
or a loop.

⚠⚠ **And the authored maps are strips too** — measured 2026-08-30:
`starter_01` is **34 x 15**, `crossroads_02` **49 x 13**, `the_gap_03`
**30 x 17**.  Long, and thirteen to seventeen rows tall.  ⚠ A perimeter with a
rail-legal curve needs room in BOTH dimensions, and at 60° a hex, a radius of
any use eats most of thirteen rows before the base interior gets any.

⚠⚠ **So what [`plans/04`](../plans/04-map-library/README.md) owes the shape
layer is ROOM, not authored bases** — the bases come from players (`@X360`).
***A player cannot build a round base on a map that is fifteen rows tall.***
⚠ That makes map ROOM a prerequisite for testing any of this with a real
player, which is the only way it can be tested at all.

## What this document does NOT decide

- **Whether a rail is a GROUND LAYER or a vehicle type.**  ⚠⚠ It matters:
  `@FR-M-One-AI` says *a class that needs its own movement code has broken this
  rule*, and a rail-bound train reads like a second mover.  ⚠ The version that
  costs a row instead of a mover is **the rail as a layer**, so *cannot leave
  the rails* becomes `can_step` reading the ground — the same move
  `MATERIALS.md` uses to make a wall-climbing spider one row of data, and the
  shape `height.loft`'s rubble layer already has.  *Not yet decided.*
- **Per-find and per-rail DATA.**  `EXPLORATION.md` § X5's *sap brings insects,
  gems wake elementals* is a second fact per object, and `@X349` says the day a
  second fact exists it must be a NAMED packing.  Unchanged by anything here.
- **Any number.**  Nothing above tunes a tower, a wall or a rail.  ⚠ The
  cadence, the funnel and the ramp are all measurements that have not been
  taken.

## See also

- [`MATERIALS.md`](MATERIALS.md) § Power, § What this design deliberately does
  NOT do — the refusal this page generalises, and the entry it revises
- [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) — the climb rule
  `(M-Ramp-Reachable)` constrains
- [`DESIGN.md`](DESIGN.md) § What kind of game this is — the test every rule
  here answers to
- [`PROGRESSION.md`](PROGRESSION.md) § 9 — assignment is a pillar, and the crew
  are the thing being transported
