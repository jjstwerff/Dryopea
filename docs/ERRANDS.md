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

> ⚠⚠ **It exists for two reasons and one test** (`@X303`): *do not
> simulate a world for the scenario*, and *get realistic routes that make
> the mobs have a believable behaviour*.  **Does this make behaviour more
> BELIEVABLE, or does it only simulate MORE?**

## ⚠⚠ WHY — two reasons, and everything else is machinery for them  `@X303`

Owner, 2026-08-28:

> *"this is all for 2 concrete reasons: **do not simulate a world for the
> scenario**, but get **realistic routes that make the mobs have a
> believable behaviour**."*

⚠⚠ **Read them as a pair, because either alone is easy and the pair is
the whole problem.**  A world simulation buys believability at a cost
this game cannot pay; a cheap fake buys the cost saving and no
believability.  Everything in this document — the rule instead of a
state (`@X299`), the bound (`@X300`), the closed form (`@X302`) — exists
to get the second without the first.

> ⚠⚠ **THE TEST: does this make behaviour more BELIEVABLE, or does it
> only simulate MORE?**

⚠ Anything that adds simulation without adding something a player can
SEE is refused by it.  Some worked answers, because the test is only
useful if it decides cases:

| proposal | verdict |
|---|---|
| node stock levels ticking during a sortie | ❌ **simulation only** — nobody can see a number that is not on screen, and `DESIGN.md` § HUD will not put one there |
| a hauler carrying something **visible**, and putting it down | ✅ **believability** — the cargo is `@X053`'s harvester body, already drawn |
| moods, needs, memory, an affinity bar | ❌ simulation, and `@X131` already refuses the bar one system over |
| a robot walking **round** a rock instead of into it | ✅ — and `crawler` measured the ugly version: greedy *"walked into the first concave obstacle and stopped there — permanently"* |
| a robot going **home** at the end of its round | ✅ — and it replaces a deletion (§ Home is a PLACE) |
| a full economy tick per scenario | ❌ — and `@X298` puts it on the server, which is where it buys something |

### ⚠⚠ BELIEVABILITY is owed where it can be OBSERVED; CONSISTENCY is owed everywhere

⚠ This is the line that makes the rule/state split principled rather than
an optimisation, and it is worth stating in one place:

- **Inside the tracked window** a mob is watched, so it owes the player a
  believable *path* — it goes round obstacles, it does not walk into
  walls, it does not teleport, it does not stand in a doorway for the
  run.  That is § Deviation, and it is why deviation exists **only**
  there.
- **Outside it** nobody can observe anything, so what is owed is not
  believability but **consistency**: the rule must never produce a
  contradiction the player can catch when they next look — a mob inside a
  wall, two mobs on one hex at the moment of materialisation, a hauler
  arriving home with a load it never picked up.

⚠⚠ **So the cheap half is not a cheat.**  It is not a lower-fidelity
simulation of the same thing; it is **the same answer, computed** — which
is exactly `@X299`, and why plan 22's *"the outcome is unchanged"*
objection does not apply.

### ⚠ What it means for AUTHORING

⚠ The reasons above are about the RUNTIME, and the rule is uniform.  What
they do change is where **authoring effort** is worth spending: a leg of
a cycle that crosses the patch is one the player will watch, and a leg
that runs off to a shed two cells away is one nobody ever sees.  ⚠⚠ **The
detail belongs on the legs that are visible** — which is a guideline for
whoever places a POI, and deliberately NOT a runtime rule, because a
runtime rule that varied with what the player can see is the thing
[`plans/22`](../plans/22-the-field-cache/README.md) refuses.

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

## ⚠⚠ Formal rules  `@X327`

⚠ The citable form of what this document argues.  ⚠⚠ **Spec-first, and
that opens NO deviation** — there is no implementation yet, so there is
nothing breaking a rule; the build obligation is
[`plans/30`](../plans/30-the-mob-routine/README.md), which cites these by
name.

```
  (E-Rule-Not-State)
                     a mob's position is a FUNCTION, not a memory:
                     `position(t) = cycle(poi.state, anchors, t − slip)`.
                     An un-tracked mob is therefore COMPUTED and not
                     simulated coarsely, which is why materialising it
                     later must not change where it is.

  (E-One-Door)       exactly one function moves a mob on a cycle, and it
                     owns `slip`.  Twelve sites could otherwise write a
                     position, every omission is silent, and a forgotten
                     `slip` shows up only as a mob arriving early.

  (E-Slip)           a deviation costs TIME and never DESTINATION.  A
                     body pushed off its cycle re-converges on the same
                     hex and the delay is in `slip` and nowhere else —
                     which is what keeps (E-Rule-Not-State) true while a
                     body steps aside.

  (E-Closed-Form)    a cycle is evaluable at an arbitrary `t` in O(legs)
                     and never by stepping forward from its start.  So
                     *elaborate* costs LEGS, and a cycle whose legs do
                     not sum to its period is refused at construction
                     rather than wrapped silently.

  (E-Bag-Steers)     what a carrier does next is decided by its BAG and
                     never by a clock: `carry > 0 ? alt : work`.  A route
                     crossing a 1.5 km cell is longer than any period a
                     clock could use, so a calendar-steered cycle
                     oscillates for ever and delivers nothing.

  (E-Non-Increasing) a mob's distance to its current destination never
                     INCREASES: an ordinary step strictly decreases it
                     and a sidestep holds it equal.  It is what makes
                     (E-Boundable) possible at all — a deviating body
                     stays inside the disc it started the leg in — and a
                     sidestep that could move a mob further away breaks
                     the bound and every phase resting on it.

  (E-Boundable)      every mob's reach is a STATIC region known before it
                     takes a step, so *could this ever be in this window?*
                     is a query rather than a scan.  A role whose route
                     cannot be bounded cheaply has broken this rule.

  (E-Poi-Owns)       a mob belongs to a POI and its anchors derive from
                     one, so the POI is the bound and culling one skips
                     its whole population.  The bound itself is the union
                     over legs of the disc centred on each anchor with
                     the incoming leg's length as its radius — which
                     (E-Non-Increasing) is what makes correct.

  (E-Poi-Persists)   a POI is never removed from a scenario.  A broken
                     one is a STATE, its workers still walk to it and
                     find out, and *not materialised* is a different word
                     from *culled*.

  (E-Place-State)    state belongs to the PLACE and not to the traveller.
                     A POI may hold as much as it likes — there are few
                     and they do not move — and a mob may hold none
                     beyond `carry` and `slip`, because there are many
                     and they are everywhere.

  (E-Order-Wins)     an explicit destination is an ORDER and the
                     automatic search never overrules one.  A verb that
                     says GO HERE is honoured or the vocabulary that
                     spells it is lying.

  (E-Remit-Trades)   a remit narrows the KIND and widens the REACH, both
                     halves.  Narrowing alone is a pure loss nobody would
                     ever choose; widening alone is free, and a decision
                     that costs nothing is not one.

  (E-Built-Not-Seen) a mob leaves its route for something the player DID
                     or BUILT, never for the player being SEEN.  A
                     distraction on sight collapses the whole routine
                     into *monsters walk at you*, which the game already
                     has.

  (E-Home-Is-A-Place)
                     a mob that finishes its round leaves the roster at a
                     PLACE — a maintenance point, a nest — and never by
                     being deleted where it happened to stop.  The
                     conservation is the same; what it gains is somewhere
                     the player can watch.
```

⚠ **Deviations: none, and none possible yet** — nothing implements these.

## ⚠⚠ The model in ten lines

Read these first; everything below is the argument for one of them, and
§ WHY is what they are all for.

1. ⚠ **The economy is the SERVER's, at 1.5 km hexes.**  A scenario is a
   frozen snapshot of ONE cell (`@X298`).
2. ⚠ **A scenario carries POINTS OF INTEREST, and a POI owns its mobs**
   (`@X301`) — the face, the shed, the nest, the heap.
3. ⚠⚠ **A mob has a RULE, not a state**: its cycle is a closed-form
   function of a few fixed anchors, so an un-tracked mob is COMPUTED
   rather than simulated coarsely (`@X299`).
4. ⚠⚠ **The rule must be BOUNDABLE** — *could this mob ever be in this
   window?* answered statically, per POI, so a cull throws away a whole
   population at once (`@X300`).
5. ⚠⚠ **Elaborate costs LEGS, and `slip` keeps the closed form true when
   a body steps aside** (`@X302`).  The rule is the ground truth; the
   body is the approximation.
6. ⚠⚠ **A mob is a rule until something makes it a STATE**, and the
   bubble is the one-way door that does — which dryopea has had since
   BACKLOG B4.
7. ⚠⚠ **STATE BELONGS TO THE PLACE, NOT TO THE TRAVELLER** (`@X304`).  A
   POI may be as stateful as it likes — there are few and they do not
   move — and **it is never culled**: collapse a mine and its workers
   still come, find out, and try to fix it.
8. ⚠⚠ **TWO TO FOUR POIs, AND EACH EARNS ITS PLACE BY MOVING THE CLOCK**
   (`@X305`).  A budget, like the key table's — and it is what makes 6
   and 7 affordable at all.
9. ⚠⚠ **THE RESULT IS THE SNAPSHOT, CHANGED** (`@X306`) — POI states and
   denied throughput back to the WORLD, the `Manifest` to the PLAYER, and
   half of it shipped with [`plans/28`](../plans/28-the-scramble/README.md).
10. ⚠⚠ **THE COARSE MAP IS A REAL MAP** (`@X307`), Ortler-shaped, and a
   planet has **two layers** (`@X308`): the GIVEN, which is author-free
   because nobody designed a mountain range, and the ACCUMULATED, which
   is what players did.

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

### ⚠⚠ And the coarse map is a REAL map, not an abstraction  `@X307`

Owner, 2026-08-28:

> *"there will be a world map in 1.5 km eventually like the ortler map in
> `../crawler`, with routines to determine details from that."*

⚠ So the server's state is not a table of rates hanging in space — it is
**a map**, at 1.5 km per hex, with the economy laid onto it.

⚠⚠ **[`WORLDGEN.md`](WORLDGEN.md) owns the world → scenario half**, and
it opens with a correction to this row (`@X309`): `crawler`'s Ortler map
is a **calibration fixture** rather than the game's world, and the game
reads a hand-authored 9×7 array.  ⚠ It also carries the owner's SECOND
reason for a world map — **a backdrop of real geography** (`@X312`) —
which is what moves the recommendation to real data after all.

⚠⚠ **Which means a scenario's terrain is DERIVED, not only its
economy** — the routines that determine detail from a coarse cell decide
what ground a base lands on, and § The two scales' ratio is the size of
the derivation: **one coarse hex has to produce a million fine ones.**

⚠ dryopea has **no procedural generation of any kind today** — the three
maps in `maps/` are authored `.keys` files built into committed `.json`
pairs, and there is no seed anywhere in `src/`.  So this is a first, and
two written rules already govern it:

- ⚠⚠ **`@X224` — other players are the SEED.**  *"If the seed is a number
  the author chose then the author knows the world; if it is what other
  people did, the author cannot know it, because it has not happened
  yet."*  ⚠ And it is not randomness: player history has no expectation
  to compute.
- ⚠⚠ **And the GATES need it deterministic**, which sounds like a
  conflict and is not: **a seed is unknowable in advance but not
  un-recorded.**  Once a cell is generated its seed is a concrete value,
  and [`plans/18`](../plans/18-scenario-capture/README.md) already turns
  any reached state into a `.keys` fixture — so the gates test *a* world
  exactly, while live worlds come from history nobody can precompute.

⚠ It also closes a loop with § The compact RESULT: **`@X306`'s delta IS
the player history `@X224` wants as a seed**, so the two rulings are the
same mechanism read from opposite ends.

#### ⚠⚠ TWO LAYERS — the GIVEN and the ACCUMULATED  `@X308`

Owner, 2026-08-28:

> *"this will be populated by user actions, yes.  But we have something
> to draw on the planet at the start."*

⚠⚠ **This fills a real gap in `@X224`**: *other players are the seed*
cannot be the whole story for the FIRST player on a fresh planet, because
there is no history yet.  So a planet has two layers and they are not the
same kind of thing:

| | what it is | who wrote it | when |
|---|---|---|---|
| **the GIVEN** | the terrain, and the initial installation graph | derived once — the Ortler-shaped half | **t = 0**, the same for everybody |
| **the ACCUMULATED** | raided POIs, denied routes, abandoned bases, caches | ⚠ **the players** (`@X224`, `@X174`) | every finished sortie (`@X306`) |

⚠⚠ **And the reason a real-world map is the right GIVEN is that it is an
AUTHOR-FREE SEED.**  Nobody designed the Ortler massif; it was imported.
So `@X224`'s requirement — *"I do not want to know what to find before I
boot up the game"* — is met **even at t = 0**, by a layer that has a
history nobody in this project invented.

> ⚠⚠ **A heightmap is not authored, so importing one buys the
> anti-optimisation property before a single player has done anything.**

⚠ It also satisfies `@X244`'s *scale must not be assumed*: a
five-player world and a capped public one are different places, and
**the given layer has to carry the whole game on its own** — a planet
nobody has played yet must already be worth landing on.

⚠ **And it is CONTENT, so its licence travels with it** — the rule
[`assets/README.md`](../assets/README.md) already keeps for the one
binary the game loads.  Real-world elevation data has a provenance and a
term, and the day a region lands in `data/` is the day that has to be
written down beside it.

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

1. ⚠⚠ **AN ECONOMY NODE IS ALMOST NEVER *AT* A BASE, AND THAT IS NOT THE
   SAME AS *not on the map*.**  A cell holding a mine means the mine is
   *somewhere* in 1.5 km, so landing on top of it is about one chance in
   a hundred — **which makes it a DELIBERATE sortie-selection choice**
   (*"land next to the repair point"*), the decision
   [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The graph says picking a
   neighbourhood should be about.  ⚠ **But the node is not the
   scenario-scale object** — see § Points of interest, which is what the
   patch actually carries.
2. ⚠⚠ **WHAT YOU GET IS ROUTES, AND THEY CROSS.**  A route through a cell
   is 1.5 km long and your patch covers a tenth of it, so a route either
   crosses your ground or it does not.  That is
   [`SETTING.md`](SETTING.md) § They were on an ERRAND made literal:
   **the player's base is an accident on somebody's commute**, and both
   ends of the commute are off the map.
3. ⚠ **The bubble is 2 % of a cell**, so *where you land inside the cell*
   decides whether it touches a road at all.  § Transport routes already
   calls the bubble the aggro radius; the ratio is why that matters.

### ⚠⚠ Points of interest — the scenario-scale object, and it OWNS the mobs  `@X301`

Owner, 2026-08-28:

> *"so a given scenario has a set of points of interest and these have
> their attached mobs to them with their cycle that can be quite far and
> elaborate."*

⚠⚠ **This is the layer the ratio above was missing, and it corrects the
reading of it.**  The economy's node is a **1.5 km cell fact** — *this
cell mines*.  A **point of interest is its local expression on a
patch**: the working face, the spoil heap, the maintenance shed, the
nest, the wreck field, the outcrop.  ⚠ So a base does not need to land on
a mine to have something to look at — **it lands in a neighbourhood, and
the neighbourhood has features**, several of them, at scenario scale.

| | the economy NODE | a point of INTEREST |
|---|---|---|
| scale | one 1.5 km cell | a hex, or a few |
| how many | one per cell, or none | ⚠ **a SET per scenario** |
| what it is | *this cell mines* | the face, the shed, the nest, the heap |
| who owns it | the server, between sorties | the scenario snapshot |
| what it does | sets a rate | ⚠⚠ **it OWNS MOBS** |

#### ⚠⚠ FEW, AND EACH ONE LOAD-BEARING  `@X305`

Owner, 2026-08-28:

> *"there should be a limited set of POIs to prevent overload for
> players.  But the ones who are there should have a profound impact on
> the scenario."*

⚠⚠ **A BUDGET, and the same one `@X139` puts on the key table** — *a new
row needs an argument* — and the same discipline `DESIGN.md` § HUD keeps
with one number on screen.  dryopea's sets are all small on purpose: four
marker kinds, four robot roles, six helpers, eleven palette hotkeys.

##### ⚠ How many — derived, not chosen

⚠ The number falls out of what a SORTIE can act on.  Dealing with a POI
is a **trip** (§ What a POI is FOR), a sortie affords a handful of them,
and `@X197` wants **always more tasks than there is time for**.  So:

> ⚠⚠ **The count should exceed what one sortie can act on, and not by
> much.**  **Two to four** is the band that reads: fewer than two and
> there is nothing to choose between; more than about four and the player
> cannot hold them, and the ones they never reach stop being a decision
> and become noise.

⚠ It is the assignment pillar (`@X197`) one layer up — *which one you go
to IS the decision* — and it is why the answer is not *as many as the map
will hold*.

##### ⚠⚠ THE ADMISSION TEST, in dryopea's own currency

⚠ *"Profound impact"* has to be falsifiable or it is a wish, and this
repo already knows how to say it — every mechanic here carries a measured
worth: a wall is **+44 ticks** (`@M050`), a trench **130 / 174 / 221**
(`@M059`), a re-armed plate **+106** (`@M057`), an order **+34**
(`@M070`).  So:

> ⚠⚠ **A POI earns its place if REMOVING IT MOVES THE SCENARIO'S CLOCK
> measurably — a scenario pair one token apart.**  A POI that changes the
> picture and not the play is scenery, and scenery does not get a
> population.

⚠ Three ways a POI can pay, and a good one pays more than once:

| it pays in | example | already designed as |
|---|---|---|
| **pressure** | a route from it crosses your bubble, so it IS your wave | `ROBOT_ECONOMY.md` § Transport routes |
| **a parameter** | a maintenance point in reach refunds your chip damage, so *kill* and *hurt* become different verbs | § 5, `damage_persistence` |
| **a target** | raiding it changes the run, and the swarm answers (`@X304`) | § 5 *deny it* |

⚠⚠ **And *scenery* is not a slur — it is a different budget.**  A wreck
field with nothing attached is worth painting; it simply is not a POI,
because a POI owns a population and a population has to be worth
watching.

##### ⚠ What the limit buys the rest of the design

⚠ Few POIs is what makes `@X304`'s *never culled* affordable: a handful
of places that always exist and always run their cycles is a cost that
does not scale with the map, however far the player drives (`@X299`).
⚠⚠ It is also what makes the CULL cheap in the first place — `@X300`
indexes bounds **per POI**, so a scenario with four of them has four
bounds to test, not four hundred.

#### ⚠⚠ A mob belongs to a POI, and its cycle radiates from one

**That is the attachment, and it is what makes the whole model cheap.**
A mob is not spawned onto the map and given a job; **a POI has a
population, and each of them has a cycle anchored on it** — which may run
far off the patch and back (*"quite far and elaborate"*), because off the
patch is simply un-tracked (§ The scenario GROWS).

⚠⚠ **And it collapses the bound from per-mob to per-POI.**  `@X300`
requires a static region that can be inspected against a window; if every
mob is anchored on a POI, then

> ⚠⚠ **the POI IS the bound, and culling one culls its whole
> population** — one query instead of `N`.

⚠ Which is the same shape `crawler` reaches for and does not build:
`near_mobs_test.loft` measures a spatial index over individual mobs at
22× fewer candidates, where an index over **POIs** is smaller again by
the population factor and is **static for the sortie** where positions
are not.

⚠ It also makes a scenario authorable and generatable in one step —
place the POIs, attach the populations — and it is what a `.keys` file
and a map should carry rather than a mob list.

#### ⚠⚠ A POI IS NEVER CULLED, AND A BROKEN ONE IS NOT AN ABSENCE  `@X304`

Owner, 2026-08-28:

> *"the POI should not be culled during a scenario, even when a mine
> collapses the workers will still move towards it and be witness that it
> is now impossible to work there and see if they can repair it."*

⚠⚠ **This corrects a word this document was using for two different
things, and the two must never be confused:**

| | what it means | may a POI be it? |
|---|---|---|
| **not MATERIALISED** | outside the window, so no body is drawn and no collision exists.  ⚠ It is still there, and its mobs still run their cycles | ✅ always, and it costs nothing |
| **CULLED** | removed from the scenario — gone, its population gone with it | ❌ **never** |

⚠ Everywhere § The rule must be BOUNDABLE says *cull*, it means the
first: **throw away the WORK of materialising, never the thing.**

#### ⚠⚠ A collapsed mine is a STATE of the POI, not the end of it

**The workers still come.**  They walk their cycle to a face that is no
longer there, they arrive, and they find out — and *that* is the
consequence of the player's raid arriving where the player can watch it.

⚠⚠ **And it is the fiction's own behaviour, not an invention.**
[`SETTING.md`](SETTING.md) § They approach to REPAIR: the robots'
whole reading of a broken thing is *"they try to get to the source of the
disruption to see what they can do about it."*  A collapsed mine
producing a stream of confused workers who mill at it and try to fix it
is **exactly what this swarm does**, and it is the single most
believable thing in this document by `@X303`'s test — the player caused
it and can stand there and see it.

⚠ Three things follow, and each is a mechanic rather than flavour:

- **A raid is not a permanent free win.**  The swarm answers a broken
  thing by sending repair capability, which is
  [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § 5 and `DESIGN.md` § Boss =
  mobile REPAIR PLATFORM — **so collapsing a mine may SUMMON the machine
  that fixes it.**  That is a real cost on the raid, and it is
  `DESIGN.md` § 20's *nothing may permanently close* at scenario scale.
- **The delta a sortie hands back gets a second column**: not only *what
  the player destroyed* but *what the world had started to repair*.
- **A POI has states, and they are few**: `working` / `damaged` /
  `collapsed` / `raided` / `repaired`.  ⚠ A state a mob can SEE on
  arrival, not a number on a screen.

#### ⚠⚠ STATE BELONGS TO THE PLACE, NOT TO THE TRAVELLER

**This is the division that lets a POI be as stateful as it likes while
`@X302`'s closed form survives untouched**, and it is worth stating on
its own:

> ⚠⚠ **A POI may hold state — there are few of them and they do not
> move.  A mob may not — there are many and they are everywhere.**

⚠ So the cycle becomes a lookup wider by one argument and no more:

```
position(t) = cycle(poi.state, t - slip)
```

⚠⚠ **Still O(legs), and the BOUND is unchanged** — every cycle a POI can
issue is anchored on that POI, so the region is the union over its states
and the union is the same region.  ⚠ A state change is a rare, timestamped
event (a player action), so the form is **piecewise** closed rather than
closed: valid between changes, with the current segment's `t0` beside the
state.  That is one extra field on a POI and nothing at all on a mob.

⚠ **And *witnessing* costs no new mechanism**: what a mob does on arrival
is a leg selected by the POI's state.  *Arrive, find it broken, mill,
go home* is three legs; *arrive, work, go home* is three legs.  The
elaborate part is the table, not the engine — which is § Roles are a
TABLE, from the first line, applied to places instead of people.

#### ⚠ What a POI is FOR, in play

⚠⚠ **A POI is where a distraction gets its reason.**  § Distraction
requires that a mob leaves its route for something the player DID or
BUILT; the POI is the other half — **the thing the mob was doing it
for** — so *"the haulers come because you left their cargo lying"* has a
place the cargo was going, and a raid on the shed is a thing the player
can go and do.

⚠ And it is where the player's interference lands: `ROBOT_ECONOMY.md`
§ 5's *deny it — a raid that makes your damage start sticking* is a POI
on a patch, at scenario scale, reachable in a sortie.

### ⚠ What the snapshot actually is

The server hands a sortie **one cell's economic state**, and it is small:

```
cell:      which coarse hex, and what is in it (a node, or nothing)
crossings: for each route through this cell — the bearing it enters on,
           the bearing it leaves on, robots per minute, and the mix
pois:      the local features on THIS patch, and the population attached
           to each — the face, the shed, the nest, the heap
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

## ⚠⚠ The compact RESULT — what a finished scenario hands back  `@X306`

Owner, 2026-08-28:

> *"and the server should get a compact result of finished scenarios to
> use as a basis for future missions."*

⚠⚠ **HALF OF IT IS ALREADY BUILT.**
[`plans/28`](../plans/28-the-scramble/README.md) S3 shipped
`scramble.loft::manifest_of` — `Manifest { points, crew, left }`, read at
liftoff, gated by `tests/28_s3` and measured by `@M068` (**200.0 cut
short against 225.3 played out**).  Plan 28 closes on the exact sentence
this request answers: *"carryover is produced and measured; **nothing
consumes it**."*  ⚠ The server is what consumes it.

### ⚠⚠ THE RESULT IS THE SNAPSHOT, CHANGED — not a second format

⚠ `@X298` has a sortie READ one coarse cell.  The cleanest possible
result is the same rows written back:

| the snapshot carries | the result carries |
|---|---|
| the cell, and what is in it | unchanged — a cell is not destroyed |
| each crossing: bearings, rate, mix | ⚠ **the rate the player DENIED** |
| each POI, and its state | ⚠ **the state it was left in** (`@X304`) |
| — | ⚠ the player's **`Manifest`** (already built) |

⚠⚠ **So there is nothing to design a format for**: read a cell, play,
write the same fields back.  A result that needed its own schema would be
a second description of the world and would drift from the first — which
is the mistake `damage.loft` § What the hex becomes and
`compare.loft`'s hand-maintained field list both already cost dryopea
once.

### ⚠⚠ TWO destinations, and they must not be confused

| | goes to | why |
|---|---|---|
| the **`Manifest`** — points, crew aboard, crew left | ⚠ **the PLAYER's account** | it is theirs: `@X188`'s per-planet-**per-player** path, and `DESIGN.md` § 14's 1:1 carryover |
| the **cell delta** — POI states, throughput denied | ⚠⚠ **the WORLD, shared** | `@X177` settles the economy per-PLANET precisely because *"you cannot compete for a mine that only exists inside your own instance"* |

⚠ Both are small.  ⚠⚠ **Compact is not an optimisation here, it is the
requirement**: a server is a *persistent world-state store plus identity*
(`@X243`) and not a live host, so what it keeps has to be the size of a
few rows per finished sortie, for ever.

### ⚠ And the world half must DECAY, or a planet only accumulates damage

⚠⚠ `DESIGN.md` § 20 is categorical — **nothing may permanently close** —
and `@X228` names the mechanism: *grow and crumble* needs decay, because
nothing that only accumulates can be seen to fall.

⚠ So a collapsed POI is **being rebuilt** between sorties, on the
server, slowly.  ⚠⚠ **Which is the same behaviour as `@X304`, one system
up**: inside a sortie the swarm sends repair capability at a broken
thing, and between sorties the store does the same job on a longer
clock.  The player's raid is a **setback they inflicted**, not a hole
they punched, and coming back to a half-repaired mine is the most
legible thing a persistent world can show them.

### ⚠ What it makes possible, and where it is written down

⚠ [`ROADMAP.md`](../plans/ROADMAP.md) § Then the run becomes a RUN item
**7 — carryover** is this, and item **6 — the landing flow** is its other
end: a sortie reads a cell that a previous sortie wrote.  ⚠⚠ **That is
`ROBOT_ECONOMY.md` § Open questions 1's per-planet answer made
concrete** — *"cutting a route in one sortie changes the next one, which
is what would make a run feel like a campaign"* — and it needs no new
mechanism beyond the two halves above.

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

### ⚠⚠ The rule must be BOUNDABLE, not merely evaluable  `@X300`

Owner, 2026-08-28:

> *"that is the reason why crawler has this AI model — in a given area
> there are far less mobs than there are on the complete map.  But that
> is fine as long as the overall mobs have a rule that can be inspected
> to see if they can be within a window."*

⚠⚠ **This is the requirement that decides the shape, and it is stronger
than *the rule can be evaluated*.**  A rule you can only evaluate tick by
tick still costs one evaluation per mob per tick over the WHOLE map — the
linear scan `crawler` measured and did not fix.  A rule you can **bound**
costs a query.

⚠ So a role must answer two questions, not one:

| | question | cost | when |
|---|---|---|---|
| **bound** | *could this mob EVER be in this window?* | static | **once**, at spawn |
| **evaluate** | *where is it right now?* | cheap | per tick, per **candidate** |

⚠⚠ **And the bound is static because the ANCHORS are.**  § What a mob
carries fixes `home` / `work` / `alt` when the mob enters the patch and
never changes them — the same property `crawler` derives its cache size
from (*"an actor can ask for at most three destinations"*,
`src/sim.loft:3216`).  So a mob's reachable region is **the corridor
hull of three fixed hexes**, known before it takes a step, and it can be
indexed once for the whole sortie rather than re-derived per tick.

Three tiers, and each throws away work the next one would have done:

1. **CULL** — the window (plus a margin) against every mob's static
   bound.  ⚠ A mob whose route never approaches is skipped **for the
   sortie**, not for the tick.  ⚠⚠ **Skipped, never REMOVED** — it and
   its POI are still there and its cycle still runs; what is thrown away
   is the work of materialising it (`@X304`).
2. **EVALUATE** — the rule, for the survivors, to get the hex.
3. **MATERIALISE** — a body, with collision and § Deviation, for the ones
   actually inside.

⚠⚠ **A role whose route cannot be bounded cheaply is a role that breaks
this model**, and that is the test to apply to a new one.  *Wander at
random* has no bound; *patrol between two posts* has an exact one.  ⚠ It
is the same shape as `ROBOT_ECONOMY.md` § The governing rule — *an
installation that needs its own movement code has broken it* — one layer
down.

#### ⚠⚠ And against the BUBBLE the cull is not just static, it is the whole wave question

[`plans/22`](../plans/22-the-field-cache/README.md) already establishes
the fact this rests on:

> *"the field's useful domain is FIXED for a whole run, because the
> bubble is centred on the CORE.  It does not follow the player, it does
> not follow the camera, and it does not move."*

⚠⚠ **So *does this route ever get cut off?* is answerable the moment the
base lands** — it is the bound against a disc of 25 hexes at a hex that
never moves — and it is decided **once, for the sortie**.

⚠ Which makes [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The graph's
*"the only thing that matters is which edges the base happens to sit
on"* a **computable predicate** rather than a phrase, and it means the
sortie knows its own wave pressure at landing — before a robot has
walked anywhere.

### ⚠⚠ And an ELABORATE cycle must stay CLOSED-FORM  `@X302`

Owner, 2026-08-28:

> *"but that should not make the calculations impossible to perform for a
> given window"*

⚠⚠ **This is the hard constraint on how elaborate a cycle may BE**, and
it is the one that keeps § Points of interest honest: *"quite far and
elaborate"* is affordable only while a window query stays cheap.

> ⚠⚠ **A cycle must be evaluable at an arbitrary time `t` in O(legs) —
> never by stepping forward from where the mob started.**

⚠ A cycle of legs gives that for free:

```
phase = (t - t0) mod cycle_length      # one modulo
leg   = the leg phase falls in         # O(legs), and legs are few
pos   = along that leg                 # one interpolation
```

⚠ So *elaborate* costs **legs**, and legs are a handful.  A cycle with
six of them is answered in six comparisons and is as cheap at hour two
as at minute one, which is the property *"a rule that can be inspected to
see if they can be within a window"* (`@X300`) actually needs.

#### ⚠⚠ What would destroy it — and DEVIATION is the thing that tries

**Anything path-dependent.**  A mob that was delayed, blocked, or pushed
off its line has a HISTORY, and history has no closed form.  ⚠ § Which is
also why DEVIATION only exists inside it is exactly that hazard: a
sidestep is a departure from the rule.

⚠⚠ **The resolution is one integer field, and it makes the deviation
COST something instead of being free:**

```
Errand { … slip: integer }        # base units this mob has fallen behind

position(t) = cycle(t - slip)
```

⚠ A deviation increments `slip` — the mob is genuinely later, not
somewhere else.  ⚠⚠ **The closed form survives** (it is the same function
with a shifted argument), and **the BOUND is untouched**, because the
region a cycle covers does not depend on when it is traversed.  That
second property is what keeps § The rule must be BOUNDABLE's cull cheap
in the presence of deviation at all.

> ⚠⚠ **THE RULE IS THE GROUND TRUTH AND THE BODY IS THE APPROXIMATION**,
> never the other way round.  A materialised mob that steps aside
> re-converges on the rule; what it spends is time, and `slip` is where
> the spending is written down.

#### ⚠ And the escape hatch is the one dryopea already owns

⚠⚠ A mob that CANNOT re-converge — it was cut off by the bubble, wrecked,
walled in — is exactly the transition § A mob is a RULE until something
makes it a STATE describes, and it is **one-way**.  From that moment it
is tracked and stepped like any enemy, and no closed form is claimed for
it.

⚠ So the model has two populations and one door between them, and the
door is `wave_cutoff`, which has existed since BACKLOG B4:

| | how it is answered | how many |
|---|---|---|
| **on a cycle** | closed form, O(legs), from a POI-bounded set | the map |
| **cut off** | stepped, every tick, like any enemy today | ⚠ the wave — and `@M005` says the longest base the corpus plays is **321 ticks** |

⚠⚠ **The budget test, and it is the one to gate on**: a window query is
**O(POIs) to cull** plus **O(legs) per surviving mob** plus the tracked
roster, which is the cost dryopea already pays.  A design that cannot
state its answer in that form has broken `@X302`.

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
    slip:  integer,// base units it has fallen behind its cycle (@X302)
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

## ⚠⚠ And this is LIBRARY work too  `@X322`

⚠ [`WORLDGEN.md`](WORLDGEN.md) § THIS IS LIBRARY WORK owns the ruling;
the same three layers apply here, and the split falls in the same place:

| layer | from this document |
|---|---|
| **the LIBRARY** | the closed form and `slip` (`@X302`), the static bound and the CULL / EVALUATE / MATERIALISE tiers (`@X300`), the rule-not-state split (`@X299`), the POI-owns-its-mobs relation (`@X301`) |
| **the CATALOGUE** — the game's, declaratively | ⚠ the ROLE rows: which anchor a role works from, whether a bag or a clock steers it, what draws it off-route |
| **the SCRIPT** — the game's, imperatively | ⚠ `.keys`: `poi`, `route`, `remit`, and the edge cases a table cannot say |

⚠⚠ **What is NOT library work is every number and every refusal**:
`@X303`'s test, the four job kinds, *the bag steers not the calendar* as
a POLICY rather than as a mechanism, and above all **a distraction must
be caused by something the player BUILT**.  ⚠ A library hard-coding any
of those would be dryopea wearing a library's name — **it supplies the
shape; the game supplies the reasons.**

⚠ And § Roles are a TABLE, from the first line was already saying this
without knowing it: the reason `crawler`'s hard-coded `role ==` branches
went stale is that **they were policy living in the mechanism's file.**

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

1. **Is a mob population a POOL or a TAP?**  ⚠ `@X301` has largely
   answered it: **a POI has a population**, and a population is a pool.
   What is left is whether killing one is visible *within* the sortie —
   fourteen haulers become thirteen and the road thins — or only in the
   delta afterwards.  ⚠⚠ **§ WHY decides it**: a pool the player can
   deplete and SEE thin is believability; a counter they cannot observe
   is simulation.  *Recommendation: a POOL, and small enough to notice* —
   which also makes a raid on a POI worth the trip, and is what
   `ROBOT_ECONOMY.md` § 5's *deny it* needs to mean something at
   scenario scale.
2. ⚠ **ANSWERED by `@X306`** — § The compact RESULT.  The delta is *the
   snapshot, changed*: throughput denied per crossing, the state each POI
   was left in, plus the player's `Manifest`, which
   [`plans/28`](../plans/28-the-scramble/README.md) S3 already built.
   ⚠⚠ **What stays open is the RATE OF DECAY** — how fast the server
   rebuilds what a raid broke.  Too fast and a raid buys nothing; too
   slow and a planet only accumulates damage, which `DESIGN.md` § 20
   forbids.  ⚠ It is the same question `ROBOT_ECONOMY.md` § Open
   questions 4 asks about waking the military, and it wants a number
   rather than an argument.
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
