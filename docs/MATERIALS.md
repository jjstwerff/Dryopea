<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# MATERIALS — what things are made of, and what a wreck is worth

⚠ **DESIGN, not built.**  Nothing in `src/` reads any of this.  What
ships today is [`DESIGN.md`](DESIGN.md) § 13's single scalar — **points**,
earned from `wallet.loft::loot_rate` and spent on two things.  This
document is the tree *behind* that scalar: the eight materials, the
parts they make, and the catalogue of things those parts become.

⚠ **Source: the owner's seed notes**, folded in 2026-08-26.  The
verbatim source and the routing table are
[`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) § 4.

## Why this document exists

[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) says **where** robots come from —
six installation types and the traffic between them.  It deliberately
does not say what they are **made of**, and that hole is load-bearing in
two places:

- **`ROBOT_ECONOMY.md` § 1a** makes crystal the boss supply and the best
  lever in the game, which only means something if crystal is *a*
  material among others rather than the only named one.
- **`DESIGN.md` § 13** prices a wreck at a flat 10 points and a harvester
  at 3×.  That multiplier is already a statement that **different robots
  are worth different things** — this document is what makes it a
  statement about *what was in them* instead of a tuning constant.

### ⚠ The governing rule: salvage is a REASON TO GO SOMEWHERE

The same rule [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The governing rule
states, one layer down: **one system, per-type DATA.**  A material is a
row.  A part is a row plus what it is made of.  A machine is a row plus
what it is made of plus what it does.  Nothing here may need new
*behaviour*.

⚠ And it has to pass [`DESIGN.md`](DESIGN.md) § What kind of game this is:
*does this put something in the player's hands at a moment when using it
costs them something?*  A material tree passes that test **only if
getting the material is a trip** — out of the base, into the kill zone,
away from the tower that needs servicing.  A tree that resolves into a
menu is the failure mode, and it is the one this design has to keep
refusing.

⚠⚠ **It must also pass the genre test** ([`DESIGN.md`](DESIGN.md) § And
the DEEP layers are what keep it a tower defence): *does this resolve
into a statement about position, terrain or timing?*  **Materials do —
because they have WEIGHT and VOLUME and are somewhere else.**  A research
tree does not, which is why there is none below.

## The eight materials

| Material | Where it comes from | ⚠ What makes it a decision |
|---|---|---|
| **Stone** | cut from the ground, anywhere | free and heavy — the wall material.  Cutting it *lowers* the hex, which is the trench half of `DESIGN.md` § 5's wall economy |
| **Wood** | the huge trees ([`SETTING.md`](SETTING.md) § The other enemy) | ⚠ cutting one is the loudest thing the player can do: § A tree is also a drill makes a tree a *place*, and its insects a mutualism that answers |
| **Chemicals** | distilled from **sap**; less efficiently from wood | needs an installation, so it is the first thing that makes a base worth *keeping* rather than holding |
| **Metals** | robot wrecks | ⚠ the main loop: `ENEMY_MOVEMENT.md` § Bodies are terrain already makes a wreck an obstacle, so the metal is *underneath the thing blocking your kill zone* |
| **Polymers** | robot wrecks — ⚠ **destroyed by explosions and burning** | the first material that prices a WEAPON CHOICE: flame and splash spend the salvage they earn (`DESIGN.md` § Damage TYPE) |
| **Copper wire** | robot wrecks | ⚠ EMP's inverse: `DESIGN.md` § Damage TYPE says EMP *destroys the high-value electrics*.  Wire is what it destroys |
| **Diamonds** | recycled from specific robots (rotary drills) | class-keyed: a *particular* enemy is worth intercepting, which is `ROBOT_ECONOMY.md`'s readout made concrete |
| **Lithium** | recycled from robots, or taken from an active mining facility | the battery material — and the one with a **raid** attached rather than a pickup |
| **Nitrogen** | distilled from the air, with power | ⚠ the only one that costs no trip and **only** power, which is what makes a power budget mean anything |

⚠ **Crystal is the tenth and it is already owned** —
[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § 1a — deliberately not restated
here.  It is the only input with one product, and it wakes elementals.

### ⚠ The two that are not materials at all

**Sap** and **special gems** are already in `DESIGN.md` § Scouting with
their own pressures attached (sap invites an insect chase by smell; gems
wake matching elementals).  They enter this tree as *inputs*, not as new
rows, and they keep their triggers.  A material that can be bought
without waking anything is the boring one.

## Parts — the intermediate layer

Parts are what a **construction plant** makes out of materials and what a
**recycler** gets back out of a wreck.  They are the level the player
actually thinks in.

| Part | Made of | Recovered from |
|---|---|---|
| **Electrical motor** | wire + metals | most movers |
| **Combustion motor** | metals | heavy movers |
| **Power cell** | metals + chemicals | anything that runs |
| **Battery** | lithium + metals | anything that stores |
| **Fuel tank** | metals | anything that burns |
| **Rocket engine** | metals + chemicals | ⚠ the scramble rocket, and the AI's own launches ([`SETTING.md`](SETTING.md) § The quarantine) |
| **AI core** | ⚠ **not manufacturable — only taken** | bosses and coordinators |

### ⚠⚠ The AI core is the one part with a fiction attached, and that is the point

An AI core cannot be built.  It is recovered, and then it has to be
**programmed or hacked**, at a level.  That makes it the single item in
the tree whose supply is the *enemy's* and whose value is knowledge
rather than mass — and it is the hook every deep-lore lever already
designed hangs on: [`SETTING.md`](SETTING.md) § Future contact's *talk to
an AI as to a person*, and `DESIGN.md` § 9's hacking helpers.

⚠ **A recycler must NOT be able to break a core down.**  The moment it
can, the highest-value thing in the game becomes a quantity, and every
reason to carry one out becomes arithmetic.

## What a wreck yields — the class is the readout

⚠ This is the same mechanism as
[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § How the player ever learns any
of this: **wave composition is a readout**, and salvage composition is
the same readout paid out.  *Miners at the wall means a mine upwind*
becomes *miners at the wall means diamonds after*.

| Class | Rich in | ⚠ Consequence |
|---|---|---|
| **scout** | wire, light metals | fastest, cheapest, arrives first — the salvage you get whether you wanted it or not |
| **miner** | **diamonds**, heavy metals | slow, deadly to a wall (23 ticks vs a scout's 454, `@M016`), and the one worth the tower time |
| **harvester** | ⚠ **whatever it was carrying** | already built: its body is `RUBBLE_CARGO` at 3× (`@X053`) |
| **builder** | polymers, motors | ⚠ the one flame and splash *destroy* on the way to killing |
| **repair platform boss** | motors, power cells, spare parts | `DESIGN.md` § Boss = mobile REPAIR PLATFORM |
| **combat boss** | **AI core**, crystal, weapon parts | `DESIGN.md` § There are TWO boss kinds — the richest and the one that shoots back |

## Weapons — the catalogue, and what it adds to the six types

⚠ **[`DESIGN.md`](DESIGN.md) § Damage TYPE already owns the AXES** — a
six-way triangle (laser / artillery / splash / EMP / flame / sniper)
where every type is bought with a real cost and two of the costs are
things the player does to themselves.  **That table is canon; this list
is not a replacement for it.**  What follows is the seed catalogue sorted
by whether it is already covered.

**Already an entry in § Damage TYPE** — a name, not a new mechanic:
particle beam and laser drill are *laser*; cannon, autocannon, rifle and
mass driver are *artillery* at different rates; explosives, unguided and
heat-seeking rockets are *splash*; flame thrower is *flame*; missile
launcher is *sniper* with a travel time.  ⚠ Adding them as separate
tower types would be six rows that resolve into the same six columns.

**Genuinely new axes**, each worth its own decision:

| Weapon | The axis it adds | ⚠ Why it is not just another type |
|---|---|---|
| **Nitrogen thrower** | **slows** rather than damages | the first weapon whose output is *time*, which is the currency a tower-defence actually spends.  Weak against armour, so it is a screen and never a kill |
| **EMP, player-side** | ⚠ **stuns own units too** | already half-designed: `DESIGN.md` § Damage TYPE gives EMP the worst salvage and the longest wreck.  Friendly stun makes it a **placement** decision — a shelter is the counter, which is a *building* |
| **Mine / explosive drone** | **single use, placed in advance** | ⚠ the purest form of the game's own test: a thing you must go and put somewhere before you need it, and it is gone after.  Placement + timing, no stats |
| **Anti-air** | a target that ignores the ground | ⚠ **blocked on a design question** — see § What this refuses, flyers |
| **Rotary / pneumatic drill** | melee, on a *worker* | not a defence at all: this is how the player's builder cuts stone.  It belongs to machines, below |
| **Hacking station / device** | ⚠ turns an enemy into a unit | already canon as a **helper skill** (`DESIGN.md` § 9) and gated on the AI core.  ⚠ A device carried into the field is the aggressive version and needs the same *analyse first* cost, or it is a win button |
| **Trampling** | close range, ⚠ **damages the wielder** | the boss's, not the player's — and the self-damage is what keeps it from being free |

⚠ **Ammunition and reload are the shared cost**, and dryopea already
built the shape: `tower.loft`'s **30-shot magazine** and the banked
charge.  A weapon catalogue does not need a second scarcity model — it
needs different *magazine sizes and refill costs* over the one that
exists.  See [`plans/16`](../plans/16-the-wave-system/README.md) § W4 for
what the magazine already decides.

## Machines — what a base is made of

Grouped by what they are FOR.  ⚠ Each is a row of data over mechanisms
dryopea has or has designed; the fourth column is the honest one.

| Machine | For | Exists? |
|---|---|---|
| **Radio jammer + bio generator** | the core.  ⚠ It IS the scrambling tower | **built** — `DESIGN.md` § 4 |
| **Recharge / charging point** | batteries | designed as tower repair (`plan 17` T1) — a battery version is the same 20-second standing clock |
| **Speeder** | the player | **built** — `vehicle.loft` |
| **Builder machine** | cuts blocks from the ground, arm + mining laser + lifter.  ⚠ **Not a combat machine** — easily broken, repairable | the missing half of § BUILDING, the roadmap's first gap |
| **Attack tower** | defence, needs power and ammunition | **built** — `tower.loft` |
| **Salvage cart + lifting crane** | hauls wrecks | ⚠ the helper already carries (`carry.loft`); a cart is a *capacity* row |
| **Salvage processing plant** | wreck → materials | the first thing this document needs that does not exist |
| **Construction plant** | materials → machines, weapons, rockets, ammunition | the second |
| **Power station / cells / turbines / hydro / sap burner** | power | ⚠ see § Power, below |
| **Moveable tower, possibly on rails** | defence that follows the front | ⚠ **the interesting one** — see § What this refuses |
| **Storage bunker** | keeps material for a FUTURE mission | ⚠ this is `DESIGN.md` § 20's persistent abandoned base, given a building |
| **Living quarters** | ⚠ crew who otherwise live in their vehicles get lonely | see § The crew are people, below |

### ⚠ The first cut is the expensive one

The seed notes carry one mechanic worth keeping verbatim: **a builder's
first cut into untouched ground is slow; cutting from an existing trench
or from the top side is fast.**

That is a *terrain* statement, and it is the best thing in the machine
list, because it means **where you started building changes what building
costs for the rest of the base** — exactly the racing-line tension
[`PROGRESSION.md`](PROGRESSION.md) § G3 measured on servicing.  A base
grown from one trench is cheap and compact; a base with three separate
starts paid three first cuts.

### ⚠⚠ The crew are people, and loneliness is a real column

*"Living quarters for better quality of life for the people, otherwise
they only live in their vehicles.  They can get lonely without means to
communicate in the wild."*

⚠⚠ **This became a real building on 2026-08-26.**
[`PROGRESSION.md`](PROGRESSION.md) § P2d gives the crew **endurance
pools, spent by work and restored by rest** — so quarters are what a
sortie long enough to need rest has to build.  A base is then **a place
people live**, not only a firing position.

⚠ [`SETTING.md`](SETTING.md) § Why the crew never walks the surface
already makes the crew fragile and suited, and the jammer already means
no communication with orbit during a mission (§ The recruitment) — so
the isolation is true in the fiction before any number is attached.

⚠ **What it must not become is a morale BAR.**  The pool is spent by
work and restored by rest, which is positional and legible; a happiness
meter the player manages through a menu is the failure mode, and it
fails `PROGRESSION.md` § P6a for the ordinary reason — it is a number
that does the player's thinking about their own crew.

## Defensive structures — three that are not walls

`DESIGN.md` § 5 owns walls (two heights, free in points, helper-seconds
the bottleneck) and § Wall topology owns drivable ends and recognised
entrances.  Three seed ideas sit beside them:

| Structure | What it is | ⚠ Where it lands |
|---|---|---|
| **Curved wall** | easier to drive along, better against outside fire | ⚠ **a rendering question first**: `DESIGN_HISTORY.md` § 2's coastline curves were deferred and `plans/25` § M0 measured that dryopea's ground is *a flat plane with pillars*.  A curve is not authorable on this lattice today |
| **Moat** | a trench allowed to fill with water; ⚠ **many machines tolerate water badly** | ✅ **BUILT 2026-08-27** — BACKLOG C5, [`src/moat.loft`](../src/moat.loft), `@X282`.  ⚠⚠ *A trench plus the drop the palette already has* was exactly right, and *"allowed to fill"* turned out to be the load-bearing half: **a pile is a surface only once it clears the water**, so water's 1 m swallows two bodies.  ⚠⚠ **Worth the whole run and it earns nothing** (`@M058`) — 378 ticks still standing against a wall's 174, on the opening 200 points, because a wave that cannot reach you cannot die.  ⚠ *Many machines tolerate water badly* is UNBUILT and is not needed: nothing on legs can enter water at all.  ⚠ Nothing FILLS one yet (BACKLOG C9) |
| **Drawbridge** | a crossing the player can open, close, or blow | ⚠ passes the game's own test outright: it puts a thing in the player's hands **at the moment using it costs them something** — closing it seals the base, and `plans/13` § V4 measured that a sealed base can only be left by boosting |

### ⚠ Bridges need anchors, and that is a design constraint rather than flavour

*"Bridges need sturdy anchors or they will easily fail.  Possibly create
sturdy arch bridges."*

`DESIGN.md` § 5 already has bridges between walls as a phase-2 item and
§ 21 open question 2 has multi-level pathing unresolved.  The anchor rule
adds the thing that makes a bridge a *decision*: **a span is priced by
what holds its ends**, so a bridge is a statement about the terrain at
two places rather than a piece placed in the middle.  ⚠ That is the
difference between a bridge and a long wall, and without it there is no
reason to have both.

## Power — the one subsystem that could eat the game

The seed notes carry a full power layer: generators of six kinds, cables,
drain calculations, rails that also carry power, fuel pipes and storage.

⚠⚠ **dryopea already has a power model and it is deliberately tiny**: a
tower has a **banked charge that decays per shot** and only a player
standing at it refills it (`tower.loft`, `DESIGN.md` § 7).  That single
mechanism is what makes upkeep a *positioning* problem —
[`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 measured two
shuttling helpers clearing all 205 robots where the same two parked reach
5 of 7 and the base falls.

**A grid replaces a trip with a wire.**  The moment power flows down a
cable, the player stops driving to the tower, and the measurement above
says that trip *is* the game.  So:

- ⚠ **A power GRID is refused for the player's base** on the same
  grounds `DESIGN.md` § What kind of game this is refuses a permanent
  advantage for a one-time placement decision.
- ⚠ **Generators are fine** — a bio generator, solar, a sap burner —
  because they are *sources with a position*, and a source you must drive
  to is the existing mechanic with different flavour.
- ⚠ **The ROBOT side may have the full grid**, and probably should:
  [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) already models the enemy's world
  as a static graph the player perturbs.  A power line between two
  installations is one more edge to cut, and cutting it is a *trip*.

## ⚠⚠ The 2023 catalogue is much bigger, and that is the warning

[`../archive/gameplay.data`](../archive/gameplay.data) — mined
2026-08-26 — carries a *filled-in* version of this whole tree, and it is
the single best argument for keeping the shipped one small.

| Layer | The 2023 catalogue | Count |
|---|---|---|
| **materials** | steel, brass, carbon, titanium, aluminium, plastic, wood, **iron wood**, bricks, clay, sand rock, volcanic rock, granite | 13 |
| **fluids** | petrol, nitrogen, rocket fuel, **sap**, oil, salt water, muddy water, water | 8 |
| **goods** | plating, bio mass, sulfur, bauxite, iron ore, coal, titanic ore, electricity, salts, plus art (painting, jewelry, sculpture, clothing) | 13 |
| **machines** (parts) | motor, actuator, battery, atomic battery, power cell, **AI core**, sensors, rocket motor, repair pack, generator, solar panel, wheels, tracks | 13 |
| **ammo** | bullets, threading bullets, shells, explosive/penetrating shells, rockets, incendiary rockets | 7 |

⚠ **Fifty-four rows before a single one of them does anything.**  That
is the spreadsheet § Open questions 3 warns about, written out in full
by the person who is now asking for it again — which is the most useful
form of evidence this document could have.  **The recommendation stands:
ship three, and let the rest arrive when a mechanic asks for one by
name.**

### ⚠ But four rows in it are load-bearing and should survive the cut

- **The ROCK TYPES differ, and the builder cares.**  *Granite — hard to
  cut, very sturdy.  Sand rock — brittle.  Volcanic rock — easy to
  handle.*  ⚠⚠ That is § The first cut is the expensive one given a
  terrain axis: **where you build decides what your walls cost AND what
  they are worth**, over a palette [`GROUND_TYPES.md`](GROUND_TYPES.md)
  already has.  It is the cheapest interesting material rule available
  and it needs no new resource at all — the *ground kind you dug* is the
  wall's strength, and `damage.loft::structure_max_hp` is already a
  per-kind figure scaled by bracing.
- **Iron wood** — *"almost as strong as steel but quite heavy"* — makes
  the forest biome's material competitive with the mountain's, so a
  swamp map (few trees, § The biomes) is a genuine squeeze.
- **Salt water vs muddy water vs water**, with a desalination plant
  between them.  ⚠ Relevant only because § Defensive structures wants a
  **moat**, and the coast has water that the swamp does not.
- **Sap is a FLUID, not a good** — so it needs a tank, a pipe or a trip.
  ⚠ That keeps `DESIGN.md` § Scouting's *sap invites insect chase*
  honest: a fluid cannot be picked up in passing.

### ⚠⚠ And one entry would delete the game if it shipped

> *"**Laser communicator** — allows to communicate under the influence
> of a scrambler."*  (`type:upgrade`)

⚠⚠ **Refuse it.**  [`SETTING.md`](SETTING.md) § The recruitment shows
the whole premise resting on one sentence — *"very little communication
with the surface possible during a mission, so there has to be personnel
below"* — and every servicing loop dryopea has measured exists because
of it.  An upgrade that restores comms under the jammer restores
**remote operation**, and remote operation is the thing the fiction
removed to make the game exist.

⚠ The version that is safe is the one that keeps the trip: a laser link
is **line-of-sight**, and `passable.loft::sight_first_block` is already
the walker for that.  A comms channel you must have a clear LOS to is a
*positional* asset — you build a relay, on a height, and defend it.
That passes; a general radio does not.

## Towers — the 2023 catalogue, and the two ideas in it worth taking

The 2023 data lists fourteen tower types: *auto gunner, spike trap,
puncture trap, grabber, spinner, stunner, demolition, flamer, ice
thrower, cannon, sling, barrage, scrambler, crane.*  ⚠ Most map onto
`DESIGN.md` § Damage TYPE's six axes (the scrambler is the **core**
itself, and the crane is a machine).  Two do not:

- ⚠⚠ **The traps that do not automatically reset.**  *Spike trap —
  "not automatically reset when activated."  Demolition — "a triggered
  explosion, not automatically reset."*  **This is `DESIGN.md` § What
  kind of game this is passed outright**: a trap is placed in advance,
  fires once, and then somebody has to **drive out to it, in the middle
  of a wave, to re-arm it.**  It is the tower-repair clock
  ([`plans/17`](../plans/17-tower-hot-swap/README.md) § T1) with the
  cost moved to the front, and it needs no new mechanism — a spent trap
  is a black tower that a standing vehicle restores.
  ⚠⚠ **BUILT** (BACKLOG C4, 2026-08-27,
  [`src/trap.loft`](../src/trap.loft), `@X281`), and the *no new
  mechanism* claim held: one file, one marker kind, and one extra
  count on the occupancy layer.  ⚠ **What the design did NOT predict
  is that a plate fired once is worth LESS than no plate at all**
  (`@M057`) — the mechanic is the trip back, and it is worth +106
  ticks where the blast alone is worth −3.
- ⚠ **The grabber** — *"grabbing broken or stunned opponents to salvage
  them later."*  It converts kills into salvage without a trip, which is
  the mechanic this document keeps refusing.  **The version that passes
  is a grabber that MOVES a body rather than banking it**: dragging a
  wreck off a chokepoint is exactly the counter-play
  [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Bodies are terrain asks
  for, and the player still has to come and collect.

⚠ **`upgrade` is a whole item type in the 2023 schema — 28 of them** —
and as of 2026-08-26 it is **adopted**: see
[`PROGRESSION.md`](PROGRESSION.md) § P3.  ⚠ Two still need an individual
ruling — the **laser communicator** survives only as a line-of-sight
relay (a general radio deletes `@X099`), and the **auto pilot** keeps
its authored drawback.  ⚠ Everything else answers to § P6a's fence:
**an upgrade buys friction, never answers.**

## What this design deliberately does NOT do

- **No research tree, and unlocks are FOUND rather than AWARDED.**  New
  tower types are found on the map (`DESIGN.md` § Scouting), which is a
  trip — and ⚠⚠ [`PROGRESSION.md`](PROGRESSION.md) § P0c makes that a
  *rule* rather than a preference: **a found unlock is permeable to
  knowledge**, so a returning veteran who knows where to look collects
  on day one what a first-timer stumbles on in week three.  An awarded
  unlock (*complete N sorties*) is impermeable by construction.
  ⚠ Buying a *capability* is fine under § P6a; buying your way past the
  looking is not.
- **No production simulation.**  A plant is a *converter with a time
  cost*, matching `ROBOT_ECONOMY.md` § What this design does NOT do.  No
  inventories ticking, no throughput graph.
- **No rails, no trains, no lifts — yet.**  ⚠ They are the seed notes'
  largest block and the least compatible: automated transport is
  explicitly *"maximize automatic transport of materials"*, and
  automating transport removes the trip.  ⚠⚠ **The version that could
  work is the enemy's**, where a rail is a route to cut, and
  [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § 3 already has transport routes
  doing exactly that.
- **No flyers on the player's side, and none on the enemy's without a
  plan.**  ⚠ [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md)'s rule is **ONE AI,
  per-class DATA** — a class that needs its own mover has broken it.  A
  flyer needs one, and it also deletes the wall, which is the game.  ⚠ A
  **wall-climbing spider** is the version that costs a row: `passable.loft`
  already takes the climb limit as a parameter (`can_climb`), so a spider
  is `CLIMB_REGULAR` raised — no new mover, and the wall still matters
  because height still costs the climber time.
- **No weight/volume simulation per item.**  ⚠ The seed schema carries
  `weight`, `box`, `armor`, `degeneration` per item.  Weight matters at
  exactly one place — **what fits in the rocket at scramble**
  (`DESIGN.md` § Scramble exit + cargo manifest) — and that is a cargo
  cap, not a physics model.

## Open questions

1. **Does the flat points wallet survive?**  It is built, gated, and
   quoted by seven measurements.  *Recommendation: points stay as the
   ORDER currency (towers, helpers) and materials become a second,
   **carried** resource — because points are abstract and abstract things
   do not have to be driven anywhere, while the whole value of this
   document is that materials do.*
2. **Is a material a `CarryObject` or a hex layer?**  `carry.loft` gives
   one record per object with an owner and conservation is structural;
   `height.loft` gives a rubble layer with a composition.  ⚠ Both already
   exist and a wreck is already both.  *Recommendation: the layer, with
   `salvage_at` converting — it is the mechanism `plans/15` C0.4 built
   for, and a new kind must cost nothing in the carrying path.*
3. **How many materials does a base actually need to be interesting?**
   Nine is a spreadsheet.  *Recommendation: ship the tree with **three**
   — metals, chemicals, and one class-keyed rarity — and let the rest
   arrive when a mechanic asks for them by name.*
4. **Does the recycler have a level — or does the CREW?**  The seed
   notes say a recycler *"cannot recycle higher level parts from
   destroyed enemies"*.  ⚠⚠ **Answered 2026-08-26, and by a person
   rather than a building**: [`PROGRESSION.md`](PROGRESSION.md) § P2
   gives `scrounge` — *"reuse of materials and items from broken
   constructions"* — the job of scaling what comes back out of a wreck.
   *Recommendation: the skill carries the level, so **the crew you
   brought decides what the field is worth**; a plant that recycles
   everything but takes longer is the building-side half.*

## See also

- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) — where the wrecks come from,
  and § 1a for crystal, which this file deliberately does not restate.
- [`DESIGN.md`](DESIGN.md) § 13 Economy — the points wallet this is the
  tree behind; § Damage TYPE — the weapon axes that are canon.
- [`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) § 4 — the seed notes this was
  folded from, verbatim, with the routing table.
- [`SETTING.md`](SETTING.md) § The pollen — why machines decay and why a
  repair economy exists at all.
- [`PROGRESSION.md`](PROGRESSION.md) § P3 — equipment as an axis, and
  § P6a for the fence every entry here answers to: **an upgrade buys
  friction, never answers.**  ⚠ § P2 § A skill scales a number that
  already exists is what connects this catalogue to the crew.
- [`../archive/gameplay.data`](../archive/gameplay.data) — the 2023
  filled-in catalogue (54 material rows, 14 towers, 28 upgrades), and
  [`../archive/seed-notes.md`](../archive/seed-notes.md) — the later
  seed notes.  Both preserved verbatim; both are sources, not designs.
