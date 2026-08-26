<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# The robot economy — the world the waves come out of

**Status: design, not built.**  Nothing in this file exists in code.
It is the destination [`SETTING.md`](SETTING.md) § They were on an
ERRAND points at and the replacement for
[`plans/16`](../plans/16-the-wave-system/README.md)'s authored wave
list, which the project owner has committed to removing before the
first game ships: *"I want natural patterns instead of waves/spawn
points"* (2026-08-14).

## Why this document exists

Today a wave is a number in `examples/waves.json` and a direction is a
marker somebody painted.  Both are stand-ins for a fact about the
world:

> A robot in a wave was never dispatched at the player.  It is a scout,
> harvester, builder or miner walking to a job somewhere else, whose
> route happens to cross the scrambler bubble — and which, deafened,
> turns toward the interference believing a peer machine has broken
> down.

So the honest generator of waves is **the robot economy going about its
business**.  This file designs that economy as six kinds of
installation and the routes between them, and says what each one does
to a base parked nearby.

### ⚠ The governing rule: ONE system, per-type DATA

The same rule [`DESIGN.md`](DESIGN.md) § 10 states for enemies — *one
AI, per-class data* — and for the same reason.  Six installation types
must not be six subsystems.  Each type is a **row in a table** that
contributes to a small fixed set of run parameters:

| parameter | what it decides | who writes it |
|---|---|---|
| `traffic_rate` | robots per minute crossing the bubble | routes, weighted by what they serve |
| `traffic_mix` | which of the four small roles arrive | the installations at the route's ends |
| `cargo_value` | what a kill is worth in salvage | what the route is carrying, and which way |
| `escalation_latency` | how long from "noticed" to combat bots | distance to the nearest military stockpile |
| `damage_persistence` | whether chip damage sticks or is refunded | distance to the nearest repair point |
| `insect_pressure` | the threat the scrambler does not answer | distance to the nearest carbon plant — ⚠ and, once [`SETTING.md`](SETTING.md) § Both factions have warriors is built, an ACCUMULATOR rather than a distance: the insect ramp does not un-hatch |

An installation that needs a seventh parameter, or its own movement
code, or its own spawn path, has broken this rule.  ⚠ **A new
installation type should cost one row and no new behaviour** — that is
the test, and it is what makes the set extensible without the wave
engine growing.

## The graph

**Nodes** are installations.  **Edges** are transport routes.  Material
flows along the edges; the flow is the traffic; the traffic is the
waves.

```
    carbon plant ──────┐
                       ├──▶ factory ──▶ (new robots, outbound everywhere)
    ore mine ──────────┘        │
      ▲                         ▼
      └──── repair point ◀── damaged units
                 ▲
                 │  (dormant, no traffic)
          military stockpile

    withered tree = a shaft into the crust
           │
           ▼
    CRYSTAL mine ──────▶ factory's boss-production machine ──▶ BOSSES
                                    (and one energy core each)
```

⚠ The second graph is the whole of § 1a: **crystal is the only input
with exactly one product**, so it is the only edge whose loss the
player can predict the effect of.

The player's base is **not on this graph**.  It is a scrambler bubble
dropped somewhere in the middle of it, and the only thing that matters
is which edges it happens to sit on and which nodes are within reach.

⚠ **This is the strategic layer the run shape has been missing.**
[`DESIGN.md`](DESIGN.md) § Base sequence makes a run a sequence of
bases chosen at the station hub; until now that choice has had nothing
to be *about*.  With this graph, picking the next sortie is picking a
neighbourhood — and every neighbourhood is a different game.

---

## 1. Mines — and the axis is DEPTH

**Fiction.** Extraction sites feeding the colonisation programme.  Ore
out, empty haulers in, miners resident.

⚠ **There is more than one kind** (owner, 2026-08-14), and rather than
enumerate them the design takes the one axis that generates the
differences: **how deep the mine goes.**  Depth decides yield, rarity,
traffic and — the interesting part — *where a mine can be at all*.

| depth | yields | traffic | where it can be |
|---|---|---|---|
| **surface** | bulk ore, structural metal | heavy, constant | anywhere the seam outcrops |
| **shallow** | conductors, refined feedstock | moderate | most terrain |
| **deep crust** | **crystal** | thin and precious | ⚠ **only where a withered tree left a shaft** — see § The vertical dimension |

Per the governing rule this is **one mine, per-depth data**: a row
giving yield, hauler period and resident mix.  No mine type gets its
own behaviour.

**Traffic signature.** Heavy, regular, and the least pleasant mix in
the game: **miners and harvesters**.  A mine is the one node whose
resident class is the one [`DESIGN.md`](DESIGN.md) § 10 names as the
wall-eater — *a miner cuts rock for a living and a scout does not*.

**What it does to a base nearby.**  Your perimeter is worth
dramatically less.  The measured wall clocks in
[`plans/12`](../plans/12-combat-resolution/README.md) § B7 assume a
generic robot at 1 HP/s; a mining crew is the reason that number is a
per-class row rather than a constant.

**What the player can do.**  Loaded haulers are the richest salvage in
the economy — a kill on the outbound side of a mine route pays several
times what a kill on the inbound side does.  So a base beside a mine is
a **farm you cannot fortify**.

**What it costs.**  You have to hold ground with clearing and towers
rather than with walls, which is exactly the work that cannot be done
from a parked vehicle.

**Design test** (*does this put something in the player's hands at a
moment when using it costs them something?*): ✓ — the loot is on the
outbound lane, which is the far side of the route from your core.

**Tunables:** `mine.yield_per_hauler[depth]`, `mine.resident_mix[depth]`,
`mine.route_period_s[depth]`.

### ⚠⚠ 1a. Crystal mines — the boss supply, and the best lever in the game

Owner, 2026-08-14: crystal is *"needed for the energy cores of bosses
and the production machines inside factories for them"*.

**Two consumption points, and they are not the same lever.**  This is
the whole design of crystal:

| consumer | what crystal buys | cutting it |
|---|---|---|
| a **repair platform's** energy core | one core per boss, consumed | throttles the **rate** — support bosses stop arriving while the pipe is dry |
| a **combat robot's** core **and power weapons** | ⚠ the dearest thing in the economy — two demands in one unit | throttles the **military** specifically; see below |
| a factory's **boss production machine** | capital plant, built once | removes the **capability** — that factory makes no bosses again until it is rebuilt |

⚠⚠ **The two boss kinds are opposites and crystal is what they
share** (`DESIGN.md` § There are TWO boss kinds).  The repair platform
is an economic unit repurposed; the big combat robot is the one thing
in the game that was *built to fight*, it comes from a woken military
stockpile rather than a factory line, and it needs crystal twice over.
**So crystal is the bottleneck on MILITARY capability specifically** —
which is what gives cutting it a consequence that outlives the
decision:

- **mid-game**, cutting the line is insurance against ever meeting a
  combat boss.  A good decision, made for good reasons.
- **end-game**, combat bosses are the units that can hurt an old one's
  servants — and the player who cut that line has crippled their own
  allies, hours later, by their own earlier correct play.

⚠ It needs per-planet state to express at all (§ Open questions 1),
and it is the best argument on that list.

So the player who finds the crystal line has two plays at different
prices: interdict the haulers (cheap, repeatable, temporary) or reach
the factory machine (expensive, deep in their territory, lasting).  ⚠
**Neither is available to a player who has not scouted**, which is
exactly the progression loop `DESIGN.md` § Scouting already names as
*the* one.

**⚠ Why crystal is rare is not "there is little of it".**  It is deep
crust, so it needs a shaft, and shafts only exist where a huge tree
withered (§ The vertical dimension).  **The supply is limited by
botany, not by geology** — which makes it locatable, finite, and worth
a map author's attention.

**⚠⚠ And the robots have a problem there that the player can read.**
Crystal is deep-crust stone, and [`SETTING.md`](SETTING.md) § The third
enemy makes stones and gems what **elementals** are keyed to — a gem
disturbed *awakens the matching elemental*.  A crystal mine is
therefore a site where economic robots, built to mine and not to fight,
are digging up precisely the thing that wakes tier 3.

Three consequences, none of them invented — they all fall out of lore
that was already written:

- **A crystal mine is the only node in the economy that is dangerous to
  its owners.**  Expect it defended, or abandoned, or worked in short
  desperate bursts — an authoring choice per map with a reason behind it.
- **The counter-play to tier 1's boss is tier 3.**  Taking the crystal
  yourself is taking a gem, and taking a gem wakes elementals.  So the
  act that denies the swarm its bosses is the act that wakes the enemy
  the player has no answer to.
- ⚠ **And it is what makes an old one STIR.**  `SETTING.md` § The old
  ones are a Lovecraft reference: sustained deep-crust extraction
  disturbs exactly the substrate elementals answer to, so the swarm
  supplies the *looming* — signs accumulating with no player involved.
  ⚠ **Stirring is not waking**: an old one is end-game, world-scale,
  many-players content and a solo sortie must be unable to reach it at
  all.  The robots have been digging toward it for years; players are
  what finally reaches it.
- **It is the sap rule, one tier up.**  `DESIGN.md` § Scouting's *every
  reward has its own pressure* was a list of two entries (sap → insects,
  gems → elementals); crystal makes the second entry **strategic** rather
  than merely a loot trap, because now it also does something to the
  robots.

**Design test:** ✓✓ — the strongest strategic play in the game is
gated behind waking the one enemy tier that has no counter.

⚠ **Reconciling with § 5.**  Crystal decides whether a boss *exists*;
a repair point decides whether one is *dispatched at you*.  Supply and
dispatch are different questions and must stay two parameters — a
single "boss frequency" number would hide which of the two a player's
raid actually changed.

**Tunables:** `crystal.cores_per_boss`, `crystal.machine_rebuild_s`,
`crystal.hauler_period_s`, `crystal.elemental_wake_radius_hex`.

---

## 2. Factories

**Fiction.** Ore and carbon in, robots and parts out.  The place new
units come from, and therefore the place wave *growth* comes from.

**Traffic signature.** The most mixed of any node: raw material
inbound, **fresh undamaged units outbound in every direction**.  A
factory's output is not aimed at you; you simply sit on one of the
roads leaving it.

**What it does to a base nearby.**  It is the **escalation engine**,
and it is what `waves.json`'s ascending list `[5, 8, 12, 20, 30, 50,
80]` is a hand-drawn picture of.  Wave size grows because a factory
keeps producing and the local AI keeps routing more units through a
region whose reports have stopped.

⚠ **This is the single best argument for the whole file.**  An
authored ascending list is a designer asserting that things get worse;
a factory upstream is a *reason* they get worse, which the player can
find, understand and act on.

**What the player can do.**  Throttle its **input**, not its output.
Cutting the mine→factory route starves it, and the effect arrives
*late* — the waves already in transit still come.  A delayed, indirect,
legible defence.

**What it costs.**  The trip, and the attention: a supply route worth
cutting is by definition not next to your core.  And a factory whose
inputs fail is exactly the trigger `SETTING.md` § Combat bots are
dormant lists at step 2 of the escalation ladder.  ⚠ **Starving the
factory is how you wake the military.**

**Design test:** ✓ — the strongest defensive play in the game is also
the thing that ends the mission.

**Tunables:** `factory.units_per_minute`, `factory.input_buffer_s`
(how long it keeps producing once starved), `factory.output_mix`.

---

## 3. Transport routes

**The edges, and the actual wave generator.**  Everything above and
below is scenery until a route passes near the player.

**What a route is.**  A corridor between two nodes carrying a
throughput and a cargo.  Three properties decide everything:

- **Throughput** — robots per minute → the wave rate.
- **Cargo and direction** — what a kill drops → the salvage economy.
- **Distance from the core** — whether the bubble touches it at all.

**How it becomes a wave.**  A robot walking its route enters the
scrambler bubble, goes deaf, stands for its reorientation interval
(`plans/16` W2's pre-walk window — *already built, and this is what it
was always for*), and turns toward the interference.  **A spawn marker
is a stand-in for the point where a route crosses the bubble edge**, so
in the finished model it is computed rather than painted, and its
direction is the route's bearing rather than an author's choice.

⚠ **Bubble radius becomes a real decision.**  It is 25 hexes today and
fixed.  Against a route graph it is the *aggro radius*: a bigger bubble
reaches more roads and deafens more robots.  If the scrambler ever
gains a strength setting, this is the mechanic it acts on, and turning
it down is a legitimate strategy that also weakens whatever else the
bubble does.

**What the player can do.**  **Block the route** — a wall across a
corridor makes the local traffic reroute.  Three outcomes, and the
player is choosing between them:

1. It reroutes **away** — quiet base, no salvage income.
2. It reroutes **through you** — a worse route becomes the main road.
3. It reroutes **nowhere** (a valley, a bridge) — the traffic backs up
   *against your wall*, which is a siege you built yourself.

**What it costs.**  Case 3 is the interesting one and is what
[`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § Sealing is punished, not
forbidden already describes at base scale, one level up.

**Design test:** ✓ — every blocking decision is reversible-looking and
is not.

**Tunables:** `route.throughput_per_minute`, `route.reroute_cost_hex`
(how far a detour has to be before traffic prefers to chew through).

---

## 4. Military bases

**Fiction.** Mothballed combat-bot stockpiles from the AI-vs-AI wars —
`SETTING.md` § Combat bots are dormant.  Parked, depowered, waiting for
orders from a sub-process that went quiet.

**Traffic signature.** ⚠ **None.**  This is the one node that generates
no traffic at all, and that is the point: it is invisible until it
matters.

**What it does to a base nearby.**  It sets `escalation_latency` — the
delay between the local AI deciding you are no longer ignorable and
combat units actually arriving.  A base with a stockpile over the ridge
has a **short fuse**; one in collapsed faction territory may have no
fuse at all, because there is no warm authority left to send the wake
command.

⚠ **It is a timer modifier, never a spawner.**  It does not emit
anything until woken, and when woken the units arrive *alongside* the
economic ones rather than replacing them (`SETTING.md` § Tied threads).

**What the player can do.**  Two things, opposite in kind:

- **Read it** — knowing the fuse length is what makes the scramble
  decision informed.  *"Have I pushed them past activation?"* is only
  answerable if you know how close the nearest stockpile is.
- **Loot it** — a mothballed stockpile is military-grade salvage and
  the most valuable scouting prize in the economy.

**What it costs.**  ⚠ **Scouting it is a plausible wake condition.**
The prize and the fuse are the same object, which is the cleanest
expression of `DESIGN.md` § Scouting's *every find is high-value AND
opens a fight* that the setting offers.

**Design test:** ✓✓ — this is the one that shapes the *run*, not the
base.

⚠ **It has a biological twin.**  `SETTING.md` § Both factions have
warriors gives the insects the same shape with a different mechanism —
their soldiers must be GROWN rather than woken, so their escalation is
a ramp where this one is a step, and a hive is to the forest what a
stockpile is to the swarm.  The two accumulate independently: a base
can be robot-quiet and wasp-furious.

**Tunables:** `military.wake_pressure_threshold`,
`military.wake_delay_s`, `military.stockpile_salvage_value`.

---

## 5. Local repair points

**Fiction.** Where damaged robots go to be fixed.  ⚠ **The node the
new setting material makes essential**: if the robots approaching your
core believe they are attending a malfunctioning peer, then repair is
the swarm's central behaviour and not a footnote.

**Traffic signature.** Inbound damaged units, outbound repaired ones —
so a repair point near you means **the same robots come back**.

**What it does to a base nearby.**  It sets `damage_persistence`, and
this is the most mechanically interesting parameter in the file:

- **No repair point in range** — chip damage accumulates across waves.
  A tower that wounds is nearly as good as one that kills.
- **A repair point in range** — a wounded robot that survives the bubble
  walks home, is fixed, and returns whole.  **Your damage is refunded.**

⚠ **That makes "kill" and "hurt" different verbs for the first time.**
Today they differ only in bookkeeping; against a repair point, a tower's
30-shot budget spent spreading damage across a wave achieves *nothing*,
while the same 30 shots concentrated kills ten robots for good.  It is a
targeting decision the player currently has no reason to think about.

**Second-order, and it is the best thing in this document.**  A fixed
repair point is where the mobile one comes from: the boss is *industrial
repair equipment* (`DESIGN.md` § Boss = mobile REPAIR PLATFORM).  So
**the boss arrives because the local repair point dispatched it to what
it believes is a serious breakdown — your core.**  Boss frequency
becomes a function of repair-point proximity and of how much damage you
have been doing, rather than an authored phase-3 event.

⚠ **Dispatch, not supply.**  Whether a boss EXISTS to send is § 1a's
crystal question; whether one is sent AT YOU is this one.  Keep them
two parameters — collapsed into a single "boss frequency" they would
hide which of the two a player's raid actually changed, and those raids
are at opposite ends of the map.

**What the player can do.**  Deny it — a raid that makes your damage
start sticking.

**What it costs.**  A long trip, to the one place in the economy where
damaged robots congregate, and there is nothing to farm there: repair
traffic carries no cargo.

**Design test:** ✓ — it changes what you shoot at, every wave, for free,
and the counter-play costs a sortie.

**Tunables:** `repair.range_hex`, `repair.turnaround_s`,
`repair.platform_dispatch_threshold`.

---

## 6. Carbon-gathering points (plants)

**Fiction.** The terraforming half of the colonisation programme —
growth stands cultivated and harvested for carbon.  The huge trees
`DESIGN.md` § Scouting already hangs sap on.

**Traffic signature.** Thin, slow, low-value: harvesters on long
cycles.  As a *robot* neighbour, a carbon plant is the quiet quarter.

**What it does to a base nearby.**  ⚠ **It is not a robot problem at
all.**  Organic growth is where the **insect tier** lives, and
`SETTING.md` § Why waves happen is explicit that *insects ignore the
scrambler — they are biological, with no comms link to disrupt*.

**So a carbon plant is the one neighbourhood where your core's defining
mechanic does not work.**  The scrambler does not deafen them, the
bubble does not steer them, and the pre-walk window does not exist for
them — they simply come, by smell, when you take the sap.

⚠ **This is the most valuable design property of the six**, because it
is the only one that makes the player's central asset irrelevant, and a
strategy game needs at least one board where the usual answer is not
available.  It is also already half-built: dryopea has an insect class
with its own climb limit, and `two-classes-two-routes.keys` measures it
crossing what a robot must walk around.

**What the player can do.**  Harvest the sap — high value, and
`DESIGN.md` § Scouting already prices it: *sap invites insect chase by
smell*.

**What it costs.**  Everything you know about defending against robots.

**Design test:** ✓✓ — the reward and the threat are the same action.

**Tunables:** `plant.sap_value`, `plant.insect_draw_radius_hex`,
`plant.insect_period_s`.

---

## ⚠⚠ The vertical dimension — trees are the way down

Owner, 2026-08-14: *"there is a tie in with the trees on the planet as
those can grow deep into the crust of the planet leaving path into it
after they wither eventually."*

**A huge tree is not scenery and not only a sap dispenser.  It is a
drill.**  It grows roots down through the crust over a very long life;
when it eventually withers, the root channel is left behind — an open
shaft into rock nothing else could have opened.

### The life-cycle, and what each stage is worth

| stage | what it is | who wants it | which tier it brings |
|---|---|---|---|
| **living** | sap — energy source and life-prolonging medicine ([`SETTING.md`](SETTING.md) § The second enemy) | insects harvest it; humans go to a cordoned planet for it | **tier 2** — taking sap invites insect chase by smell |
| **withering** | both, briefly | everyone | both |
| **withered** | a **shaft** into the deep crust | robots, for crystal | **tier 3** — what the shaft reaches is elemental country |

⚠ **So a tree is a resource that CHANGES KIND**, and the two kinds
attract different enemies.  That is a genuinely unusual property for a
map fixture and it is worth protecting in the design: nothing else in
dryopea turns into something else.

### What follows from it

**1. Crystal mines are located at dead trees.**  Not near them —
*at* them.  The mine is the shaft, and the shaft is the tree.

⚠ **And a shaft is a BRAID, so a crystal mine has a FOOTPRINT.**  The
growth form is rhododendron-like — a tangle of thick sinuous limbs with
no single trunk, the same form above ground and below
([`SETTING.md`](SETTING.md) § A tree is also a drill) — so what a dead
one leaves is a network of interwoven channels over an area rather than
one hole.  A crystal site is therefore *a place*: several ways down for
the robots, several ways up for whatever is down there, and enough
ground for a map author to shape an encounter on.  ⚠ It also means a
player cannot plug it: denial at a crystal mine is interdiction of the
haulers, never corking the hole.  So a
crystal site is a place with a **history**: there was a huge tree
here, which means insects worked it, which means there are probably
living trees nearby still.  A crystal neighbourhood is therefore the
one place all three enemy tiers overlap — robots hauling, insects on
the neighbouring stands, elementals below — and dryopea gets its
hardest map type **derived rather than declared**.

**2. It explains the scarcity honestly.**  § 1a: the crystal supply is
bounded by how many huge trees have died in a region, which a map
author controls by placing trees rather than by tuning a rate.

⚠⚠ **And the swarm is farming that supply without knowing it.**
[`SETTING.md`](SETTING.md) § And it closes the tree life-cycle loop:
insects guard tree wounds until they heal, robots clearing terrain kill
trees and drive off what lives in them, an unguarded tree withers
sooner, and a withered tree is a crystal shaft.  **The robot economy
destroys the forest and is paid in crystal for it.**  For the player
that is a lever with a long handle: protecting a stand sides with the
insects against the swarm's boss supply, and clearing one is the
fastest way to make new crystal sites.

**3. The shaft is TWO-WAY, and that is the interesting half.**  A path
into the crust is a path *out* of it:

- **Elementals** — `SETTING.md` says earth elementals answer to stone.
  A shaft is where the stone is, and the most natural explanation of
  how tier 3 reaches a surface map at all.
- **Insects** — their hives *"presumably exist somewhere in the wilds"*
  and have never been placed.  A root system is a candidate.
- ⚠ **The underground humans.**  `SETTING.md` § Future contact needs a
  physical route for people who *"can't easily come up to the
  surface"* and who nonetheless *"notice things"*.  A tree shaft is
  one, and it is a route the fiction produced rather than one invented
  to solve the problem — which is the test that section's own
  no-shortcut rule sets.  **Recorded as a candidate, not a decision**;
  it belongs to Tier E and to the owner.

**4. The robots did not make the hole, which is why they are exposed
there.**  They are opportunists at a crystal site — working a shaft
they could not have dug, in a place that wakes something they were not
built to fight.

### ⚠ What this deliberately does NOT design

⚠⚠ **CHALLENGED 2026-08-26, and the refusal HOLDS for the campaign**
(`@X184`).  `DESIGN.md` § 20 records an owner idea for a **territorial
competitive mode** built on hackable underground installations — and it
is allowed to want a second movement model **precisely because it is
separate from the campaign** (`@X181`, `@X182`).  ⚠ The refusal below is
unchanged *here*; the cheaper version worth trying first is
installations with **entrances on the surface map**, which is
territorial without descending.

**No underground level.**  A shaft is a *place on the surface map* that
material and creatures come out of.  ⚠ **Caverns do not change this**:
[`DESIGN.md`](DESIGN.md) § Trees as terrain treats a cavern as a HOLE
in the surface map — a non-walkable kind, the way sea already is — that
the limbs span.  The underground gets geography without becoming a
level.  Whether the player can descend is
a different game — a second movement model, a second lighting model, a
second everything — and nothing above needs it.  If it ever happens it
should be because the underground earned its own plan, not because a
hole was drawn.

**No tree simulation.**  Withering is an authoring state, not a clock
ticking during a mission.  ⚠ A *between-missions* clock is a real
possibility (a run returning to a region and finding a tree has died,
opening a crystal site that was not there before) and it is the
cheapest version of a living world this design could ever get — but it
belongs with per-planet persistence (§ Open questions 1), not here.

## How the player ever learns any of this

Three layers, in the order a player meets them:

1. **The sortie brief** (station hub, `ROADMAP` Tier D) — the
   neighbourhood at low resolution: *"heavy mining traffic, no known
   stockpile"*.  This is what makes base selection a decision.
2. **Scouting** — driving out to find the nodes themselves, which
   `DESIGN.md` § Scouting already establishes as *the* progression
   activity.  A found node is intel that persists.
3. **The waves themselves** — composition is a readout.  Miners at the
   wall means a mine upwind; the same robot arriving twice means a
   repair point.  ⚠ **This is diegetic in the way § No wave HUD
   demands**: the player learns the map's economy by watching who turns
   up, with no UI at all.
   ⚠⚠ **BLOCKED, and by one movement rule rather than by anything
   here** (plan 23 K3, `@M018`).  A convoy is a MIX, and a mixed wave
   currently behaves like a pure wave of its fastest member: only three
   hexes of a wall are ever attacked, and the quickest four robots hold
   all of them while the rest stand in a field.  So *"miners at the
   wall"* is not what a convoy of miners screened by scouts produces —
   what reaches the wall is scouts, and the readout says *scouts*.
   ⚠ The fix is the equal-distance sidestep
   ([`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) § The siege front is three
   hexes wide), and it is a prerequisite for this whole intel layer
   rather than a polish item on it.

## ⚠⚠ The whole graph inverts in the end game

`DESIGN.md` § The end game, and why it is still this game: when an old
one wakes, robots and insects become **co-belligerents** — they can
attack it and humans cannot.  Owner, 2026-08-14: the player *"will
need to aid the robots in their war economy instead of the mid-game
hampering of the robots."*

**Every lever above inverts its sign.  Not one changes shape.**

| node | the mid-game play | the end-game play |
|---|---|---|
| **ore mines** | farm the outbound haulers | ⚠ **protect** them — that metal is the war economy |
| **crystal** | cut the line to deny bosses | ⚠⚠ **keep it flowing** — a boss is a mobile repair platform, and now it repairs *your side* |
| **the factory's boss machine** | destroy it to remove the capability | **defend it** — it is the only thing that makes heavy units |
| **factories** | starve the inputs | **feed** them |
| **transport routes** | block, or interdict for salvage | **hold open**, and clear what blocks them |
| **repair points** | raid it so your chip damage sticks | **garrison** it — it is what puts your allies back on their feet |
| **military stockpiles** | ⚠ the thing to avoid waking | ⚠⚠ **wake it deliberately** — and it is where the **big combat robots** come from, the only units ever built to fight |
| **carbon plants** | the insect-pressure hazard | still the insects' ground — and the insects are on your side now |

⚠⚠ **The boss is the sharpest one.**  The mid-game's most dangerous
enemy is a *repair platform*; in the end game it is the support unit
you are trying to keep supplied.  Nothing about it changes — the
player's relationship to it does.

⚠ **And the escalation ladder inverts with everything else.**  All
mid-game long, pushing the local AI toward its dormant military is the
mistake you avoid (`SETTING.md` § Combat bots are dormant).  In the end
game **it is the objective**: you want that wake command sent.

⚠ **So none of this is throwaway content.**  A player's mental model of
the economy is *re-used* at the end rather than discarded, which is the
cheapest possible way to make an end game feel different — and it is a
strong argument for building the graph to be **readable**, since it has
to be read twice and mean opposite things.

⚠⚠ **And it is what "build strong bases" means at the end**: your base
stops defending your core and starts defending **their road**.  Same
walls, same towers, same crew, different thing behind them.

## ⚠⚠ THE SPREADSHEET TEST — the economy must not be solvable, even with the source  `@X200`

Owner, 2026-08-26:

> *"the same rule for skills of a helper should also apply for the
> economy.  It should be influenced by the players but I do not want to
> give them enough information to write their own spreadsheets **even if
> they know the source code of the game**."*

⚠⚠ **That last clause sets the bar, and it is much higher than *do not
show the numbers*.**  This repo is public and the rules are readable, so
**any defence made of secrecy has already failed.**  The economy has to
resist optimisation **by its shape**, not by its presentation.

> ⚠⚠ **The test: can a player write down a single number to maximise?**
> If yes, they will — and the design has failed.

### ⚠⚠ Two instinctive answers that DO NOT work

| ❌ | why it fails |
|---|---|
| **hide the numbers** | source is readable, and even closed games are datamined and measured.  ⚠ It also punishes the honest player and rewards the one with a wiki |
| ⚠⚠ **make it random** | **a spreadsheet just computes the expected value** — randomness alone makes the optimum *easier* to state, not harder, because it replaces a hard question with an average.  ⚠ It is the instinctive answer and it is the wrong one |

### ⚠⚠ What actually defends it — four, and the first is the rule being transferred

**1. NO SCALAR OBJECTIVE.**  ⚠⚠ *A spreadsheet needs something to
maximise.*  This is `DESIGN.md` § 9 § ASSIGNMENT IS A PILLAR's `@X198`
moved one system over: if the economy pays in things that **cannot be
ranked against each other**, there is no objective function to write
down.

⚠ **And this design is already built that way** — `DESIGN.md` § Scouting
states it outright: *every reward has its own pressure.*

| the gain | the pressure it carries |
|---|---|
| **crystal** — the highest value in the economy (§ 1a) | ⚠ it **wakes elementals** |
| **cutting a transport route** (§ 3) | ⚠ it **reroutes through you** |
| **starving a factory** (§ 2) | denied throughput is what **wakes the military** (§ Open questions 4) |
| **sap** | invites an **insect chase by smell** |

⚠⚠ **Value, risk, time and attention are four currencies with no exchange
rate**, so *"how much crystal should I take"* has no computable answer —
only a positional and temporal one.

**2. THE WORLD REACTS.**  ⚠ § 3 already reroutes traffic when a route is
cut, so **a computed optimum invalidates itself the moment it is acted
on.**  A spreadsheet describes a fixed system; this one moves in response
to being used.

**3. THE INSTANCE IS NOT THE RULES.**  ⚠⚠ `@X122` — a player may know
every rule and still not know **this** map: where the mine is, which tree
withered, what is in the wave.  ⚠ So a spreadsheet over the rules tells
you *what to look for*, never *what is there* — and looking is the trip
(§ How the player ever learns any of this).

**4. THE DECISIONS ARE WHERE AND WHEN, NOT HOW MUCH.**  ⚠ A spreadsheet
is good at quantities and bad at *should I drive there now, with a wave
inbound*.  ⚠⚠ Keeping the economy's decisions **positional and temporal**
is what keeps them off the page.

### ⚠⚠ Honest check: *"a static graph plus a rate per edge"* is spreadsheet-shaped

⚠ § What this design does NOT do commits to exactly that — and **a static
graph with rates is the most computable object there is.**  It has to be
said rather than smoothed over.

⚠⚠ **What saves it is that the graph is not what the player optimises.**
The rates are legible; **the pressures attached to each reward are not
commensurable**, the graph reacts, and the map is unknown until scouted.
⚠ So the simplicity of the model is fine — *the economy is easy to
understand and hard to solve*, which is the right way round.

⚠ **The thing to watch**, and it is a real risk as this fills in: **do
not add a resource that is purely good.**  One reward with no pressure
attached is a scalar, and a scalar is a spreadsheet's foothold.
⚠⚠ Every new node, route or material must arrive with **what it costs
you to take it**, or it hands the player exactly what this rule is
refusing.

## What this design does NOT do

- **No economy simulation.**  Nodes do not need inventories, production
  chains or an internal tick.  Everything above is a static graph plus
  a rate per edge; the player's actions perturb the rates.  ⚠ A
  simulated economy is a strictly larger project and none of the design
  value here needs it.
- **No AI strategy.**  Nothing in the graph reacts intelligently.  The
  escalation ladder is a threshold, and rerouting is a shortest-path
  recompute.
- **No new mover.**  Robots on a route are the enemies dryopea already
  has, walking their heading, becoming a wave when the bubble deafens
  them — `spawn.loft`'s approach mode, unchanged.
- **No replacement for `plans/16` yet.**  That plan's schedule and
  triggers stay as scaffolding until this ships; see its § Status.

## Open questions

1. ⚠⚠ **SETTLED 2026-08-26 — per-PLANET** (`@X177`, `DESIGN.md` § 20).
   `DESIGN.md`'s multiplayer makes the default PvP a **race for
   resources**, and *"you cannot compete for a mine that only exists
   inside your own instance"* decides it.  ⚠ The recommendation below is
   now the **migration path** rather than an open choice.  Original
   question, kept for its reasoning: per-map is cheap and
   makes each base self-contained; per-planet means cutting a route in
   one sortie changes the next one, which is what would make a *run*
   feel like a campaign.  *Recommendation: author per-map, but keep the
   node identifiers global so the per-planet version is a later join
   rather than a rewrite.*  ⚠ § The vertical dimension raises the stakes
   on this one: a between-missions tree-withering clock would open
   crystal sites that were not there last sortie, which is the cheapest
   living world this design could buy — and it needs per-planet state.
   ⚠ **The stronger argument is escalation**: `SETTING.md` § Both
   factions have warriors makes insect pressure a RAMP that does not
   un-hatch, which is meaningless unless the next sortie can meet it.*
2. **Is the bubble radius a player setting?**  § Transport routes makes
   it the aggro radius.  A dial is a real decision; it is also a way to
   turn the game off.  *Recommendation: leave it fixed until a base
   exists that is unplayable at 25.*
3. **Does blocking a route need pathfinding over the graph?**  Case 2
   (*it reroutes through you*) requires knowing where traffic would go
   instead.  *Recommendation: author two or three named alternates per
   route rather than computing detours — the player needs the outcome
   to be learnable, and a shortest-path recompute is not.*
4. **What wakes the military, exactly?**  `SETTING.md` gives four
   ladder steps and no numbers.  Kills? Elapsed time? Throughput
   denied?  *Recommendation: denied throughput, because it is the one
   the player controls deliberately and the one that makes § Factories'
   starve play cost something.*

5. **Can the player carry crystal out?**  It is the highest-value thing
   in the economy and `DESIGN.md` § Scramble exit already has a cargo
   manifest.  If yes, crystal becomes the run's currency and the
   elemental wake is its price; if no, it is purely a denial target.
   *Recommendation: yes — a reward the player can only destroy and
   never take is a weaker decision, and the elemental consequence is
   already the cost.*

## See also

- [`SETTING.md`](SETTING.md) § Why waves happen, § They were on an
  ERRAND, § Combat bots are dormant — the fiction this implements.
- [`DESIGN.md`](DESIGN.md) § 6 Spawn system + waves (what it replaces),
  § Scouting (how it is discovered), § Base sequence (where the choice
  lands).
- [`plans/16`](../plans/16-the-wave-system/README.md) — the scaffolding
  this retires.
- [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) — the mover, unchanged by
  any of it.
