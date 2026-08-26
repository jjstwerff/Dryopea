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

### ⚠⚠ The HELPERS and the ECONOMY need different answers  `@X203`

Owner, 2026-08-26: *"to the helpers it is more about hiding numbers, but
inside the economy we just define individual NPC traders…"*

⚠⚠ **Hiding numbers is the RIGHT answer for helpers and the WRONG one for
the economy**, because the two are defending different things:

| | ⚠ **helper skills** | ⚠⚠ **the economy** |
|---|---|---|
| what hiding buys | ⚠⚠ **character** — you read a person through what they say and do ([`PROGRESSION.md`](PROGRESSION.md) § P2h), not off a sheet | it would buy **unsolvability**, and it cannot |
| does it hold against a source-reader? | ⚠ **it does not need to** — knowing *"repair 34"* still leaves `@X198`'s four incommensurable axes, so the decision is not solved by the number | ⚠⚠ **no** — and this is the case the owner's clause was about |
| verdict | **hide them, for feel** | **hiding is not a defence; shape is** |

⚠ So `@X201` below applies to the **economy**.  A helper's stat sheet
stays off the screen for the reason § P2c gives — the roster is legible
through behaviour, and a character screen would replace people with rows.

### ⚠ And the one instinctive answer that DOES NOT work anywhere

| ❌ | why it fails |
|---|---|
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

### ⚠⚠ The mechanism: individual NPC TRADERS, on their own daily business  `@X204`

Owner, 2026-08-26:

> *"inside the economy we just define individual NPC traders that do not
> really adhere to the general trend but to their daily activities.  So
> there is a general trend but it is hidden enough."*

⚠⚠ **This is how the previous section's requirement is actually met, and
it is NOT the randomness that was just refused.**  The difference is
exact:

| ❌ randomness | ✅ individual traders |
|---|---|
| a distribution you **sample** | ⚠ **a person you VISIT** |
| a spreadsheet averages it | ⚠⚠ there is nothing to average — you are not sampling, you are going to **one** of them |
| noise with no cause | ⚠ **what that trader happens to be doing today**, which has a reason |

⚠⚠ **So the variation becomes POSITIONAL and TEMPORAL rather than
statistical**, which is § What actually defends it's fourth support made
concrete: *the decisions are where and when, not how much.*

#### ⚠ It reconciles with `@X162`'s stable prices — the trend is the stable part

[`PROGRESSION.md`](PROGRESSION.md) § P2i requires prices **stable and
knowable**, so a sortie can be planned around what is worth taking, and
refuses a price that **fluctuates on entry** as a refresh button on the
economy.

⚠⚠ **Both hold, and the split is what makes them hold:**

- **the general TREND is stable and learnable** → you can plan a sortie
  on it, and player action moves it (*"it should be influenced by the
  players"*);
- **an individual trader deviates** → you cannot compute the exact
  outcome, ⚠ and it is **not a re-roll**, because you cannot refresh a
  person.  Going to a different one is **a trip**.

> ⚠⚠ **You can plan on the trend; you cannot compute the result.**  That
> is the owner's requirement stated in one line.

#### ⚠⚠ And the traders are the ECONOMY's partial sensors

⚠ The player learns the trend **through** traders, imperfectly, over
time — which is exactly [`PROGRESSION.md`](PROGRESSION.md) § P2c's crew
pattern (`@X129`) applied to a system instead of a place: **honest,
individual, partial reports that the player has to assemble.**

⚠⚠ **The design keeps reusing one shape** — *learn a system through
partial honest reports* — and that is worth noticing rather than
re-deriving each time.  It is also what makes § P2i's **trader role**
political (`@X165`): if prices come from **people**, then dealing is
about *those people*, and the politics has a face.

#### ⚠⚠ TRUST per trader — which is what stops *shop around* being the answer  `@X205`

Owner, 2026-08-26:

> *"switching traders can be done by players, but the level of trust for
> individual traders has an influence on their pricing, so that becomes
> also a hard decision."*

⚠⚠ **Without this, the whole trader design has an obvious solution:
visit them all and take the best price.**  That is a shopping algorithm —
computable, mechanical, and it turns § The mechanism's *positional*
variation straight back into a solvable search.

⚠⚠ **Trust is what closes it**: switching costs you accumulated standing,
so

> ⚠⚠ **the question stops being *where is the best price* and becomes
> *who am I doing business with*.**  A search becomes a **commitment**,
> and a commitment is a decision.

#### ⚠ It is the same LEDGER as the crew's, one system over

⚠ [`PROGRESSION.md`](PROGRESSION.md) § P2e (`@X131`) already refuses an
affinity bar and records **what actually happened** instead.  Trust here
is the same: how much you have traded, whether you came back, whether you
dealt straight.

⚠⚠ **So no new mechanism, and no number on screen** — trust reads out of
events the player remembers: *"I have sold to her for six sorties."*

⚠ And it makes the market **political from both sides**: `@X165` gives a
**trader-player**'s lever as *who they deal with*, and this gives the
**sortie-player** the same lever pointed back.  Symmetric, and it is why
the market is a social space rather than a shop.

#### ⚠⚠ The hard part is a NOW-versus-LATER trade

⚠ The tension the owner names is exact: **the best price today may be
elsewhere; the best price over the campaign is here.**  ⚠⚠ Immediate
value against accumulated standing — two currencies with no exchange
rate, which is § What actually defends it's first point arriving again.

#### ⚠⚠ Two hazards, and the first would kill the decision outright

| hazard | ⚠ what it needs |
|---|---|
| ⚠⚠ **loyalty becomes automatically optimal** — if trust only ever accumulates, the play is *pick one early, never switch*, which is a **dominant strategy** and the decision dies after the first sortie | ⚠⚠ **traders must SPECIALISE** — one deals metals, another chemicals, another is simply somewhere else.  So your trusted metals buyer is **no help when you come back with sap**, and the switching question is live every time your cargo changes.  ⚠ Same shape as helper specialisation (`@X196`), one system over |
| **trust everywhere** — build standing with all of them slowly and the decision dies again | *Recommendation: make trust accrue slowly enough that a campaign only deepens two or three.*  ⚠ The harder version — dealing with one **costs** standing with another — is more political and more frustrating; keep it in reserve rather than shipping it |

#### ⚠ Related to `@X169`'s reputation, and probably not the same thing

⚠ [`PROGRESSION.md`](PROGRESSION.md) § P2e gives a player who reliably
returns other people's stranded crew a **reputation**.  ⚠⚠ That is
**general standing** (how the station sees you); trust here is
**per-person** (a ledger with one trader).  Both exist, they are
genuinely different, and they should probably **feed each other** — a
known-straight dealer starts a little higher with a new trader.

⚠ *Recorded as a connection rather than a decision*, because collapsing
them into one *social score* is exactly the affinity bar `@X131` refuses.

#### ⚠ Honest limit: this is *not worth computing*, not *uncomputable*

⚠ A determined player observing many traders over many sessions **can**
build a model.  ⚠⚠ What stops it paying off is everything else: the
trend **moves with player action**, the map instance varies (`@X122`),
and the payoff is still **incommensurable** (§ What actually defends it,
point 1) — so a perfect price model still does not tell you whether to
take the crystal.

⚠⚠ **AND THE STRONGEST MECHANISM IS ELSEWHERE**: `DESIGN.md` § 20
§ WORLD EVENTS (`@X218`) — ***the state stores the past and never the
future, content carries conditions and never a schedule, and the engine
discards the join.***  So *reading the content tells you what exists,
reading your save tells you what you did, and **neither tells you what
you have already qualified for***.  ⚠ That is the open-source-safe answer
this section was reaching for, taken from `../crew_punk/BLOCKS.md`.

⚠ **State it that way rather than overclaiming.**  The goal is *the
spreadsheet is not worth writing*, which is achievable; *the spreadsheet
is impossible* is not, and a design that claimed it would be wrong the
first time somebody tried.

### ⚠⚠ AND THE CAPSTONE: FACTIONS, and the relations BETWEEN them  `@X207`

Owner, 2026-08-26:

> *"that should be enough to get the players out of their calculations
> and into the game.  Add to that the factions and relations between
> those inside and outside the station and you have a **living ecosystem
> instead**."*

⚠⚠ **The word doing the work is *instead*.**  A **system** is something a
player solves; an **ecosystem** is something they live in — and that is
the thesis every rule above has been serving.

#### ⚠⚠ What it adds: a WEB, where today there is a HUB

⚠ Everything so far is **player ↔ X**: the player's trust with a trader
(`@X205`), their standing after returning somebody's crew (`@X169`),
their pressure on the robot economy.  ⚠⚠ **Factions add X ↔ Y** — and
the player becomes **one node among many rather than the centre.**

| | ⚠ **inside the station** | ⚠ **outside, on the planet** |
|---|---|---|
| who | traders, officials of the cordon, competitors' agents, the co-op, other players | the several **AIs** (rivalrous, personality-shaped), the natives, the insects, the elementals, other operators' bases |
| the source | [`SETTING.md`](SETTING.md) § The quarantine, § The competitors, § The recruitment | § History, § The settlers today, § The other enemy, § The third enemy |
| ⚠ already named | the 2023 data's nine: **spacers, economy, natives, shaman, robots, world, oceanic, ancients, aliens** | — |

⚠⚠ **Nothing here needs inventing.**  The factions exist, their tensions
exist, and what is missing is only that **they do not yet have relations
with each other.**

#### ⚠⚠ Why it finishes the spreadsheet argument

⚠ Trust (`@X205`) made a price depend on **who you deal with**.  Factions
make that choice have **second-order consequences the player cannot
enumerate**: dealing with one trader does not only raise that trader —
it moves you with their rival, and with their rival's ally.

⚠⚠ **A spreadsheet would now need the whole web AND its dynamics**, and
the same three defences still apply to it: the relations **move**, the
payoffs stay **incommensurable**, and the state is only visible through
**partial reports**.

#### ⚠⚠ So there is NO FACTION STANDINGS SCREEN

⚠ That would be `@X131`'s affinity bar at ecosystem scale, and `@X156`
refuses the UI besides.

⚠⚠ **You learn where you stand by how people TREAT you** — a price that
moved, a trader who is cool, a crew member who mentions what they heard.
⚠ That is the **partial-sensor pattern for the third time** (the crew for
the world, traders for the market, factions for the ecosystem), and by
now it is a house pattern rather than a coincidence: **learn a system
through honest, individual, incomplete reports.**

#### ⚠ Two constraints, and the first keeps a standing refusal intact  `@X208`

| | |
|---|---|
| ⚠⚠ **reactive, never STRATEGIC** | § What this design does NOT do refuses *AI strategy*: *"nothing in the graph reacts intelligently — the escalation ladder is a threshold, and rerouting is a shortest-path recompute."*  ⚠ Faction relations must respond the same way: **to what the player did**, by threshold and tally, **never by planning against them.**  Reactive is not strategic, and the distinction is this document's own |
| ⚠ **sparse and AUTHORED, never a matrix** | nine factions is 36 pairs, and most of them never touch.  ⚠⚠ Author **a handful of live tensions**, the way § Open questions 3 already prefers *two or three named alternates per route* over a computed detour — because the player needs the web to be **learnable**, and a full matrix is not |

#### ⚠⚠ THE LESSON THE PLAYER MUST LEARN: a good deal for the TRADER is an investment  `@X210`

Owner, 2026-08-26:

> *"it should become clear to a player that giving a trader a good deal
> hurts in the short term but can mean survival in the long term struggle
> against potential other players in the political landscape of the
> station.  There is no PvP here, but also a shared resources system where
> players can win."*

⚠⚠ **This inverts the usual trading game.**  Extracting maximum value per
transaction is the obvious play and it is **short-sighted** — and that is
a lesson the player learns by *playing*, which is `@X117`'s dominant axis
doing its job at the station.

⚠ **The asymmetry is what makes it a real decision:**

| ⚠⚠ **the cost is VISIBLE** | ⚠⚠ **the payoff is NOT** |
|---|---|
| you can see the worse price you accepted | standing is never a number on screen (`@X207`) |
| it is felt **this sortie** | it arrives as **being served when supply is tight** |

⚠ **That is the design's own test at the social layer** — *something put
in the player's hands at a moment when using it costs them something* —
and here the thing is **a margin you chose not to take**.

#### ⚠⚠ How it becomes CLEAR without a tutorial or a standings screen

⚠ `@X137` allows no tutorial and `@X207` allows no standings screen, so
the lesson has to arrive the way every other one does — **through
consequence, and through partial reports**:

- the trader you dealt well with is the one **who has stock for you when
  it is scarce**, or who holds something back, or who does not sell it to
  your rival;
- somebody **mentions** that another operator has been buying up what you
  need (traders and crew as sensors, `@X129` / `@X204`);
- ⚠⚠ and the first time a rival gets the supply you were counting on,
  **you understand** — which is `@X174`'s *it teaches* arriving in the
  station instead of on the planet.

#### ⚠⚠ *"No PvP, but a shared resource system where players can win"*  `@X211`

⚠ **The station is competitive and non-violent** — which is `@X175`
(*a base is impregnable to a person*) applied one layer up: **the
competition is entirely social and economic.**

⚠ What is shared and genuinely scarce:

| the resource | why it is contested |
|---|---|
| a trader's **stock** | finite — if a rival bought it, it is gone |
| a trader's **priority** | who gets served first when supply is tight |
| ⚠⚠ **permits** | the chokepoint of the whole fiction ([`SETTING.md`](SETTING.md) § The quarantine) |
| **standing with a particular trader** | ⚠ partly positional — *the* trusted operator is a place only one can occupy |
| **regions of the planet** | `@X177`'s race, per-planet |

#### ⚠⚠ *Winning* is BEING A BIGGER PART OF THE ECOSYSTEM — like winning in a democracy  `@X212`

Owner, 2026-08-26:

> *"winning here is to be a bigger part of the station ecosystem.  Not a
> fixed thing — like winning in a democracy is never a fixed
> condition."*

⚠⚠ **That is a better model than *a series of contests you win*, and it
is exact**: § The capstone says factions make the player **one node among
many rather than the centre**, so

> ⚠⚠ **winning is CENTRALITY IN THE WEB, not victory over anybody.**

⚠ The analogy carries all the way, and each part of it is a design rule:

| in a democracy | ⚠ here |
|---|---|
| you can win an election; you never win *democracy* | you can win a supply or a permit; there is **no state where the station is finished** |
| influence is a **degree**, not a binary | standing is continuous, and nobody is *in* or *out* |
| ⚠⚠ it must be **MAINTAINED** — neglect loses it | ⚠⚠ **station standing DECAYS**, and that is the same rule the whole game runs on |
| your position moves when **others** act | ⚠ you can become less central having done nothing wrong, because somebody else did something right |

#### ⚠⚠ Decay is not a new mechanic — it is this game's signature applied socially

⚠ `DESIGN.md` § What kind of game this is is **entirely** about advantages
that must be maintained in person: a tower's charge decays per shot,
salvage decays, bodies pile back into the kill zone.

⚠⚠ **Standing decaying is that rule at the social layer**, and it is a
*better* answer to § TRUST per trader's second hazard than the one
recorded there: ⚠ *trust everywhere* is prevented not by making trust
slow to accrue, but by **maintenance costing trips and deals** — you
cannot hold every relationship at once because keeping them is work.

⚠ **And it preserves `DESIGN.md` § 14 exactly**: there is no fail screen
and no win state, a run ends when the player stops, and a bad run is one
with meagre carryover.  ⚠⚠ *More central* and *less central* are the only
readings, and **neither is an ending.**

⚠ **No rank, no leaderboard, no score.**  A democracy shows you no
number either — ⚠⚠ **you know your influence by what happens when you ask
for something**, which is `@X207`'s *you learn where you stand by how
people treat you*, said again.

#### ⚠ Which also means losing is never elimination

⚠ A supply lost is a **harder next sortie**, not a defeat — and § The
guard below is why: the planet is the primary tap, so a player who is
peripheral at the station still has the whole game.

#### ⚠⚠ The guard that keeps it from locking anybody out

⚠ The obvious failure of a zero-sum social economy is that an established
player **starves out** a new one.

⚠⚠ **What prevents it is already structural: the planet is the primary
tap.**  Salvage is produced by sorties and by nothing else (`@X166`), so
a player denied at the station can always **go and get more** — the
station is a **multiplier, never the only source.**

⚠ *That is the boundary to hold when pricing any of this*: station
standing may make a sortie's haul worth more; it must never become a
substitute for going down, or the game inverts exactly as § The market
must not become a better way to play the sortie game already warns.

⚠ **And it degrades to single player unchanged**: the rivals are
[`SETTING.md`](SETTING.md) § The competitors, which `@X170` already uses
for the same purpose one system over.

#### ⚠⚠ A LIGHT POLITICAL SYSTEM — centrality converts into INVESTMENT  `@X213`

Owner, 2026-08-26:

> *"eventually I want a light political system in the station where
> players can influence where bigger investments are done, perhaps even
> building a second station."*

⚠⚠ **This is what makes `@X212`'s centrality MATTER.**  Without it,
being more central is a feeling; with it, **centrality is a lever over
where shared money goes** — a new landing corridor, a refinery, a repair
dock, an expanded permit quota, or **a second station** as the extreme
case.

⚠ **It is genuinely political because the investment is COLLECTIVE**:
nobody can do it alone, the outcome affects everybody, and ⚠⚠ **players
with holdings in different regions want it in different places** — a
real conflict of interest **with no bad actors in it.**

#### ⚠⚠ *"Light"* means it has NO SEPARATE INTERFACE — your play IS your vote

⚠ `@X156` refuses question lists and `@X207` refuses a standings screen,
so a parliament UI is out on both counts.

⚠⚠ **The version that fits: you do not cast a ballot — your accumulated
position IS the input.**  Your trade, your standing with the traders who
matter, the deals you took worse terms on (`@X210`).  ⚠ **The political
system has no interface because participating in it is what you were
already doing**, and that is what makes it light rather than a second
game.

⚠ It also closes `@X210`'s loop: *a good deal for the trader hurts now*
finally has a visible long-term payoff — **influence over where the next
investment lands.**

#### ⚠⚠ INFORMATION has value — for NPCs, never between players  `@X214`

Owner, 2026-08-26:

> *"in a campaign information is not traded between players, but
> certainly has value for NPCs.  The general knowledge that there are
> humans on the planet, that the robots are not haywire but steered, and
> the city underground and the 'art' inside that city are all pieces of
> information that have value — but lose their value when the common
> public knows them."*

⚠⚠ **This resolves a tension `@X164` created.**  Knowledge may not pass
**player → player**, because buying an answer is buying the dominant
axis.  ⚠ Selling to an **NPC** is a different act entirely:

| ❌ player → player | ✅ player → NPC |
|---|---|
| hands somebody the answer they should have earned | **converts your own discovery into value** |
| the buyer skips the work | ⚠⚠ **you keep the knowledge** — you sold a copy, not the thing |

⚠⚠ **And it makes `@X102`'s knowledge tree economically live for the
first time.**  Until now the tree changed *what the player could attempt*;
this gives it a price without making it purchasable — ⚠ you still cannot
**buy** a fact, only **sell** one you found.

⚠ The named pieces are all already in [`SETTING.md`](SETTING.md), and
each is a truth the cordon does not have:

| the information | where it lives | ⚠ what revealing it does |
|---|---|---|
| **there are humans on the planet** | § History, § The settlers today | ⚠⚠ brings the government *in* — and the settlers **hide from the AI and from the players** |
| **the robots are steered, not haywire** | § The premise — *"the outside government's read is half wrong"* | ⚠⚠ the biggest one: the **cordon is built around a phantom** |
| **the city underground** | § The buried city | opens the deepest layer to outsiders |
| **the ART inside it** | § The statues, and the cloth | ⚠ the 2023 data already sells *painting, jewelry, sculpture* — so art has **two** values: the artefacts, and **the knowledge that they exist** |

⚠ § Future contact already said a player who discovers the truth *"holds
serious leverage"*.  ⚠⚠ **This is that leverage given a price.**

#### ⚠⚠ And the DECAY is the strongest anti-optimisation rule in this document  `@X215`

> *"…but lose their value when the common public knows them."*

⚠⚠ **A piece of information pays ONCE, EVER, across the whole player
base.**  Three consequences, and the third is the important one:

- ⚠ **first to sell takes the value** — so there is a race, and it is a
  race in **discovery** rather than in grinding;
- ⚠ **selling destroys the future value** — for you and for everybody,
  so the decision is *take it now* against *it is worth more while I am
  the only one who knows*;
- ⚠⚠ **it cannot be farmed.**  A resource that exists in quantity **one**
  has nothing to optimise, which is § The spreadsheet test satisfied
  outright rather than defended.

⚠ **And it carries its own pressure**, as § What actually defends it
requires of every reward: revealing *there are humans down there* is a
**betrayal with a price tag** — the settlers hide from players too, and
⚠⚠ **it cannot be undone.**  Irreversible, morally weighted, and priced
is exactly the shape this design wants.

#### ⚠ The mystery does not evaporate in week one, for two reasons

| | |
|---|---|
| ⚠ **late sellers get nothing** | so only the first few profit, and the rush is small |
| ⚠⚠ **supply is naturally slow** | `@X155` keeps the deep truths on the **planet**, found by hard work — and `@X150` forbids the crew knowing them in advance |

⚠ **The thing to watch**: if a single sale pays enough to fund a
campaign, somebody will sell the setting for cash on day one.  ⚠⚠ Price
it as *significant but not transformative*, and let the **strategic**
cost — what the cordon does once it knows — be the larger half.

#### ⚠⚠ And it is what makes the whole thread cohere  `@X209`

⚠ Read together, every rule in this thread is the same instruction from a
different angle:

| the rule | the angle |
|---|---|
| `@X198` four incommensurable axes | **no objective function** |
| `@X202` every reward has its pressure | **no free gain** |
| `@X204` traders you visit, not distributions | **positional, not statistical** |
| `@X205` trust makes switching a commitment | **relationship, not search** |
| `@X207` factions relate to each other | ⚠⚠ **the player is not the centre** |
| `@X210` a good deal for the trader is an investment | **now versus later, with the cost visible and the payoff earned** |
| `@X212` winning is centrality, and it decays | ⚠⚠ **nothing stays won** |
| `@X215` information pays once, ever | ⚠⚠ **nothing to farm** |

> ⚠⚠ **Each one removes a way to compute, and puts a way to LIVE in its
> place.**  That is the sentence to keep when judging anything added to
> the economy later.

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
