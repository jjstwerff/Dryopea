<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# PROGRESSION — several axes, and the player stays small

⚠⚠ **REWRITTEN 2026-08-26 on the owner's ruling, and the previous version
of this file was wrong in a specific way worth recording** — it claimed
progression *was* the player's own skill with the controls and refused a
character-stat layer outright (`@X016`, `@X017`, and the refusal at
`@X103`).  **That was a session's derivation, not the owner's design.**

The ruling, in the owner's own words (2026-08-26):

> *"the RPG part here will win … progression should not be about player
> skill … there can be multiple dimensions, so player skill is about
> building bases, moving around.  And there can still be opportunity for
> build speed, repair speed, detection radius etc."*

⚠ **So the error was not that player skill exists — it is that it was
made the WHOLE of progression, and used to refuse everything else.**
Player skill is one axis and it is real.  It is not what a run
accumulates.

⚠ The 2026-08-15 owner quote this file was originally built on — *"the
player skill progression should be that it is fun to glide around the
landscape finding things … and when the controls are learned using that
inside the own base to create an efficient layout"* — is about **feel**
and it still stands as § P1.  Reading it as *therefore there are no
stats* is the part that is superseded.

Codes: `@X016`, `@X017` **superseded**; `@X103` **reversed**; `@X018`,
`@X019` stand with a wider reading; `@X111`–`@X123` are the design
below.  Index: [`DECISIONS.md`](DECISIONS.md).

## P0 — ⚠⚠ the model is BLUE PRINCE: information dominates, and friction is what upgrades buy

Owner, 2026-08-26:

> *"think about the structure of blue-prince, it is totally player
> driven in the sense that information and choices to build rooms by the
> player dominate progression, but there is still a lot of room for game
> progression that gradually makes traversing/building the house
> easier."*

⚠ **This is the structure, and it resolves the argument the previous
version of this file was having with itself.**  Blue Prince runs two
progressions at once and they never compete, because they are answers to
different questions:

| | Blue Prince | dryopea |
|---|---|---|
| **the dominant one** | ⚠⚠ **what the player KNOWS** — puzzle answers, what a symbol means, which rooms combine.  It lives in the player's head, not the save file, so a player who knows can act on day one | what the player knows about **this world's rules and this base's shape** — that a miner opens a wall in 23 ticks and a scout needs 454, that a sealed perimeter doubles the clock and a gate buys nothing, that miners at the wall mean a mine upwind |
| **the act it drives** | ⚠ **drafting rooms** — a limited choice at every door, and the house is the player's own doing | ⚠ **laying out the base** — what to build, where, and what to leave out |
| **the run reset** | the house rebuilds every day; permanent unlocks persist | ⚠ each base is a fresh map; the scramble carries what you got out (`DESIGN.md` § 14) |
| **the second progression** | permanent unlocks and run resources — shortcuts, keys, better draft odds — that ⚠ **make a day cheaper without answering anything** | § P2 and § P3 — **build speed, repair speed, detection radius**, better equipment |

⚠⚠ **And that gives the fence its exact shape, which is § P6: an
upgrade buys FRICTION, never ANSWERS.**  A permanent shortcut in Blue
Prince saves you steps; it does not tell you the combination.  Faster
building in dryopea does not tell you *where* to build, and a wider
detection radius does not tell you what a miner-heavy wave means.

⚠ **The practical consequence is that the second progression can be
generous.**  This is what the deleted version got wrong: it treated
every upgrade as a threat to the game's integrity, when in a
Blue-Prince-shaped design the friction-reducers are what let a player
spend a run's attention on the part they have not solved yet.  **There
is a lot of room here, and taking it is the correct move.**

## P0b — the five axes

| # | Axis | What improves | Purchased? | Owner |
|---|---|---|---|---|
| **P1** | ⚠⚠ **what the player knows and decides** — the dominant one | base layout, and reading the world.  ⚠ Of the **rules**; the instance is re-scouted every run (§ P1d) | ⚠ **never** — it lives in the player | § P1 |
| **P2** | **the crew** | build speed, repair speed, detection radius, mining yield, hacking level, endurance | yes — people are hired, trained, and lost | § P2 |
| **P3** | **equipment** | what the vehicle and the towers can do | yes — built from [`MATERIALS.md`](MATERIALS.md), or found | § P3 |
| **P4** | **knowledge, as game state** | what the player can ATTEMPT and what the map will show | ⚠ found, never bought | [`SETTING.md`](SETTING.md) § The knowledge tree |
| **P5** | **carryover** | what survived the last scramble | earned | `DESIGN.md` § 13, § 14 |

⚠ **P1 and P4 are the same axis seen from two sides**, and the
distinction is worth keeping: P1 is what the *person* learned and cannot
be taken away; P4 is what the *run* recorded, so the game can gate a
mechanic on it.  ⚠ Blue Prince has both too — the player's own
understanding, and the notes and unlocks the estate keeps.

## P0c — ⚠⚠ the second progression's job is SMOOTHING, not power  `@X120` `@X121`

Owner, 2026-08-26:

> *"so it is still possible to make huge progress in the first mission
> when a player decides to restart the game after a long campaign, but
> that doesn't take away that after doing many scenarios the game helps
> the player to get to content/actions easier later in the game to
> smooth the experience."*

⚠ **Two rules fall out, and they are the sharpest operational
statements in this document.**

### ⚠⚠ 1. A returning player may skip the ramp, and nothing may stop them

A player who restarts after a long campaign brings § P1's knowledge with
them, and **should be able to tear through the first mission.**  That is
not an exploit to be balanced away — it is the dominant progression
working as designed.

| Consequence | ⚠ What it forbids |
|---|---|
| **the first base may not be balanced for a naive player** | difficulty that assumes ignorance, so that knowing the answer feels like cheating |
| **content may not be gated on progress already earned in the player's head** | *"clear three missions to unlock walls"* — a wall the player knows how to use, withheld because a counter says so |
| **the tutorial cannot be a wall** | anything unskippable.  ⚠ `DESIGN.md` § HUD already wants everything diegetic, which is the same instinct: a world you can read needs no lesson you must sit through |

⚠⚠ **And the corollary is a rule for every unlock this project ever
adds: prefer FOUND over AWARDED.**

- `DESIGN.md` § Scouting already says new tower types are **found on the
  map**, brought back, and orderable from then on.  ⚠ **A veteran knows
  where to look** — so a found unlock is *permeable to knowledge*, and a
  returning player collects on day one what a first-timer stumbles on in
  week three.
- An **awarded** unlock (*complete N sorties*) is impermeable by
  construction.  It measures attendance, not understanding, and it is
  precisely the wall this rule exists to keep out.

⚠ That is the Blue Prince property exactly: the estate does not hand you
the answer because you turned up often enough — it leaves the answer
where it always was, and you now know where that is.

### ⚠ 2. And the long campaign still gets smoothed

The other half is not a concession, it is the reason § P2 and § P3
exist: **after many scenarios the game should get the player to
content and actions faster.**

⚠ **Smoothing is not difficulty compensation and it is not a power
curve.**  Its job is to stop a long campaign spending its minutes on
problems the player solved twenty sorties ago:

- the base you have built forty times goes up **faster** (`build`);
- the round you have made a thousand times costs **less of the wave**
  (`repair`, `drive`);
- the ground you have read before **resolves sooner** (`scout`);
- the trip you always make is **worth more** (`mine`, `scrounge`).

⚠⚠ **Every one of those makes a SOLVED problem cheaper and an UNSOLVED
one no easier**, which is why § P6a's fence and this section are the
same rule seen from two ends.  ⚠ It is also why the friction-reducers
can be generous without risk: generosity on solved problems buys
pacing, and buys nothing else.

## P1 — what the player knows and decides  `@X018`

⚠⚠ **This is the dominant progression and it needs no mechanism at all**,
which is why it was easy to mistake for the *whole* design.  It has two
kinds inside it, and they are not equally important:

### P1a — DECISIONS made with information — the dominant kind

**Laying out a base is dryopea's draft.**  Where the walls go, where the
towers look, how the routes between them run, what gets built at all.
⚠ Every one of those is a *choice made with information*, and the
information is learnable:

- a **sealed** perimeter nearly doubles the fall clock, and a **gate
  buys nothing** ([`plans/12`](../plans/12-combat-resolution/README.md)
  § B7) — so the player who knows this stops cutting gates;
- a wave is worth its **front class plus what the front cannot cover**
  ([`plans/24`](../plans/24-the-siege-front/README.md), `@M020`) — so
  the player who knows this reads a composition and knows what is
  coming;
- **wave composition is a readout** ([`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md)
  § How the player ever learns any of this) — *miners at the wall means
  a mine upwind*.

⚠ **That is the same currency Blue Prince trades in**, and dryopea has
been accumulating it in `plans/` for twenty-six plans without ever
naming it as the progression.

### P1b — EXECUTION — real, and the minor kind

Boost management, reading a climb, taking a line through a base under
pressure.  ⚠ It matters — [`plans/13`](../plans/13-the-vehicle/README.md)
§ V4 measured that a sealed base can only be left by boosting — but it
is **not what a run accumulates**, and a design that leaned on it would
be a game about reflexes, which this is not.

⚠ **Neither kind is measured, priced, or purchasable, and neither should
become so.**  There is no piloting score and no drive stat on the
*player*; the crew's `drive` in § P2 governs the **helpers**, who are
driven by the game.

### P1c — ⚠ it leaves the player's head, which is what makes § P0c possible

⚠⚠ **P1 is the only axis that survives deleting the save file**, and
that is not a curiosity — it is the property § P0c is built on.  A
player who restarts brings all of P1 and none of P2-P5, and the design
promise is that **P1 alone is worth a lot**.

⚠ So there is a test for any new mechanic that claims to be progression:
*would a player who already understood this get it for free?*  If yes it
belongs to P1 and must not be sold.  If no, it is friction, and § P6a
says it may be sold generously.

### P1d — ⚠⚠ the RULES are learnable, the INSTANCE is not — which is why P1 never saturates  `@X122`

Owner, 2026-08-26:

> *"and there are enough random elements in each scenario that
> exploration stays a big part of it because even after playing the game
> many times you roughly know what to expect but cannot predict
> everything."*

⚠⚠ **This is the load-bearing constraint on the whole design, because
without it § P1 eats itself.**  A dominant knowledge progression in a
fixed world is a game that is *solved* after enough runs — the player
learns the answer and there is nothing left to learn.  Randomness per
scenario is what stops that, and the shape of the randomness is the
whole trick:

| The player learns, permanently | The player must find out, every run |
|---|---|
| a **sealed** perimeter doubles the fall clock; a gate buys nothing | where the ridge is on **this** map, and whether the ground even allows a seal |
| a **miner** opens a wall in 23 ticks and a **scout** needs 454 (`@M016`) | whether **this** wave has miners in it, and how many the front can cover (`@M020`) |
| a wave is worth its front class plus what the front cannot cover | what is upwind of **this** base — which is read off the traffic (`ROBOT_ECONOMY.md`) |
| a withered tree is a shaft to something worth having | which tree on **this** map withered, and what is down it |
| a tower needs 20 s of standing to come back | whether **this** layout lets you reach it in time |

⚠ **So knowledge is of the RULES; the instance is fresh.**  That is the
Blue Prince property exactly — a veteran knows what every room does and
still cannot say which three they will be offered at the next door.

### ⚠ What it means for exploration, and it is the answer to "why scout again"

⚠⚠ **Scouting is how rules-knowledge is converted into instance-knowledge,
and it has to be done every single run.**
[`EXPLORATION.md`](EXPLORATION.md) already ranks exploration *the*
discovery loop; this is why it never goes stale — the veteran scouts
**faster and better**, not **less**.  ⚠ And it is the cleanest possible
justification for § P2's `scout` skill and detection radius being a
friction-reducer (§ P6a): they make the conversion cheaper, and convert
nothing on their own.

### ⚠ It also bounds how random the randomness may be

Two failure modes, and the design sits between them:

- ⚠ **Too little** — a fixed world.  P1 saturates, the game is solved,
  and § P0c's returning player has nothing left to return to.
- ⚠⚠ **Too much** — expectations stop paying.  If a base can be anything,
  *roughly knowing what to expect* is worth nothing, and the dominant
  axis is destroyed from the other side.  **A veteran must be able to
  form good expectations and be surprised at the margin.**

⚠ **The practical rule: vary the INSTANCE, never the RULES.**  Vary
where the ridge is, which classes are in the wave, which tree withered,
where the wrecks lie — never what a sealed wall is worth or how fast a
miner chews.  ⚠ A patch that retuned `enemy_speed` between missions
would break P1 in a way no amount of map randomisation does, which is
also why `numbers.json` is a global and not a per-map field.

⚠ **And every random element must be something you have to go and
LOOK at** (§ P6a: a readout that names the composition supplies the
answer; traffic you can watch does not).  Randomness the game tells you
about is not exploration — it is a dice roll with a caption.

## P2 — the crew are CHARACTERS  `@X111`

⚠⚠ **This is the layer the previous version refused, and it is
adopted whole** from the 2023 design
([`../archive/gameplay.data`](../archive/gameplay.data),
[`../archive/seed-notes.md`](../archive/seed-notes.md)).

`DESIGN.md` § 9 § Future skills already had six helper skills and a
per-helper id *"so future skills hang off existing characters without
re-engineering"* — the data model has been waiting for this since plan
14.  What the 2023 sheet adds is the full shape.

### The twelve skills

*boost, build, combat, drive, hack, mine, operate, repair, scout,
scrounge, social, stealth.*

### The six statistics, each bonusing four skills

| statistic | *"…"* | bonuses |
|---|---|---|
| **agility** | quickly react to situations | boost, build, drive, stealth |
| **charisma** | working together, anticipating others | hack, operate, social, stealth |
| **observe** | reacting precisely to various signs | combat, drive, scout, scrounge |
| **plan** | thinking ahead to future problems | build, operate, mine, repair |
| **stamina** | how problematic is stress | combat, mine, scout, social |
| **tinker** | getting machines to do what you want | boost, hack, repair, scrounge |

⚠ **Every skill is bonused by exactly two statistics and every
statistic bonuses exactly four skills** — a 6x12 lattice with no
dominant stat, which is why it is worth taking as authored rather than
re-derived.

### ⚠⚠ The rule that keeps it in-genre: a skill SCALES A NUMBER THAT ALREADY EXISTS  `@X112`

This is the same rule [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) and
[`MATERIALS.md`](MATERIALS.md) run on — **one system, per-type data** —
and here it is what makes the whole layer cheap:

| Skill | Scales | Which lives today in |
|---|---|---|
| **build** | ⚠ **build speed** — and `DESIGN.md` § 13 already says *walls are free in points, **helper-seconds is the bottleneck***, so this scales the one resource walls actually cost | the (unbuilt) build order; the bottleneck is already named |
| **repair** | ⚠ **repair speed** — the 20 s of standing within one hex, banked **on the tower** so a relief crew finishes what a lost one started | `tower.loft::tower_repair_tick`, [`plans/17`](../plans/17-tower-hot-swap/README.md) § T1 |
| **scout** | ⚠ **detection radius** — how far intel resolves, and how fast a sortie reads a route | the haze radius; `EXPLORATION.md` § X8's per-node `found` flag |
| **mine** | yield per hex cut, and what a hex yields at all | `vehicle.loft::salvage_at` |
| **scrounge** | ⚠ **what comes back out of a wreck** — the recycler's level, which `MATERIALS.md` § Open questions 4 left open.  ⚠ A *person* answering it is better than a building answering it: it makes the crew you brought decide what the field is worth | `wallet.loft::loot_rate`, `MATERIALS.md` § What a wreck yields |
| **hack** | ⚠ *"the complexity of the AI that can be hacked"* — a **level**, gating AI cores by tier | `MATERIALS.md` § The AI core; `DESIGN.md` § 9 |
| **operate** | *"getting more out of a machine by manually aiding the process"* — ⚠⚠ **the presence bonus**, which is § What kind of game this is in one word: a machine does more while somebody is standing at it | the tower charge refill |
| **boost** | *"maintenance and tuning of machines"* — upkeep quality, and how slowly things decay under [`SETTING.md`](SETTING.md) § The pollen | `DESIGN.md` § 7 tower decay |
| **drive** | helper travel speed between jobs | `helper.loft::helper_bank` |
| **combat** | tower accuracy / traverse under a crew member's hand | `tower.loft` aim time, `DESIGN.md` § Aiming costs time |
| **stealth** | ⚠ whether a sortie is noticed — the seed notes' *"stealth is possible, however not for the jammer"* | `wave_provoke_step`'s thresholds |
| **social** | ⚠ crew retention and native contact — see § P2b | run-level, `SETTING.md` § Future contact |

⚠ **So no skill introduces a mechanism.**  Each one multiplies a
constant dryopea either ships or has already named, which means the
layer can land incrementally — one skill at a time, each with a gate
that reads a number that is already measured.

### P2b — pools, rest, and losing people  `@X113`

The seed notes: *"individual humans can have skills and different pool
levels.  So they need rest/sleep."*

- **A pool is endurance, spent by work and restored by rest.**  ⚠ It
  is the crew-side version of the tower's charge, and it makes a long
  sortie cost something *besides* the permit window
  ([`EXPLORATION.md`](EXPLORATION.md) § X2d).
- ⚠⚠ **This is what makes `MATERIALS.md` § The crew are people a real
  building**: *living quarters* restores pools, so a mission long
  enough to need rest is a mission that has to build for it.  A base is
  then a **place people live**, not only a firing position.
- **Losing people is a run-level consequence**, and the 2023 data adds
  the sharp version: *"**Defection** — personnel can join competitors
  when not treated well."*  ⚠ The crew you neglect do not merely leave
  — they turn up on somebody else's payroll
  ([`SETTING.md`](SETTING.md) § The competitors).

⚠ **Backgrounds and classes are the flavour layer that carries the
numbers** — *colonist, spacer, hive dweller, trader, artist*;
*astro-miner, engineer, hacker, ranger, anthropologist, templar*.  A
class is a starting profile, not a permanent multiplier.

## P3 — equipment, and the 28 upgrades  `@X114`

⚠ **Reversed with the rest**: the 2023 `type:upgrade` list is design
material, not a refusal.  *Reactive armor, shielding, titanium frame,
cargo pods, stabilizers, tuning, bionic eyes, powered legs, claws,
drill arm, mind jack.*

⚠⚠ **Two of them still need a ruling, and it is § P6's, not a blanket
one:**

| Upgrade | Ruling |
|---|---|
| **Laser communicator** — *"allows to communicate under the influence of a scrambler"* | ⚠⚠ **the LOS version only.**  [`SETTING.md`](SETTING.md) § The recruitment (`@X099`) rests the entire premise on *no comms during a mission, so there has to be personnel below*.  A **line-of-sight** relay you build, place on a height and defend is a positional asset and passes; a general radio deletes the reason the game exists |
| **Auto pilot** — *"enhance vehicles, but create bored and distracted drivers"* | ⚠ it automates § P1, and the 2023 note **already prices it** — bored and distracted.  Keep the drawback: an autopilot that is strictly better removes the axis the player enjoys |

⚠ **The vehicle MAY improve.**  `@X017`'s *"the player's vehicle must
not get faster"* is superseded — it was the same over-correction.  What
constrains a vehicle upgrade is § P6 and nothing else.

## P4 — knowledge

Owned by [`SETTING.md`](SETTING.md) § The knowledge tree — ~55 facts in
per-faction arcs, recovered from the 2023 data.  ⚠ Its own rule stands
because it is about *discovery* rather than about stats: knowledge
changes **what the player can attempt and what the map will show
them**.  ⚠ It is the one axis that is **found and never bought**, which
is what keeps `SETTING.md` § Future contact's no-shortcut rule intact.

## P5 — carryover

`DESIGN.md` § 13 § Starting budget + 1:1 carryover and § 14 § Scramble
exit.  ⚠ With § P2 in the design, **the crew are part of the manifest**
— who boarded before force-launch is now a question about *which
skills* you still have next base, not only a headcount.

## P6 — ⚠⚠ the fence: an upgrade buys FRICTION, never ANSWERS

**Two constraints, and only the second was in the deleted version.**

### P6a — the Blue Prince test, and it is the one to use day to day

> ⚠⚠ **Does this reduce the COST of acting on what the player knows, or
> does it supply what they do not know?**

A permanent shortcut in Blue Prince saves you steps; it never tells you
the combination.  That is the whole rule, and it is much easier to apply
than an argument about genre:

| Passes — it buys friction | Fails — it buys the answer |
|---|---|
| **build speed** — the base you decided on goes up sooner | a layout the game recommends, or auto-placed walls |
| **repair speed** — the round you decided to make takes less of the wave | towers that repair themselves |
| **detection radius** — you see further, sooner | a readout that *names* the incoming composition instead of showing it |
| **mining yield** — the trip you already made is worth more | salvage that arrives without the trip (`@X104`, `@X105`) |
| **endurance pools** — a longer sortie is possible | a sortie with no clock |
| a vehicle that carries more, climbs better, sees further | a vehicle that fights (`DESIGN.md` § 8) |

⚠ **The failing column is not "too strong" — it is *doing the player's
thinking*.**  That is why generosity on the passing column is safe:
making the same decision cheaper to execute cannot erode a progression
that lives in deciding.

### P6b — and the player stays SMALL

The owner's constraint (2026-08-14), from `DESIGN.md` § And the DEEP
layers are what keep it a tower defence:

> **Every deep layer is unbeatable by personal power.**  A solo player
> can never wake an old one, understanding them is the *failed*
> defence, resistance cannot be had without ceasing to be a person…
> ⚠ **Not one of those offers "become strong enough and win"** — which
> is the drift that would turn this into an action RPG.

⚠ So the second question is *does this open a route to winning by
personal power?*  ⚠⚠ **Note it is a much narrower fence than the
deleted version drew**: it forbids a weapon skill that lets one person
hold a perimeter alone, and hacking that converts a boss into a
bodyguard on demand.  It does not forbid a stat.

⚠ **`DESIGN.md` § 8's noncombatant player is the sharpest statement of
it and it does not move.**  The crew get better at *working*.  Nobody
gets better at *fighting* — the towers fight, and the player services
the towers.

## P7 — the base is still the exam, and the numbers now read on THREE axes  `@X019` `@M004` `@M007`

The measurements are unchanged and still true; what the previous
version got wrong was reading all of them as *pilot skill*.

| measured | where |
|---|---|
| a **sealed** wall nearly **doubles** the fall clock | [`plans/12`](../plans/12-combat-resolution/README.md) § B7 |
| a wall with a **GATE buys nothing at all** | plan 12 § B7 |
| **boost is the only way out of a sealed base** — 3.0 m climb for three ticks, where an idle vehicle climbs 0.4 m | [`plans/13`](../plans/13-the-vehicle/README.md) § V4 |
| **upkeep is a POSITIONING problem, not a resource** — two *shuttling* helpers clear all 205 robots; the same two *parked* reach 5/7 and the base falls | [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 |
| a black tower needs **20 s of standing within one hex** to come back | plan 17 § T1 |

⚠⚠ **Read on the P1a axis — the dominant one**: *a sealed wall doubles
the clock, a gate buys nothing.*  **That is a fact the player learns**,
and once learned it changes every base they ever draft.  It is the
purest Blue Prince item dryopea has: no upgrade grants it, no stat
substitutes for it, and a player who has it plays differently on their
first day of a new run.

⚠ **Read on the P1b axis**: sealing is then a bet on your own boost
management, because a sealed base can only be left by flying
(`plans/13` § V4).  Real, and secondary — the decision was the
interesting half.

⚠⚠ **Read on the P2 axis, which is the one that was missing**: the
shuttling-vs-parked result is **a statement about crew coverage**, and
the moment a crew member has a `repair` skill and a `drive` speed, the
*same base* is serviceable by three good people or five ordinary ones.
**That is the progression the run accumulates** — the layout you can
afford is a function of the crew you brought.

### ⚠ The tension between the axes is the interesting part

- a **compact** base is fast to service and concentrates the approach
  fan onto fewer wall hexes — which plan 12 B3 says is where a wall
  *breaks*;
- a **spread** base covers more ground and braces more wall, and costs
  travel time between towers on a 20-second repair clock.

⚠⚠ **The spread a base can afford is bought on three different
axes, and that is the design working.**  The player buys it by
*knowing* which hexes actually get chewed (P1a); the pilot buys it with
boost lines (P1b); the crew buy it with drive speed and repair speed,
which are **purchasable** (P2).  ⚠ Three routes to one affordance is
exactly the Blue Prince shape — the upgrades lower the cost of a layout
the player still has to have thought of.

⚠ **What it needs from a map author** is height — boostable ledges, a
3.0 m step that a good pilot crosses and a bad one goes round.
`DESIGN.md` § Trees as terrain supplies the extreme case.

⚠ **What it needs from this project is still measurement**, and there
is none: no scenario varies the *layout* while holding the defences
equal, and none varies the *crew profile* at all.  Those are two
scenarios to write, and the second is now the more interesting one.

## P8 — ⚠ the perspective rule, and why it makes the roster matter  `@X115`

From the seed notes:

> *"Always show the world from the perspective of a human.  Possible to
> switch to another human when needed.  Though there needs to be an
> inherent cost to prevent steering multiple units at the same time.
> **Only switching to a human that is at the main communication
> terminal.**  So there is a clear reason to want to return there."*

⚠⚠ **With § P2 adopted, this stops being a curiosity and becomes the
mechanic that makes a skilled crew legible.**  If characters have
skills and the player only ever drives one of them, the roster is a
spreadsheet the player watches.  If the player can *become* the
engineer when a tower needs engineering, the crew profile is something
they act through.

⚠ **And the cost is exactly the right kind** — the switch happens at a
terminal, so using it means **driving back to the core mid-wave**,
which is § What kind of game this is in its purest form: a thing put in
the player's hands at a moment when using it costs them something.

⚠ **Status: designed, not decided.**  `DESIGN.md` § 8 has one player
vehicle and § 9 has helpers as NPCs, so this is a change to the control
model rather than an addition to it.  It is recorded here because § P2
is much weaker without it.

## Open questions

1. **Are skills per-CHARACTER or per-ROLE?**  The 2023 sheet is
   per-character; dryopea's helpers are interchangeable with an opaque
   id (`DESIGN.md` § 9).  *Recommendation: per-character — the id is
   already there for exactly this, and § P2b's defection needs people
   who are distinguishable enough to miss.*
2. **Do skills improve WITHIN a run or only between?**  ⚠ Within-run
   improvement makes a long base self-reinforcing, which cuts against
   the scramble decision.  *Recommendation: between runs only, so the
   sortie's crew is a choice made at the station (`DESIGN.md` § 16) and
   the run is played with what you brought.*
3. **How many of the twelve ship first?**  ⚠ § P2's rule makes the
   answer cheap: ship the skills whose scaled number is already
   measured — **build, repair, scout** are the owner's own three
   examples and all three have a constant to multiply today.
4. **Does the player's own character have stats?**  If the player is a
   person (§ P8), they must.  ⚠ But a `drive` stat on the driven
   vehicle collides with § P1 — the player *feels* their own driving.
   *Recommendation: the player's character carries the WORK skills
   (build, repair, mine, hack) and not the movement ones; movement
   stays P1.*

## See also

- [`DESIGN.md`](DESIGN.md) § What kind of game this is — the test every
  axis answers to; § And the DEEP layers — § P6's fence, owner-stated;
  § 9 Helpers — where the per-character id already lives.
- [`SETTING.md`](SETTING.md) § The knowledge tree — axis P4.
- [`MATERIALS.md`](MATERIALS.md) — axis P3's catalogue, and § The crew
  are people for the quarters building § P2b needs.
- [`EXPLORATION.md`](EXPLORATION.md) — where P1 is practised and P4 is
  gathered.
- [`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) §§ 4-5 — the routing of the
  2023 material this file now adopts.
- [`DECISIONS.md`](DECISIONS.md) — the index, including the superseded
  codes.
