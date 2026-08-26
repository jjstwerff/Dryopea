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
`@X019` stand with a wider reading; `@X111`–`@X136` are the design
below, and `@X137`–`@X141` (no tutorial) are `DESIGN.md` § 11's;
`@X142`–`@X143` are the speaker rule and `@X144`–`@X149` the debrief and `@X150`–`@X151` what the crew
can know at all, `@X152`–`@X156` the window into the
universe, and `@X157`–`@X158` personality.  Index: [`DECISIONS.md`](DECISIONS.md).

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
| ⚠⚠ **there is no tutorial at all** (`@X137`) | anything unskippable — and since 2026-08-26, anything at all: no tutorial mission, no first-run overlay, no tooltip layer.  ⚠ `DESIGN.md` § There is NO TUTORIAL is the ruling; § Position triggers is why it is affordable, because most of the game has no key to learn |

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

### ⚠⚠ P2a — the two layers are a LOOP: practice raises the statistic, and the statistic permeates  `@X124`

Owner, 2026-08-26:

> *"a statistic gives a bonus to skills but is also increased with the
> practice in that skill.  So this is a system where characters (NPCs)
> get gradually better at what they actively do and via the 2 related
> statistics that permeates to other skills too."*

**So the arrow runs both ways, and that is the whole system:**

```
   practise a SKILL  ──raises──▶  its 2 STATISTICS  ──bonus──▶  their 4 SKILLS each
        ▲                                                              │
        └──────────────  the crew get better at what they DO  ─────────┘
```

⚠ **Fast layer and slow layer.**  The skill itself rises quickly with
use; the two statistics behind it rise slowly, and they lift **six other
skills** a little.  So **specialisation is fast and generalisation is
slow**, which is exactly the shape a roster wants: a helper who repairs
becomes a repairer within a sortie and a broadly useful engineer over a
campaign.

#### The lattice, verified against the data

⚠ Read out of [`../archive/gameplay.data`](../archive/gameplay.data),
not retyped: **12 skills, 6 statistics, every skill bonused by exactly
2 and every statistic bonusing exactly 4.**  24 edges, no exceptions,
nothing declared-but-unused in either direction.

**Practise the left column, and these are the skills it quietly lifts:**

| practise | via | it permeates to | n |
|---|---|---|---|
| **boost** | agility + tinker | build, drive, hack, repair, scrounge, stealth | 6 |
| **build** | agility + plan | boost, drive, mine, operate, repair, stealth | 6 |
| **combat** | observe + stamina | drive, mine, scout, scrounge, social | ⚠ 5 |
| **drive** | agility + observe | boost, build, combat, scout, scrounge, stealth | 6 |
| **hack** | charisma + tinker | boost, operate, repair, scrounge, social, stealth | 6 |
| **mine** | plan + stamina | build, combat, operate, repair, scout, social | 6 |
| **operate** | charisma + plan | build, hack, mine, repair, social, stealth | 6 |
| **repair** | plan + tinker | boost, build, hack, mine, operate, scrounge | 6 |
| **scout** | observe + stamina | combat, drive, mine, scrounge, social | ⚠ 5 |
| **scrounge** | observe + tinker | boost, combat, drive, hack, repair, scout | 6 |
| **social** | charisma + stamina | combat, hack, mine, operate, scout, stealth | 6 |
| **stealth** | agility + charisma | boost, build, drive, hack, operate, social | 6 |

#### ⚠⚠ The graph has DIAMETER 2 — there are no dead ends

Measured: **every skill reaches every other skill in at most two hops**,
and the eccentricity is 2 for all twelve.  ⚠ So a crew member put on one
job for a whole campaign never becomes *narrow* — anything they were
never asked to do is still only one statistic away from something they
were.

⚠ **That is what makes the hard cap of six helpers (`DESIGN.md` § 9)
comfortable rather than tight**: a specialised crew generalises on its
own, so the player is not forced to hire redundancy against a job that
might come up.

#### ⚠ `combat` and `scout` are statistical TWINS — and in dryopea that is a finding

Both are **observe + stamina**.  They are the only pair in the table
that collapses, which is why both reach 5 instead of 6, and it means
**practising one advances the other exactly as much as practising it
directly would.**

⚠⚠ **dryopea should probably drop `combat` and keep `scout`.**
`DESIGN.md` § 8 makes the player a noncombatant and the towers do the
fighting, so `combat` has almost nothing to scale — while `scout`
scales the detection radius, which is one of the owner's own three
examples.  ⚠ Dropping it costs nothing structurally: its statistic pair
stays covered, so no statistic is orphaned and no other skill's
permeation changes.

#### ⚠ Four statistic pairs are unused, and they are the slots for a 13th skill

11 of the 15 possible pairs are in play.  ⚠ **A new skill should take a
free pair rather than duplicate an occupied one** — duplication is how
`combat`/`scout` became twins:

| free pair | ⚠ the shape a skill there would have |
|---|---|
| **agility + stamina** | fast *and* enduring — physical field work; the closest thing to a *labourer* |
| **charisma + observe** | reading people and signs — ⚠ the natural home for **native contact** ([`SETTING.md`](SETTING.md) § The settlers today) |
| **observe + plan** | noticing and then thinking ahead — ⚠ this is *surveying*, which is what a base LAYOUT decision actually is |
| **stamina + tinker** | long grinding machine work — sustained repair under load |

#### ⚠⚠ Why this passes § P6a's fence automatically

**Use-based advancement can only ever reward work the player already
decided to do.**  It cannot supply an answer, because it has no way to
act before the player acts — the player's assignment decisions are its
only input.  ⚠ That makes it the purest friction-reducer in the design:
it lowers the cost of the thing you have been doing, and it is silent
about everything you have not.

⚠ **And it is § P0c's smoothing with no dial on it.**  *"After doing
many scenarios the game helps the player to get to content/actions
easier"* — a crew that has built forty bases builds faster, and nobody
had to tune a curve to make that true.

#### ⚠ The one real danger: advancement must not reward a NO-OP

Use-based systems invite busywork — grinding repairs on an undamaged
tower to farm `tinker`.  ⚠ **The rule: practice counts only for work
that had a reason to be done**, and dryopea's existing mechanics
already enforce most of it — a **firing tower refuses repair**
(`DESIGN.md` § 7), salvage **decays**, and the permit clocks the run
([`EXPLORATION.md`](EXPLORATION.md) § X2d).  ⚠ A new skill whose
practice can be repeated at zero cost has broken this, and that is the
question to ask of each one before it ships.

#### ⚠⚠ When practice BANKS — and the scramble already answers it

⚠ Practice accrues **during** a sortie, which looks like it cuts against
§ Open questions 2's *between runs only*.  It does not, because
`DESIGN.md` § 14's cargo manifest already draws the line: **a helper's
gains bank when that helper boards before force-launch.**

⚠⚠ **So leaving somebody behind now costs their experience as well as
their body** — which sharpens the scramble decision rather than
softening it, and needs no new mechanism at all.

### ⚠⚠ P2b — TEMPLATES, never rerolls  `@X125`

Owner, 2026-08-26:

> *"I do not like the system where players have to do many rerolls to
> get the optimal NPC in the game, so we give them templates instead
> that can be optimized by actually letting the NPC perform related
> tasks."*

⚠ **A background is a TEMPLATE the player picks with known contents**,
not a roll they repeat until it comes up good.

| | reroll-for-stats | ⚠ template + practice |
|---|---|---|
| how you get a good NPC | spend real time re-drawing until the numbers are right | **pick the profile you want, then give them the work** |
| what it tests | patience | ⚠ **what the player knows about the base they intend to build** |
| where it happens | in a menu, before the game | in the game, as assignments |
| under § P6a | ⚠⚠ **fails** — you are paying time for the game to hand you an answer | **passes** — the player decides, and the payoff follows the decision |

⚠⚠ **The reroll loop is § P6a's failure mode wearing a dice cup**, which
is why the objection is not merely taste: it takes the dominant axis
(§ P1a, deciding with information) and replaces it with attrition.
A template turns hiring into exactly the kind of decision `@X117`
rewards — the veteran knows a swamp base needs `build` more than
`scout`, and hires accordingly on day one.

#### ⚠ The trap: do not randomise which templates are OFFERED

That re-invents the reroll one level up.  ⚠ **The catalogue must be
stable and knowable** — the 2023 data's eight backgrounds (*colonist,
earthling, hive dweller, member, spacer, trader, artist, athlete*) and
sixteen classes (*astro-miner, engineer, hacker, ranger,
anthropologist, doctor, ecologist, enforcer, military, miner, official,
templar, researcher, animal handler, diplomat, investigator*) are a
**menu**, not a draw.

⚠ It looks like it should be allowed under `@X122` (*vary the
instance*), and it is not — because § P1d's other half says every random
element must be **something you go and LOOK at**.  A hiring pool is not
a place; re-rolling it is waiting, not exploring.

#### ⚠ The template slots exist in the schema and are EMPTY

`archive/world.gcp` gives `Item { name, type, description, statistics[Stat] }`
— so a background *can* carry starting statistics — but every background
and class in `gameplay.data` carries **a description and no numbers**.

⚠ **That is the authoring job, and it is small**: eight backgrounds and
sixteen classes, each a handful of starting values over 6 statistics and
12 skills.  ⚠ Design them so **no template is dominant** — the pairs
above are the tool: a template should be strong on two statistics that
share few skills, so its strength is a genuine shape rather than a
bigger number.

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
| **social** | ⚠ crew retention and native contact — see § P2d | run-level, `SETTING.md` § Future contact |

⚠ **So no skill introduces a mechanism.**  Each one multiplies a
constant dryopea either ships or has already named, which means the
layer can land incrementally — one skill at a time, each with a gate
that reads a number that is already measured.

### ⚠⚠ P2c — the crew REMARK on the world, and each one sees only their own half  `@X129`

Owner, 2026-08-26:

> *"there can be helpers/companions that had different jobs in the past
> (asteroid miner, security, repair handyman, scout) and that determines
> their starting skills.  And they will remark on the player when they
> land on a planet (not intrusively) about what they think should be
> done based on their skills."*

⚠ **The past job is the template** (§ P2b), and all four are already in
the 2023 class list: *astro-miner*, *enforcer* (**"was employed by one
of the security Agencies"**), *engineer* (**"repairing and constructing
the machines humans need"**), *ranger* (**"roaming around to find
valuables"**).  So the catalogue this needs is authored except for its
numbers.

#### ⚠⚠ Why it passes § P6a, and the rule that keeps it passing

This is the closest thing in the whole design to the forbidden side of
the fence — *an upgrade must not supply what the player does not know* —
so the boundary has to be exact:

> ⚠⚠ **A remark POINTS; it never CONCLUDES.**  It says *what is there*,
> in the speaker's own domain.  It never says *what to build*, and it is
> never a substitute for going and looking.

| ✅ a remark | ❌ not a remark |
|---|---|
| *"That ridge east — soft ground.  Anything heavy bogs down."* | *"Build your wall on the east ridge."* |
| *"Tracks in the mud here.  Wide ones."* | *"Wave 3: eight miners, four scouts."* |
| *"That tree's dead.  Something came up out of it."* | a marker appearing on a map the game draws for you |

⚠ **The second column is § P6a's own failing row** — *a layout the game
recommends*, and *a readout that NAMES the incoming composition instead
of showing it*.  The first column is instance-knowledge the player would
have got by scouting, handed over **cheaper**, which is exactly what
`scout` is licensed to buy.

⚠⚠ **And it preserves the trip**, which is this project's deepest rule
(`@X104`, `@X105`): a remark is **a reason to go and look**, so it
*creates* trips rather than removing them.  A crew member who saved you
the drive would be the failure; one who tells you which drive is worth
making is the design.

#### ⚠⚠ THE CREW DO NOT KNOW MORE THAN THE PLAYER — they are on this planet for the first time too  `@X150`

Owner, 2026-08-26:

> *"the helpers do not know more than the player, but they observe things
> and comment on it.  They are on this planet for the first time too."*

⚠⚠ **This is the load-bearing constraint on everything in § P2c and
§ P2f, and it is already the fiction.**
[`SETTING.md`](SETTING.md) § The recruitment has Centaur Mining
specialising in **automated asteroid mining**, talked into a contract
they have no experience of — *"we do not involve ourselves with
conflicts"*.  **Nobody in the vehicle has been anywhere like Dryopea.**

⚠ **So the distinction that governs every line they say is NOTICING vs
KNOWING:**

| ✅ what a past job gives them | ❌ what it does not |
|---|---|
| **trained ATTENTION** — a miner's eye goes to the ground, a security officer's to the sight lines | **world knowledge** — what Dryopea's ground actually does, where its robots come from, what its trees are |
| *"that rock is fractured — on an asteroid I'd not cut near it"* | *"this is bad rock, don't build here"* |
| noticing a thing the player would have driven past | explaining what the thing means |

⚠⚠ **And this is why `@X129`'s fence needs no discipline to hold: they
do not conclude because they CANNOT.**  A rule enforced by the world
costs nothing to maintain, where a rule enforced by restraint erodes the
first time somebody writes a helpful line.  ⚠ Their honest register is
**uncertainty** — *"that ground looks soft to me, but I've only ever
worked vacuum"*.

#### ⚠⚠ Which means they ONBOARD by drawing attention, never by explaining

The obvious objection is that a crew who know nothing cannot be
`@X137`'s onboarding.  ⚠ They can, because **a lost player does not need
an explanation — they need somewhere to look.**

*"There's something over that ridge."*  *"That's not right."*  *"I'd
want a look at that before we commit."*  ⚠⚠ Every one of those moves a
stalled player without telling them anything, which is the nudge the
owner described from the start — and it is **the same sentence a
first-timer would actually say.**

#### ⚠⚠ And the crew LEARN ALONGSIDE the player — which is audible progression  `@X151`

If they arrive knowing nothing, then **over a campaign they come to know
what the player knows**, and their remarks should say so:

| sortie 1 | sortie 10 |
|---|---|
| *"that ground looks soft to me"* | *"that's like the third base — the ground gave way there too"* |
| *"something's moving out past the ridge"* | *"that's the same as the ones that came at us through the gully"* |

⚠⚠ **That is the crew's version of `@X117`**, and it is the one form of
progression the player can *hear*.  ⚠ It composes exactly with `@X131`'s
ledger — a crew member can only refer to a base they were **on** — and
with `@X102`'s knowledge tree, which is the run's record of the same
learning.

⚠ **It also keeps them from ever overtaking the player**: they learn
from the sorties the player ran, so their knowledge is a **subset** of
what the player had access to.  ⚠⚠ A crew member who knew something the
player could not have learned would be the failure this section exists
to prevent.

#### ⚠⚠ The crew are PARTIAL SENSORS, and assembling them is the player's job

**This is what makes the whole idea earn its place rather than being
flavour.**  Each crew member's *attention* is trained somewhere
different — so they notice different things, and none of them is looking
everywhere:

| the ex- | their eye goes to | ⚠ and they do not look at |
|---|---|---|
| **asteroid miner** | the ground, and what it would take to cut it | whether anything could be defended there |
| **security** | approach lanes and sight lines | whether the ground is worth anything |
| **repair handyman** | wear, damage, what is already being eaten ([`SETTING.md`](SETTING.md) § The pollen) | where any of it is coming from |
| **scout** | distance, routes, what is out past the haze | everything close in |

⚠ **None of those columns is expertise about Dryopea.**  They are
*habits of attention* carried from another job — which is exactly what a
real specialist brings to an unfamiliar place, and it is why the
observations are worth having without being answers.

⚠⚠ **So no single crew member's account is a plan, and following one
exclusively is how a base goes wrong.**  Every remark is *true*; the
error is never a lie, it is **incompleteness** — which keeps the
synthesis in the player's hands, where § P1a needs it.

⚠⚠ **REFINED 2026-08-26 — and the distinction is the whole thing.**
What is ruled out is **arbitrary error**: a crew member who is wrong *by
dice*, some fraction of the time, to keep the player honest.  That is
being lied to by your own people, it is not learnable, and it is
frustrating rather than interesting.

⚠ What is **adopted** is the opposite kind of wrongness — **systematic
bias from perspective**, which § P2f develops as *unreliable narration*.
The facts are true; the **account** is coloured by where the speaker
stood and what they know.  ⚠⚠ Arbitrary error is noise; systematic bias
is **character**, and it is learnable, which is what makes it feed the
dominant axis instead of fighting it.

#### ⚠ It is the SKILL SHEET made diegetic — which is how a roster stays legible with no UI

⚠⚠ **The player learns what their crew are good at by noticing who
speaks up about what.**  You do not read that somebody has `scout` 7;
you notice they are the one who mentions the ridge.

That answers a question § P2 would otherwise have had to answer with a
character screen, and it answers it the way `DESIGN.md` § HUD wants
everything answered — **in the world**.  ⚠ And it is where § P2b's
template choice pays off *immediately*: you hired an ex-security
officer, and on landing they talk about approach lanes.

⚠ **It is also the detection radius made audible.**  `scout` scales how
far intel resolves (§ P2's table); a better scout therefore says **more,
and earlier** — a skill the player perceives without a bar or a number.

#### ⚠ "Not intrusively" is a hard constraint, not a tone note

- **No pop-up, no modal, no forced camera, no pause.**  Ambient — and
  ⚠ **timed by the player's own state**, which is § WHEN they speak
  below: off-time only, silent while the player is working.
- ⚠ **A player who ignores every remark must lose nothing they could
  not get themselves.**  The remark is a shortcut to looking, and
  looking remains available.
- ⚠ **It must not become a quest log.**  A remark is said once and is
  gone; a list of outstanding crew suggestions is a task UI, and
  `DESIGN.md` § HUD refuses it.

#### ⚠⚠ WHEN they speak: off-time only — and SILENCE is the load-bearing part  `@X135`

Owner, 2026-08-26:

> *"The crew should spread their remarks during off-time in the scenario
> so when a player is wondering what to do.  They do not hold all the
> answers too.  When the player is busy with planning/scouting they will
> just not remark on it.  It is the gentle nudge to a player that feels
> lost."*

⚠ **This is what makes *"not intrusively"* a mechanism rather than a tone
note.**  The trigger is **the player's own state**, not a timer and not a
script:

- **A player who is getting on with it hears nothing.**
- **A player who has stalled hears one line, from whoever has something
  to say about where they are standing.**

#### ⚠⚠ These remarks ARE the onboarding — there is no tutorial to fall back on  `@X137`

`DESIGN.md` § There is NO TUTORIAL (owner, 2026-08-26) rules out a
tutorial entirely: the controls are meant to be found by playing around,
and most of the game has no key at all (§ Position triggers).  ⚠ What
that leaves unanswered is **what to do first**, and this is what answers
it — which makes the system **load-bearing rather than a convenience**.

⚠ **One consequence: the LANDING remark must be near-certain.**  Every
other remark is conditional on somebody having something to say (`@X136`),
but on fresh ground *every* crew member has an observation in their own
domain — so the opening is both where the system can reliably speak and
the moment a lost player most needs it.

⚠ **It still may not conclude** (`@X129`): *"soft ground on that ridge"*
gets a lost player moving, *"press Q to paint a wall"* is the tutorial
that was just refused.

#### ⚠⚠ It skips itself for the player who does not need it — which is `@X120` for free

`@X120` says a returning player may skip the ramp and **nothing may stop
them**, and that *the tutorial cannot be a wall*.

⚠⚠ **A nudge gated on stalling satisfies that automatically**: a veteran
who never stalls **never hears a hint**, and nobody had to build a
difficulty setting, a skip button or a tutorial toggle to make it so.
⚠ It is `@X121`'s smoothing aimed at the one moment it is wanted, and
absent everywhere else.

#### ⚠⚠ SILENCE has to be a real outcome, or the whole thing inverts

> *"They do not hold all the answers."*

⚠⚠ **This is not modesty — it is the defence, and without it the feature
becomes an exploit.**  If the crew *always* produce a useful line when
the player stalls, then **stalling becomes the way to get answers**, and
a player who learns that will stall deliberately.  That is `@X118`
violated through a back door: waiting turns into a means of making the
game supply what you do not know.

⚠ **So the honest shape is:** the crew say something only when one of
them actually has an observation their skills would produce, about the
place the player is actually standing.  When nothing matches, **nobody
speaks**, and the player is left with the problem — which is where
§ P1a wants them.

#### ⚠⚠ WHO speaks: the helper with NO TASK — and it is a better trigger than the one below  `@X142`

Owner, 2026-08-26:

> *"And because the player gives orders about what helpers should do, the
> helper that has no tasks assigned to them is the natural one to comment
> on that fact."*

⚠⚠ **This supersedes most of § The trigger is the absence of PROGRESS
below, and it is strictly better.**  That section reaches for a
heuristic — counters, a window, *when did anything last move* — and this
needs none of it, because **the game already knows who has nothing to
do.**

| | the progress heuristic | ⚠ the idle helper |
|---|---|---|
| what it reads | six counters and a tuned window | **a task list that already exists** |
| how it can misfire | a parked player is repairing, so idleness ≠ lost | ⚠ cannot: it is not a guess about the player at all |
| what the player can reason about | nothing — it is invisible | ⚠⚠ **a fact they can see and act on** |
| standing to speak | a hint system talking | ⚠⚠ **an employee with no work**, which is legitimate and true |

⚠ **It is the management loop's own feedback channel.**  `DESIGN.md` § 8
makes the player a manager who issues orders that NPC workers construct;
an unassigned worker reporting in is **exactly what a foreman hears**,
and it needed no design.

#### ⚠⚠ Idleness picks WHO and WHEN; the skill lattice picks WHAT

The two rules compose cleanly and neither needs to know about the other:

- **Idle** → this crew member has standing to speak, *now*.
- **Skills** (`@X129`) → what they say is their own domain, about where
  the player actually is.

⚠ So the ex-miner with nothing to do does not say *"I am idle"* — they
say **"there's ore on that ridge, want me to cut it?"**  ⚠⚠ The idleness
is the *occasion*; the observation is the content; and the player hears
one sentence that is simultaneously a status report, a hint and a
character note.

#### ⚠⚠ It self-calibrates, which is `@X120` and `@X135` for free

- **A player who has assigned everyone hears nothing** — there is nobody
  idle to speak.  A veteran who lands and puts the whole crew to work is
  silent by construction, with no stall detector involved.
- **A player who is drifting has idle helpers**, and they speak.
- ⚠⚠ **On landing nobody has orders yet**, so the whole crew is idle —
  which is exactly why § These remarks ARE the onboarding can promise the
  opening remark is near-certain.  **The onboarding moment falls out of
  the rule rather than being special-cased.**

⚠⚠ **And assigning work is the in-fiction OFF SWITCH**: a player who
finds the crew talkative silences them by *doing the thing the remarks
were nudging them toward*.  ⚠ The remedy for the hint system is the play
it wanted — which is the best possible shape for one.

#### ⚠ Two hazards, both cheap to avoid

| hazard | ⚠ the rule |
|---|---|
| **six idle helpers all speaking** — a chorus of complaints is the opposite of *not intrusively* | **ONE speaks.**  Pick the idle helper whose domain best matches where the player is standing; the rest stay silent even though they qualify |
| **nagging** — *"I've got nothing to do"* repeated is worse than silence | ⚠ an idle helper speaks **when there is something new in their domain to point at**, not on a repeat timer.  `@X136`'s silence rule still governs: no observation, no line |
| **busywork to shut them up** — assigning a pointless order to buy quiet | ⚠ self-limiting, and already: **helper-seconds are the real bottleneck** (`DESIGN.md` § 13), so a wasted order costs the player exactly what they were short of.  ⚠ `@X127` covers the same shape one system over |

#### ⚠ The progress heuristic below is now a FALLBACK, and may not be needed at all

The one case idleness does not cover: **every helper is busy and the
player is still drifting.**  ⚠ That is a much narrower gap than the one
the next section was written against, and it is worth building nothing
for it until it is observed — a player who has the whole crew working is
usually not the player who is lost.

#### ⚠ FALLBACK — the absence of PROGRESS, and why a naive idle detector is harmful

⚠ **Superseded as the primary trigger by § WHO speaks above**; kept
because it records a trap that any future *"is the player stuck"*
heuristic will walk into.

⚠⚠ **Standing still is WORK in this game**, which makes a naive
player-idleness detector actively harmful:

| looks idle | is actually | ⚠ nudging here would be |
|---|---|---|
| parked beside a black tower | **repairing it** — `plans/17` § T1: repair is a POSITION, so a parked player is working whether they meant to or not | the worst possible moment: interrupting the most important job in the game |
| stationary on a ridge | **planning** — reading the ground before committing a layout | interrupting § P1a, the dominant axis |
| driving in a wide arc past the haze | **scouting** — `@X123`, converting rules-knowledge into instance-knowledge | interrupting the thing the remark was going to be *about* |

⚠ **So the signal is progress, and dryopea already computes every part
of it**: ground newly seen, wallet spent, an order placed, a hex
salvaged, a repair clock ticking, something carried.  None of it is new
state — it is a handful of counters and *when did any of these last
move*.

⚠⚠ **And the off-time this fires in is already a designed phase**:
`@X022` — [`EXPLORATION.md`](EXPLORATION.md) § X2b — the game **waits**,
because the wave list does not start until the player pokes a spawn
marker.  **The recon window is unbounded and the player ends it
deliberately**, which is exactly the *"wondering what to do"* the owner
is describing, and it already exists.

#### ⚠ Bias the window LONG, because the two errors are not symmetric

- ⚠ **Too early** is a crew member talking over a player who was
  thinking — annoying, and it teaches the player to tune them out.
- ⚠ **Too late costs nothing at all**: the player solved it themselves,
  which is the outcome the design wanted anyway.

⚠⚠ **So there is no reason to be clever about detecting *planning*
specifically** — a generous window does it, because a thinking player
will have acted before it expires.  ⚠ The one-shot timer this needs is
built: `fixstep`'s `timer_arm` / `timer_spend` (plan 26 L3), which is
exact at all seven tick lengths.

⚠ **And it may read the CAMERA, where the simulation may not.**
`@X033` forbids *simulation granularity* following the camera, because
where the player looks would change the outcome.  A nudge is
**presentation**, and *where the player is looking* is a legitimate
signal that they are engaged — but ⚠ it must never feed anything the
simulation reads back.

#### ⚠⚠ BLOCKED: dryopea cannot draw text at all  `@X130`

**This is the first designed feature that genuinely requires text**, and
it turns a non-issue into a prerequisite.

⚠⚠ **And `@X137` raises the stakes on it**: with no tutorial, the crew's
remarks are the onboarding *and* — with § P2e's shared history — the
campaign's engagement.  **Two load-bearing jobs now sit behind a font
file**, which moves this from a deferred nicety to the prerequisite
worth solving early.

`DESIGN.md` § HUD and `@X097`: `graphics::draw_text` rasterises through
a `#native` call unavailable under `loft test`, **and needs a font file
this repo does not have.**  That is why the wallet is seven-segment
rectangles.  ⚠ Everything else in the design was able to route around it
— the HUD is one number, and every other signal is a colour, a shape or
a pulse.  **A spoken line cannot be.**

⚠ So this feature carries a real dependency, and it is worth stating so
nobody designs three more text features on top of it:

1. a font file in the repo, and
2. `draw_text` reachable under `loft test`, or the remark is a thing
   **no test and no `snap` can see** — which is the exact standard
   `@X097` used to refuse a text HUD.

⚠ **The cheap interim is worth considering**: a remark needs a *speaker*
and a *subject*, and both can be drawn without text — the crew member's
own vehicle indicating, and the thing they are remarking on lit or
outlined.  ⚠ That is strictly weaker (it points without saying what),
but it is buildable today and it is gateable by pixels, which the
sentence is not.

### P2d — pools, rest, and losing people  `@X113`

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

### ⚠⚠ P2f — the DEBRIEF: after a mission, the player may ask what the crew thought  `@X144`

Owner, 2026-08-26:

> *"And after a mission the helpers can be optionally briefed by the
> player to ask their opinions about what happened during the mission."*

⚠ **PULL, where § P2c's remarks are PUSH.**  In a sortie the crew speak
when they are idle and the player may ignore them; between sorties the
**player asks** and the crew answer.  ⚠⚠ That is the complete pair — *a
nudge you can ignore* and *a question you can ask* — and neither is a
wall, which is what `@X137`'s no-tutorial ruling requires of both.

Lives at the station, `DESIGN.md` § 16 § Meta-game hub /
[`SETTING.md`](SETTING.md) § Between missions.

#### ⚠⚠ This is HOW the dominant axis accumulates, and § P1 never said

§ P1a says what the player knows dominates progression, and `@X122` says
the **rules** are learnable while the instance is not.  ⚠ Neither said
**how a player who just lost a base finds out why** — and *"play again
and notice"* is a weak answer for the axis the whole design rests on.

⚠⚠ **The debrief is the delivery mechanism.**  It is where a sortie
becomes a lesson, and it is diegetic, optional and character-driven
rather than a statistics screen.

#### ⚠⚠ The fence RELAXES here, and for a precise reason

§ P6a is strict in-mission — *a remark points, it never concludes* —
because the instance is live and the player still has to decide.  ⚠⚠
**After the mission that base is finished**, so there is no open question
left for an answer to short-circuit:

| in-mission | after |
|---|---|
| *"soft ground on that ridge"* — points, does not conclude | ⚠ *"the wall came down where the ground was soft.  I'd not build there again"* — **concludes freely** |
| naming the incoming wave would be `@X118`'s failing row | ⚠ naming what the wave **was** costs nothing; it is over |

⚠⚠ **REFINED 2026-08-26 — the fence relaxes on the INSTANCE and NOT on
the RULES**, which is `@X122`'s own distinction one layer up:

| may be said freely | ⚠⚠ stays the player's to derive |
|---|---|
| **what happened on this base** — it is spent, and no live decision hangs on it | **what to do differently next time** — that is the transferable axis, and it is the thing that must be earned |
| *"the tower was black from the third wave and nobody came"* | *"you should build tighter perimeters"* |
| *"they came from the marker nobody went out to"* | *"hire another scout"* |

⚠⚠ **So the debrief's version of *points, never concludes* is: they
report, they do not PRESCRIBE.**  The owner's own statement of the goal
is the test — *"the player need their own judgement to piece together
what should be done/enhanced the next run."*  ⚠ A crew member who
recommends the fix has done the piecing together, and the player's
progression is what was spent to pay for it.

⚠ What the player therefore carries away **is** rules-knowledge — but
they **derived** it, which is the only way it counts on `@X117`.

#### ⚠ Four partial accounts of one sortie, and assembling them is still the player's job

`@X129`'s partial sensors, now pointed at a completed event:

| the ex- | their account of the same base |
|---|---|
| **security** | *"perimeter was too spread.  I could not cover the north face and the east one."* |
| **miner** | *"there was something worth cutting two hexes outside the wall.  Nobody went for it."* |
| **scout** | *"they came from the marker nobody went out to."* |
| **handyman** | *"that tower was black from the third wave on and nobody came."* |

⚠ **Every one of those is something they SAW** (`@X150`) — no line
requires knowing anything about Dryopea that the player could not have
learned on the same sortie.  ⚠⚠ *"Miners at a wall mean a mine upwind"*
would be **world knowledge they do not have**, and it stays the player's
to derive — unless the crew learned it over the campaign the player ran
(`@X151`), which is the one way it may ever be said aloud.

⚠⚠ **They may disagree, and that is free and true** — two people who were
in different places saw different things.  ⚠ The synthesis stays the
player's, exactly as it is in-mission, so the debrief does not become an
oracle just because the fence relaxed.

#### ⚠⚠ The roster is therefore a SENSOR LOADOUT — which makes hiring a knowledge decision

**The crew you brought decides what you can learn afterwards.**  A sortie
with no scout comes back with **nobody who can tell you where the wave
came from.**

⚠ That is a strong, non-obvious consequence: § P2b's template choice was
a labour decision (*who can do the work*), and this makes it **also a
knowledge decision** (*whose account will I have*).  ⚠⚠ It connects the
purchasable axis to the dominant one without either supplying the
other — you can buy a *witness*, and you still have to do the thinking.

#### ⚠⚠ And there is an empty chair, at no cost

**A helper who was left behind is not there to be debriefed.**

⚠ The person who could have told you what happened on the east side is
the person you abandoned on the east side.  ⚠⚠ That is § P2e's ledger and
`DESIGN.md` § 14's *"cost of haste"* producing a consequence **nobody had
to design** — the scramble decision reaches into the debrief and takes an
account away from you.

#### ⚠⚠ The crew are UNRELIABLE NARRATORS — and that is what keeps the relaxed fence safe  `@X147`

Owner, 2026-08-26:

> *"And again here the helpers with comment from their skills and actual
> actions in game perspective, they should be unreliable narrators of the
> mission here."*

⚠⚠ **This closes a hole § The fence RELAXES opened.**  Letting the crew
*conclude* after the mission is safe only while their conclusions are not
authoritative — a debrief that relaxed the fence **and** spoke with
authority would simply hand over the dominant axis, which is the one
thing `@X118` exists to prevent.

⚠ **Unreliable narration is the safety valve**: the player must
**triangulate**, and triangulating is doing the thinking.

#### ⚠ What unreliable means here — four sources, none of them lying

| source | what it colours |
|---|---|
| **position** | they only saw their part of it — a helper parked west has nothing true to say about the east |
| **expertise** | ⚠ they interpret through what they know: a miner reads a collapse as **bad rock**, a security officer reads the same collapse as **a breach**.  Same event, two honest accounts |
| **stake** | they were *in* it.  Somebody left outside the wall remembers that base differently |
| **salience** | the thing that nearly killed them looms larger than the thing that actually decided the base |

#### ⚠⚠ NOTHING THEY SAY IS FALSE — the skew is SELECTION, EMPHASIS and OMISSION

Owner, 2026-08-26:

> *"They will not state a wrong thing, they will just skew to their own
> perspective enough that the player need their own judgement to piece
> together what should be done/enhanced the next run."*

⚠⚠ **This is stricter than *unreliable* normally implies, and it is the
version to build.**  Every sentence a crew member says is **true**.  The
unreliability lives entirely in three places, and none of them is the
content of a statement:

| ✅ fixed | ⚠ skewed |
|---|---|
| **every statement is true** | **selection** — which true things they mention |
| | **emphasis** — how much weight each one gets |
| | **omission** — what goes unsaid, because they were elsewhere, it is not their domain, or it did not feel important |

⚠ **Worked example.**  *"Nobody came for that tower"* is **true** —
nobody did.  The skew is that the handyman offers it as **the** reason
the base fell, when the wall was also mis-sited fifteen hexes away where
he never went.  ⚠⚠ Two true accounts, different weights, and **the
player is the only one who hears both.**

⚠⚠ **Which makes the player able to trust every sentence and still not
have the picture** — the good kind of hard.  A debrief can never push
somebody toward a worse decision by asserting something false; the worst
case is an **incomplete** picture, which is the intended difficulty.

⚠ **And it makes the implementation trivially safe**: this is a
**filter over a true event list**, weighted by the speaker's domain and
by where they were.  There is no fiction generator and nothing that
invents a claim — which is exactly why it composes with `@X132`'s ledger
rather than needing a layer of its own.  Each helper narrates the subset
**they were actually present for**.

⚠ **Mechanically it is free**: a helper's account is limited to the
hexes they occupied and the events they were near, and *where a helper
was over time* is something the simulation already knows.

#### ⚠⚠ It is LEARNABLE, which is why it is a progression rather than an obstacle

**The player calibrates their crew over a campaign.**

> *"Vasquez says the perimeter was too spread.  She says that every
> time."*

⚠⚠ That is `@X122` exactly — **the crew's biases are stable RULES and
the sortie is the instance** — so learning to read your own people is
the transferable kind of knowledge, accumulating on the dominant axis
(`@X117`).  ⚠ It cannot be bought, it survives a restart in the player's
head (§ P1c), and **nobody had to author a progression for it**: it is a
side effect of the crew being people.

#### ⚠ Three rules that keep unreliability from becoming noise  `@X148`

| rule | why |
|---|---|
| ⚠⚠ **Every statement is TRUE; only selection, emphasis and omission skew** | ⚠ *"the tower went black on the third wave and nobody came"* must be true in **both** halves.  What is unfair is that he offers it as **the** reason.  ⚠⚠ An opinion is allowed **when it is built out of true things the speaker witnessed** — never a false claim, and never a fact they were not there for |
| ⚠⚠ **Bias misleads in DEGREE, never in DIRECTION** | the security officer over-weights the perimeter; they never send you to the wrong wall.  ⚠ If acting on crew advice is worse than ignoring it, players learn to ignore them — and `@X137`'s onboarding role collapses with it |
| ⚠ **Bias is CONSISTENT per character** | a bias that varies run to run is indistinguishable from lying, and it is unlearnable.  ⚠ Consistency is what turns it from a dice roll into a person |

#### ⚠ Two things it must not become

| ❌ | why |
|---|---|
| **a scoring screen** — *"4 waves survived, 12 towers built, grade B"* | ⚠⚠ `DESIGN.md` § 14 § No hard run-loss: there is **no fail screen**, and *"a run the player feels was bad is simply a run that produced meagre carryover … the difference is felt across the sequence, not announced by the game."*  A grade announces it.  **Opinions, not a scorecard** |
| **an interrogation UI** — a menu of questions, a dialogue tree | ⚠⚠ § P2g § UI-LIGHT (`@X156`) makes this a **system-wide** rule and not a local one: **there is no question list anywhere in this design**.  You go to somebody, they say their piece.  ⚠ And it must be **skippable in one action**, because `@X120`'s returning player will skip it |

### ⚠⚠ P2g — the crew are the WINDOW INTO THE BIGGER UNIVERSE  `@X152`

Owner, 2026-08-26:

> *"they are also the window into the bigger universe.  The player gets
> pushed head-first into the scenario but the helpers have a background.
> They give structure to everything and link things together to the
> 'outer world' instead.  So they can chatter on their own or between
> them.  But they can also be asked for their opinion when the player
> takes time to get to them."*

#### ⚠⚠ This answers a hole nobody had named: SETTING.md has no delivery channel

[`SETTING.md`](SETTING.md) is **~2 300 lines** of wormholes, a military
cordon, three concentric truths, an empire where interstellar shipping is
ruinous, a company that mines asteroids.  ⚠⚠ **And the game has no way to
tell the player any of it** — `@X137` refuses a tutorial, `DESIGN.md`
§ HUD refuses an overlay, and everything is diegetic.

⚠ `@X102`'s knowledge tree covers what is **discovered on the planet**.
It cannot cover the **outer** world, because none of that is lying
around on Dryopea's surface to be found.

⚠⚠ **The crew are that channel, and they are the only one available.**

#### ⚠⚠ It does NOT contradict `@X150` — the split is Dryopea vs everywhere else

| about | the crew |
|---|---|
| **Dryopea** — this planet, its robots, its trees, its ground | ⚠ **know no more than the player** (`@X150`).  First time here, same as you |
| **the outer world** — the empire, the cordon, the company, home, how anything got this way | ⚠⚠ **know vastly more, and are the only source** |

⚠ **Novices about the planet; natives of the setting.**  Both rules hold
at once, and together they say something exact: **the crew can explain
where you all came from and not where you are.**

#### ⚠⚠ Which gives BACKGROUNDS a third job, and makes them the right primitive

§ P2b's templates were already doing two things — starting skills, and
`@X150`'s habits of attention.  This is the third: **a background is a
VANTAGE ON THE UNIVERSE.**

⚠ The 2023 list is exactly a set of vantage points, which is why it is
worth keeping as authored: *colonist, earthling, **hive dweller**,
member (of a technologist family), **spacer**, trader, artist, athlete.*
⚠⚠ A hive dweller and a spacer have **different outer worlds**, so the
same universe arrives through different eyes — and the player who hired
them chose which eyes.

⚠ **And it is how the crew give a campaign STRUCTURE.**  Without them a
run is a sequence of disconnected sorties; with them, somebody keeps
linking this base to the last one (§ P2e), this planet to the empire,
and this contract to the company that took it.

#### ⚠⚠ Four channels, and they must not be collapsed into one  `@X153`

Each has a different direction, trigger and cost.  ⚠ Writing them out
because the obvious refactor — *one dialogue system* — would destroy
what makes each of them safe:

| channel | direction | trigger | ⚠ what it carries | its cost to the player |
|---|---|---|---|---|
| **idle remark** (§ P2c) | push | a helper has **no task** (`@X142`) | what they notice, **here, now** | none — ignorable |
| **crew chatter** (this section) | push, **ambient** | between themselves, overheard | ⚠⚠ **the outer world**, each other, past sorties | none — it is atmosphere |
| **ask them** (this section) | **pull, in-mission** | ⚠ the player **drives to them and stays a moment** — ⚠⚠ presence is the whole interaction (`@X156`), there is no topic list | whatever that person has to say now | ⚠⚠ **the clock and the position** |
| **debrief** (§ P2f) | pull, post-mission | at the station (`@X144`) | their account of the sortie | none — it is between sorties |

#### ⚠⚠ *"Ask them"* passes the game's own core test outright

`DESIGN.md` § What kind of game this is: *does this put something in the
player's hands at a moment when using it costs them something?*

⚠⚠ **Driving across the base to a crew member and spending time there,
mid-sortie, costs exactly what this game is made of** — the clock, the
position, the trip.  ⚠ It is the same shape as tower repair
([`plans/17`](../plans/17-tower-hot-swap/README.md) § T1): **a position,
not a keypress**, and the player who wants the conversation pays for it
in the currency the design already charges in.

⚠ And it is optional, so `@X120`'s returning player skips it without
losing anything they could not get by playing.

#### ⚠⚠ UI-LIGHT: presence IS the interaction — there is no list of questions  `@X156`

Owner, 2026-08-26:

> *"I want to keep this system UI light, so getting to them and
> interacting gives them a moment to comment but the player has no list
> of questions to ask of them."*

⚠⚠ **So *ask them* is not really asking.**  It is **being there long
enough that they say something** — and the player's only input is
**where they are and when.**

⚠ That is `DESIGN.md` § 11 § Position triggers, the deepest input rule in
the project, applied to conversation: *the player should feel they
activated something through motion, not by typing.*  ⚠⚠ **A topic menu
would be the one thing § 11 exists to avoid**, and it would need a UI
that `@X137`'s no-tutorial ruling and § HUD both refuse.

| ✅ | ❌ |
|---|---|
| drive over, stay a moment, hear what they have to say | a list of topics |
| the line appears **near them, in the world** | a dialogue panel with a speaker header |
| leave whenever | a conversation to exit |
| **it is the same verb as repairing a tower** — a position, banked over time | a `talk` key |

#### ⚠⚠ The player chooses WHO and WHEN; the crew choose WHAT

⚠ **This is `@X142`'s rule with the selector swapped**, which means the
push and pull channels are **one mechanism, two selectors**:

| | who selects the speaker | who selects the content |
|---|---|---|
| **idle remark** | ⚠ the **game** — whoever has no task | the speaker, from their own skills and vantage |
| **ask them** | ⚠ the **player** — whoever they drove to | the speaker, unchanged |

⚠⚠ **So the player never steers what is said**, and does not need to:
what they get is what that person has to say *right now*, filtered by
their skills (`@X129`), where they have been (`@X150`) and what has
happened (`@X131`).  ⚠ One system, two doors — which is the shape this
project reaches for everywhere else.

⚠ **And it keeps the debrief honest too** (§ P2f): the same rule applies
at the station.  You go to somebody, they say their piece.  ⚠⚠ **No
question list anywhere in this design.**

#### ⚠ Chatter is OVERHEARD, never delivered

⚠⚠ **Nobody explains the cordon to the player.**  Two crew members
disagree about it while driving, and the player picks it up in pieces.
That is how the setting arrives without a lecture, and it is the same
synthesis rule as everything else here — ⚠ the player assembles it, and
a fragment that explained itself fully would be an exposition dump with
a face on it.

⚠ **It must not repeat.**  A finite pool that depletes over a campaign;
a chatter line heard twice is wallpaper, and wallpaper is what players
learn to tune out — taking the idle remark's channel down with it.

#### ⚠⚠ Chatter and the NUDGE compete, and the nudge wins  `@X154`

**`@X142`'s idle remark works because speech is RARE.**  If the crew
chatter constantly, a helper speaking stops being a signal and becomes
background noise — and the onboarding role `@X137` depends on goes with
it.

⚠ **So chatter is sparse, and it yields**: while the player is stalled
and a helper has something to point at, **the nudge gets the channel**.
Atmosphere is what plays when nothing needs saying.

#### ⚠ What the crew may carry, and what stays the planet's to reveal

⚠⚠ A clean division falls out of [`SETTING.md`](SETTING.md) § The
quarantine's own three concentric truths:

| truth | who delivers it |
|---|---|
| **1. the public story** — *"a haywire AI; the planet is quarantined"* | ⚠ **the crew.**  Everybody out there believes it, so everybody in the vehicle can say it |
| **2. the military reality** — two battleships, nothing leaves without a permit | ⚠ **the crew**, partially and with rumour attached — it is the world they live in |
| **3. the hidden truth** — personality-altered AIs, the underground humans, the portal | ⚠⚠ **the PLANET.**  `@X102`'s knowledge tree, found on the ground — and `@X150` forbids the crew knowing it in advance |

⚠ **So the crew can carry a great deal of setting without touching the
mystery**, which is exactly what a window should do.

#### ⚠ And it raises the text prerequisite again

⚠⚠ `@X130` now blocks **four** things: the onboarding nudge, the
campaign's engagement, the debrief, and now the entire delivery of
`SETTING.md`.  **A font file is the highest-leverage missing piece in
the project**, and nothing else in the design is close.

### ⚠⚠ P2h — they have real PERSONALITY: class decides what they notice, voice decides how they say it  `@X157`

Owner, 2026-08-26:

> *"as characters they should show real personality.  A miner has a
> different way to respond than a scout, a hacker or a researcher."*

⚠⚠ **This is a second axis, not a restatement of § P2c.**  Everything so
far governs **what** a crew member notices — domain (`@X129`), habits of
attention (`@X150`), where they stood (`@X148`).  ⚠ **None of it governs
how they SOUND**, and four people reporting the same fact should be four
recognisably different people.

| the ex- | professional register, and where it comes from |
|---|---|
| **miner** | ⚠ **plain and physical.**  Talks in what a thing would take — *"that's two days of cutting"* — because a mine measures everything in effort |
| **scout** | ⚠ **terse, positional, present-tense.**  Reports the way somebody reports over a radio while moving: *"movement, past the ridge, three of them"* |
| **hacker** | ⚠ **oblique and amused**, interested in how a thing is put together rather than what it is for.  ⚠ [`SETTING.md`](SETTING.md) makes this the loaded one — hacking a machine's mind is the founding act of this planet's history |
| **researcher** | ⚠ **hedged and precise**, qualifies everything, will not commit past the evidence.  *"It may be the ground.  I would want to see it again."* |
| **security** | ⚠ **clipped, worst-case first.**  Names the threat before the situation |
| **handyman** | ⚠ **grumbling, particular, proprietary about the machines.**  Talks about kit as if it were owed something |

⚠ **The 2023 data already carries the classes** (§ P2b) and gives each a
one-line description — *"quite useless outside of gaining knowledge"* for
the researcher, *"technologist that uses equipment in non-intended ways"*
for the hacker.  ⚠⚠ **What it does not carry is a voice**, and that is
the new authoring field.

#### ⚠⚠ Personality reaches WORDING and EMPHASIS — never the FACTS

`@X148` is absolute: nothing a crew member says is false.  ⚠ Personality
must not become a fourth distortion on top of selection, emphasis and
omission — it slots into the ones already there:

| layer | driven by |
|---|---|
| **the facts** | ⚠⚠ **fixed and always true** — personality does not reach here at all |
| **selection and omission** | position and domain, primarily |
| **emphasis** | domain, and ⚠ *secondarily* temperament — a cautious miner mentions the fracture, a bold one mentions the ore.  Both saw both |
| **wording and register** | ⚠⚠ **purely personality** |

⚠ **So a blunt person and a hedging person report the same fact**, and
the player who has learned to read them extracts the same information
from either — just faster from one.  That keeps § P2f's triangulation
honest: **the differences are in the telling, and the telling is
learnable** (`@X147`).

#### ⚠⚠ The authoring cost is the real constraint — so it is a FILTER, not a matrix

⚠ The trap: if every line is written per **(class × personality ×
situation)**, the content budget explodes combinatorially and the feature
dies of authoring.

⚠⚠ **The affordable shape is two stages, and it is the same shape
`@X148` already uses:**

```
   situation  ──(domain: what would this class notice?)──▶  a true thing to say
                                                                    │
                          (voice: how does this person say it?)  ◀──┘
```

⚠ **Content is picked by domain; the voice colours the phrasing.**  So
authoring is **additive** — a new class costs its observations, a new
voice costs a register — rather than multiplicative.  ⚠⚠ And it is the
same *filter over a true event list* the debrief already needs, with one
more stage bolted on the end.

#### ⚠ Two rules that keep a personality from being a liability

| rule | why |
|---|---|
| ⚠⚠ **Personality changes how PLEASANT or QUICK a source is — never whether it is a source** | a player who finds the researcher's hedging tiresome may avoid them, and that is a **real choice with a real cost** (they lose that domain).  ⚠ But a personality that makes somebody's information *unusable* punishes the player for hiring them, and § P2b's template choice stops being a decision |
| ⚠ **Clashing temperaments belong in CHATTER, not in mechanics** | two crew who rub each other are excellent § P2g material.  ⚠⚠ But a compatibility table that changed **work efficiency** would be `@X131`'s affinity bar with extra steps — the thing that section refuses.  Keep the friction *audible* and out of the numbers |

#### ⚠⚠ And this is what makes the empty chair hurt

§ P2f notes that a helper left behind is **not there to be debriefed**,
and § P2e makes going back for them the campaign's best story beat.

⚠⚠ **Both of those only land if the player would MISS the person.**  You
do not miss *a scout*; you miss the one who reported in three words and
was always right about the ridge.  ⚠ **Personality is what converts the
roster from a loadout into a crew** — and without it, `DESIGN.md` § 9's
stranded-helper rescue is a fetch quest for a unit.

⚠ It is also what makes `@X151` audible as *people learning* rather than
as a counter going up: the researcher stops hedging about the ground,
because by the tenth base they have seen it three times.

#### ⚠ Individual variation on top of class — natural, and flagged as an extension

The owner's ruling is that **the class carries the voice**, which is the
cheap and legible version: hire a researcher, get a researcher.

⚠ Two people of the same class differing is the obvious extension and it
is *not* free — it needs a personality field independent of class, and
the authoring stays additive only if a voice is a **small named set**
(*cautious, bold, dry, warm, sour*) rather than a stat block.  ⚠⚠ **Never
numbers**: a *"cautious: 7"* field is the RPG-affinity trap in new
clothes, and it would need a UI that `@X156` refuses.

### ⚠⚠ P2e — the relationships are a LEDGER of what happened, never an affinity bar  `@X131`

Owner, 2026-08-26:

> *"I want a relation between the player and the helpers to develop
> through the campaign and possibly between helpers too.  Where a shared
> history gets them together and where they can remark on previous
> sorties too."*

#### ⚠⚠ The trap, named first, because it is the default version of this feature

An affinity system is normally **a hidden number the player raises** —
gifts, dialogue choices, loyalty missions, a bar somewhere.  ⚠ That
version breaks three rules of this project at once:

| it would | which rule |
|---|---|
| turn attention into a stat | `@X118` — the player pays time and the game hands back a bonus |
| need a bar, a portrait row, a relationship screen | `DESIGN.md` § HUD, which refuses every overlay it does not already have |
| become a thing you **farm** | `@X125` — the reroll objection, one subject over |

#### ⚠⚠ The version that passes: it is the SAME MECHANISM as advancement

**`@X124` says the crew get better at what they DID.  Relationships are
that sentence with one word changed: the crew get closer through what
they WENT THROUGH.**  One mechanism, two readouts — which is the *one
system, per-type data* rule this project runs on everywhere else.

⚠ **So a relationship is not raised.  It is RECORDED.**  The player never
spends anything on it; they run sorties, and the crew remember.

#### The history dryopea ALREADY generates, with no new mechanism

This is why the feature is cheap: every one of these is a fact the
simulation already produces.

| the event | already built / designed | ⚠ what it is worth as history |
|---|---|---|
| **who retrieved whom** | `DESIGN.md` § 9 — downed → carried to the core → 60 s recovery ([`plans/14`](../plans/14-helpers/README.md)) | ⚠⚠ **the strongest single bond in the game**, and it is shipped.  Somebody drove into a live kill zone for you |
| **who was left behind, and where** | § 14 — force-launch leaves stragglers, *"by design.  The cost of haste"* | ⚠⚠ the strongest **negative**, and it is the player's own worst moment |
| **who boarded** | § 14's cargo manifest, and `@X126` banks practice on it | the roster of a sortie is already a record |
| **sorties survived together** | the base sequence, § 14 | a count, not a log — see below |
| **a base that fell with you both in it** | `wallet_broke`, § 14 | the shared defeat, which is usually the better story |
| **living in the same base** | `@X113`'s pools + `MATERIALS.md`'s quarters | ⚠ and [`SETTING.md`](SETTING.md) § The recruitment already makes them **each other's only company** — no comms with orbit during a mission |

⚠⚠ **The fiction has been asking for this since the seed notes**:
*"living quarters … otherwise they only live in their vehicles.  They
can get lonely without means to communicate in the wild."*  The jammer
that charters the whole game (`@X099`) is also what isolates the crew
together.

#### ⚠⚠ The stranded helper is the campaign's best story beat, and nobody has to write it

`DESIGN.md` § 9 § Stranded helpers already says a helper **not retrieved
by force-launch** is stranded at their wreck hex, and becomes **a
rescue-quest target for the same player on a later run.**

⚠ With a shared history that stops being a fetch quest.  **You left them
because the wave was on the core and the rocket was lit.**  Going back
is a decision about a person you know, generated by the scramble
mechanic that already exists — ⚠⚠ *the signature mechanic of the game
producing its own drama, rather than a writer supplying some.*

⚠ **And the dark branch is already designed too**: § P2d's *"personnel
can join competitors when not treated well"*.  A crew member abandoned
often enough turns up on a competitor's payroll
([`SETTING.md`](SETTING.md) § The competitors) — the same ledger, read
with the opposite sign.

#### ⚠ Helper ↔ helper costs nothing extra, and is heard rather than managed

The same records answer both directions.  Two helpers who have run
eleven sorties together, or one who dragged the other back, **remark to
each other** and the player overhears it.

⚠ **That is deliberately not interactive.**  It obeys `@X129`'s *not
intrusively* — no conversation to manage, no relationship to steer, no
choice presented.  ⚠⚠ **The player's influence on crew relationships is
entirely indirect: who they send together, and who they come back for.**
That is a positioning and priority decision, which is what this game is
made of.

#### ⚠ May it have a MECHANICAL effect?  Yes — through the lattice that already exists

⚠⚠ **`social` is already a skill and `charisma` is already a statistic**
(*"getting along with others, even in stressful situations"*), so a
relationship effect is **not a new system** — it is the § P2a loop with
shared work as the practice.

The effect has to pass `@X118`, and the passing form is narrow:

| ✅ friction | ❌ answer |
|---|---|
| two people who have worked together **hand a job over without re-walking it** | a bonded pair that **unlocks an ability** |
| a crew member finishes what a familiar partner started, faster (⚠ `plans/17` § T1's repair already banks **on the tower** for exactly this reason) | a **loyalty threshold** that grants a bonus at 5 sorties |
| a helper more willing to work far from the core when somebody they trust is nearer to it | a relationship **gate** on any content |

⚠ **The first column is all coverage** — [`plans/17`](../plans/17-tower-hot-swap/README.md)
§ T3 measured upkeep as a positioning problem, and a bond that shortens
a handoff is measured in the same units.

#### ⚠ Remarks about previous sorties — the payoff, and its two rules

1. ⚠⚠ **It must name something that actually happened in THIS campaign.**
   *"Third time you've parked me next to a wall that came down"* is the
   feature; a generic line about hardship is not.  That means the ledger
   has to be specific enough to name a base and an event.
2. ⚠ **It stays PARTIAL** (`@X129`).  A crew member remembers **their**
   version — the handyman remembers what broke, the scout remembers how
   far out they were when it started.  ⚠ Two crew members recalling the
   same sortie differently is free characterisation and costs one field.

#### ⚠⚠ Keep COUNTS and LANDMARKS, never a log

The obvious implementation is an event log per pair, and it **grows for
ever** — the shape `play_view.loft` § `@X095` already refused for a
different subject (*no dirty list to grow for ever in 1397 headless
tests*).

⚠ **The bounded version**: a few counts (sorties together, retrievals,
abandonments) plus **two or three landmark events kept by name** (the
base where it went wrong, the retrieval that was close).  A campaign of
fifty sorties then costs a fixed handful of fields per pair, and the
remarks have something concrete to point at.

#### ⚠ Prerequisites, and they are real

- ⚠⚠ **Persistent campaign state across sorties**, which
  [`plans/ROADMAP.md`](../plans/ROADMAP.md) Tier D says is **not
  shipped** — *"each mission is independent; persistence isn't shipped"*.
  Crew records are per-campaign by definition, so this cannot land
  before that does.
- ⚠⚠ **Text** — `@X130`.  Relationships compound the same blocker
  remarks have, and they need it more: a bond is *expressed* almost
  entirely in what people say.

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
   already there for exactly this, and § P2d's defection needs people
   who are distinguishable enough to miss.*
2. ~~**Do skills improve WITHIN a run or only between?**~~  ⚠⚠
   **ANSWERED by § P2a**: practice accrues *during* a sortie and
   **banks when that helper boards before force-launch** — the cargo
   manifest `DESIGN.md` § 14 already keeps.  ⚠ The worry that a long
   base becomes self-reinforcing is answered by the same line: it does,
   and **only if you get the crew out**, which sharpens the scramble
   decision instead of softening it.
3. **How many of the twelve ship first, and does `combat` ship at
   all?**  ⚠ § P2's rule makes the first half cheap: ship the skills
   whose scaled number is already measured — **build, repair, scout**
   are the owner's own three examples and all three have a constant to
   multiply today.  ⚠⚠ For the second half, § P2a's twin finding says
   **probably not**: `combat` shares its whole statistic pair with
   `scout`, and a noncombatant player (`DESIGN.md` § 8) gives it almost
   nothing to scale.  *Recommendation: drop it, and leave
   observe+stamina to `scout`.*
4. **Does the player's own character have stats?**  If the player is a
   person (§ P8), they must.  ⚠ But a `drive` stat on the driven
   vehicle collides with § P1 — the player *feels* their own driving.
   *Recommendation: the player's character carries the WORK skills
   (build, repair, mine, hack) and not the movement ones; movement
   stays P1.*
5. **What is in each template?**  ⚠ § P2b's slots exist in the 2023
   schema and are **empty** — eight backgrounds and sixteen classes
   with descriptions and no numbers.  *Recommendation: author them
   against the pair table, so a template is strong on two statistics
   that share few skills — a genuine shape rather than a bigger
   number — and no template is dominant.*
6. **How fast is the slow layer?**  ⚠ The whole design rests on
   statistics rising **much** more slowly than skills; if they rise at
   comparable rates, specialisation stops meaning anything and every
   crew member converges.  *No recommendation — this is the one number
   here that needs a measurement rather than an argument, and it cannot
   be taken from the 2023 data, which carries no rates.*

## See also

- [`DESIGN.md`](DESIGN.md) § What kind of game this is — the test every
  axis answers to; § And the DEEP layers — § P6's fence, owner-stated;
  § 9 Helpers — where the per-character id already lives.
- [`SETTING.md`](SETTING.md) § The knowledge tree — axis P4.
- [`MATERIALS.md`](MATERIALS.md) — axis P3's catalogue, and § The crew
  are people for the quarters building § P2d needs.
- [`../archive/gameplay.data`](../archive/gameplay.data) — the source
  of § P2's lattice, and of the template names § P2b has to fill in.
- [`EXPLORATION.md`](EXPLORATION.md) — where P1 is practised and P4 is
  gathered.
- [`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) §§ 4-5 — the routing of the
  2023 material this file now adopts.
- [`DECISIONS.md`](DECISIONS.md) — the index, including the superseded
  codes.
