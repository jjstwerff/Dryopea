<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# EXPLORATION — it is SCOUTING, it already opens the run, and almost none of it needs inventing

*(project owner, 2026-08-15: "A big part of the game will be exploration with
your vehicle beside orders for your helpers and some direct work on tower tops",
then: "design exploration from what is already designed to explore earlier")*

⚠⚠ **This document ASSEMBLES; it does not add a pillar.**  Exploration was
already designed, under a different name, and it was already ranked first:

> **Scouting** … the player's main path to special materials, upgrades, and new
> tower types … *This makes scouting **the** progression activity (not building,
> not combat)*.
> — [`DESIGN.md`](DESIGN.md) § 13 § Scouting — the primary discovery loop

So there is nothing here to justify.  What was missing was five things, and the
X0–X2d sections are them: **what exploration is FOR** (X0 — it is where the
player learns to fly, and the base is where that cashes out), **how the player
meets it** (X2 — the run already opens with a sortie), **what its product is**
(X2b — a build decision, and the game already waits while you make it), and
**why it must happen EARLY** (X2c — a find accelerates building, and building is
what you stop having time for).

⚠ **Every section below is a JOIN of existing design.**  Where a section adds
something, it says so in its first line.  There are two such places, and both
are one row of data.

---

## X0 — progression  →  moved to [`PROGRESSION.md`](PROGRESSION.md)

⚠ **This section MOVED** (2026-08-15).  It answered *what exploration is FOR* —
the landscape is where the player learns, and the base is where that competence
cashes out — and it grew into a subject of its own.

⚠⚠ **And it was REWRITTEN on 2026-08-26, in a way that changes what this
document is written against.**  `PROGRESSION.md` used to say the progression
*was* the player's own skill with the controls and refused a character-stat
layer.  The owner has ruled otherwise: progression runs on **several axes**,
the crew are characters with skills, and the model is **Blue Prince** —
information and the player's own build choices dominate, while build speed,
repair speed and detection radius smooth a long campaign.  `@X016` and `@X017`
are **superseded**; `@X018` and `@X019` stand with a wider reading.

⚠⚠ **Two of the new rulings are ABOUT exploration and belong in view here:**

- **`@X122` — the RULES are learnable, the INSTANCE is not.**  Enough varies
  per scenario that a veteran *roughly knows what to expect and cannot predict
  everything*.  ⚠ **This is what stops a knowledge-dominant progression from
  solving the game**, and it is the reason everything below has a future: the
  world's rules are stable, the map in front of you is not.
- **`@X123` — scouting is how rules-knowledge becomes instance-knowledge, every
  run.**  ⚠ So the veteran scouts **faster and better, never less**, and § X1's
  four rings stay a gradient rather than becoming a checklist.

⚠ The pointer stays rather than the section being deleted, because everything
below is written against it: § X2c's *find it early* and § X2d's permit are both
pressures on a player who is trying to convert an unknown map into a build
decision before the clock runs out.

---

## ⚠⚠ X0b — THE FIRST FIFTEEN MINUTES ARE SCOUTING, and the world must be ALIVE  `@X245`

> ⚠⚠ **THIS SECTION DESCRIBES THE FINISHED GAME, AND IS A TEST RATHER THAN A
> TASK** (owner, 2026-08-26: *"this is the experience a player has when the game
> is finished — we are not there yet"*, `@X251`).
>
> ⚠ Read it the way this repo reads its other tests — *does this feature serve
> the opening described here?* — and **not** as a work queue.  ⚠⚠ The critical
> path is unchanged: [`../plans/ROADMAP.md`](../plans/ROADMAP.md) § The critical
> path, where **BUILDING** is still the load-bearing gap and everything below
> sits on top of it.

Owner, 2026-08-26, asked what a new player's opening actually consists of:

> *"they are squarely in the scouting phase of their first mission.  There is a
> world around them with trees, sometimes with visible insects on them, perhaps
> movement of an elemental in it, and you encounter robots in their normal work
> flow.  They can be busy with whatever project they happen to be in around the
> place.  **The landing zone is neutral** — no big mining operations, factories or
> shipping routes are there, but they are everywhere on the planet, so small-scale
> operations are happening."*

⚠⚠ **This settles the opening, and it matters because `@X238` makes the early
game the product**: most players will see nothing else.

⚠ **The opening is LOOKING, not building.**  Building comes after — it remains
[`../plans/ROADMAP.md`](../plans/ROADMAP.md)'s biggest missing mechanic — but the
first thing a new player does is **read a living world.**

### ⚠⚠ Which means the opening needs THREE things, and only one of them exists

| the opening needs | ⚠ status today |
|---|---|
| **a world to look at** — robots at work, insects on trees, an elemental moving | ⚠⚠ **SHIPPED 2026-08-27** — BACKLOG B4 (`@X276`).  Entities *draw* ([`plans/20`](../plans/20-entity-art/README.md) A5), and `traffic <rate>` now puts robots on a road that walk their heading across the map and go on their way.  ⚠⚠ **It needed no new mover**: `ROBOT_ECONOMY.md` already said *the traffic is the waves*, and `enemy_walk_heading` had walked one along its business since plan 11 F5b — what was missing was that a business never ENDED and nothing ever STARTED one.  ⚠ **The bubble decides what it becomes**: a road that runs PAST is scenery, one that runs THROUGH is a wave (`@M053`).  ⚠ What is left is CONTENT — a map with roads on it — and insects and elementals, which are not built |
| **somebody to point at it** — the crew's remarks | ⚠⚠ **NO LONGER BLOCKED** — BACKLOG B1 shipped text 2026-08-27 (`@X268`), so `src/font.loft` can draw a line and this stopped being the gate on the opening.  ⚠ What remains is the remark DESIGN itself (`@X129`'s *point, never conclude*) and where a line is composited (`@M047`) |
| **somewhere to go** — the gradient | ⚠ **exists**: § X1's four rings, plus authored markers |

⚠⚠ **So ambient life is what the finished opening most depends on** — and
⚠ **that is a statement about the destination, not about what to build next**
(`@X251`).  It is not on the critical path, and it should not be moved onto it
on the strength of a description.

### ⚠⚠ *"Robots in their normal work flow"* is `ROBOT_ECONOMY.md` arriving early

⚠ The fiction has said this from the start —
[`SETTING.md`](SETTING.md) § They were on an ERRAND, and § Nobody is attacking
anybody — yet.  ⚠⚠ **What is new is that it must be VISIBLE in the first fifteen
minutes**, which makes a small piece of
[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) an **opening** requirement rather than a
Tier E one.

⚠ It is also the cheapest possible version of that document: **not the six
installation types and their routes, but a handful of robots doing small work
near the landing zone.**  ⚠⚠ *"Busy with whatever project they happen to be in"*
— the readout (`ROBOT_ECONOMY.md` § How the player ever learns any of this) can
come later; **the sight of them cannot.**

### ⚠⚠ THE LANDING ZONE IS NEUTRAL — and that is a load-bearing rule  `@X246`

*"No big mining operations, factories or shipping routes are there, but they are
everywhere on the planet, so small-scale operations are happening."*

⚠⚠ **This is a difficulty and legibility gradient built into the GEOGRAPHY**,
and it does four jobs at once:

| it gives | |
|---|---|
| ⚠⚠ **a safe place to learn** | which is what makes `@X137`'s no-tutorial ruling affordable — § X2b's recon window is unbounded *because nothing near you wants anything* |
| **small things first** | a lone robot at a small job is **legible**; a factory's traffic is not.  ⚠ The player learns to read robots on easy examples |
| ⚠ **a reason to LEAVE** | the interesting things are elsewhere by construction, so the sortie outward is the geography's own suggestion rather than a prompt |
| **a mechanical reason the run starts when it does** | `wave_provoke_step` needs a vehicle **12+ hexes out**, and a neutral landing zone is why that is a journey rather than an accident |

⚠ **It also fits `@X225`'s generation rule**: *neutral landing zone, installations
elsewhere* is an **assembly constraint** a generator can hold, and one an author
can hold too.

### ⚠⚠ THE BRIEFING — the military tell you when to be gone  `@X248`

Owner, 2026-08-26:

> *"the player has got a short in-world briefing from the military about when
> they should be gone there, with the possibility to ask the military one or two
> questions (in the same way the helpers can be asked)."*

⚠⚠ **This is how the PERMIT arrives**, and § X2d already makes the permit the
run's clock.  ⚠ It is delivered **by the people who enforce it** —
[`SETTING.md`](SETTING.md) § The quarantine: *"permit on file = pass; permit
missing = destroyed"* — which is the most legible possible source.

⚠ **A briefing is not a tutorial** (`@X137`): it is a character stating a fact
about a contract, and it explains nothing about how to play.

#### ⚠⚠ And it is the FIRST *ask*, which teaches the mechanic by being one

⚠ `@X156` makes **presence the interaction** — you go to somebody and they say
their piece, with no topic list anywhere in the design.

⚠⚠ **The briefing is the player's first instance of that**, before anything is
at stake, with real content rather than an example.  ⚠ *"One or two questions"*
is a **budget, not a menu**: the player learns immediately that **speech is
scarce and worth spending**, which is exactly the expectation `@X154` needs for
the crew's remarks to read as signal later.

⚠ It is also `@X247`'s landing choice in miniature — **a limited, irreversible
spend at the moment the player knows least.**

#### ⚠⚠ The military are a NARROW source, and deliberately so

⚠ `@X150` makes the crew novices about Dryopea and natives of the setting.
⚠⚠ **The military are the sharpest version of that split:**

| they know | they do not |
|---|---|
| ⚠ **the cordon, the permit, the rules, the exit** — in authoritative detail | ⚠⚠ **the planet.**  They watch from orbit and *"barely intervene on the surface"* |
| `SETTING.md`'s truth **2** — the naval-blockade reality — as their own job | ⚠⚠ truth **3**, which they would **deny** if asked (`@X155`) |

⚠⚠ **So they are a source who knows a great deal about one thing and nothing
about what will actually matter** — which is characterful, useful, and quietly
tells the player that the people in charge do not understand this place.

#### ⚠ OPEN — how does the player TRACK the permit afterwards?

⚠⚠ `DESIGN.md` § HUD says the **wallet is the only number**, and refuses a
countdown.  So the briefing tells you *when*, and then there is **no clock on
the screen** to watch.

⚠⚠ **The proposal, and it is already written down for another reason:**
[`SETTING.md`](SETTING.md) § The quarantine puts **two battleships in the sky**
as ambient atmosphere — *"the sky just has them, slowly traversing the upper
view."*

> ⚠⚠ **The cordon tells you when to be gone, and the cordon is VISIBLE
> OVERHEAD.  The clock and its enforcer are the same object.**

⚠ A traversal is a period; where they are is how long you have.  ⚠⚠ **Diegetic,
no second number, and it makes an existing piece of atmosphere load-bearing** —
which is the same move `@X098` made when the wallet's colour became the warning.
*Recorded as a proposal; the owner's to rule on.*

### ⚠⚠ THE LANDING LOADOUT — the scrambler OR two towers, and walls always  `@X247`

Owner, 2026-08-26:

> *"with the landing the player has the opportunity to activate the scrambler or
> build two towers.  **The helpers are always able to build walls.**"*

#### ⚠⚠ Walls are UNCONDITIONAL, and that is what makes every other choice survivable

⚠ `DESIGN.md` § 13 already prices walls at **free in points — helper-seconds is
the bottleneck**.  This makes it absolute: **the crew can always dig, from the
first minute, whatever else went wrong.**

⚠⚠ **So walls are the base's FLOOR**, and it is the same shape as `@X211`'s *the
planet is the primary tap* one scale in:

| the always-available thing | what it guarantees |
|---|---|
| ⚠⚠ **the planet** (`@X211`) | a player peripheral at the station still has the whole game |
| ⚠⚠ **walls** (this) | a player who spent badly at landing **still has a defence** |

⚠ **Which is why the landing choice can be sharp without being punishing**: spend
wrong and you are worse off, never defenceless.  ⚠⚠ And `@X238` makes that
matter — the early game is what **most players will ever see**, so its floor has
to hold.

#### ⚠⚠ And it sharpens `@X140` rather than removing it

⚠ The wall **capability** is never gated; the wall **expression** still is —
paint mode is a key (`Q`), and `@X140` names it the design's worst
discoverability risk.

> ⚠⚠ **So the risk was never that the player cannot build.  It is that they
> never learn they can.**

⚠ Which is precisely what a scouting opening plus an idle crew member exists to
solve (§ above) — and it means the fix is **a remark, not a mechanic.**

#### ⚠ The opening choice itself — *scrambler or two towers*

⚠ The scrambler **is** the core (`@X099`, `DESIGN.md` § 4), so activating it is
what makes robots converge — **income and pressure at once** — while two towers
are **defence with no income yet.**

⚠⚠ **Read that way it is the design's own test at minute one**: *something in
the player's hands at a moment when using it costs them something*, and the cost
is the other option.  ⚠ It also gives the seed notes' *"it is possible to turn
off the jammer, with an impact on attack waves"*
([`../plans/ROADMAP.md`](../plans/ROADMAP.md) Tier B) its first and most
important use — **the switch matters most before it has ever been on.**

⚠ **OPEN — is *or* exclusive?**  A landing rocket carries limited weight and
volume ([`SETTING.md`](SETTING.md) § Mission), which would make it a genuine
either/or; it could equally be a list of what the initial budget affords.
*Recorded as read, not decided* — but ⚠⚠ **the exclusive version is the better
game**, because it puts an irreversible decision before the player has seen the
ground, which is the one moment they know least and the recon window is what
they spend to fix it.

##### ⚠⚠ MEASURED 2026-08-28, and it is the SECOND reading  `@X288` `@M065`

**There is no `or`, because the two halves share no scarce thing.**

- **The scrambler costs nothing and is already on.**  `jammer_new()` returns
  `off: false`, so **a base lands jamming and the player does not activate it**;
  switching it costs no points in either direction.  Spend every point on towers
  and the scrambler is still on — measured, in
  [`../tests/d2_the_landing_choice.loft`](../tests/d2_the_landing_choice.loft).
- **The towers are already exclusive, and the WALLET does it.**  200 points over
  a 100-point beacon is exactly **two**, `beacon_buy` refuses the third, and
  *"failed deposit does not refund"* makes the spend irreversible at the moment
  it is made.

⚠⚠ **So the design's test at minute one is already met — by arithmetic, between
the things the wallet buys** — and the exclusion this section wanted did not have
to be invented.  ⚠ The paragraph above is right that *the exclusive version is
the better game*; what the measurement changes is **where the exclusivity lives**.

⚠ **And the wallet is sized for a purchase that is not built**:
`numbers.json` § helper.order_cost_points is 100 and **nothing reads it**, so the
documented *1 tower + 1 helper* is *two towers* in the game that exists — as are
`DESIGN.md` § 15's free starter tower and two helpers.

⚠ **What a ruling would still be ABOUT** is the STATION's pre-mission manifest
(`DESIGN.md` § 16 — *"the player picks from inventory which specialised tops to
take down"*): that is a limited-weight loadout, it is where an exclusive pick
belongs, and it is gated on carryover and on a station that does not exist.

### ⚠ And it resolves `@X140`'s sharpest hazard without a tutorial

⚠⚠ `@X140` names **Q** — the wall-paint toggle — as the worst discoverability
risk in the design, because a player who never presses it never builds and never
plays the actual game.

⚠ **A scouting opening is the answer**: the player has *time*, nothing is
attacking them, and the crew have every opportunity to point at the ground before
anything is at stake.  ⚠⚠ **The hazard is mitigated by the SHAPE of the opening
rather than by anything being explained** — which is exactly what `@X137`
requires.

---

## X1 — the four rings already ARE the exploration gradient

dryopea already has four concentric radii, each designed for its own reason, and
together they are a difficulty curve nobody drew on purpose:

```
 0 ────────10────12──────────────25──────────────────────40 ──►
 core       │     │               │                       │
            │     │               │                       └ atmosphere_haze_radius
            │     │               │                         — the edge of what you SEE
            │     │               └ SCRAMBLER_BUBBLE_RADIUS
            │     │                 — inside, the field steers; outside, headings do
            │     └ WAVE_1_PROVOCATION_HEXES
            │       — a marker out here wakes the wave list
            └ SPAWN_DISABLE_RADIUS
              — markers inside here never fire ("this won't fire on you")
```

⚠ **Read outward, that is a sortie.**  Under 10 is safe by construction; 10–12
sends enemies but cannot be poked; 12+ both; 25 is where the game stops steering
enemies and they simply walk; 40 is where the base goes out of sight.

⚠⚠ **And the simulation is CHEAPEST exactly where exploration happens.**  The
distance field is only read inside the bubble (`plans/22` § four measured facts)
— outside it, `enemy_tick` takes the heading branch and never asks.  So a player
at radius 35 is in the part of the world that costs almost nothing to run.  That
is a coincidence worth banking rather than a design: **exploring outward does not
buy simulation cost the way a bigger base would.**

---

## X2 — the run ALREADY opens with a scouting sortie  `@X021`

**This is the whole of "explore earlier", and it needs no mechanic at all.**

[`plans/16`](../plans/16-the-wave-system/README.md) W3 shipped `wave_provoke_step`:
the wave list does not start until **a live vehicle stands on a spawn marker 12
or more hexes from the core**.  Nothing else starts it.

⚠ So the first thing a player does in every run is *already* drive out past the
haze-lit ring and poke something.  **Exploration is not late — it is literally
turn one, and it is a prerequisite for the game beginning.**

⚠⚠ **What is missing is not a phase; it is CONTENT ON THAT DRIVE.**  Today the
provocation trip is a bare errand: 12 hexes out, nothing on the way, drive back.
Put one find between the core and the first spawn marker and the player meets
scouting in the first thirty seconds, having been given no instructions and no
UI.

That reframes the whole ask: **the cheapest way to make exploration early is to
author it onto a trip the player already takes**, not to add a mode, a map layer
or a phase of play.

---

## X2b — the sortie is RECONNAISSANCE, and its product is a BUILD DECISION  `@X022` `@X023`

*(project owner, 2026-08-15: "exploration stays as the first focus for advanced
player, they need the information gathered there to formulate a plan for their
own base, what will they encounter in the next half hour")*

⚠ **"The next half hour" is the base session.**  `docs/NUMBERS.md` § Design
targets sets *"a single base session ≈ 15-25 minutes"*.  So the sortie is not
meant to tell the player about the next minute — it is meant to predict **the
whole session**, which is a hard requirement on the intel: it has to be specific
enough to change what gets built.

### ⚠⚠ The game ALREADY waits, and nobody designed it as a recon window

[`plans/16`](../plans/16-the-wave-system/README.md) W3: the wave list does not
run until a live vehicle stands on a spawn marker 12+ hexes out, and
`wave_provoke_step` is the **only** thing that starts it.

⚠⚠ **So there is already an unlimited, free reconnaissance phase that the player
ENDS DELIBERATELY.**  Land, look at everything, decide, then go and poke
something.  That is not a feature anyone asked for — it fell out of W3's trigger
— and it is precisely the window this design needs.  ⚠ It also means *"exploration
is the first focus for an advanced player"* is not advice: **it is already the
optimal opening**, and the code has supported it since plan 16.

### What the intel has to convert INTO

Intel is only worth gathering if it changes a build, and dryopea already prices
the builds it would change:

| what the sortie could tell you | the decision it changes | already measured |
|---|---|---|
| **which robot classes** the local nodes feed | wall vs tower — the four classes *"differ a lot in how fast they chew a wall, and they differ in NOTHING else"* | the per-class figure is `numbers.json`'s, unbuilt |
| **how many approaches** the terrain allows | where the perimeter goes, and whether to seal it | a sealed wall nearly **doubles** the clock; a gate buys **nothing** (plan 12 B7) |
| **where a stockpile is** — ⚠ the thing to avoid waking | which way *not* to go, for the whole session | `ROBOT_ECONOMY.md` § 4 |
| **how far the spawn markers sit** | which marker to poke first, and therefore when the clock starts | `WAVE_1_PROVOCATION_HEXES` = 12 |

### ⚠⚠ The hole this section named is CLOSED IN CODE and open in CONTENT

> ⚠⚠ **CORRECTED 2026-08-29.**  This section used to read *"`ENEMY_KIND_REGULAR`
> is the only class the game emits… that makes the four classes the FIRST thing
> this design needs built"*.  **They were built by
> [`plans/23`](../plans/23-the-small-robots/README.md), K0-K3** — so the
> sentence that routed work to them is stale, and it was the stated blocker on
> everything else on this page.

⚠ **What shipped, and it is more than the classes**: scout, harvester, builder
and miner have their own wall-damage rates (`@M011`: 23 / 35 / 50 / 96 / 454
ticks) and their own speeds (`@M016`: nine hexes in 6 / 9 / 14 ticks), a
`.keys` file composes a mixed wave (`compose 1 4 miner 8 scout`, `@X055`), and
**`wave_schedule_step` spawns each part in its own class** — `WaveSchedule`
carries `vector<WavePart>` and every part has a `kind`.  ⚠ So there is no code
path left that flattens a wave to one symbol.

⚠⚠ **What is still one symbol is the AUTHORED CONTENT.**
`examples/waves.json` is `"waves": [5, 8, 12, 20, 30, 50, 80]` — a bare
`vector<integer>` — and `waves.loft::regular_parts` turns each entry into an
all-`ENEMY_KIND_REGULAR` part, *"what a `vector<integer>` list means"*.

⚠⚠ **It was a bare vector because the JSON cast could not carry a
`vector<Struct>`, AND THAT IS NO LONGER TRUE** (`@M088`, probed 2026-08-29):
the whole cast family is fixed on both backends — twelve declared fields with
two `vector<Struct>` fields read correctly, declared defaults survive a cast
([loft#876], closed), and the native backend no longer answers an empty
vector ([loft#866], closed).

⚠⚠ **DONE 2026-08-29 — `examples/waves.json` carries a `parts` column and
`wave_schedule_new` builds from it**, so the schedule every `WaveState` is
born with is composed and **the game sends more than one symbol at the
player**.  A `schedule` line still means a flat list, which is what keeps
every scenario in the corpus meaning what it meant.

⚠⚠ **And the measurement inverted the obvious expectation** (`@M089`,
`@M090`).  The first composition made the base last **63 ticks LONGER** —
452 flat against 515 — because it is `@M020` cashed at the level of a wave's
CONTENTS: the siege front is the wall's WIDTH, so a fixed number of robots
chew at once and ***a wave is decided by the WORST class holding a slot***.
A scout moves 2.5 hex/s against 1.5 and does **0.1 wall damage against 1.0**,
so scouts arrive first, take every slot and chew at a tenth of the rate.
⚠⚠ **And more bodies make a wave WEAKER** — the counts plus extra scouts read
641 against the flat 452 — so a wave cannot be strengthened by adding to it,
only diluted.

⚠ **Swept as pure lists: all-regular 452, all-builder 452, all-scout 671,
all-miner 752, all-harvester never falls.**  So `builder` is the one class
that joins a wave for free, and the shipped composition is regular + builder
at **452 against 452** — the mix costs nothing.  ⚠⚠ **A richer mix is a RATES
problem, not a tuning one**, and rates are balance, deferred to the economy
(item 10).

⚠ The same probe frees `MapFile`'s six-field cap and
[`plans/01`](../plans/01-ground-editor/README.md) E4's *"expanded once loft
JSON-cast bugs land"*, and neither has been widened.  ⚠⚠ Until then
*"composition is a readout"* (`ROBOT_ECONOMY.md` § How the player ever learns
any of this, layer 3) is true of every scenario in `tests/scripts/` and false
of the shipped wave list, which is a **content** gap wearing a design gap's
clothes.

⚠ And `@M020` is what makes the readout worth having at all: the siege front is
the wall's WIDTH, so **a wave is worth its front class PLUS what the front
cannot cover** — before [`plans/24`](../plans/24-the-siege-front/README.md)
every mix landed within four ticks of a pure wave of its fastest class
(`@M018`, retired).

### ⚠⚠ What phase 2 measured — the racing line is real, and it is not the WALL  `@M093`

⚠⚠ **Run 2026-08-30, and it answers this section's own claim** — *the sortie is
RECONNAISSANCE and its product is a BUILD DECISION*.  If two bases with the same
defences arranged two ways played the same, intel would convert into **nothing**
and exploration would be a fetch quest.  It does not:

| | ticks | against the tight column |
|---|---|---|
| `a-base-drawn-in-tight.keys` — front ±5, towers ±4 | **170** | — |
| `a-base-drawn-out-wide.keys` — front ±14, towers ±13 | **207** | **+37** |
| `a-tight-base-with-no-towers.keys` | **102** | — |
| `a-wide-base-with-no-towers.keys` | **102** | **+0** |

⚠ Everything else is held: eight wall hexes and their face width (`@M020`),
their shape and so their bracing, one painted kind for the footing (`@M061`), a
list that ramps so the front cannot saturate (`@M085`), and — the load-bearing
one — **the spawn markers at ±24 and the core at (0, 0) in all four, so a robot
walks 24 hexes to the core whichever file it is in.**  Moving the front outward
shortens the approach to it by exactly what it lengthens the walk behind it.
*A pair that did not null that would be measuring the ROAD and calling it the
layout.*

⚠⚠ **And the null is the finding.**  Moving the whole front line nine hexes
outward — eight wall hexes and both crew with them — buys **0.0** once nothing
on the map shoots.  ***What a layout is worth is not where the wall is; it is
how far from the core the fight happens, and a base with nothing that shoots has
no such distance to sell.***

⚠⚠ **The towers were never the variable either**: all four cells burn **all 30
shots each**, so the sixty shots are identical and only their geometry moved.
⚠ The swept 2×2 separates the halves and they are additive — front ±5 with
towers ±13, and front ±14 with towers ±4, **both read 187**.

⚠ **What this does NOT say** is which layout a player should want.  207 beats
170 *on this base*, whose approach is long and whose crew are ordered to stand
still.  § P7's other half — a compact base being cheaper to SERVICE on a
20-second repair clock — needs a crew that shuttles, and nothing here measures
one.

⚠⚠ **And the swept factorial found something bigger than the layout** (`@M094`,
recorded here because it came out of these fixtures and belongs to combat rather
than to exploration): deleting the walls and the towers in turn reads **neither
95 / 95, wall only 102 / 102, towers only 250 / 324, wall AND towers 170 / 207**
— so **a wall standing in front of a tower costs more than it buys**, by 80
ticks tight and 117 wide.  ⚠ The layout null survives it: **0 in both
tower-free rows.**

### ⚠ The open problem: free unlimited recon is a lean-back

`CLAUDE.md`'s first design rule is that **the player cannot lean back**.  A recon
phase with no clock on it is, on its face, exactly that: an optimal player scouts
the entire map every single time, at zero risk, and the opening becomes a chore.

⚠ **The resolution is that the pressure is missing for a REASON that is
temporary.**  Nothing competes for the pre-wave window today because **building
does not exist yet** — [`plans/19`](../plans/19-the-interactive-loop/README.md)
§ What this plan does NOT build: *"walls and towers are placed in the EDITOR
today"*.  The beacon ferry, tower orders and wall-paint are all designed
(`DESIGN.md` § New towers via beacon ferry, § 13 Economy, § Wall paint) and none
is built.

⚠⚠ **Once building lands, the pre-wave window becomes a BUDGET** split between
looking and building, and *that* is a decision with a cost — which is the design
test passed.  Until then the only thing bounding recon is the player's patience,
and that is a weak answer honestly labelled rather than a mechanic to invent now.

*Decision: do not add a recon timer.*  ⚠ A clock on the opening would pre-empt
the building mechanics that are supposed to create the pressure, and W3 chose a
POKE over a timer deliberately.  If building lands and the opening is still a
chore, the fix is **a reason to start** — not a limit on looking.

⚠⚠ **§ X2c and § X2d are the reason to start, and the paragraphs above are
SUPERSEDED by them.**  They were written first and left the pressure as *"weak,
honestly labelled"* — waiting on building to land.  ⚠ That was wrong for a
reason worth keeping: the pressure was already in the FICTION
(`DESIGN.md` § 2's *permit-bound sortie*), and I looked for it in the mechanics
only.  § X2d has it.

---

## X2c — a find is a BUILD ACCELERANT, and its value collapses once you are busy  `@X024` `@M006`

*(project owner, 2026-08-15: "the things they can find are possibly ways to
quicker build certain parts of their base, so they still want to find those early
instead of when they are already too busy with other things")*

⚠⚠ **This resolves § X2b's open problem without adding a mechanic.**  The
pressure to scout early is not a clock — it is that **the reward is worth less
the later it arrives**, because what it accelerates is something you only have
time to do before the waves start.

### ⚠ It is the design's own signature, inverted

`CLAUDE.md`'s opening rule describes a game made of decaying things: *"a tower's
budget decays per shot … salvage decays, so it must be collected at the worst
moment"*.  A build accelerant is the same shape with the sign flipped:

| | what decays |
|---|---|
| salvage, tower budget | **the thing itself** decays, so you must fetch it at a bad moment |
| a build accelerant | **the opportunity to use it** decays, so you must fetch it at a *good* one |

⚠ That is a genuinely different pressure from anything dryopea has, and it is the
one that makes an opening interesting rather than a chore: *go now, while going
is cheap*.

### ⚠⚠ The decay is not a hypothesis — it is this repo's most repeated measurement

The same errand has been priced twice, and the two numbers differ by everything:

| measurement | worth | why |
|---|---|---|
| [`plans/16`](../plans/16-the-wave-system/README.md) § W4 | **one tick** | the crew member came back at tick 187 — *"the JOB is gone by the time they return"* |
| [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 | **+76 points** | a base with upkeep, where there was still a job |

⚠⚠ **So "find it early or it is worth nothing" is already a measured property of
this game**, discovered twice, by accident, while measuring something else.  This
design is not introducing that pressure — it is naming it and building content
that uses it.

### What is already designed that ACCELERATES building

Nothing below is new; this is the inventory a find can draw from:

| accelerant | already designed as | why finding it early compounds |
|---|---|---|
| **a crew member** | `DESIGN.md` § 9 — and § Future skills gives **"Building — faster wall / tower construction"** as a per-helper skill | more hands for the whole session; and a helper *"handles any remaining construction time"* on every beacon (§ New towers via beacon ferry) |
| **points** | § 13 Economy — tower orders and helper orders are **100 pts each** | points found before wave 1 are towers standing when it arrives |
| **materials brought back** | § 13, and `ROBOT_ECONOMY.md`'s nodes | the input side of the same thing |
| **a tower TYPE** | § Future tower types — *"found on the map, brought back to the core, become orderable from then on"* | ⚠ the one that pays across RUNS rather than this session (§ Open 1) |

### ⚠ So § X7's stranded helper is doubly right, for a second reason

§ X7 recommends a stranded helper as the first find because it needs **no code at
all**.  Under this section it is also the *correct* find on its merits: a crew
member is literally *"a way to build certain parts of your base quicker"*, and it
is the one find whose value **compounds** — it builds, it scouts, it repairs and
it salvages for the remainder of the session.

⚠⚠ **THAT LAST CLAIM IS THE ONE PHASE 1 REFUTED** (`@M092`, § What phase 1
measured below): a crew member is the one find on this list whose value does
**not** compound, because it is the one that is **CONSUMABLE** — it is killed in
the gate it is sent to, in both the early and the late column.  ⚠ The three
others on the table buy something that stays.

⚠ **And its recovery clock is what makes early-vs-late bite.**  A retrieved crew
member rejoins after **exactly 90 ticks** ([`plans/15`](../plans/15-the-carry-model/README.md)
§ C2).  Found in the opening, that clock runs out before the first wave and costs
nothing; found at wave four, it runs out into a base that is already falling —
which is plan 16 W4's *one tick*, exactly.  ⚠⚠ **Measured, that is not what
happens**: the 90 ticks are real and they are not the pressure — a body delivered
at 181 stands up at 272 and is worth **more** than one delivered at 21, so the
clock delays the reward without discounting it.

⚠ **This is the first content in dryopea whose value the existing corpus can
already measure**, and § The order of work phase 1 is what would do it.

### ⚠⚠ What phase 1 measured — a sortie PAYS, and the decay runs the OTHER WAY  `@M092`

⚠⚠ **Run 2026-08-30, and it inverted the order this section predicts.**  One
base, one ramping wave list, one stranded crew member authored at (14, 0) on the
road to the spawn marker; the only difference between the three files is when the
player picks them up:

| | ticks | against the control |
|---|---|---|
| `a-find-nobody-fetched.keys` | **248** | — |
| `a-find-fetched-on-the-way.keys` — two presses on the opening trip | **322** | **+74** |
| `a-find-fetched-late.keys` — a trip of its own at wave three | **364** | **+116** |
| *(control)* the same crew member GIVEN at tick zero — no wreck, no trip, no recovery | **327** | **+79** |

⚠⚠ **The control is not a ceiling**: the late fetch beats the free version by
**37 ticks**.  ***A body given early is a body spent early.***

⚠⚠ **So the find is worth HALF AS MUCH AGAIN fetched late — and it buys that
with HALF THE LIFE.**  Both bodies are killed in the gate they are sent to:

| delivered | stands up | killed | alive | worth |
|---|---|---|---|---|
| tick 21 | tick 112 | 187–202 | **~80 ticks** | **+74** |
| tick 181 | tick 272 | 302–317 | **~40 ticks** | **+116** |

⚠⚠ ***What a body is worth is the PRESSURE it stands against, not the time it
stands*** — about **0.9 ticks of base per tick alive** early against about
**3** late, because the list ramps and the last waves are three times the first.
A stranded crew member is a **BODY and not a structure**, and a body is *spent by
being used*, so spending it early spends it on the cheap half of the base.
***What decays is not the opportunity to use the find — it is the find
itself.***

⚠ **The section is not wrong about the pressure, it is wrong about the
mechanism.**  Its own inventory names four accelerants and this measurement
tested the one that is CONSUMABLE.  A find that buys something PERMANENT — points
spent on a tower, a wall raised, a tower type unlocked — still decays exactly as
written, because a structure standing early is a structure standing for the whole
session.  ⚠⚠ **So the rule this section wanted is sharper than the one it
states: *a find decays with lateness only to the extent that what it buys is
PERMANENT*.**

### ⚠⚠ And the late fetch's real price is a RISK, not a discount

⚠ The fetch tick was swept at **+0, +20, +40, +60, +80, +100, +120 and +140**
past the opening trip, and **four of the eight FAILED**: the player is killed
carrying the wreck and `vehicle_respawn` clears the carry (`plans/26` L2 — *what
reappears at the core is a repaired vehicle rather than the wreck that was
destroyed*), so the find goes back on the ground and the trip bought nothing.
⚠⚠ **Tick 141 works only because it is a gap between waves 2 and 3.**  So the
late sortie's expected value falls because the trip stops being SURVIVABLE, not
because the reward shrinks — which is `CLAUDE.md`'s design test in its strongest
form: at wave three, *using it* costs the vehicle.

### ⚠⚠ And the BUILD half is not measurable at all yet — which is an ORDERING finding

⚠⚠ **`@X024`'s mechanism is *more hands, early, to BUILD*, and dryopea cannot
price that today.**  § X2b's own discovery is why: `@X022`'s recon window is
**free and unlimited** — the list does not start until the player pokes a spawn
marker — so a second builder before wave 1 saves the player's *wall-clock* and
not one game-tick.  A build accelerant has nothing to accelerate against.

⚠⚠ **The pressure this section needs is the PERMIT's** (§ X2d, `@X025`,
[`ROADMAP.md`](../plans/ROADMAP.md) item **8**).  ⚠ That does not reorder the
roadmap — finds still pay, and by a lot — but it qualifies it: **the incentive to
scout EARLY does not exist until something is running while the player builds**,
so item 8 is what turns exploration from *worth doing* into *worth doing NOW*.

---

## X2d — the PERMIT is the run-level clock, and the fiction already gave it teeth  `@X025`

*(project owner, 2026-08-15: "they have a time window before they need to get
back into space, so there is always a pressure to be efficient, but aspects about
the scenarios they run will always be unknown to them so they need to explore to
be efficient")*

⚠ **This was already designed and I nearly wrote it as new.**  It is in three
places, and together they are a run-level deadline with a reason:

> The player is a field-head of a small mining cooperative, hired on a
> **permit-bound sortie** into a planet sealed by a military cordon.
> — [`DESIGN.md`](DESIGN.md) § 2 The pitch

> The "permits" + **"limited-time sorties"** structure could even be the
> government's way of probing for what's actually down there.
> — [`SETTING.md`](SETTING.md) § History

> They **WILL shoot anything that leaves the planet without their knowledge or
> permit.**  This is the cordon's actual teeth: **orbital exit is the
> chokepoint.**  … permit on file = pass; permit missing = destroyed.
> — [`SETTING.md`](SETTING.md) § The quarantine

⚠⚠ **So the clock is not a game-design timer bolted on for pressure — it is the
cordon's rule, and the cordon's teeth are placed exactly at the exit.**  The
permit is what authorises an ascent, and it is time-bound.

### ⚠⚠ This closes § X2b's open problem properly

§ X2b called free unlimited recon a lean-back and could only offer *"the pressure
is missing for a reason that is temporary"*.  It is not missing.  **Every minute
spent looking is a minute off the permit**, and looking, building, defending and
salvaging all spend the same scarce thing.

⚠ **And the two-sided pressure is the design's sharpest moment yet:**

| | |
|---|---|
| you cannot afford to scout | it spends the run's only scarce resource |
| you cannot afford **not** to | *"aspects of the scenarios will always be unknown"*, and ignorance makes every other minute inefficient |

That is `CLAUDE.md`'s design test — *something in the player's hands at a moment
when using it costs them something* — operating at the level of the whole
session rather than a single mechanic.

### ⚠⚠ How the player TRACKS it — measured, and the proposal is out by 0.96°

`@X250` proposed that the cordon be its own clock: `SETTING.md` § The
quarantine already puts two battleships in the sky *"slowly traversing
the upper view"*, so **the thing that tells you when to be gone and the
thing that enforces it would be the same object** — diegetic, no second
number, existing atmosphere made load-bearing.

⚠⚠ **The game's default frame contains no sky** (`@M064`).  The follow
camera sits at `CAMERA_FOLLOW_ELEVATION` 30.96° with a 60° vertical fov,
so the top edge of the frame is **0.96° below the horizon** — fourteen
pixels on a 720-high frame.  The horizon enters at exactly 30.0°, and
the overview preset at 89° is nowhere near.

⚠ The player *can* orbit down to see sky, and that is the refutation
rather than the rescue: **a clock you must tilt the camera to consult is
the HUD number `DESIGN.md` § HUD refused, with a tax on top.**

⚠ Three answers survive, they differ materially, and the ruling is the
owner's (`@X287`):

| | |
|---|---|
| **tilt the follow camera** | the horizon enters at 30.0°, so ~25° puts it a tenth of the frame down — ⚠ it changes every frame the game draws, and the elevation is `atan(3/5)` from a boom `plans/21` R2 tuned |
| **bring the signal DOWN** | a battleship's SHADOW crossing the base is in the default view, adds no number, and keeps *the clock and its enforcer are the same object* — ⚠ **the recommendation** |
| **drop the overhead reading** | the briefing's *when* stands alone, and the permit is tracked by the player rather than by the screen |

⚠⚠ **And none of them is buildable yet.**  Expiry costs the CARGO
(below), the cargo is the SCRAMBLE, and the scramble is
[`../plans/ROADMAP.md`](../plans/ROADMAP.md) § The critical path item 4
and unbuilt — so a permit clock shipped today would be a number that is
moved, saved and displayed while nothing reads it, which is `@D002`
exactly, on the day BACKLOG C7 closed it.

### ⚠ What expiry must NOT be, and the answer § 14 already gives

`DESIGN.md` § 14 Run structure is categorical:

> dryopea **does not have a fail screen.**  … A run the player feels was *bad* is
> simply a run that produced **meagre carryover**.

⚠ So the permit expiring must not kill the player, and a straight
*"destroyed on ascent"* would be a fail screen wearing fiction.  But the same
paragraph supplies the resolution: **a bad outcome is measured in CARRYOVER.**

*Recommendation: an expired permit does not stop the launch — it costs the
cargo.*  The cordon impounds an unauthorised ascent's manifest.  The player
still leaves, the next base still starts, and the run produced nothing — which
is precisely what § 14 says a bad run looks like, with no new failure concept
invented.

⚠ It also keeps force-launch exactly as designed: *"the cargo manifest is
whatever made it onboard … Force-launch leaves stragglers behind — by design.
The cost of haste."*  The permit simply adds a second, opposite cost — **the cost
of dawdling** — to a mechanic that already priced haste.

### ⚠ It turns an ungateable design target into a tunable

`docs/NUMBERS.md` § Design targets carries *"a single base session ≈ 15-25
minutes"*, and `CLAUDE.md` notes it has been **ungateable since it was written**
— *"NOT gateable, and it needs a PLAYER"*.

⚠⚠ **A permit window IS that number, as a mechanic.**  The session length stops
being an aspiration about pacing and becomes a value the game enforces and a
test can read.

⚠ **Reality check before anyone sets it to 20 minutes:** the longest base the
corpus can currently play falls at **321 ticks** ([`plans/16`](../plans/16-the-wave-system/README.md)
§ W4) — about 3.5 minutes at `TICK_SECONDS`.  The authored content is an order of
magnitude short of the target, so the window is a number to be **derived from
content that exists**, not chosen from the design doc.  Setting it first would
make every base end in silence.

### What the unknown has to be, for this to work

*"Aspects about the scenarios they run will always be unknown to them."*  ⚠ That
is an authoring requirement, and the design already protects it from both sides:

- `ROBOT_ECONOMY.md` § How the player learns, layer 1 — the sortie brief is
  **deliberately low-resolution** (*"heavy mining traffic, no known stockpile"*).
- `DESIGN.md` § 12 HUD — **no minimap**, and § No wave HUD forbids a wave
  readout.  ⚠ So there is no UI through which the unknown could leak.

⚠ **The thing to protect is that the brief must never be sufficient.**  If a
player can plan the base from the landing screen, the sortie has no product and
§ X2b's recon window becomes dead time.

---

## X3 — what is out there is already designed, in three documents

Nothing below is new.  This is the inventory, so a map author has a list.

| what | where it is designed | tier it opens |
|---|---|---|
| six installation types — mines, factories, transport routes, military stockpiles, repair points, carbon plants | [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The graph | robots |
| **crystal mines, located AT dead trees** — a braid of shafts, so a *place* with a footprint and a history | `ROBOT_ECONOMY.md` § 1a + § The vertical dimension | robots → elementals |
| **a tree that CHANGES KIND** — living = sap, withering = both, withered = a shaft into the crust | `ROBOT_ECONOMY.md` § The vertical dimension | insects (sap) → elementals (shaft) |
| sap → invites insect chase **by smell**; gems → **awaken matching elementals** | `DESIGN.md` § Scouting | insects / elementals |
| new tower TYPES as unlocked content — found, brought to the core, orderable from then on | `DESIGN.md` § Future tower types | — |
| **stranded helpers** — a downed crew member left behind, a rescue target on a later run | `DESIGN.md` § 9 § Stranded helpers | — |
| abandoned bases | `DESIGN.md` § Future expansion | — |
| the military stockpile — ⚠ **the thing to AVOID waking** | `ROBOT_ECONOMY.md` § 4 | robots, escalated |

⚠ **The last row is the one that makes a map a map.**  A destination the player
should *not* approach is what turns "drive outward" into a decision, and it is
already designed with its own escalation ladder.

---

## X4 — a find is ONE marker row and ONE cargo row  `@X026`

⚠ **This section is the one addition, and it is deliberately the smallest thing
that could work.**

`CLAUDE.md` already documents both extension points, and both are a row:

- **Add a marker kind** — a constant, bump `MARKER_KIND_COUNT`, a row in
  `place_marker` and `marker_kind_name`.  ⚠ Its stated cost is that the editor's
  place-kind CYCLE grows, so every `.keys` script that cycles back to spawn needs
  another press (plan 12 B5a paid that for nine scenarios).
- **Add a carryable kind** — a `CARGO_*` constant plus what a valid destination
  is and what arriving there does, and **nothing in the carrying path**.  ⚠ *"A
  kind that needs new carrying code has broken `plans/15` § C0.4."*

So a find is: a **marker** where it sits, a **cargo kind** for what you take
home, and a **destination rule** (the core).  Carrying it is the model plan 15
already shipped — one slot, one owner, conservation structural.

⚠ **And it must NOT become a new subsystem.**  The moment a find needs its own
inventory, its own tick or its own UI, it has stopped being a find and become an
economy — which `ROBOT_ECONOMY.md` § What this design does NOT do already
refuses in its own domain: *"no economy simulation … a static graph plus a rate
per edge"*.

---

## X5 — every find opens a fight, and the fight is a wave that already exists  `@X027`

`DESIGN.md` § Scouting states the rule and it is the good half of the design:

> Scouting is a **bet**: every find is high-value AND opens a fight.  Stay near
> the core (no gains, low risk) vs push outward (real rewards, real
> consequences).

⚠ **Mechanically this is free.**  `ROBOT_ECONOMY.md` says so outright about its
own traffic — *"**No new mover.**  Robots on a route are the enemies dryopea
already has, walking their heading, becoming a wave when the bubble deafens
them — `spawn.loft`'s approach mode, unchanged"* — and a find's consequence is
the same shape: taking it makes its hex a spawn source.

⚠ **The pressure is per-find and authored, not global.**  Sap brings insects,
gems wake elementals, a stockpile escalates robots.  That is three different
consequences over one mechanism, which is the project's governing rule again:
ONE system, per-type DATA.

---

## X6 — the cost of leaving is already MEASURED, so exploration needs no new one  `@M007`

`CLAUDE.md`'s first design test is *does this put something in the player's hands
at a moment when using it costs them something?*  Exploration passes it **with a
number already in the repo**:

> ⚠ The authored seven-wave list is playable: seven towers and two **shuttling**
> helpers clear all 205 robots.  ⚠ Parked on their towers the same two reach 5/7
> and the base falls — **upkeep is a POSITIONING problem, not a resource**.
> — [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3

⚠⚠ **So driving away already costs a measured amount of base**, because repair
is a position (20 s of standing within one hex of a black tower) and salvage is a
position (a crew member clearing bodies at 1 m/s).  A player who leaves is not
repairing and not clearing, and plan 17 T3 priced the difference at *two waves of
the authored list*.

⚠ **Nothing needs adding to make exploration a real decision.**  What it needs is
for the player to be *able to leave*, which is [`plans/13`](../plans/13-the-vehicle/README.md)
V4's boost — *"four hexes a tick and a 3.0 m climb for three ticks, so a crew
leaves a sealed base and comes home"*.  That is already the exploration traversal
tool and it was built as one without saying so.

---

## X7 — the earliest find should be a STRANDED HELPER, and it is buildable today  `@X028`

⚠⚠ **This is the recommendation, and its whole force is that it needs no code
at all.**

`DESIGN.md` § 9 already designs stranded helpers: a downed crew member not
retrieved before force-launch stays at their wreck hex and becomes *"a
rescue-quest target for the same player on a later run"*.

Every mechanic that needs is **already shipped**:

| step | what runs it | shipped in |
|---|---|---|
| a wreck sits at a hex | `cargo_spawn(cargo, CARGO_WRECK, …)` | plan 15 C1 |
| drive out to it | `vehicle_drive` / boost | plan 13 V1 / V4 |
| pick it up | `wave_take` — one slot, one owner | plan 15 C0 |
| carry it home | the carry model, unchanged | plan 15 C1 |
| deliver at the core | `wave_drop` → `helper_begin_recovery` | plan 15 C2 |
| it rejoins the roster | exactly 90 ticks later | plan 15 C2 |

⚠ **So the first exploration scenario in dryopea is a `.keys` file, not a
feature.**  It can be authored and gated with today's binary, against today's
520-measurement harness, and it would measure the one thing nobody has measured:
*what is a sortie worth?*

⚠ **And its value is already known to be real but conditional** — +76 points
with a job to come back to, one tick without.  § X2c is where that pair is
unpacked, and it is why a stranded helper is the right first find on its
MERITS as well as on its cost: a crew member is a build accelerant whose value
compounds, and whose 90-tick recovery clock is free in the opening and worthless
at wave four.

---

## X8 — what persists is INTEL, and that is the run-level reward  `@X029`

`ROBOT_ECONOMY.md` § How the player ever learns any of this already gives the
three layers, in the order a player meets them:

1. **the sortie brief** — the neighbourhood at low resolution, *"heavy mining
   traffic, no known stockpile"*.  What makes base selection a decision.
   (`ROADMAP` Tier D, unbuilt.)
2. **scouting** — driving out to find the nodes.  ⚠ *"A found node is intel that
   persists."*
3. **the waves themselves** — composition is a readout.  ⚠ *"Miners at the wall
   means a mine upwind"* — diegetic, no UI at all.

⚠ **Layer 3 is free and is not being used.**  dryopea already has four robot
classes that differ only in how fast they chew a wall; making a wave's
composition *say where it came from* costs nothing and is the cheapest
exploration reward in the design — the player learns the map by being attacked
by it.

⚠ **Layer 2 is the state this design adds beyond a find**: a per-node `found`
flag.  That is what makes a sortie worth something after the base is over, and it
is what a run — *"a sequence of bases, chained by what you carry out"* — carries
between them.

---

## What this does NOT design

**No fog of war.**  The haze radius already bounds what is drawn and
`DESIGN.md` § 3 gives it a fiction (physically blocked beyond ~40 hex).  A
second, remembered visibility layer is a different feature and this design does
not need it — a found node is remembered as *intel*, not as revealed pixels.

**No economy simulation, no AI strategy, no new mover** — `ROBOT_ECONOMY.md`
§ What this design does NOT do already refuses all three, and this document
inherits every one.

**No travel mechanic.**  Boost exists (plan 13 V4).  ⚠ If sorties turn out to be
tedious at 3 hex/s over 40 hexes, the answer is a NUMBER (speed, or boost
economy), not a fast-travel system — and it should be measured on a scenario
before it is believed.

**No bigger world.**  ⚠ Radius 40 is one of the three numbers the tick budget is
derived from (`CLAUDE.md` § Cost), and the haze already bounds sight at 40.
Exploration fits *inside* today's world; growing the world is a separate decision
with a measured cost, and [`plans/22`](../plans/22-the-field-cache/README.md) is
what would have to land first.

**No sortie brief / station hub** — that is `ROADMAP` Tier D and it is the layer
*above* a base.

---

## The order of work, when this earns a plan

⚠⚠ **It does not have one yet, and that is deliberate: `plans/README.md` caps
active plans at 2–3 and there are already FOUR** (19 P5, 20, 21, 22).  Opening a
fifth would be the thing that convention exists to stop.

When one closes, the phases are in this order.  ⚠ **The first two are not
features**, and the third is the cheapest code in the design:

1. ~~**A scouting scenario, in `.keys`, with today's binary**~~ ✅ **DONE —
   2026-08-30, `@M092`, and it needed no code exactly as § X7 said.**
   `tests/scripts/a-find-nobody-fetched.keys` /
   `a-find-fetched-on-the-way.keys` / `a-find-fetched-late.keys`: a stranded
   crew member authored at (14, 0) on the road to the spawn marker, and the only
   difference between the three is WHEN the player picks them up.
   ⚠⚠ **248 never fetched, 322 taken in passing, 364 fetched at wave three**
   — so a sortie PAYS (+74 and +116 on a base that lives 248), and this page's
   own falsifier below does not fire.  ⚠⚠ **But the two are in the wrong ORDER
   for § X2c**, and §§ X2c and X2d carry what that changed.
2. ~~**A LAYOUT scenario**~~ ✅ **DONE — 2026-08-30, `@M093`, and it needed no
   code either.**  `tests/scripts/a-base-drawn-in-tight.keys` /
   `a-base-drawn-out-wide.keys` and their two no-tower nulls: one base, eight
   wall hexes, two towers, two crew and two spawn markers at ±24, and the only
   thing that moves is how far from the core the defence sits.
   ⚠⚠ **170 tight, 207 wide — the racing line is REAL, +37 ticks and +22 % —
   and the same nine hexes with the towers deleted are worth 102 against 102,
   to the tick.**  ⚠ § X2b § What phase 2 measured carries it.
3. **The four robot classes** — ⚠⚠ **the gating item for everything below**,
   because until they exist every wave is the same wave and a sortie can predict
   nothing (§ X2b).  One row each in `numbers.json` plus one branch in
   `spawn.loft`'s damage-to-wall lookup; no new mover, no new targeting.
4. **The find marker + cargo kind** (§ X4) — one row each.
5. **The consequence** (§ X5) — taking a find makes its hex a spawn source.
6. **Intel that persists** (§ X8) — the `found` flag, and what carries between
   bases.
7. **Wave composition as a readout** (§ X8 layer 3) — free once 3 lands,
   diegetic, and the cheapest reward in the design.

⚠ **Phase 1 was worth running before any of the rest was designed further**,
because it could falsify the whole thing cheaply: if a sortie to radius 20 and
back cost more base than the find is worth, then exploration as designed is a
trap and the numbers — not the mechanics — are what need work.  ⚠⚠ **It did
not fire, and the curve it was supposed to confirm came out INVERTED** — see
§ X2c § What phase 1 measured.  ⚠ *A phase whose stated purpose is to falsify
the page it is on is the one to run first*, and this is the second thing on it
that a measurement has moved.

---

## Open, and decided rather than asked

1. **Does a find's reward help THIS base or the NEXT one?**  ⚠ Plan 16 W4 and
   plan 17 T3 measured the same errand at *one tick* and at *+76 points*, and the
   difference was whether there was still a job when it came back.  *Decision:
   the EARLY find helps this base (a helper, a tower-top, points) and the
   run-level reward is intel (§ X8).  A find whose only value is next base makes
   the first sortie feel like homework.*
2. **Is exploration blocked on helper orders?**  I argued it was.  ⚠ Re-reading
   § X6, it is not: leaving already costs a measured amount of base *because*
   helpers keep working where you left them.  Orders make the cost
   **controllable** rather than fixed, which is better — but exploration ships
   without them.  *Decision: not blocked; orders raise the ceiling.*
3. **Per-map or per-planet intel?**  `ROBOT_ECONOMY.md` § Open 1 already asks
   this and recommends *author per-map, keep node identifiers global*.  Inherited
   unchanged; nothing here needs it decided.

## See also

- [`DESIGN.md`](DESIGN.md) § 13 § Scouting — **the source**, and where the design
  test is already passed.
- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The graph, § The vertical dimension,
  § How the player ever learns any of this.
- [`SETTING.md`](SETTING.md) — the three tiers a find can wake.
- [`plans/16`](../plans/16-the-wave-system/README.md) § W3 — the provocation that
  already makes turn one a sortie.
- [`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 — what leaving costs,
  measured.
- [`plans/15`](../plans/15-the-carry-model/README.md) § C2 — the retrieval loop
  § X7 rides on, already shipped.
