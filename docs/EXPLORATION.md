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

## X0 — the progression is SKILL  →  moved to [`PROGRESSION.md`](PROGRESSION.md)

⚠ **This section MOVED** (2026-08-15).  It answered *what exploration is FOR* —
the landscape is where the player learns to fly, and the base is where that
competence cashes out — and it grew into a subject of its own.

[`PROGRESSION.md`](PROGRESSION.md) owns it now: `@X016` (skill, not stats),
`@X017` (⚠⚠ the vehicle must not get faster), `@X018` (the landscape is a
school), `@X019` (the base is the exam, and there is a racing line).

⚠ The pointer stays rather than the section being deleted, because everything
below is written against it: § X2c's *find it early* and § X2d's permit are both
pressures on a player who is trying to get **good**, not merely rich.

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

### ⚠⚠ And here is the hole: there is nothing yet to be intelligent ABOUT

`src/spawn.loft` says it plainly — *"the validation tier still emits only
regulars"*.  **`ENEMY_KIND_REGULAR` is the only class the game emits.**  Scout,
harvester, builder and miner are designed and not built.

⚠ So *"composition is a readout"* (`ROBOT_ECONOMY.md` § How the player ever
learns any of this, layer 3) is currently a readout of **one symbol**, and a
sortie cannot predict anything because every wave is the same wave.

⚠⚠ **That makes the four classes the FIRST thing this design needs built**, ahead
of find markers, intel persistence or anything else on this page — and
`CLAUDE.md` already prices it as the cheapest item in the design:

> Four enemy types for **one row each in `numbers.json` plus one branch in
> `spawn.loft`'s damage-to-wall lookup** — no new mover, no new targeting, no new
> code path.

That is the whole of it, because *"ONE AI, per-class DATA"* was built as a rule
from plan 11 onward specifically so this would cost a row.

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

⚠ **And its recovery clock is what makes early-vs-late bite.**  A retrieved crew
member rejoins after **exactly 90 ticks** ([`plans/15`](../plans/15-the-carry-model/README.md)
§ C2).  Found in the opening, that clock runs out before the first wave and costs
nothing; found at wave four, it runs out into a base that is already falling —
which is plan 16 W4's *one tick*, exactly.

⚠ **This is the first content in dryopea whose value the existing corpus can
already measure**, and § The order of work phase 1 is what would do it.

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

1. **A scouting scenario, in `.keys`, with today's binary** — a stranded helper
   at radius ~20, on the way to the first spawn marker.  Measures what a sortie
   costs and what it is worth, against the base that does not take one, and
   against the SAME find collected at wave four (§ X2c).  ⚠ It is the § X7 case
   precisely because it needs no code.
2. **A LAYOUT scenario** — the same defences and the same wave list, arranged
   two ways: compact, and spread.  ⚠ **Nothing in the 28-scenario corpus varies
   the layout while holding the defences equal**, so § X0's racing line is a
   claim with no number under it.  ⚠ It must control for BRACING first —
   `CLAUDE.md` warns that `q -> -q` is not a symmetry of this lattice, and plan
   14 H2 measured a 99-tick artefact that read exactly like a finding.
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

⚠ **Phase 1 is worth running before any of the rest is designed further**,
because it can falsify the whole thing cheaply: if a sortie to radius 20 and back
costs more base than the find is worth, then exploration as designed is a trap
and the numbers — not the mechanics — are what need work.  ⚠ And it is the one
measurement that would confirm § X2c's early-versus-late curve, which every
incentive on this page rests on.

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
