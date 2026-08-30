<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Claude Code Instructions for the dryopea Project

## What dryopea is

**dryopea** is a sci-fi free-build / tower-defence game built on
[loft](https://github.com/jjstwerff/loft).  The defining mechanic
is **scramble-and-salvage**: when a base is about to be overrun,
the player fires a rocket out of the core building and evacuates
key components — each carried-out component disables the tower
it came from, so grabbing salvage *hastens* the overrun.
Evacuated components give an advantage at the next base.  A run
is a sequence of bases, chained by what you carry out.

⚠ **It is a strategy game built out of tower-defence mechanisms,
and the player cannot lean back** (project owner, 2026-08-13;
`docs/DESIGN.md` § What kind of game this is).  That is a design
TEST, not flavour, and it is worth knowing before adding a
mechanic: *does this put something in the player's hands at a
moment when using it costs them something?*  A tower's budget
decays per shot and only a player standing at it can refill it;
bodies ramp a kill zone shut until someone drives in and clears
them; salvage decays, so it must be collected at the worst
moment.  A mechanic that hands out a permanent advantage for a
one-time placement decision is what this design avoids — and the
few things that DO let the player rest (an idle tower never
decays, a tower that never fired is never retaliated against)
are load-bearing pressure valves rather than slack.

The full design lives in [`docs/DESIGN.md`](docs/DESIGN.md); the
fiction in [`docs/SETTING.md`](docs/SETTING.md); the feature
roadmap in [`plans/ROADMAP.md`](plans/ROADMAP.md).

## Status

**Active implementation.**  ⚠ **Each plan's own `## Status` is the source of
truth** and [`plans/README.md`](plans/README.md) indexes them.

- **What exists today, one line per shipped phase** —
  [`docs/STATUS.md`](docs/STATUS.md).  Read it to find out whether a thing is
  built before designing around it; ~45 rows from the hex editor through the
  terrain mesh.
- **How the toolchain fails, and how to tell that from a real defect** —
  [`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md).

⚠⚠ **THE CRITICAL PATH'S FOUR GAPS ARE ALL CLOSED** — building
([`plans/27`](plans/27-building/README.md), 2026-08-27) and the SCRAMBLE
([`plans/28`](plans/28-the-scramble/README.md), 2026-08-28) were the last
two.  ⚠ What follows is `ROADMAP.md` § Then the run becomes a RUN, and
its item **5, helper ORDERS**, shipped the same day
([`plans/29`](plans/29-the-crews-own-work/README.md)): **the crew find
their own work, and you can tell one of them what to do.**  The next
items are **6 the landing flow**, **7 carryover** — the scramble produces
it and nothing consumes it — and **8 the permit clock**, blocked on
`@X287`'s ruling rather than on a mechanism.

**Where the game is right now:** the simulation is complete enough to play a
seven-wave base to its end, and **the game is PLAYABLE AND VISIBLE** — `make
play SCRIPT=a-base-that-plays-its-list`, press **P**, and the map editor
becomes the game: the ground as
triangles, every entity as a part-tree, through an eased camera that follows
the vehicle, with the WALLET in the corner, ramping amber to red as it drains.  Press P again and the editor
comes back.  ⚠ The HUD is one number and `docs/DESIGN.md` § HUD says it should
be — no wave counter, no health bar, no minimap; everything else is diegetic.
⚠⚠ **`MAP=` and `SCRIPT=` are what give it a base to be.**  `make play
MAP=starter_01` opens one of the three AUTHORED maps in `maps/` (BACKLOG A2,
2026-08-27); `SCRIPT=<name>` opens any of the 55 `.keys` files in
`tests/scripts/` + `tests/gl/` as a live starting position, cut at its first
`tick` (`@X263`).  Bare `make play` opens the empty default slot.
⚠⚠ **AND A ROBOT ON THE MAP CAN GO SOMEWHERE FOR A REASON**
([`plans/30`](plans/30-the-mob-routine/README.md) R1-R3 + Rc, complete
2026-08-28) — `Errand`'s five anchors, a role TABLE, a CLOSED-FORM cycle
that answers *where is it at time `t`* in one modulo, and a MOVER that
descends a field toward the anchor its BAG names.  ⚠⚠ **It is INERT and
that is deliberate**: no scenario has a routine, so `ROLE_NONE` is what
every `Enemy { … }` literal already means and the **920 gate measurements
did not move**.  ⚠⚠ **AND A ROUND HAS AN ENDING** (R4, `@X338`,
`@M076`): a gatherer takes rounds for a 120 s **shift** and then leaves
the roster **at its nest** — home is a LEG of the round, never a place a
finished mob walks to, and the departure sits at the **TOP** of the tick
because *what the player cannot see the gate cannot see either*.
⚠⚠ **AND SINCE R4b IT CAN END SOMEWHERE THE ROUND DOES NOT PASS**
(2026-08-29, `@X341`, `@M078`) — the world's commonest robot dumps in
one place and is serviced in another (`@X339`, `@X340`), so the working
legs repeat until the shift and then ONE **terminal leg** walks it to
the repair point.  ⚠⚠ **The turn is a THIRD VALUE OF THE BAG** and never
a second clock: the mover has no cycle and cannot know the period, but
*the first empty leg after the shift* IS `T = ceil(S / period) × period`.
⚠ And it is compared in **hexes, never in time** (`@M077`).
⚠⚠ **AND SINCE R5 A PLACE OWNS ITS MOBS AND ITS REACH IS A REGION YOU
CAN ASK ABOUT BEFORE ANYTHING MOVES** (2026-08-29, `@X342`, `@M079`,
`@M080`) — a `Poi`, a population that is a set of ROUTES rather than a
list of BODIES, and the BOUND, which is what turns *could this ever be
in this window?* from `N` queries into one.  ⚠⚠ **The claim is per-LEG
and the union is only the QUERY**: a union is SLACK, and against it a
radius one hex short, a terminal leg with no disc, an off-by-one rim and
**a sidestep that breaks `@FR-E-Non-Increasing`** all read green — four
of nine mutations, and the four that matter.  ⚠⚠ **And the probe chose
the fixture**: on an authored map the bound covers **1466 of 1467**
standable hexes, because a round that crosses its patch has legs as long
as the patch.
⚠⚠ **AND SINCE R6a A MOB NOBODY CAN SEE COSTS ONE INTEGER** (`@X343`,
`@M081`) — everything about it that CAN be a function of `t` is one, and
**only `slip` accumulates, because only a BODY can be pushed**.  ⚠ The
three tiers: CULL is per POI and is one `bound_meets` for a whole
population; EVALUATE and MATERIALISE are per mob.  ⚠⚠ **And the BANK is
the one nobody would think of** — a fresh body carries nothing where the
rule is part-way through a hex, so it releases its next hex late by
exactly `cycle_carry`, and at 1.5 hex/s that is always ZERO (`@M014`'s
class, a fifth instance).
⚠⚠ **AND SINCE R6b THE GAME MATERIALISES ITS OWN MOBS** (`@X344`,
`@M082`) — `WaveState.pois` carries the places, `PlayState.reach` carries
**one integer apiece**, and the tick's per-POI question is
`lat_distance(poi, player) <= reach + window`: one subtraction, no world
read, no round built.  ⚠⚠ **And two window sizes give one answer** —
identical where nothing can push a body, and differing by exactly `slip`
where something can, which is the pair `@X299` needed and the narrower
claim that survives it.  ⚠⚠ **Its mutations found all four survivors in
what the GATE could not see** (`@M082`), headed by ***a saving is not a
behaviour***: deleting the cull changed no position and made no extra
body, because the cull is work NOT DONE and only a COUNT can read it.
⚠⚠ **AND SINCE R7a A PLACE IS FOUR VERBS IN A `.keys` FILE** (`@X345`,
`@M083`, `@D009`) — `poi` / `route` / `mob` say what a PLACE issues and
`routine` says what one BODY holds, and neither is derivable from the
other once a tick has run.  ⚠ **A route ENLISTS its own population**, so
a file says *how many* and never *which ones*.  ⚠⚠ **All three defects came
from a GATE laid earlier and none was on the mutation list** — the FLAG
is a second fact (an ambient robot is `errand: true` with `ROLE_NONE`),
**`slip` is the one duration that may be NEGATIVE** while the authoring
door refuses one on purpose, and **R1's `src/` sweep caught both new
verbs comparing a role** because `errand_role_named` cannot say no.  ⚠⚠ **And the corpus has a base the game
populates itself**: `a-place-that-sends-robots` places no robot, and it
is the first fixture in which `slip` is VISIBLE.
⚠⚠ **AND SINCE R7b A HAULER TURNS FOR WHAT YOU LEFT** (`@X346`,
`@M084`) — `@FR-E-Built-Not-Seen` in code: a heap the player left on a
route is picked up and carried off, and the pair reads **230.0 against
200.0** of a 200-point wallet.  ⚠⚠ **The finding is the ACCOUNTING**: a
hex walked AWAY from the anchor costs **two** — the one it spent and the
one it owes — because a detour is paid both ways and the return leg is
indistinguishable from progress.  Then the rule and the body agree in
DISTANCE at every moment of it **with nothing remembered**, which is
what `@FR-E-Place-State` requires.  ⚠⚠ **And the BAG is not touched**:
the stolen heap is a `CarryObject` keyed on an identity that outlives a
body, so killing the thief gives it back.  ⚠ The negative control is in
the fixture's own opening half — the player in plain sight for thirty
ticks, and the robot on its rule's hex to the tick.
⚠⚠ **AND R8 CLOSED THE PLAN WITH A NUMBER** (`@M085`): a base landed
**ON** a robot road falls in **123** ticks against **269** beside one —
the same map, wall, waves and painted road, four coordinates apart, and
four haulers nobody sent at the player are worth more than the wave
itself.  ⚠⚠ **But the SWEEP is the finding**: the road is worth
196 / 146 / 10 / **0** ticks against waves of 2 / 3 / 5 / 8, because the
siege front is the wall's WIDTH (`@M020`) and a big enough wave
saturates it — ***the first version of that pair read 118 against 118
with four extra besiegers plainly on the map***.
⚠ **[`plans/30`](plans/30-the-mob-routine/README.md) is COMPLETE.**

⚠⚠ **AND SINCE [`plans/31`](plans/31-carryover/README.md) N1-N4 A BASE OPENS
WITH WHAT THE LAST ONE CARRIED** (2026-08-29, `@X347`, `@M086`) —
`ROADMAP.md` item **7**, and the thing `plans/28` says it made possible and
deliberately did not do: `manifest_of` had no reader outside its own test.
⚠⚠ **The field is spelled as the CARRY and never as the BUDGET**, because
[loft#914] takes an omitted field's default silently — a defaulted `budget`
opens every base ALREADY FALLEN, a defaulted `carried` reproduces the game
that shipped.  Third instance of that class after `wallet.spent` and
`play.digging`.  ⚠⚠ **Counting the re-assertion sites BEFORE writing any code
is what found the fourth**: three are in `wallet.loft` and the fourth is
`hud.loft::hud_ink_for`, which read the BASELINE for the wallet ramp's span
while clamping above it — a base opened with 200 carried would have sat at
the full colour from 400 points down to 200, **the ramp dead for half the run
on the one number the game shows**, and `tests/19_p7`'s exhaustive sweep of
every reachable colour would have stayed green because ***it sweeps the span
it is given***.  ⚠⚠ **Exactly one of the manifest's three columns crosses**
and each refusal is blocked on a thing with a NAME — the crew on an IDENTITY
(`@X290`) and a STATION, the stragglers on the same, the tops on a DEPOSIT.
⚠ **The ratio is applied once by construction**: `manifest_of` refuses to
convert, `manifest_opens` BUILDS a wallet rather than crediting one.
⚠⚠ **AND THE WAVE LIST CARRIES A COMPOSITION, IN JSON AND NOWHERE ELSE**
(2026-08-29, `@M089`, `@M090`) — `examples/waves.json` § `parts`, read by
`wave_schedule_new`; `waves.loft` holds **no copy**, and `wave_list_default`
is seven counts with nowhere to put a class.  ⚠⚠ **A WAVE IS DECIDED BY THE
WORST CLASS HOLDING A SLOT IN THE FRONT** — `@M020` cashed at the level of a
wave's contents: all-regular **452**, all-builder **452**, all-scout **671**,
all-miner **752**, all-harvester **never falls**, so **no composition can be
harder than all-regular**.  ⚠⚠ **And MORE BODIES MAKE A WAVE WEAKER** — the
counts plus extra scouts read **641** against the flat **452** — *a wave
cannot be strengthened by adding to it, only diluted*.  ⚠ `builder` is the
one class that joins for free (1.5 hex/s like a regular, and its 1.5x damage
buys nothing where the breach clock is saturated), so the shipped mix is
regular + builder at **452 against 452**.  ⚠ A richer mix is a **RATES**
problem, not a tuning one, and rates are balance — deferred to the economy.

⚠⚠ **AND THE CARRY COMPOUNDS** (`@M087`): **230.0 against 106.0** on the
same base, crew, wave and tower sites **with the same 100 points left when
the ferry ends** — a hundred points spent on two more towers came back as a
hundred and twenty-four, which is what makes `DESIGN.md` § 14's *sequence of
bases* a RUN rather than a list.  ⚠⚠ **The first version of that pair read
198 against 198**, both "fallen" on the tick the ferry ended with twice the
towers on the map: `fall` plays until `wallet_broke`, and **the wallet is
what BUYS towers as well as what enemies drain**, so a base that spends its
budget on defence is broke before a robot arrives (`@X292` in a fixture).
⚠⚠ **And the SWEEP is the finding** — the carry is worth **+124 points** at
the authored wave, **the difference between standing and falling** from 26
robots to 50, **+11 ticks** past 115 and then nothing, because both columns
flatten once the wave saturates the siege front (`@M020`, `@M085`'s reading
with a new subject).  ⚠ It also buys **four beacons where the first base
bought two** (`@M086`), so `@M065`'s landing exclusion moves with the wallet.
⚠ [`plans/31`](plans/31-carryover/README.md) is COMPLETE.

⚠⚠ **AND SINCE [`plans/32`](plans/32-the-landing/README.md) WHERE THE BASE
GOES IS A DECISION AND NOT AN AUTHORED FACT** (2026-08-29, `@M091`) —
`ROADMAP.md` item **6**, and the step § THE SESSION IS THE GAP NOW put first
because `plans/31` made a run possible and **nothing in the game could reach
it**.  `land <q> <r> [seed]` is a `.keys` verb, and `landing_of` is a pure
function of the world, the pick and the seed.  ⚠⚠ **A landing MOVES the
authored core rather than creating one**, which is the load-bearing choice:
`map_fault` validates a map AGAINST its core, so a map stays valid as
authored and **a map cannot be checked against a pick nobody has made yet**.
⚠⚠ **Three of `DESIGN.md` § 15's eight steps needed NO CODE and saying so was
half the plan** — the core's six faces do not exist (`@X294`), close-spawn
silencing has read the core's CURRENT hex since plan 16, and step 8's wallet
shipped as `wallet_carrying`.  ⚠ **Random is a HASH OF POSITION**
(`@FR-W-Position-Hash`, given its **first code** here by a plan that is not
worldgen's): dryopea has no RNG and this plan introduced none.
⚠⚠ **BOTH OF THE PLAN'S OWN DEFECTS WERE ONE RULE WRITTEN TWICE.**  L0's
probe restated `landing_ground_ok` with a disc of ONE where the real door
needs THREE, reporting 210 / 395 / 196 landable hexes against a truth of
**80 / 159 / 71** — *two implementations of one rule is a defect with a
delay* — and L1's search bound was derived cleanly from
`LANDING_PICK_EDGE_BUFFER` but measured from the **hashed start**, itself 3
hexes out, so 3 + 5 = 8 reached past the very buffer the derivation rested
on.  ⚠ Only **14-30 %** of an authored map takes a landing, dominated by the
CLEARANCE and not the spawn rule, and `crossroads_02` loses nothing only
because its two markers sit at opposite ends — **so the content rule is
*spawns at opposite ends***.
⚠⚠ **AND IT CLOSED ON A PROBLEM RATHER THAN A NUMBER** (`@M091`): the pick is
worth **58 ticks** across the band it can be made in, and holding the pick
while sweeping only the SEED is worth **59**.  ***The dice are worth as much
as the decision***, so `@X317`'s *land in the OVERLAP, because the overlap is
what makes a choice exist* cannot be felt through this landing, and
`DESIGN.md` § What kind of game this is' *the player cannot lean back* is a
decision the dice can match.  ⚠ The cause is § 15 step 6's **random
direction** for the free starter tower, not the touchdown — a hashed bearing
puts the base's only defence anywhere on a ring twenty hexes across.
⚠⚠ **Pointing it at the nearest LIVE SPAWN is a one-line change and
contradicts § 15 as written**, so it is left OPEN as the owner's ruling.
⚠ [`plans/32`](plans/32-the-landing/README.md) is COMPLETE, and the map
selection / planet view half of item 6 is still [`plans/04`](plans/04-map-library/README.md).

⚠⚠ **AND A SORTIE IS PRICED, WITH NO CODE WRITTEN** (2026-08-30, `@M092`) —
[`docs/EXPLORATION.md`](docs/EXPLORATION.md) § The order of work **phase 1**,
which that page put first precisely because *it can falsify the whole thing
cheaply*.  One base, one ramping wave list, one stranded crew member authored on
the road to the spawn marker, and the ONLY difference between three files is
when the player picks them up: **248 never fetched, 322 taken in passing, 364
fetched at wave three**.  ⚠⚠ **So a sortie PAYS — +74 and +116 against a base
that lives 248 — and the falsifier does not fire.**  ⚠⚠ **But the two are in
the WRONG ORDER and that is the finding**: `@X024` says *a find's value collapses
once you are busy*, and a stranded crew member is a **BODY rather than a
structure**, so it is ***spent by being used*** — and it buys the extra with
**HALF THE LIFE**: ~80 ticks alive for +74 delivered at tick 21, ~40 ticks alive
for +116 delivered at 181, because ***what a body is worth is the PRESSURE it
stands against and not the time it stands***.  ⚠ The sharper
rule the design wanted: **a find decays with lateness only to the extent that
what it buys is PERMANENT.**  ⚠⚠ **And four of eight swept fetch ticks
FAILED** — the player is killed carrying the wreck and `vehicle_respawn` clears
the carry — so the late fetch's price is a **RISK, not a discount**, and tick 141
works only because it is a gap between waves.  ⚠⚠ **The BUILD half of `@X024`
is UNMEASURABLE today**, because `@X022`'s recon window is free and unlimited: a
second builder before wave 1 saves the player's wall-clock and not one game-tick.
**That makes the permit ([`plans/ROADMAP.md`](plans/ROADMAP.md) item 8) the
missing INCENTIVE rather than a follow-on** — 9 before 8 for the CONTENT, 8 for
the reason to go NOW.  ⚠⚠ **And the control is NOT a ceiling**: the same crew
member GIVEN at tick zero — no wreck, no trip, no recovery — reads **327**, so
the late fetch beats the free version by 37 ticks.  ***A body given early is a
body spent early.***  ⚠ The generic control came FIRST and condemned a base: a
ONE-front version read the same wallet to the decimal with the find, without it,
and with a second crew member from tick zero (`@M020`'s saturation, new
subject).

⚠⚠ **AND THE LAYOUT IS WORTH SOMETHING, ALSO WITH NO CODE WRITTEN** (2026-08-30,
`@M093`) — [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § The order of work
**phase 2**, `PROGRESSION.md` § P7 and `BACKLOG.md` E2, and the opening probe of
[`plans/33`](plans/33-exploration-finds/README.md).  Four `.keys` files, one
base, eight wall hexes, two crew and two spawn markers at ±24 held identical —
two towers in the first pair, none in the second; only how far from the core the
defence sits moves.
⚠⚠ **170 tight against 207 wide — the racing line is REAL, +37 ticks and
+22 %** — so `@X019`'s *the base layout is the exam* has a number under it and
§ X2b's *the sortie's product is a BUILD DECISION* is answered.
⚠⚠ **But the NULL is the finding**: the same nine hexes of outward displacement,
eight wall hexes and both crew with them, is worth **0.0 — 102 against 102, to
the tick** — once nothing on the map shoots.  ***What a layout is worth is not
where the WALL is; it is how far from the core the FIGHT happens, and a base
with nothing that shoots has no such distance to sell.***  ⚠ Which is
`plans/12` B7's *a gate buys nothing* one level up: **a defence that does not
kill cannot be positioned well.**
⚠⚠ **And the towers were never the variable** — all four cells burn **all 30
shots each**, so the sixty shots are identical and only their geometry moved,
which is `@M025`'s hazard sidestepped by READING the instrument (`shots` counts
shots FIRED, and 30 is a spent magazine rather than a full one).  ⚠ The swept
2×2 separates the halves and they are ADDITIVE: front ±5 / towers ±13 and front
±14 / towers ±4 **both read 187**.
⚠⚠ **The construction is what makes it readable**: the spawn markers at ±24 and
the core at (0, 0) put the total walk at **24 hexes in every file**, so moving
the front outward shortens the approach to it by exactly what it lengthens the
walk behind it — ***a pair that did not null that would be measuring the ROAD
and calling it the layout***, and the no-tower pair is what proves the
cancellation exact.  ⚠ What is still NOT measured is § P7's other half: a
compact base being cheaper to SERVICE needs a crew that shuttles, and no
scenario varies one.
⚠⚠ **AND THE SWEPT FACTORIAL FOUND SOMETHING BIGGER THAN THE LAYOUT**
(`@M094`) — deleting the walls and the towers in turn over the same two bases
reads **neither 95 / 95, wall only 102 / 102, towers only 250 / 324, wall AND
towers 170 / 207**.  ⚠⚠ **A WALL STANDING IN FRONT OF A TOWER COSTS MORE THAN IT
BUYS**: the eight wall hexes are worth **+7** alone, the two towers **+155 and
+229** alone, and adding the wall to a towered base takes **80 and 117 ticks**
back off it.  ⚠⚠ **`plans/12` B7 saw the shape and its DEAD RAMP is falsified
here** — a `pile` sweep of thirteen hexes along the approach, the wall's own
outside face included, reads **0.0 at every one**.  ⚠ The leading hypothesis is
SIGHT (`tower_sees` walks a line and a wall blocks it) and it is **not gated**;
the decisive probe is named rather than run.  ⚠ It does not contradict `@M050`'s
+44 for a wall: **the wall's SIGN depends on whether something that shoots
stands behind it.**  ⚠ And the layout null survives it — **0 in BOTH tower-free
rows.**

⚠⚠ **AND THE SITE COUNT, TAKEN BEFORE ANY CODE, MOVED THE PLAN'S DESIGN**:
`EXPLORATION.md` § X4 says a find is *one marker row and one cargo row*, and a
**CARGO** kind is ~5 sites where a **MARKER** kind is **~96 files** — 70 `.keys`
(266 `do cycle_kind` presses) and 23 `.loft`.  ⚠ `CLAUDE.md`'s own *"the cycle
grows in 47 places"* is STALE by the corpus doubling, which is `@M044`'s rule
with a new instrument.  ⚠ So `plans/33` buys the cargo row and defers the
marker row to [`plans/04`](plans/04-map-library/README.md)'s content half —
`@M092`'s fixtures authored a find with `object <q> <r> wreck …` and needed no
marker at all.

⚠⚠ **AND SINCE [`plans/33`](plans/33-exploration-finds/README.md) E1 A FIND IS
A CARGO ROW AND ANYTHING ON THE GROUND CAN BE SEEN** (2026-08-30, `@X349`) —
`object <q> <r> find <points> <owner>` is a `.keys` verb and delivering one at
the core credits the wallet by exactly `<points>`.  ⚠⚠ **`subj` IS THE POINTS,
and the TRIPWIRE is written into the constant rather than left to be
discovered**: the day a find yields something that is not points — the DISH
(`@X348`) — `subj` holds two facts and must SAY so, as a `find_pack` beside
`errand_load_pack`.  ⚠ Points because `plans/31`'s wallet already turns them
into towers and the compounding is MEASURED (`@M087`), so **E2 sweeps a number
rather than rebuilding a mechanism** — and the wallet had no income outside
salvage until this.
⚠⚠ **AND THE VISIBILITY QUESTION IS WHAT DECIDED THE MARKER ROW**:
`entity_view.loft` is *the ROSTER, as triangles* and a `CarryObject` was never
in it, so **a beacon set down and a tower top on the ground were INVISIBLE** —
`@M092`'s wreck drew only because the downed crew member is still a roster
body.  One catalogue row and one walk fixes all three at once, against ~96
files for a marker kind.  ⚠⚠ **The walk filters on `CARGO_GONE` and nothing
else, and that is the invariant rather than a shortcut**: *on the ground* is a
VALUE of `owner`, so a renderer asking `owner == BLOCKER_NONE` would be a
SECOND rule about where cargo lives — drawing every record that is not
consumed makes the frame's count and `cargo_count` the same number BY
CONSTRUCTION, and the mutation fails at the one state that separates them.
⚠ It also finishes `DESIGN.md` § Carry visibility, which was half built: the
canopy already said LOADED, the object itself was not there.
⚠ **ONE drawn class for all five cargo kinds**, because nothing the player can
DO about a thing on the ground differs by kind.
⚠⚠ **And the colour nearly repeated `@M043`** — `PROXY_ART.md`'s gold
`#ffd000` sits **256** squared-RGB from the wallet ramp's amber where the
lane's floor is 3000, so the entity colour is PEACH: ***the warm lane belongs
to the wallet.***

⚠⚠ **AND E2 PAID THE OTHER HALF OF `@M092`'S RULE AND FOUND A THIRD SIGN**
(2026-08-30, `@M095`) — one base, a hundred-point find at (14, 0):
**209 never cashed, 239 cashed in passing, 245 cashed at tick 100**, and
**swept across seven late fetch ticks it is FLAT** (+37 / +35 / +36 / +36 /
+23 / +30 / +45, no trend).  ⚠⚠ Where `@M092`'s find was a **BODY** and got
BETTER late (+74 → +116, *spent by being used*), a hundred points is spent by
nothing — ***what decays is the find, never the opportunity***.  ⚠ And the
+30 READS as arithmetic — every column reports 5 enemies ALIVE at the fall and
five nibblers at 1.0 point/s over a 2/3 s tick is 100 / 3.33 ≈ 30 ticks — which
is what makes the flatness a claim rather than a coincidence.  ⚠⚠ **A
CONSISTENCY reading and not a derivation**: what is observed is five ALIVE, and
`nibble_in_reach` counts only those within one hex of the core.
⚠⚠ **BUT THE FOURTH COLUMN IS THE FINDING: THE SAME HUNDRED POINTS AS A TOWER
READS 199 — MINUS TEN.**  ⚠ Not because it does not shoot: at tick 150 it has
fired **30 shots, a spent magazine**, while its base holds **73.3 points
against 106.0**.  ***It is poorer, not weaker.***  ⚠⚠ **Upkeep is paid in the
currency of INCOME** — salvage is the wallet's only income and `wallet_broke`
ends the run, so the crew member repairing a black tower is the crew member
who was clearing bodies for points (`plans/17` T3 with a price on it).  ⚠⚠
**And MORE HANDS MAKE IT WORSE**: −37, −91, −96 at one, two and three free
crew, because every extra pair is one more the tower can capture.  ⚠⚠ **`plans/12` B7's DEAD RAMP is NOT the
reason** — a `pile` sweep of thirteen approach hexes reads 0.0 at twelve and
1.5 m at the thirteenth, eight hexes OUTSIDE the wall — and nor is the crew
being diverted to build: a beacon planted but never raised reads **209,
identical to never fetching**.  ⚠ **That probe found the reason it was never
raised**: `job_pick` takes the NEAREST job, so a crew member standing in a gate
full of bodies never reaches a build order two hexes away **even when `send`
puts them on it**.

⚠⚠ **AND SINCE E3 TAKING A FIND OPENS A SPAWN SOURCE — WHICH IS WORTH +31 TO
THE PLAYER** (2026-08-30, `@X350`, `@M096`).  `EXPLORATION.md` § X5 in code:
`wave_taken` plants a spawn MARKER on the hex the find lay on, and `spawn_wave`
already round-robins across the active ones — **no spawner, no counter, no
second schedule**, so a find never makes a wave BIGGER.  ⚠ It is the mirror of
`wave_arrived`, one site each on the two verbs a player presses.
⚠⚠ **AND IT MEASURES BACKWARDS**: **209** with two spawn markers, **240** with
a third on the side the tower is on, **211** with a third on the side it is
not.  ⚠⚠ **The west control is what makes it a rule** — it is not generic
dilution, it is a third of every wave arriving ten hexes closer *to the gun*:
***a new spawn source is worth something only where something can shoot at
it***, which is E0's null (`@M093`) with a new subject.  ⚠⚠ **So § X5 needs a
RULING and not a tune**: opening a fight means ADDING bodies — a change to what
`spawn_wave` is called *with* — and that contradicts its own *"mechanically
this is free"*.  Left OPEN, the way `plans/32` left the starter tower's bearing.
⚠ **Three refusals**: `CARGO_FIND` only (a WRECK is the rescue the design asks
for), nothing inside `SPAWN_DISABLE_RADIUS` (an inert marker would make that hex
unbuildable for ever, because `wave_drop` asks `has_marker`), and never over a
marker already there (the core is one).
⚠⚠ **And the hex is the FIND's and never the CARRIER's** — the case **five of
six mutations could not see**, because reach is one and every fixture parked the
player ON the find.  `@M075`'s hazard by name.

⚠⚠ **AND THE CREW WORK ON THEIR OWN** ([`plans/29`](plans/29-the-crews-own-work/README.md),
complete 2026-08-28) — a crew member nobody has told anything takes the
nearest of four jobs **inside their own senses** (3 hexes untrained), and
press **G** beside one to narrow them to a single job they will then hunt
across the whole map.  ⚠⚠ **The pillar comes out intact as arithmetic**:
the default is worth **+44** ticks where the work is near and 0 where it
is not; one order is worth **+34** where it is far and 0 where it is near
(`@M069`, `@M070`).  ⚠ `helper_drive` is an ORDER the search does not
overrule (`@X296`), which is how a `.keys` fixture says *stay*.

⚠⚠ **THE PLAYER CAN BUILD** ([`plans/27`](plans/27-building/README.md),
complete 2026-08-27) — `ROADMAP.md` § The critical path item **3** is closed.
Press **Q** and every hex you drive over is ordered as a wall your crew raise
(**+44 ticks on a base that otherwise falls at 130**, `@M050`); press **E** at
the core and 100 of the opening 200 points becomes a tower beacon to carry out
and plant (`@M051` — the first thing the wallet has ever bought).  ⚠ Still
unbuilt and deliberately out of scope: **helper orders**, the 8-walls wave
trigger, bridges, and `@X252`'s *directed* helpers.  ⚠ The next item on the
critical path is **4, the SCRAMBLE**.

### The three gates, and their numbers

| gate | command | today |
|---|---|---|
| tests | `scripts/test.sh` | **1843 green**, ~320 s on a busy box (592 s beside another suite, 713 s, 753 s, 775 s and 1252 s — one of them a `graphics` cdylib rebuild), 151 files |
| scenarios | `scripts/validate.sh` | **78 scripts, 1383 measurements**, ~20 s |
| drawn pixels | `scripts/validate_gl.sh` | **3 fixtures, 55 measurements** (needs xvfb) |
| ⚠ decision tags | `scripts/tags.sh` — inside `test.sh` | **446 defined, 442 cited, all resolve** (`@X325`).  A dangling `@X` reads as authoritative and answers nothing |
| ⚠⚠ formal rules | `scripts/rules.sh` — inside `test.sh` | **49 defined, 30 ENFORCED in 156 code sites** (`@X327`).  ⚠⚠ **The `M-*` family went 6 covered → 10 by EVALUATION, and 2 of 6 would have been MIS-CITED** — `@FR-M-Waterline` had TWO implementations (`hex_ground` and `hex_surface_index` restated the threshold line for line, and the second one SAID SO in a comment), and `@FR-M-Besiege` is worded in `can_stand` + `can_climb` where `enemy_target` asks `can_step`.  ⚠ `@FR-M-One-AI` is **not citable at a site at all** — *no second mover exists* is a claim about ABSENCE and wants `@X333`'s `src/` sweep test.  ⚠⚠ **`--review` cannot answer this question**: it reports only rules with MORE THAN ONE site, and an uncovered rule has none.  ⚠⚠ **This read 49 until 2026-08-29 and the extra one was a GHOST**: `docs/FORMAL.md`'s worked example showing *where a rule is DEFINED* is a fenced line at exactly two spaces, so **the document teaching the registry was silently adding to it** — and the phantom RESOLVED, so the gate could never go red.  ⚠ `<!--norule-->` existed for the CITATION half of that hazard and the DEFINITION half never got it.  ⚠⚠ **And the fix produced its own false positive immediately**: a file is classified by its EXTENSION, so the checker's own `.sh` comment naming the rule counted as an ENFORCING site — ***prose about a rule that lives outside `docs/` reads as enforcement***.  ⚠⚠ **`plans/32` moved it by FOUR while defining THREE**, and the fourth is the interesting one: `@FR-W-Position-Hash` was a WORLDGEN rule with no code anywhere, and the LANDING gave it its first — *a rule earns code from whichever plan first needs it, not from the plan that wrote it*.  ⚠⚠ **And the gate caught a citation broken by a LINE WRAP** — a rule name hyphenated across two lines of a doc reads as a dangling tag naming a rule that does not exist, which is `@X325`'s whole point one family over.  ⚠ **Never let a `@FR-`/`@X`/`@M` tag straddle a line break**; reflow the line instead.  ⚠ `plans/31` added `@FR-R-Carry-Once` and `@FR-R-Opening` and gave both code the same day, which is the order `@X328` asks for.  ⚠ It moved at `plans/30` R5 because `src/poi.loft` gave `@FR-E-Poi-Owns` and `@FR-E-Place-State` their first code at all — never because citations were sprinkled (`@X328`).  ⚠⚠ **This read 89 until 2026-08-29 and the tree measured 93 before R7a touched it** — the number aged against a tree that moved, which is `@M044`'s class one instrument over: **re-measure before quoting a delta**.  R7a's own contribution is **three**.  ⚠ Resolution GATES; coverage only REPORTS.  ⚠⚠ A doc reference is **not** an enforcing site (`@X330`).  ⚠ `plans/33` E1 added `@FR-C-Ledger-Drawn` and gave it code the same day, which is `@X328`'s order again — and it is a rule the DESIGN section it belongs to had only half stated: `DESIGN.md` § Carry visibility said *carried* and said nothing about the ground, so a beacon set down was drawn by nothing and **nothing was wrong** |

⚠ `scripts/test.sh` is the canonical runner — **never `loft test` directly**
(§ Key commands says what it does that you would otherwise skip).

⚠⚠ **The tests number in that table READ 1407 UNTIL 2026-08-26 AND THE TREE
MEASURED 1395** (`@M044`) — same tests, twelve fewer collected, because
`loft test` collects zero-argument functions in a test file and the binary on
PATH is dated **2026-08-25** where `@M043` measured on 2026-08-18.  **A suite
total that does not reconcile is a finding** (`@M041`); re-measure the baseline
before quoting a delta, because the number in a doc ages against a binary that
moves ([`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § The BINARY moves under you).

⚠⚠ **`loft test` HARD-KILLS AT 300 s BY DEFAULT and the suite is close
enough that a busy box kills the run** — the message names a PARSE phase in
an unrelated file and reads exactly like the cdylib fault.
**`LOFT_TIMEOUT=1500 scripts/test.sh` is the way through it.**  ⚠ It is also
a real budget constraint on new tests: one phase's first version cost 63 s
alone and pushed the run over the cliff.

⚠ **Do not run two `scripts/test.sh` at once** — both pre-clean
`tests/actual/`, so they clobber each other and fail for no reason.
⚠⚠ **`scripts/gate.sh start` REFUSES to, which is the rule enforced
rather than documented** — and it is the entry to reach for whenever you
are not going to sit and watch the run (§ Key commands).

⚠ **Both gates run INTERPRETED, and that is not a preference** — on the
native backend `load_palette` answers 0 entries
([`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)), which no test could see
because `loft test` runs the interpreter only.

⚠⚠ **Before believing any timing figure here, look at what else is running.**
Three sessions have now measured the suite against a busy box and two
declined to rewrite the number; both were right to.
[`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § The wall clock is not yours alone.

⚠⚠ **The `graphics` cdylib can take out BOTH gates, and it is a toolchain
fault every time** — *"native function not loaded"*, a 300 s hard-kill in an
unrelated PARSE phase, a `SIGABRT` at the end of a green run, or
`rust-lld: unable to find library -lloft_graphics_native`.  Not reproducing
as of 2026-08-17.  ⚠ Two tidy explanations have been FALSIFIED, so do not
re-derive them: [`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § The `graphics`
cdylib fault has the symptoms and the hand rebuild that works.


## Hard-won rules — [`docs/HARD_WON_RULES.md`](docs/HARD_WON_RULES.md)

**Every rule there cost a real defect to learn, and most of them describe a
test that CANNOT see the thing it appears to test.**  ⚠ The headlines are
below so the warning fires without a lookup; **the evidence, the numbers and
the dates are in the file** — go and read the section before changing the
thing it names.

### Movement + passability — [detail](docs/HARD_WON_RULES.md#movement--passability)

- **How an enemy moves today**: `wave_tick` rebuilds the distance field ONCE
  per tick, one field per climb limit; `enemy_tick` steps down it.  Two
  steering modes hand off at the **scrambler bubble** (a straight-line 25
  hexes, never a route length); an enemy with no route follows the DESIRE
  field and besieges what it cannot climb.  ⚠⚠ Since BACKLOG C3 the mover
  asks **`enemy_engaged`** — *cut off AND inside* — never the geometry
  alone: they are one question only because `wave_cutoff` sweeps at the
  top of every tick, and a DARK core separates them.
- ⚠ **A robot climbs 2.0 m** (`CLIMB_REGULAR`), and the number is DERIVED:
  a single-hex body ramp onto a structure `H` high needs `H / 2`, so 2.0 is
  the interior of four constraints — `src/passable.loft` § Why a robot climbs
  2.0 m.  It was 0.0 until plan 12 B1.
- ⚠ **Rubble is a LAYER, never a repaint** (`src/height.loft`) — a pile makes
  the SURFACE `rubble` while the authored ground underneath is untouched, so
  clearing restores exactly what was authored.  That is what dissolves the sea
  trap: a breach that ERASED its hex would be *less* passable than the wall it
  replaced, while "the wall broke" asserted true.
- ⚠ **The SURFACE is not the painted kind** — a hex under a pile stands on
  `rubble`.  `painted_ground` answers the HEIGHT, `hex_ground` the SURFACE;
  swap them and piling debris onto a wall LOWERS it.
- ⚠ **Passability is TWO questions**: the field filters NODES by `can_stand`
  and EDGES by `can_step`.  Filtering nodes by `can_occupy` is **vacuous** —
  it compiles, reads well, and deletes the height rule with no test moving.
- ⚠ **A drop is free and a climb is not**, which is why `flow_build` asks
  `can_step(n, a)`: the sweep runs outward and the enemy walks inward.
- ⚠ **You attack what you could STAND on and cannot climb** — an enemy at the
  water's edge besieges nothing.
- ⚠⚠ **THE CHASSIS FLOATS AND THE DEPTH IS THE COST** (`@D006` closed by
  BACKLOG C10, `@X286`) — `drive_along` asks `can_hover`, so the player and
  the crew cross flat sea for free, **fall INTO a trench** (a drop always is
  free) and then owe a climb out that 0.4 m has not and a 3.0 m boost has.
  ⚠ So *boost is the only way out of a base you have sealed* is true of
  trenches again, and the palette's 0-1-3-8 is priced against the boost:
  `water` and `rapids` are trenches a boost leaves, a `waterfall` is a hole
  nothing gets out of.  ⚠ ONE rule, two DOORS — `can_travel` is the rule,
  and a second traversal is what plan 11 F1 forbids, not a second door.
  ⚠⚠ **`steep_rock.walk_vehicle` went FALSE**, and it is a correction: a
  0.4 m clearance does not clear a cliff, and a cliff has no HEIGHT to be
  stopped by until plan 02 — so `walk_ground: false` was carrying the whole
  of *this is a cliff*, and `the_gap_03`'s *the gap is the only way through
  for anybody* depends on it.  The two columns now differ for the four
  WATER kinds and nothing else.
- ⚠⚠ **A PILE IS A SURFACE ONLY ONCE IT CLEARS THE WATER** (`src/moat.loft`) —
  `hex_ground`'s threshold is the hex's own DEPTH, not zero, which is what gives
  the palette's `drop` a job: `water`'s 1 m swallows **two bodies**.  ⚠ On land
  and on the sea the depth is 0.0, so it is the old `rise > 0` unchanged
  everywhere anybody has ever painted.
- ⚠ **ONE AI, per-class DATA** — a design rule, not an accident.  A class that
  needs its own mover has broken it; the four small robots cost one row each.
- ⚠ **Blocked by a COMPANION → step beside; blocked by the GROUND → stand and
  attack** (F7b).  The condition is the whole rule, and it was missing for
  three phases and was the whole BALANCE (161/311/180 → 61/104/95).
- ⚠⚠ **THE SIEGE FRONT IS THE WALL'S WIDTH** (`@M020`) — 4 hexes on a
  five-row wall, 6 on a seven-row one.  A wave is worth its front class PLUS
  what the front cannot COVER; **the screen is arithmetic, bodies against
  face width**.  ⚠ It was THREE for any length until plan 24, and five
  documents named the wrong fix.
- ⚠ **The siege chews where the ROUTE meets the wall**, never where it is
  weakest — and plan 24 did NOT change that, which is the surprise.

### Cost — [detail](docs/HARD_WON_RULES.md#cost)

- **The tick's budget is ~667 ms**, derived from the design's own numbers.
- ⚠ **Do not reach for a standalone stopwatch** — an unchanged probe answers
  173 / 737 / 754 ms on three runs, because discarded structs are not freed.
  `tests/11_f8_the_tick_budget.loft` is the number of record.
- ⚠ **NEVER bind a `FlowField` to a local in a per-enemy path** — a
  whole-value bind COPIES the heap value: 2250x, unseen since F5.
- ⚠ **A copy changes no behaviour, only cost**, so 490 green tests sat over a
  tick 25% past budget for four phases.  The gate is a RATIO, not a stopwatch.
- ⚠ **The incremental field rebuild is deliberately NOT built** — and its
  third trigger (**the TICK getting shorter**) is now ARMED, which is why
  [`plans/22`](plans/22-the-field-cache/README.md) is the prerequisite for a
  shorter tick rather than a follow-up to it.

### Testing something that moves — [detail](docs/HARD_WON_RULES.md#testing-something-that-moves)

- ⚠ **A 1-hex corridor cannot tell a flow field from a fixed heading** — every
  enemy test dryopea had was blind to the field when it landed.
- ⚠ **A corridor cannot see F5c either**: on a hex AXIS the field offers ONE
  closer neighbour, off it TWO — so a blocked enemy has no *beside*.
- ⚠ **A wave spawns STACKED** — `range` over a walking wave is a SPAN.
- ⚠ **Route every step through `lat_neighbour`** — a `+ 1` on a `q` anywhere
  else is the bug, and it is how moros#10 sheared every reach computation.
- ⚠ **A walking test must paint the ground it walks on** — an unpainted hex
  IS sea.
- ⚠ **A world where every source hex is at 0 m cannot tell a RISE from a
  destination height** — the whole height rule can change, suite green.
- ⚠ **"N enemies attack N hexes" does NOT gate the desire field** — measured;
  their spawn headings already spread them.
- ⚠ **A MIRRORED base is not a symmetric one** — 112 vs 211 ticks on a map
  that looks mirror-symmetric, and none of it was the crew's.  `q -> -q` is
  not a symmetry of this lattice; control for BRACING first.
- ⚠⚠ **A gate that reads PERFECT is as suspect as one that reads wrong** —
  0.0 rad of disagreement, twice, for two unrelated reasons.  **The missing
  control is generic and costs two lines: can this gate produce a non-trivial
  reading at all?**
- ⚠⚠ **AND THE VACUITY CAN BE IN THE NUMBERS RATHER THAN IN THE ASSERTION**
  (`@M034`) — *a 1 Hz clock driven by a 30 Hz clock* cannot disagree for ANY
  implementation, because 1 Hz is exactly thirty 30 Hz steps.  **So the second
  form of the control is: are the numbers I chose capable of disagreeing?**
  ⚠ And a **negative control the plausible WRONG version also satisfies is not
  one** — a DEFERRING cap answers fewer ticks too (4 vs 24), so the control has
  to be that version reproduced, not the null one.
- ⚠⚠ **SEVERAL COUNTS IN ONE TEST FUNCTION ARE RANKED, NOT INDEPENDENT** —
  loft abandons at the first failed assertion, so three of four counts are
  unmaintained decoration.  ⚠ Where the rows are ONE claim about one subject,
  the fix is not four functions but one assertion whose MESSAGE carries every
  reading (`tests/26_l0`).
- ⚠⚠ **A CONFORMANCE GATE CANNOT SEE A PAIR THAT STOPPED TOGETHER** (`@X337`)
  — a frozen rule agrees with a frozen body, so four equality gates read green
  over a guard that had stopped patrolling for ever.  **Liveness is a SECOND
  gate**, and the question it asks is the one an equality never can: *did it
  actually get anywhere?*  ⚠ And **a gate aimed at a hazard somebody NAMED can
  still miss it**, because the fixture may make the wrong version behave
  identically (`@M075`) — R4 hit that twice more and **swept the
  fixture's one free number** to find one that cannot be (`@M076`).
- ⚠⚠ **ANY QUANTITY A CONFORMANCE CHECK SUBTRACTS IS A PLACE A DEFECT CAN
  HIDE** (`@D008`) — a mover that lost hexes charged them to `slip`, the
  rule is read at `now − slip`, and **the equality followed the body
  down**: twelve hexes of drift with every count green.  *Ask what your
  equality is allowed to absorb*, and ask liveness of a member that can
  actually reach the branch.
- ⚠⚠ **A TIDY EXPLANATION FOR A MEASURED EFFECT IS A HYPOTHESIS, AND THE
  SWEEP FOR ITS EVIDENCE COSTS ONE RUN** (`@M094`) — a wall in front of a
  tower measured at **−80 ticks**, `plans/12` B7 had already published the
  reason (*its own dead ramp*), and a `pile` sweep of thirteen approach
  hexes read **0.0 at every one**.  ⚠ **A prior measurement that predicts
  your result is the most persuasive wrong explanation available**, because
  it arrives pre-argued.
- ⚠⚠ **A GATE AIMED AT THE MECHANISM YOU EXPECT TO BE THE HAZARD IS NOT ONE
  AIMED AT THE HAZARD** (`@M025`) — and its own control is what said so.
  *The right code with the wrong justification* is what to look for when a
  gate refuses to fail.
- ⚠⚠ **A COST GATE CAN BE A COUNT INSTEAD OF A CLOCK, AND USUALLY SHOULD BE**
  (`@M029`) — two identical calls differed **5.4x**.  Ask what the change
  would actually DO before reaching for a stopwatch.
- ⚠⚠ **A COUNT IS PERMUTATION-INVARIANT, SO IT CANNOT SEE A MIRRORED WORLD**
  (`@M027`) — every band green at 490.8 px of error.  **Any gate that counts
  pixels needs one assertion about WHERE.**
- ⚠⚠ **AN EMPTY ARTEFACT SATISFIES EVERY EQUALITY** — `mesh_crc` of an empty
  mesh is 0, so every equality carries a non-zero floor.
- ⚠⚠ **A SMOOTHER MEASURED ON THE THING IT SMOOTHS CANNOT SEE WHAT IT FAILS
  TO SMOOTH** (`@M035`) — `@M023` proved the camera's ease by reading
  `cam.target`, and every number in it is true.  In PIXELS the eased camera
  leaves **96 px** of jump on the vehicle where the un-eased one leaves **0**:
  **the ease moves the jump off the world and onto the mover**, and the one
  thing a follow camera cannot smooth is what it is chasing.  ⚠ Point the gate
  at the ARTEFACT, not at the mechanism.  ⚠ And a fix applied to half a
  composition leaves a FRACTION behind — alpha under a hex-following camera
  takes 96 px to **14**, not to 0.
- ⚠ **A gate whose reading is already saturated cannot see what you built** —
  price the SUPPLY against the CAPACITY before believing a flat reading.
- ⚠ **A cost gate over a world with none of the thing you changed is not a
  gate** — and even a healthy one cannot see a 20x regression in a 3% share;
  price the ALTERNATIVE and compare.

### Profiling the suite — and why the wall clock cannot do it — [detail](docs/HARD_WON_RULES.md#profiling-the-suite--and-why-the-wall-clock-cannot-do-it)

`LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1`.  Method, numbers of
record and dates: [`docs/PROFILING.md`](docs/PROFILING.md).

- ⚠ **The report goes to STDERR** — a plain `> out.txt` silently drops it.
- ⚠ **Read the SAMPLE COUNT, never the seconds** — the wall clock has pointed
  the wrong way twice on real improvements.
- ⚠⚠ **A profile AGES, and the stale one gets quoted** — *"58% is `canvas()`"*
  was cited three plans after it stopped being true.  Re-profile, quote the
  date.  Three readings now agree: the field family is **~69%**.
- ⚠ **A test that RE-DERIVES an expensive value a sibling already computed is
  the cheapest thing to find** — −10.1% of the suite, assert counts identical.
  ⚠ And the refactor that LOOKS identical measured as **free**.

### Timers and epsilons — [detail](docs/HARD_WON_RULES.md#timers-and-epsilons)

⚠⚠ **A family of SIX sites, found one at a time over four plans, and
`fixstep` is what finally killed the class.**  The detail — every
measurement, in the order it was found — is in the linked section; these
are the rules it left behind.

- ⚠⚠ **A COUNT asked for in SECONDS comes back SHORT** — `n * TICK_SECONDS`
  through an accumulator answers `n - 1` for **602 of the first 1000 `n`**,
  and neither exactness nor an epsilon saves it.  That is why `play_ticks`
  and `play_advance` are two verbs.
- ⚠⚠ **DIRECTION is not the discriminator — a GUARD is.**  Counting UP and
  counting DOWN accumulate differently and **neither is safe**; the healthy
  sites were the guarded ones and two of them counted DOWN (`@M033`).
- ⚠ **An epsilon whose removal leaves the suite green is a guard that cannot
  fire** — exercise the branch directly.
- ⚠⚠ **And the branch test is NECESSARY, not SUFFICIENT: a guard can be
  invisible at the value you SHIPPED** — at 1.5 hex/s the carry is 0.0 for
  ever (`@M014`).  **Sweep the NEIGHBOURS of the shipped value** (`@M013`),
  and then **ship one that can see it** (`@X063`, `@M017`).
- ⚠⚠ **THE INSTRUMENT IS A CROSS-PRODUCT — sweep the tick length AND every
  mover** (`@M030`, `@D003`).  The worst member never got a guard at all:
  the player read **180 / 120 / 180 / 0 / 0 / 0 / 0** hexes a minute against
  a true 180 and stopped moving entirely under a 250 ms tick, while every
  banked mover was exact at all seven — and three accidents hid it, the
  third being that the ONE tick-length gate in the repo banked an ENEMY.
- ⚠⚠ **Two agreeing instruments are not a control; the TRUE count is.**
  Over six exact-multiple durations the two float directions disagree at
  ONE and **agree while both being a tick long at FOUR**.
- ⚠⚠ **In integer base units there are no longer two directions to be
  unsafe in**, and every epsilon in the family is **deleted rather than
  zeroed** — `fixstep`'s `Bank`, `Timer` and `TickClock` are what is left.
  ⚠ A `Timer` MAY hold its `total` where a `Bank` may not hold its `whole`
  — same [loft#914] rule, opposite conclusion.  ⚠⚠ **And the census was
  SEVEN and there are EIGHT**: the tower's CHARGE is still a hand-rolled
  float `bank_gain`, pinned rather than converted.
- ⚠⚠ **A measurement's resolution is not its authority** — the 654 gate
  measurements could not see a 5e-7 shift in the tick's LENGTH that broke
  **17 tests** (`@M031`, `@X079`).  ⚠ The step's BASE UNIT is the decision,
  and µs is the wrong one: 2/3 of a second is not a whole number of them.

## Relationship to loft

loft is the language + runtime; dryopea is a consumer project.
Dryopea is also the **second partner** for loft's universal
hex-world editor (loft `lib_plan 24`) — moros is the first;
dryopea drives the bug-hunt phase that hardens the shared
libraries.

When dryopea surfaces a need from loft — a language feature, a
stdlib gap, a runtime bug — **file it as a GitHub issue on
`loft-lang/loft`** (`gh issue create --repo loft-lang/loft`;
`jjstwerff/loft` redirects there).  A write-up that stays in this
repo is not filed: `QUESTIONS_FOR_LOFT.md` is dryopea's outbound
queue, not loft's inbox.

The flow, in order:

1. Cut the minimal reproducer into
   [`loft_repros/`](loft_repros/README.md) and check it fails
   standalone on the backends you claim.
2. `gh issue create` with the repro **inline** in the body —
   dryopea is a separate repo, so a link into `loft_repros/` is
   not self-contained.  Label it: `bug` / `enhancement`, plus
   `sev:*`, `area:*`, `wa:*` and `hit-by:dryopea`
   (`gh label list --repo loft-lang/loft` for the set).  Search
   open AND closed issues for the shape first.
3. Record it in [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)
   under **Submitted** with the issue link, per that file's own
   Open → Submitted → Resolved convention.

Do **not** fix it locally by patching loft from this repo; loft
has its own contribution flow.  Internal-to-dryopea bugs go in
[`PROBLEMS.md`](PROBLEMS.md) with `@D<NNN>` IDs.

## Key commands

dryopea uses the **installed** `loft` binary — **`loft` on PATH, which is
`~/.local/bin/loft`**.  ⚠⚠ This said `/usr/local/bin/loft` until 2026-08-18 and
that path is a DIFFERENT, OLDER binary on this box: both report
`loft 2026.8.0`, so **the version string cannot tell them apart and only the
mtime can**.  ⚠ It matters — plan 20 A2 spent a session blocked on a heap
corruption that the PATH binary had while `/usr/local/bin`'s predecessor also
had it, and a rebuild during the session fixed it silently
([loft#969](https://github.com/loft-lang/loft/issues/969)).  **Check
`which loft` and its mtime before believing any toolchain symptom.**  There is no local loft build step: the
libraries it depends on resolve from the loft package registry
via `loft.toml` + `loft.lock`, so no `--lib` path is passed
anywhere.

```bash
# ⚠⚠ RUN THE GATES DETACHED — the way to do it when you are not going to
# sit and watch.  `scripts/test.sh` takes MINUTES on a busy box, and
# polling its log costs a look a minute and answers "still running" most
# of them.  `start` returns at once; `wait` blocks ONCE and then prints
# the FAILING ASSERTIONS rather than the log; `status` answers without
# waiting and re-reads the PID, so a run that was OOM-killed reports DIED
# instead of hanging a waiter for ever.
#   ⚠ `start` also REFUSES a second run while one is going, which is
#   § Do not run two `scripts/test.sh` at once enforced rather than
#   documented.  Modelled on loft's `scripts/ci-run.sh`.
scripts/gate.sh start          # or: make gate
scripts/gate.sh wait           # or: make gate-wait   ← launch in the BACKGROUND
scripts/gate.sh status         # or: make gate-status
scripts/gate.sh report         # the last run's failures again
GATES=test scripts/gate.sh start        # test only; validate / full also
# Run dryopea's test suite (canonical entry — DO NOT run `loft test` directly).
# ⚠ Fine in the FOREGROUND when you mean to wait for it; use gate.sh otherwise.
scripts/test.sh

# Play every tests/scripts/*.keys and gate on what they measure —
# the SECOND gate (plan 08 V4).  Prints each measurement beside its
# band, writes a PNG per `snap` into shots/, exits non-zero on a
# reading out of band.  `make validate` is the same thing.
scripts/validate.sh                  # all of them (~11 s)
scripts/validate.sh paint-a-base     # just one, while iterating

# Draw every tests/gl/*.keys through REAL GL and classify the frame —
# the THIRD gate (plan 25 M3).  Needs xvfb; `validate.sh` deliberately
# does not, so a machine without it still runs the 654.  Captures land
# in shots/gl-*.png.  `make validate-gl` is the same thing.
scripts/validate_gl.sh               # both fixtures
scripts/validate_gl.sh the-ground    # just one

# Run the game / editor (opens a 960x720 GL window; P toggles play).
# Use `make play` — it passes --interpret, and the NATIVE backend is
# broken for dryopea today: it panics on the marker load, and where it
# does not panic it silently loads an EMPTY palette (both filed in
# QUESTIONS_FOR_LOFT.md).  `loft src/main.loft` is `make play-native`,
# kept for testing the eventual fix.
make play
# One of the three AUTHORED maps in maps/ (BACKLOG A2) — repo content.
make play MAP=starter_01
# Open one of the 50 `.keys` scenarios as a live starting position
# (BACKLOG A1).  ⚠ `script=`, never `--script` — loft strips a leading
# `--` argument as its own and the entry would open a MAP of that name.
make play SCRIPT=a-base-that-plays-its-list
make play SCRIPT=tests/gl/an-island.keys

# Rebuild the authored maps from their `.keys` sources (BACKLOG A2).
# ⚠⚠ NOT a gate — it WRITES maps/*.json, which are COMMITTED, because
# `make play MAP=` loads them and a fresh checkout has no build in it.
# `make maps` / `make maps MAP=starter_01` are the same thing.
scripts/build_maps.sh
scripts/build_maps.sh starter_01

# Parse-check a single .loft file without running it.
# ⚠⚠ NOT the aggregator — `loft --native-emit … src/dryopea.loft`
# PANICS today (loft#962: a const initialised from a sibling module's
# const), naming `spawn.loft::per_tick`, which is not the cause.  Every
# real entry and all 91 test files compile clean; check one of those.
loft --native-emit /tmp/check.rs src/<file>.loft
loft --native-emit /tmp/check.rs src/main.loft      # the entry check

# Inspect a dependency's public API (never guess a signature)
loft api                 # every reachable library + its path
loft api graphics        # one library's full public surface
```

`scripts/test.sh` is the canonical test runner.  It:
- Creates `tests/actual/` — it is gitignored, so a fresh
  checkout does not have it, and neither `save_png` nor the file
  writer creates parent directories.  Without it every write
  silently goes nowhere and the golden tests fail as a
  "mismatch" against a file that was never written.
- Pre-cleans `tests/actual/*.png` and `tests/actual/*.json`
  between runs so stale artefacts can't masquerade as current.
  **Running `loft test` directly skips this** and leaks a save
  file into the next run's cold-start assertions.
- Invokes `loft test` against the dryopea `tests/` directory,
  with warnings VISIBLE (the suite is kept warning-clean).
- Exit code 0 = all green; non-zero = failures (the loft test
  runner surfaces assertion failures as FAIL since `@P367`
  shipped on the loft side).

### Relative paths resolve against the PROGRAM's directory

A relative path in a `.loft` file resolves against
`source_dir()` — the directory of the program entry, not the
process cwd, and not the directory of the file containing the
`file()` call.  Under `loft test` the entry is the test file, so
`source_dir()` is `tests/`.

dryopea's paths (`examples/palette.json`, `tests/golden/…`,
`maps/…`) are all repo-root-relative, so every entry point
declares the **`#cwd`** directive at the top of the file, before
the first declaration.  That restores cwd-relative resolution,
and both `scripts/test.sh` and the `Makefile` run from the repo
root.  A new test file needs `#cwd` or its palette load and
golden compare will silently miss.

⚠ **`#cwd` is legal only in a program ENTRY.**  A file carrying it
cannot be `use`d as a library — the import fails to parse with
`Syntax error: unexpected '#' at <file>:1:2`, and the aggregator
goes red naming the importer rather than the directive.  So an
entry point cannot also be an aggregator member, which means it is
compiled by nothing and every entry must stay a shell with no
decisions in it: `main.loft` over `editor_step.loft`,
`validate_main.loft` over `validate.loft`.

## Architecture — src/ layout

⚠⚠ **This is a ONE-LINE INDEX.  The full listing — what each file owns,
the trap in it, and the key data structures — is
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).**  Read that file's entry
before editing any file below; each `.loft` file's own header is the
source of truth and the listing is a navigational summary of it.

| File | Owns |
|---|---|
| `dryopea.loft` | the library aggregator — `use dryopea;` brings every submodule into scope |
| `main.loft` | the interactive entry, the GL shell.  ⚠ NOT in the aggregator; parse-check it by hand |
| `editor_step.loft` | **the editor's input seam** — `EditorState` + `EditorInput` + `editor_step` |
| `play.loft` | **the game's seam** — `PlayState`, `play_ticks` / `play_advance` / `play_step`, and the ONE call to `wave_tick` |
| **`fixstep`** (library) | ⚠⚠ **a PACKAGE, not in `src/`** — `TickClock`, `Bank`, `Timer`, the `approach` ease.  `loft api fixstep` is the surface |
| `play_view.loft` | **what a live session LOOKS LIKE** — the composition `main.loft` calls in play mode |
| `bindings.loft` | **the ONE key table** — keys → actions → `EditorInput`.  ⚠ Never add a `gl_key_pressed` |
| `script.loft` | the `.keys` script runner and its whole vocabulary — commands name ACTIONS, never keys.  ⚠⚠ Since `plans/30` R7a it can say a PLACE: `poi` / `route` / `mob` / `routine` (`@X345`), and **a route ENLISTS its own population**.  ⚠⚠ Since `plans/32` L3 it can say a LANDING: `land <q> <r> [seed]`, which **runs the landing rather than recording its result** (`@FR-L-Landing-Is-A-Function`) — a `.keys` file names the seed because dryopea has no RNG |
| `scenario.loft` | **a `.keys` scenario opened as a live STARTING POSITION** — and the command line's `script=` |
| `validate.loft` / `validate_main.loft` | the SECOND gate: sweep `tests/scripts/`, sum the measurements, report the FIRST failure |
| `maps.loft` / `mapbuild_main.loft` | **a MAP, as repo content** — its two files, how one is BUILT from a `.keys` source, and `map_fault` |
| `editor_view.loft` | `render_editor_frame` — composed ONCE for both the GL loop and `snap` |
| `measure.loft` | frame measurement — `classify_canvas` / `classify_world` → `FrameCounts` |
| `golden.loft` | `assert_golden` — write `tests/actual/`, compare bytes to `tests/golden/` |
| `compare.loft` | `state_diff` — are two runs in the same state? |
| `emit.loft` | write a situation down as `.keys`, plus `crop_keys`.  ⚠ The WRITER half of every `.keys` pair (`@D007`, `@D009`) |
| `reduce.loft` | cut a fixture to what a behaviour needs, against a predicate |
| `lattice.loft` | **THE lattice** — pointy-top odd-r offset, `Hex`, every `lat_*` verb.  Delegates to `hex_grid` |
| `relabel.loft` / `convert.loft` | plan 09's old-label → new-label bijection, and the `.keys` converter.  ⚠ `keys_schemas` carries **three** pair positions since `plans/30` R7a — `routine` is the one command with three hexes |
| `camera.loft` | `EditorCamera` + `camera_update`.  ⚠ pan NORTH is `r += 1` |
| `render_camera.loft` | **the GAME's camera** — `RenderCamera`, the two presets, `lat_to_world`, and the EASE (`CameraRig`, `camera_boom_free`) |
| `ground_mesh.loft` | **the GROUND, as triangles** — a six-triangle top fan per hex, one vertical quad per faced edge, one TILE per palette kind |
| `mesh_chunks.loft` | **the mesher's DOMAIN** — which hexes are drawn, which tile each lands in, which tiles an edit invalidates |
| `ground_gl.loft` | **the ground, DRAWN** — one flat-unlit shader, one `GroupVboSet` per palette kind, the colour as a UNIFORM |
| `gl_gate.loft` / `gl_gate_main.loft` | **the THIRD gate** — draw each `tests/gl/*.keys` through real GL and count with `classify_canvas` ITSELF |
| `mesh_crc.loft` | **do two mesh builds agree?** — the geometry folded to one integer, triangles included |
| `painted.loft` | `PaintedHex` / `PaintedWorld` — sparse, sea-default ground |
| `palette.loft` | `GroundType` + `load_palette` + `GROUND_RUBBLE` |
| `markers.loft` / `marker_file.loft` / `marker_render.loft` | the marker layer, its save format and its drawing.  `place_marker` is the ONE dispatch |
| `map_file.loft` / `save.loft` | the save record (6 fields — see § Known constraints) and the save/load path |
| `render.loft` | the software rasteriser over `graphics::Canvas` |
| `build.loft` | **THE BUILD ORDER** — the only way a structure comes into existence during a run.  ⚠ Work is stored as SPENT, in INTEGER units |
| `persist.loft` | **A PLANET — a place that REMEMBERS**.  `dryopea_planets/<planet>/<player>/world.json`; the GROUND and the MARKERS, not a run |
| `errand.loft` | **A ROBOT GOING ABOUT ITS BUSINESS** — `Traffic`, `errand_done` / `errand_depart`, `plans/30` R1's **ROUTINE** (`Errand`'s five anchors, the role TABLE, `errand_destination`), R2's **CYCLE** (`cycle_at` / `cycle_build` / `cycle_fault`) R3's **MOVER** (`errand_step`, `errand_fields`, `errand_cycling`), R4's **ENDING** (`Role.shift`, `errand_home_done`) and R4b's **TERMINAL LEG** (`Cycle.terminal`, `cycle_turn`, `errand_terminal`, `errand_shift_over`, `ERRAND_BAG_HOMEWARD`).  ⚠ The bubble takes the errand, ONE WAY.  ⚠⚠ **The BAG steers and never a clock**, and the table is INDEXED — never a `role ==`.  ⚠⚠ **The bank does not RESTART at a leg boundary** (`@X335`).  ⚠⚠ **`errand_step` is the ONE DOOR that moves a mob on a cycle and it owns `slip`** (`@FR-E-One-Door`); the position is exact in its PHASE and only conditionally in its HEX (`@X336`), and **a DWELL is not a BLOCK** (`@X337`).  ⚠⚠ **HOME is a LEG of the round** and a mob leaves the roster the tick its cycle brings it there (`@X338`); ⚠ `errand_fields` builds one field per ANCHOR and never per DESTINATION (`@D008`).  ⚠⚠ **A round whose home is OFF it grows a TERMINAL leg** (`@X341`), and **the turn is a third value of the BAG, compared in HEXES** (`@M077`) |
| `poi.loft` | **A PLACE THAT OWNS MOBS — and the BOUND that is the point of one** (`plans/30` R5).  `Poi`, the KIND table, the five states, `PoiRoute` / `PoiWorld`, `poi_errand`, `poi_bound`, and `Bound` + `bound_disc_holds` / `bound_holds` / `bound_meets`.  ⚠⚠ **The record is scaffolding; the BOUND is the phase** — `@FR-E-Poi-Owns` collapses *could this ever be in this window?* from `N` queries to ONE.  ⚠⚠ **The population is a set of ROUTES and never a list of BODIES**, so forty haulers on one route have the bound of one.  ⚠⚠ **The CLAIM is per-LEG and the UNION is only the QUERY** (`@X342`) — a union is SLACK, and against it four of nine mutations survived including the sidestep that breaks `@FR-E-Non-Increasing`.  ⚠ Read in LATTICE distance on purpose: a superset is the safe direction and it costs ONE hex across three maps (`@M079`).  ⚠ A POI is never CULLED, so there is no verb here that removes one.  ⚠⚠ **AND SINCE R6a IT KNOWS WHAT AN UN-MATERIALISED MOB IS** (`@X343`): `PoiMob { route, seat, slip, gone }`, where the round is an index, the phase a derived SEAT, the hex `cycle_at`, and the BAG and the BANK derived — **only `slip` accumulates, because only a body can be pushed**.  ⚠ The three tiers: `poi_culled` is per POI and is one `bound_meets` for a whole population; `poi_survey.asked` is the COST GATE and it is a COUNT.  ⚠⚠ **AND SINCE R6b IT RUNS IN THE GAME** (`@X344`) — a POI's whole bound folds to ONE integer (`poi_reach`), so the tick's per-POI question is one subtraction; `poi_step` is the MATERIALISER and it runs in `play.loft` BEFORE `wave_tick`.  ⚠ A dead robot is not a body its POI owns — `wave_deaths` MARKS rather than removes |
| `skill.loft` | **CREW SKILLS — build, repair, scout**.  `Skills` on `Helper`, `skill_factor`, and the DETECTION rule |
| `endure.loft` | **ENDURANCE — work spends it, rest restores it**.  ⚠ A tired person works LESS and never stops |
| `jammer.loft` | **THE JAMMER SWITCH — turning your own core off**.  ⚠ It stops the SUPPLY and never the SIEGE |
| `task.loft` | **A JOB, and which one a crew member goes to** — the four `TASK_*` kinds, `jobs_in_scope`, `job_pick`.  ⚠⚠ **The remit trades BREADTH for REACH**, and the radius is the crew member's ALONE |
| `trap.loft` | **A TRAP THAT DOES NOT AUTOMATICALLY RESET** — placed in advance, fires ONCE, re-armed by a standing vehicle.  ⚠ The trigger is a CROSSING, never a standing position |
| `moat.loft` | **A MOAT — the one hex whose surface is BELOW the ground around it**, the palette's `drop` read at last, and what a besieger shovels into one.  ⚠⚠ Its depth decides ONE thing: how much it takes to FILL — so it is a TIMER |
| `font.loft` | **THE FONT — the ONE seam to `graphics::draw_text`**.  ⚠ The path is ABSOLUTE and that is enforced at the door |
| `picker.loft` / `hud.loft` / `editor_mode.loft` / `chunks.loft` / `history.loft` | palette UI, HUD (⚠ and **the ONE number the game shows** — the wallet), the mode flag, the dirty-chunk set, undo/redo |
| `spawn.loft` | **the tick** — `WaveState`, `wave_tick`, enemy movement, targeting, deaths, the schedule, `TICK_SECONDS` |
| `waves.loft` | the authored wave list, its lull, and what a wave is MADE OF.  ⚠ A wave's size is SUMMED from its parts |
| `flow.loft` | the distance field — `flow_build` / `flow_step` / `flow_steps` / `flow_desire`, and `flow_route`: ⚠⚠ **a leg is a PATH and never a line** (`@M071`) |
| `passable.loft` | may a class MOVE here? — `can_stand` / `can_step` / `can_occupy`, `hex_height`, and the SIGHT line |
| `occupancy.loft` | who is standing where this tick — enemy counts, and the separate `BlockerMap` |
| `height.loft` | the RUBBLE layer — metres piled at runtime, and what they are made of |
| `damage.loft` | what a structure has TAKEN, bracing, and `break_structure` |
| `tower.loft` | towers — range, the banked charge, the 30-shot magazine, LOS, repair, the detachable top |
| `wallet.loft` | the run's budget, and `wallet_broke` — ⚠⚠ POVERTY, not an ending (`@X292`) |
| `scramble.loft` | **THE RUN'S ENDING, and the only one there is** — ⚠⚠ *the base never ends itself* (`@X293`).  Drive onto the core's own hex and stay six seconds; exiting CANCELS and resets.  ⚠ Beside the core you TRADE, on the core you LEAVE (`@X294`) |
| `vehicle.loft` | the PLAYER — drive, boost, salvage.  `salvage_at` is the shared chassis |
| `landing.loft` | **WHERE THE BASE GOES — a pick, a map and a seed become a landed base** (`plans/32`).  ⚠⚠ **A landing MOVES the authored core rather than creating one** (`@FR-L-Map-Stays-Valid`), so a map stays valid as authored and **a map cannot be checked against a pick nobody has made yet**.  ⚠⚠ **Random is a HASH OF POSITION and never a stream** (`@FR-W-Position-Hash`, given its FIRST CODE here) — dryopea has no RNG.  ⚠ **The hashed offset is the START and the search is the RESCUE**, bounded from the PICK: an unbounded outward search always succeeds, on the far side of the map.  ⚠ **A refusal changes nothing** (`@FR-L-Landing-Is-Total`) and **the TOWER is dropped, not the landing**.  ⚠⚠ What a pick is WORTH is an open design question (`@M091`) |
| `helper.loft` | the NPC crew — banked movement, wrecking, and the 60 s recovery |
| `carry.loft` | one record per carryable thing, with an `owner` — conservation is STRUCTURAL.  ⚠ Since `plans/30` R7b an owner may be a MOB (`BLOCKER_MOB + PoiMob index`), which is an identity that OUTLIVES a body.  ⚠⚠ Since `plans/33` E1 there is a FIFTH kind, `CARGO_FIND`, and **`subj` is the POINTS it pays at the core** (`@X349`) — with the tripwire for a second yield written into the constant |
| `part.loft` | **what an entity IS** — the `Socket` a part offers and the `Binding` that fills it, over `hex_body::Rig` |
| `catalogue.loft` | **what each entity is MADE of** — the hover unit, the robot, the tower base + top, the cargo pylon, as `Limb` tables.  ⚠ `cat_cargo` is ONE part for all five cargo kinds (`@X349`) |
| `part_mesh.loft` | **a part, as TRIANGLES** — and it contains no forward kinematics |
| `pose.loft` | **the pose comes from the SIMULATION** — ⚠ read the sim, never a second flag |
| `entity_view.loft` | **the ROSTER, as triangles** — ⚠ nothing here is STATE.  ⚠⚠ Since `plans/33` E1 the ledger is drawn too (`entity_bake_cargo`), and it filters on **`CARGO_GONE` and nothing else** so the frame's count and `cargo_count` are one number by construction (`@X349`) |
| `entity_gl.loft` | **the entities, DRAWN** — one `GroupVboSet` per drawn class, re-upserted WHOLE every frame |

## Important conventions

### Hex convention

**Pointy-top, odd-r offset** — `hex_grid`'s convention, which every
`hex_*` library and moros already speak.  `src/lattice.loft` is the
layer and it DELEGATES to `hex_grid`, so dryopea cannot drift from the
ecosystem: there is no second implementation to drift with.  Plan 09
converted everything and **C6 deleted the axial layer entirely** —
`src/world.loft` is gone, and `grep` finding `hex_offset`,
`cube_round_axial`, `hex_to_world`, `world_to_hex` or `visible_hexes`
anywhere means someone resurrected it.

`q` is a COLUMN and `r` is a ROW.  Odd rows sit half a hex EAST of
even ones, so a neighbour's delta depends on `r & 1` — which is why
⚠ **nothing may step a coordinate except `lat_neighbour`**, and why
there is deliberately no constant `(dq, dr)` table to reach for.

HEX_DIAMETER = 1.5 m vertex-to-vertex; one `hex_grid` unit is one
dryopea circumradius (0.75 m), so centre-to-centre is
HEX_FLAT_TO_FLAT = 1.299038.

World +y grows **south** (same direction as canvas +y); there is no
y-flip in the render path.  ⚠ `hex_grid`'s frame has +y NORTH, so
`lat_to_metres` / `lat_from_metres` / `lat_corner_*` NEGATE y — that
is what makes its compass true on dryopea's screen (direction 0 = E,
1 = SE, 2 = SW, 3 = W, 4 = NW, 5 = NE).  The metre round-trip cannot
see a consistent flip; `tests/09_c3_geometry.loft`'s sign check is
what gates it.

⚠ **The axial arithmetic survives in exactly one place** —
`tests/09_c2_relabel.loft`'s oracle — because `relabel_hex`'s DOMAIN
is axial (every coordinate dryopea ever wrote to disk) and a
bijection cannot be proved from one side.  Take an axial reference
from there if you ever need one; do not recreate a module.

⚠ **dryopea follows `hex_grid`'s COMPASS** (project owner,
2026-08-13).  The library documents "r increases upward" and names
direction 5 `NE` while placing row `r+1` at larger y; dryopea's +y is
south.  So `lat_to_metres` / `lat_from_metres` / `lat_corner_metres`
**negate y**, and direction 5 really is north-east on screen.  The
negation lives in the lattice→metres conversion, beside the metre
scale — the two things `hex_grid` cannot know.
⚠ The cost is accepted, not overlooked: **maps authored before C3/C5
render vertically MIRRORED**, and C5 added no compensating flip — a
map that came back looking the same would have meant the compass never
moved.  ⚠ Corner WINDING reversed with it (counter-clockwise in
`hex_grid`'s frame, clockwise in dryopea's); consecutive corners are
still adjacent and one side apart, which is all a convex fill needs.

### Naming

- Functions, variables: `lower_case`
- Types, structs, enums: `CamelCase`
- Constants (file-scope): `UPPER_CASE`
- Loop variables prefixed per function (`tslr_w`, `tpi_pal`)
  to dodge the flat-namespace gotcha
- `dryopea_*` save path is local-cwd-relative + gitignored

### Test discipline (moros-style)

- Factories for state construction (`camera_default()`,
  `painted_empty()`, `picker_default(path)`).
- Pure tick functions: `camera_update(c: &EditorCamera, input: InputState)`.
- `InputState` is a struct of named boolean fields, not a flag
  bitmask.  Tests construct it directly + assert on field changes.
- Golden-image tests via `assert_golden(cv, name)` — render to
  Canvas, write to `tests/actual/<n>.png`, compare bytes to
  `tests/golden/<n>.png`.  Bootstrapping a new golden: run, FAIL,
  review `tests/actual/<n>.png`, copy to `tests/golden/<n>.png`.
  ⚠ **A golden depends on BOTH the geometry and the coordinate
  labels**, so plan 09 could not move them once: C3 changed the
  geometry and C5 the labels, and in between a ring rendered as a
  lopsided blob.  They were rebaselined ONCE, in C5c, on a
  self-consistent system — see
  [`tests/golden/README.md`](tests/golden/README.md).  A golden is
  a review aid, not the drawing's gate: the exact ones live in
  `tests/09_c3_geometry.loft`.
  ⚠ **A golden AGREES WITH A SHEAR.**  Rebaseline it and it certifies
  whatever the code now draws, so it cannot gate a coordinate or
  geometry change at all — that needs an independent ORACLE (plan 09
  used `hex_grid` itself).  A golden that was rebaselined during the
  change under test has verified nothing; it is how moros#10 survived.

### Loft language gotchas we hit

Dryopea-side workarounds for known loft behaviour.  ⚠ **Almost every
one compiles clean and fails SILENTLY**, so a green suite cannot see
them — which is why they are written down rather than linted.  The full
list, with the shape of each defect and its issue link, is
[`docs/LOFT_GOTCHAS.md`](docs/LOFT_GOTCHAS.md); reproducers live in
[`loft_repros/`](loft_repros/README.md) and the outbound queue in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md).

**By name, so you know when to go and read it.**  ⚠ The one-line form
is a tripwire, not the rule — go to `LOFT_GOTCHAS.md` before working
around one.

- A struct **RETURNED from a function is a COPY** — mutating it is a
  silent no-op ([loft#894]).
- A struct **stored in a FIELD** of another struct is a copy too
  (`advice[avoidable-copy]`).  A struct passed as a PARAMETER, or read
  out of a field into one, does alias.
- A struct returned through **TWO nested tail calls** loses what its
  loop wrote ([loft#880]); bind the inner call to a local.
- **Never index a call's result in TAIL position** ([loft#877]) — but
  binding a call whose callee is declared LOWER in the file **panics the
  parser** ([loft#918]), so the fix for the two above triggers a third.
- **Never interpolate a struct with a `hash` field** — SIGSEGV
  ([loft#873]).
- A struct literal that **omits a field takes that field's default
  silently** ([loft#914]) — build from `*_empty()`, never a partial
  literal.  ⚠⚠ This one shapes DESIGN, not just code: it is why a field
  is spelled `top_removed` and `off` rather than `has_top` and `on`.
- ⚠⚠ A **`const` initialised from a SIBLING MODULE's const, imported
  through `use dryopea;`, PANICS the compiler** ([loft#962], both
  backends) — and the panic blames an unrelated function.  ⚠ There is no
  import style that compiles both the aggregator and every entry; the
  tree takes the side the gates are on.
- ⚠⚠ A file-scope **`const vector` holding a NEGATIVE number is EMPTY**
  ([loft#955], both backends) — `len()` 0, every index `null(oob)`, no
  diagnostic.  ⚠ A **local** with the same literal is correct.  ⚠⚠ A
  loop over an empty vector runs zero times, so **every assertion inside
  it holds VACUOUSLY**.
- **Loop variable names** must keep one type per function scope and
  OUTLIVE their loop ([loft#915]) — prefix them per function.
- A **missing `use`** reports as `Expect token ;` on a later `.0`, and
  the whole aggregator goes red naming the importer.
- ⚠⚠ **The JSON-cast family is FIXED and this row used to say otherwise**
  (`@M088`, probed 2026-08-29 on both backends).  It read *"HANGS on ≥8
  declared fields with a `vector<Struct>` — `MapFile` is capped at 6 —
  and ignores declared defaults ([loft#876])"*, and **none of it
  reproduces**: 12 declared fields with two `vector<Struct>` fields read
  correctly, a declared default survives a cast, the native backend no
  longer answers an empty vector, an empty `[]` no longer corrupts the
  text field before it, and `:j` no longer omits empty fields.
  ⚠ `MapFile`'s six-field cap and `waves.json`'s classless wave list are
  both **unblocked** and neither has been widened yet.
- `graphics::KEY_*` need **explicit qualification**.
- ⚠ **A zero-argument function in a TEST FILE is COLLECTED AS A TEST**,
  so it inflates the suite total while asserting nothing.  The practical
  rule is *no zero-argument helper in a test file*.
- ⚠ `ticks()` is loft's clock builtin — **never shadow it**, not even as
  a parameter name.  A probe that did reported a tick 4x cheaper than it
  was.
- ⚠ **A `vector<Struct>` local in a very large function** corrupted the
  interpreter heap at COMPILE time ([loft#935]).  **FIXED and CLOSED
  upstream 2026-08-16** — historical, and the `compose_fault` /
  `compose_parts` / `script_compose` split stays because it reads
  better, not because it is load-bearing.

[loft#873]: https://github.com/loft-lang/loft/issues/873
[loft#876]: https://github.com/loft-lang/loft/issues/876
[loft#877]: https://github.com/loft-lang/loft/issues/877
[loft#880]: https://github.com/loft-lang/loft/issues/880
[loft#894]: https://github.com/loft-lang/loft/issues/894
[loft#914]: https://github.com/loft-lang/loft/issues/914
[loft#915]: https://github.com/loft-lang/loft/issues/915
[loft#918]: https://github.com/loft-lang/loft/issues/918
[loft#935]: https://github.com/loft-lang/loft/issues/935
[loft#955]: https://github.com/loft-lang/loft/issues/955
[loft#962]: https://github.com/loft-lang/loft/issues/962

### Save path

The interactive editor saves to `dryopea_save.json` in the
cwd.  Tests write to `tests/actual/*.json` (also gitignored).
Both paths are blown away between runs by `scripts/test.sh`.

⚠⚠ **A PLANET is the third save path** (BACKLOG B3,
[`src/persist.loft`](src/persist.loft)): `make play PLANET=kepler` opens
`dryopea_planets/<planet>/<player>/world.json`, gitignored, and a wall you
built is there when you come back.  ⚠ `maps/` is the opposite and
deliberately so — a MAP is committed content, a planet is what your play
turned one into.

**Eventual destination:** path-backed mmap'd `Store` (the hash
IS the file — no save loop).  ⚠⚠ **`store_persist_bind` HAS SHIPPED**
(`QUESTIONS_FOR_LOFT.md` moved it to Resolved; this section called it
missing until 2026-08-27) **and `ROADMAP.md`'s "one-line annotation"
prediction is FALSIFIED** (`@M052`): it works across processes, but
dryopea's world is a FIELD of `EditorState`, so a bind writes the
EDITOR's store — the undo history rides along and the on-disk layout
becomes the editor's working struct's, which any new field silently
invalidates.  ⚠ Taking it needs `EditorState` restructured so the world
lives in a store-owning container.  **Don't take the manual binary
`file()` + `#read` detour** — it's strictly worse than the JSON we have
today.

### Plan structure

dryopea follows **moros's plan conventions** — see
[`plans/README.md`](plans/README.md) for the binding, and
[`plans/_TEMPLATE.md`](plans/_TEMPLATE.md) to start one.  The
essentials:

- **Layout is FLAT**: `plans/<NN>-<slug>/`.  There is no
  `future/` · `finished/` · `deferred/` — lifecycle is a field
  in the plan's own `## Status` section, so a plan that ships
  does not move on disk and invalidate every link to it.
- **Never renumber existing plans.**  New plans take the next
  unused integer.  Numbering carries no priority —
  `plans/ROADMAP.md` carries the ordering.
- **Most work is not a plan.**  A plan earns its directory only
  when the work is genuinely multi-phase; cap active plans at
  2–3.  See § Pick the lightest workflow that fits.
- Every phase names a **gate** — how you *see* it works.
  "It compiles" is not a gate.
- Value tags `S/R/G/F/U/C/Q/N` and effort letters
  `XS/S/M/MH/H/VH`, the same letters as moros and loft.

## Plans, ROADMAP, docs

```
plans/          one directory per multi-phase plan, flat: `<NN>-<slug>/`.
                ⚠⚠ **BACKLOG.md is the concrete, UNORDERED list of things to
                build** — grouped by what each unblocks, never by priority
                (owner, 2026-08-26).  ROADMAP.md keeps the dependency order;
                the two are complements, and the absence of an order in
                BACKLOG.md is deliberate.
                README.md carries the conventions + the index (value,
                effort, lifecycle, one line each); _TEMPLATE.md starts a
                new one; ROADMAP.md carries the feature ordering across
                5 tiers; DEFERRED.md parks them.
                ⚠ Each plan's own `## Status` is the SOURCE OF TRUTH.
                Never keep a second copy of per-phase state here or in
                the index — it drifts, and the copy is what gets read.

docs/           ⚠ **listed once, in § Documentation index below** — a
                second copy of this listing is the one that drifts, and this
                one had grown three EXPLORATION.md rows saying three things.

PROBLEMS.md             — dryopea-internal bugs (@D-prefixed; ⚠ NOTHING is open —
                          @D002 and @D006 were the last two and BACKLOG C7 and C10
                          closed both on 2026-08-28, and @D007 was found and fixed
                          the same day — a dropped BEACON round-tripped as a WRECK,
                          because a writer and its reader are a PAIR and nothing in
                          the corpus had ever produced the value.  @D008 likewise, on
                          2026-08-28 — a mob that turned at an anchor MID-TICK had no
                          field for the next leg and its hexes went to `slip`, which
                          the conformance gate ABSORBED because the rule is read at
                          `now - slip`.  ⚠⚠ @D006 is the one worth
                          reading: it called itself "not a patch" and predicted
                          `tests/11_f6` would go red, and NEITHER held — it moved
                          not one of 833 measurements, while the thing that DID
                          need deciding was invisible to every gate)
QUESTIONS_FOR_LOFT.md   — outbound queue to loft (Open / Submitted / Resolved)
README.md               — public project intro
loft.toml               — package manifest (depends on graphics)
```

## Loft consumer relationship + library dependency

**Reuse is the rule.**  Do not write a dryopea-local version of
a routine a library already provides, and do not work around a
library bug with a private copy — fix it upstream (or file it)
and consume the release.  Libraries are owned by their
first-class projects; dryopea may ADD to them under their
existing contract, which is the right move when dryopea needs
something adjacent to what a library already does.

**Always check the real surface before writing against a
library** — `loft api <name>` prints its full public API, and
`.loft/api/<name>.api` holds the generated stubs.  Never guess a
signature.

- **Today:** `graphics`, `gridmesh` and `input` resolve from the
  loft package registry (`loft.toml` + `loft.lock`); the first two
  migrated out of loft's monorepo to `loft-libs-graphics`.
  `moros_map` is a path-dep into the moros checkout
  (`../moros/lib/moros_map`) — it is not published, and is declared
  but not yet consumed.
  ⚠ **`input` ships a PARKED banner that is STALE.**  Its header
  says it is blocked on loft `@P391` (`input_new`'s state in
  CONST_STORE under a cross-package call, so writes through
  `&InputState` panic).  It is not: dryopea consumes it from plan 09
  I1, and `input_new` / `input_tick_from_state` /
  `input_set_bindings` all work interpreted.  Probe it again before
  believing either the banner or this note.
- **The shared hex substrate now EXISTS as published libraries.**
  What the docs still call `lib_plan 24` shipped as the `hex_*`
  family in the registry: `hex_field` (exact-integer hex cell
  sets + outlines — the base), `hex_grid` (geometry: axial/pixel,
  neighbours, distance, corners), `hex_shape` (line / box / arc),
  `hex_form`, `hex_place`, `hex_draw`, `hex_edge`, `hex_way`,
  `hex_roof`, `hex_fit`, `hex_recover`, `hex_world` (sparse
  32×32-chunk world model with binary save/load), `hex_terrain`,
  `hex_body`.  moros additionally carries `moros_map` /
  `moros_render` / `moros_sim` / `hex_editor` / `hex_mesh` in
  `../moros/lib/`.
- **Convention mismatch — SETTLED 2026-08-12: dryopea adopts the
  libraries' convention.**  The question was which lattice is
  authoritative.  Answer, from the source: the entire `hex_*`
  family and `moros_map` are **pointy-top, odd-r offset** —
  `hex_grid` calls it "THE CONVENTION (shared with moros — the
  single executable source of it)", `hex_field`'s neighbour table
  is "odd-r offset, same SET as `hex_grid::hex_neighbor`", and
  `moros_map` carries a fixed bug from applying axial cube
  distance to odd-r coords (moros#10).  Plan 07's note that
  moros_map is axial was the stale one, and dryopea's
  **axial flat-top** was the odd one out.
  **The decision (project owner, 2026-08-12): dryopea converts**
  — one lattice across the ecosystem, and it is not the libraries
  that move.  **[Plan 09](plans/09-lattice-conversion/README.md)
  executed it and is complete** (2026-08-13), so § Hex convention
  describes what the code does today; the ask for a second
  `gridmesh` layout was withdrawn on the strength of it
  ([loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24)).
- **Plans 06 and 07 should be re-read against this.**  Both were
  written waiting on an extraction that has since happened, so
  their "blocked on lib_plan 24" framing is stale.

## Documentation index

⚠ **One line per document — what it is FOR, never what it says.**  The
argument, the warnings and the numbers belong to the document itself; a
second copy here is the one that drifts.

| File | Topic |
|---|---|
| [README.md](README.md) | Public-facing project intro |
| [docs/READING_BY_GOAL.md](docs/READING_BY_GOAL.md) | ⚠ **§ Reading by goal in full** — every row's destination *and its argument*.  This file keeps the router; that one keeps the reasons |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | ⚠ **The full `src/` layout** — what each file owns, the trap in it, and the key data structures.  § Architecture above is its one-line index |
| [docs/HARD_WON_RULES.md](docs/HARD_WON_RULES.md) | ⚠⚠ **Every rule that cost a real defect to learn, with the measurement that produced it.**  § Hard-won rules carries the HEADLINES; this is the evidence |
| [docs/PLAYING.md](docs/PLAYING.md) | ⚠ **How to play what exists** — the ONE controls list, GATED against `bindings.loft::editor_actions` |
| [docs/STATUS.md](docs/STATUS.md) | What exists today, one line per shipped phase.  ⚠ Orientation only — each plan's own `## Status` is the source of truth |
| [docs/TOOLCHAIN.md](docs/TOOLCHAIN.md) | ⚠⚠ **How the gates go red for reasons that are not defects** — the 300 s hard-kill, clobbering suites, a shared wall clock, the `graphics` cdylib fault.  ⚠ And § Run the gates DETACHED: `scripts/gate.sh`, because polling a long run's log is wrong three ways |
| [docs/EXAMPLES.md](docs/EXAMPLES.md) | ⚠ **The worked-example convention** — an index tag `@XXX-###` in a comment above a test, cited from the function.  ⚠ NEW work only |
| [docs/PROFILING.md](docs/PROFILING.md) | How to profile the suite, the numbers of record and their date |
| [docs/LOFT_GOTCHAS.md](docs/LOFT_GOTCHAS.md) | Every loft behaviour dryopea works around — ⚠ almost all compile clean and fail silently |
| [docs/FORMAL.md](docs/FORMAL.md) | ⚠⚠ **What a formal RULE is, and the system that generates from one** (`@X327`, `@X328`) — `@FR-<Name>`, a **third family and a third tense**: `@X`/`@M`/`@D` are all HISTORY, a rule is timeless.  ⚠ Three tiers — PROSE, CHECKED, and **GENERATIVE**, where *the rule IS the code's input*.  ⚠ Read against `../loft`, and it carries eight of its failures |
| [docs/DECISIONS.md](docs/DECISIONS.md) | ⚠ **The greppable INDEX** — `@X###` decisions and `@M###` measurements, one line each.  ⚠ Write a code as `<plan>-<phase>`.  ⚠⚠ **GATED since 2026-08-28** by `scripts/tags.sh`: every citation must RESOLVE, and a row may define a RANGE |
| [docs/DESIGN.md](docs/DESIGN.md) | Master design — towers / walls / waves / scramble / camera / HUD / economy / run shape |
| [docs/SETTING.md](docs/SETTING.md) | Fiction — the autonomous AIs, the dormant faction wars, the quarantine, the recruitment, the pollen |
| [docs/DESIGN_HISTORY.md](docs/DESIGN_HISTORY.md) | 2023 prototype seeds, and where each one was routed.  ⚠⚠ § 2 gained the rule it never stated (`@X313`), and the PNG sampler it wrote off is core after all (`@X312`) |
| [docs/PROGRESSION.md](docs/PROGRESSION.md) | ⚠⚠ **REWRITTEN 2026-08-26** — the crew's skills, the station, and what an upgrade may buy.  The old *no stats* answer is superseded |
| [docs/EXPLORATION.md](docs/EXPLORATION.md) | ⚠⚠ **Exploration IS scouting** — it ASSEMBLES rather than adding a pillar.  ⚠ § X0 points at `PROGRESSION.md`, which was rewritten under it |
| [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) | ⚠ DESIGN, not built — the six robot installations whose traffic is what waves are made of, and the station capstone.  ⚠⚠ Its *no economy simulation* bullet is SUPERSEDED by `@X298`: the simulation exists, on the SERVER |
| [docs/WORLDGEN.md](docs/WORLDGEN.md) | ⚠ DESIGN, not built — **world → scenario**, at 1.5 km a hex.  ⚠⚠ Two reasons: the economy (`@X298`) and a **BACKDROP of real geography** (`@X312`, which needs a skyline and so wants real data).  ⚠ `../crawler`'s Ortler map is a CALIBRATION FIXTURE, not its world (`@X309`) |
| [docs/ERRANDS.md](docs/ERRANDS.md) | ⚠ DESIGN, not built — **what a mob is DOING**, between the economy's graph and the mover.  ⚠⚠ Read § WHY first: *do not simulate a world for the scenario; get believable behaviour* (`@X303`).  A mob has a RULE rather than a state, and the rule must be BOUNDABLE and CLOSED-FORM (`@X298`-`@X302`); read against `../crawler`'s mob AI |
| [docs/MATERIALS.md](docs/MATERIALS.md) | ⚠ DESIGN, not built — what things are MADE of.  ⚠⚠ Read § The governing rule first: a material earns its place because getting it is a TRIP |
| [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) | Enemy movement — the two steering modes, passability as a height step, bodies as terrain, retaliation |
| [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) | Palette spec — 11 painted types plus `rubble`, which the runtime deposits and nobody paints |
| [docs/NUMBERS.md](docs/NUMBERS.md) | Guide to `examples/numbers.json`.  ⚠ Nothing LOADS it yet |
| [docs/PARTS.md](docs/PARTS.md) | ⚠ **Entity art — every entity is a PART-TREE and its GEOMETRY is derived.**  ⚠ § D4 replaced a SPRITE design |
| [docs/RENDERER.md](docs/RENDERER.md) | ⚠ **The camera and the pipeline** — FOLLOW behind the facing, and `camera_overview` at 89° IS the editor's view |
| [docs/PROXY_ART.md](docs/PROXY_ART.md) | Placeholder shapes.  ⚠ Its SIZES stay and become a gate; its SHAPES retire as plan 20's catalogue covers them |
| [assets/README.md](assets/README.md) | ⚠ **The binary content the running game loads** — one file, and ⚠⚠ its licence travels with it |
| [maps/README.md](maps/README.md) | ⚠ **The three authored bases**, what each teaches, and how to add one |
| [loft_repros/README.md](loft_repros/README.md) | Minimal reproducers for loft bugs — filed, and ready to file |
| [plans/README.md](plans/README.md) | Plan conventions (moros-style) + the index |
| [plans/_TEMPLATE.md](plans/_TEMPLATE.md) | Template for a new plan |
| [plans/ROADMAP.md](plans/ROADMAP.md) | Comprehensive feature roadmap (5 tiers) + § The critical path |
| [plans/BACKLOG.md](plans/BACKLOG.md) | ⚠⚠ **Concrete things to build, deliberately UNORDERED** — grouped by what each unblocks |
| [PROBLEMS.md](PROBLEMS.md) | Dryopea-internal bugs (`@D<NNN>`) |
| [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) | Outbound queue to loft (Open / Submitted / Resolved) |

## Reading by goal

⚠⚠ **The rows below are the ROUTER — goal, destination, and a flag where
the answer is likely to be *no*.  The ARGUMENT behind each one — the
warning, the measurement, the decision code — is
[`docs/READING_BY_GOAL.md`](docs/READING_BY_GOAL.md), which carries the
same rows in full.**  Read that file's row before doing the thing a row
names; most of them exist because somebody did it without reading.

### The game, and whether an idea belongs in it

| Goal | Start here |
|---|---|
| Understand the game | [README.md](README.md) → [`docs/DESIGN.md`](docs/DESIGN.md) § What kind of game this is |
| Understand the fiction | [`docs/SETTING.md`](docs/SETTING.md) |
| Understand the END GAME | [`docs/DESIGN.md`](docs/DESIGN.md) § The end game |
| Understand the DIFFICULTY CURVE's shape | [`docs/DESIGN.md`](docs/DESIGN.md) § It shoots TOWERS |
| Judge whether a new MECHANIC belongs | [`docs/DESIGN.md`](docs/DESIGN.md) § What kind of game this is — ⚠ **two tests**, the second in [`docs/SETTING.md`](docs/SETTING.md) § Nobody is attacking anybody |
| Judge whether DEEP-LORE content belongs | [`docs/DESIGN.md`](docs/DESIGN.md) § And the DEEP layers are what keep it a tower defence |
| Add a SETTING, an option, a config or a mode | [`docs/DESIGN.md`](docs/DESIGN.md) § 11 — ⚠⚠ **a fine default, options only when reached for** |
| Ask about a TUTORIAL, onboarding, tooltips or a hint system | [`docs/DESIGN.md`](docs/DESIGN.md) § There is NO TUTORIAL — ⚠⚠ **there is none, and that is the design** |
| Add a KEY BINDING, or a mechanic that needs explaining | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X139`) — ⚠⚠ **the key table is a BUDGET**; a new row needs an argument |
| Add something to the HUD | [`docs/DESIGN.md`](docs/DESIGN.md) § HUD — ⚠⚠ **it almost certainly says NO**; one corner number, everything else diegetic |
| Put a signal in the WORLD instead of on the HUD | ⚠⚠ **Measure whether it is in the DEFAULT frame first** (`@M064`) — the follow camera stops 0.96° below the horizon, so nothing overhead is ever seen, and a signal you must orbit to consult is a HUD element with a tax |
| Design a base site that is not flat ground | [`docs/DESIGN.md`](docs/DESIGN.md) § Trees as terrain |
| Find a mechanic that is designed but NOT built | [`docs/DESIGN.md`](docs/DESIGN.md) + [`plans/ROADMAP.md`](plans/ROADMAP.md) |
| Cite a design decision, or find where one was made | [`docs/DECISIONS.md`](docs/DECISIONS.md) — ⚠ never cite a bare plan phase; write `22-S0`.  ⚠⚠ **A citation must RESOLVE** — `scripts/tags.sh` runs inside `test.sh` and fails on a dangling `@X` |
| Find TWO implementations of one rule, or ask whether to merge them | ⚠⚠ `scripts/rules.sh --review` (`@X329`) — **two sites may enforce one rule only if the difference between them can be NAMED, and the name must be a fact about the DOMAIN.**  ⚠ Differ COMPLETELY → the rule is too broad, **split** it.  Differ NAMEABLY → healthy, write the difference down.  Barely differ → **one implementation with a FLAG** carrying the difference as data, which is `can_climb`'s CLIMB and the mover's class row.  ⚠ *Equality is evidence; sameness-of-rule is the claim* |
| Find every site that enforces a RULE | `scripts/rules.sh sites @FR-M-Sidestep` — ⚠ the query [`docs/FORMAL.md`](docs/FORMAL.md) exists to make possible, and what turns *count the re-assertion sites* into a lookup |
| Write down a RULE, or ask what one IS | [`docs/FORMAL.md`](docs/FORMAL.md) (`@X327`) — ⚠⚠ **a rule is not a decision and not a measurement**: `@X`/`@M`/`@D` are all past tense, and a rule claims something about **every case including the ones nobody has run**.  ⚠ *The rules do not change to match the code; the code changes to match the rules.*  ⚠ `@FR-<Name>`, defined in a FENCED block, cited as *"Enforces @FR-X"*, **boundary-exact** because sub-rule names are prefixes |
| Adopt the rule system, or raise the citation COUNT | ⚠⚠ **DO NOT** ([`docs/FORMAL.md`](docs/FORMAL.md) § Resolution GATES, `@X328`) — *"a tree at 76 cited → 255 cited with the same duplication underneath would read as progress while nothing had changed."*  ⚠ **The order is: evaluate the sites → de-duplicate onto one home → fix what the disagreement was causing → THEN cite.  The citation is the RECEIPT, not the task** |
| Trust a count from a new instrument | ⚠⚠ **Not until it has been wrong once** (`@X328`) — loft's rule count moved **five times** as *"the instrument learned what it was counting"*, and dryopea reproduced it **twice in one day**: `tags.sh` could not read a RANGE row, and the `numbers.json` probe mis-parsed four relations.  **Both instruments were wrong before the tree was** |
| Count the sites that re-assert a RULE | ⚠⚠ **grep its tag** — that is what `docs/DECISIONS.md` is for, and `scripts/tags.sh` is what makes the number trustworthy rather than a lower bound.  ⚠ [`plans/30`](plans/30-the-mob-routine/README.md) § Count the RE-ASSERTION SITES is the worked example: **twelve sites, every omission silent** |
| Find where an OLD idea of the owner's went | [`docs/DESIGN_HISTORY.md`](docs/DESIGN_HISTORY.md) §§ 4-5 |

### Progression, the crew, and what they say

| Goal | Start here |
|---|---|
| Judge a PROGRESSION idea (upgrades, unlocks, XP, stats) | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) — ⚠⚠ **REWRITTEN 2026-08-26; the old *no stats* answer is inverted** |
| Ask why a stat or an upgrade is ALLOWED now | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P6 |
| Add a crew SKILL, or ask what one is allowed to change | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2 — ⚠ a skill SCALES a number that already exists |
| Ask HOW a crew member improves, or add a skill to the lattice | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2a |
| Set an advancement RATE, or ask how fast a crew member improves | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2a2 |
| Ask how a player GETS a good crew member | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2b — ⚠ templates, never rerolls |
| Author the STARTING crew, or ask why the player cannot choose them | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2b |
| Ask WHO the starting pair are, or write a line for one of them | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2b § WHO THEY ARE (`@X291`) — **VESK**, a Spacer engineer on plan/tinker, and **TALLOW**, a Hive-dweller ranger on observe/stamina.  ⚠ Their clusters are disjoint, which is what `@X258`'s *well balanced* means; the VOICES are the authored part |
| Add a RELATIONSHIP, loyalty or affinity mechanic | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2e — ⚠⚠ **not as a BAR**; a ledger of what happened |
| Ask what happens to a helper LEFT BEHIND, or whether crew move between players | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2e |
| Ask what the CREW say, or add a line of dialogue | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2c — ⚠ a remark POINTS, it never CONCLUDES |
| Ask WHO speaks, or when a crew remark fires | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X142`) — the helper with no task assigned |
| Ask whether a crew member may be WRONG | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X147`) — ⚠⚠ **no; nothing they say is false** |
| Ask what a crew member is allowed to KNOW | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X150`) — no more than the player |
| Write a crew member's LINE, or give a class a voice | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X157`) |
| Deliver LORE, or ask how the player ever learns the setting | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X152`) — the crew are the only channel |
| Add a DIALOGUE UI, a topic menu or a conversation system | [`docs/DECISIONS.md`](docs/DECISIONS.md) (`@X156`) — ⚠⚠ **there is no question list in this design** |
| Add anything to the POST-MISSION screen, or ask what a debrief may say | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2f — ⚠ never a SCORING screen |
| Ask how a player DISCOVERS an advanced option | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2f |
| Ask who does a job, or whether helpers pick their own work | `src/task.loft` + [`plans/29`](plans/29-the-crews-own-work/README.md) — ⚠⚠ **they do now**: the nearest of four jobs inside `detect_radius`, and the ONE site where a crew member decides anything is `spawn.loft::wave_assign` |
| Send a crew member somewhere, or ask why the search ignores them | `src/helper.loft::helper_drive` — ⚠⚠ **it is an ORDER and `wave_assign` does not overrule one** (`@X296`); `helper_seek` is what a crew member chooses for themselves |
| Widen the DEFAULT's radius, or make the crew cleverer | ⚠⚠ **Read `@X295` first — it was measured and refused.**  At six hexes the default absorbed the work `DESIGN.md` § 9 says growth is supposed to CREATE, and 18 tests across 8 files moved one way |
| Direct a crew member, or ask what a REMIT costs | `src/task.loft` + `@X297` — ⚠ one kind, base-wide, on key **G**, and the cycle is what makes widening cost what narrowing cost |
| Ask who does a job when nobody is looking | [`docs/DESIGN.md`](docs/DESIGN.md) § 9 |
| Judge a task-assignment, priority-list or automation idea | [`docs/DESIGN.md`](docs/DESIGN.md) § 9 — ⚠⚠ **assignment is a PILLAR**; a priority list deletes it |
| Ask whether the game may GATE content, or balance the first mission | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P0c — ⚠ no; prefer FOUND over AWARDED |
| Design a RANDOM element, or ask why exploration does not go stale | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P1d — vary the INSTANCE, never the RULES |

### Exploration, the economy, and the station

| Goal | Start here |
|---|---|
| Design EXPLORATION, or judge a scouting idea | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) — ⚠ not a new pillar; the first scenario is a `.keys` file |
| Author a FIND, or ask what one pays | `object <q> <r> find <points> <owner>` + `@X349` — ⚠⚠ **`subj` is the POINTS and delivering at the core credits the wallet by exactly them**, so `plans/33` E2 sweeps a number.  ⚠ The tripwire is in `carry.loft`'s constant: a find that yields something that is NOT points holds two facts in one integer and must name the packing (`errand_load_pack`'s precedent) |
| Ask why a find has to be found EARLY | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2c — ⚠⚠ **MEASURED and INVERTED** (`@M092`): a stranded crew member is worth **+74 taken in passing and +116 fetched at wave three**, because a BODY is *spent by being used*.  ⚠ The rule that survives is **a find decays with lateness only to the extent that what it buys is PERMANENT** |
| Judge whether a SORTIE is worth taking, or price exploration at all | `tests/scripts/a-find-nobody-fetched.keys` + its two partners (`@M092`) — ⚠⚠ **248 / 322 / 364**, and `EXPLORATION.md`'s own falsifier does not fire.  ⚠ **Four of eight swept fetch ticks FAILED**: the player is killed carrying the wreck and `vehicle_respawn` clears the carry, so a late fetch is a RISK rather than a discount |
| Judge whether a base LAYOUT is worth anything, or price the racing line | `tests/scripts/a-base-drawn-in-tight.keys` + `a-base-drawn-out-wide.keys` and their two no-tower nulls (`@M093`) — ⚠⚠ **170 against 207 with the defences held equal, +37 ticks**, so `PROGRESSION.md` § P7's racing line is REAL.  ⚠⚠ **And the same nine hexes with the towers deleted read 102 against 102, to the tick** — ***the layout is not the wall; it is how far from the core the FIGHT happens***, and a base with nothing that shoots has no such distance to sell.  ⚠ All four cells burn all 30 shots each, so the sixty shots are identical and only their GEOMETRY moved.  ⚠ The SERVICE half of § P7's trade is still unmeasured — it needs a crew that shuttles |
| Judge whether a WALL is worth building where a TOWER stands behind it | `@M094` — ⚠⚠ **it is NOT: −80 ticks tight and −117 wide.**  The swept factorial over `plans/33` E0's bases reads **neither 95 / 95, wall only 102 / 102, towers only 250 / 324, wall AND towers 170 / 207** — eight wall hexes with a gate are worth **+7** alone and two towers **+155 / +229** alone.  ⚠⚠ **`plans/12` B7's DEAD RAMP is falsified as the reason**: a `pile` sweep of thirteen approach hexes, the wall's own outside face included, reads **0.0 at every one**.  ⚠ SIGHT is the leading hypothesis (`tower_sees` walks a line and a wall blocks it) and it is **not gated** — the probe is `range` / `target` / `tower_sight_fault`, already in the `.keys` vocabulary.  ⚠ It does not contradict `@M050`'s +44: **the wall's SIGN depends on whether something that shoots stands behind it** |
| Ask why a BUILD accelerant cannot be priced, or propose one | ⚠⚠ **`@X022`'s recon window is FREE AND UNLIMITED** (`@M092`) — the list does not start until the player pokes, so more hands before wave 1 save the player's WALL-CLOCK and not one game-tick.  **The pressure `@X024` needs is the PERMIT's** ([`plans/ROADMAP.md`](plans/ROADMAP.md) item 8) |
| Ask what CLOCKS a run, or why the player must be efficient | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2d — the permit.  ⚠⚠ **How the player TRACKS it is measured and still the owner's to rule** (`@X287`): the battleships cannot be the clock, because the default frame has **no sky** (`@M064`) |
| Ask how the player learns the PERMIT window, or what the military say | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X0b |
| Ask what a new player's FIRST FIFTEEN MINUTES are | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X0b — ⚠ a TEST, not a work queue |
| Ask what a wreck is MADE of, or add a material / weapon / machine | [`docs/MATERIALS.md`](docs/MATERIALS.md) — ⚠ read § The governing rule first |
| Judge a POWER, AUTOMATION or TRANSPORT idea (cables, rails, conveyors, autopilot) | [`docs/MATERIALS.md`](docs/MATERIALS.md) § Power — ⚠⚠ **almost certainly refused, and the refusal is MEASURED** |
| Design where WAVES eventually come from | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) |
| Add a RESOURCE, a node or a route to the economy | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) § The spreadsheet test — ⚠⚠ **never a resource that is purely GOOD** |
| Add a FACTION, or a relation between factions | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) § The capstone |
| Design STATION POLITICS, or judge a social/alliance feature | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) § The capstone |
| Price a piece of KNOWLEDGE, or add a station investment | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) § The capstone — ⚠ pays ONCE, ever, across the player base |
| Ask what WINNING means, or add a rank, score or leaderboard | [`docs/ROBOT_ECONOMY.md`](docs/ROBOT_ECONOMY.md) § The capstone — ⚠⚠ **no rank, no leaderboard, no score** |
| SELL salvage, price something, or design a market | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2i — ⚠ prices are stable and knowable |
| Judge a TRADER, merchant or economy-role idea | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2i |
| Change the ROSTER between missions, or design the station office | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2i — the office is a PLACE, not a screen |
| Judge a KNOWLEDGE, lore-unlock or discovery idea | [`docs/SETTING.md`](docs/SETTING.md) § The knowledge tree |
| Ask why the player must be physically present, or why there is no remote control | [`docs/SETTING.md`](docs/SETTING.md) § The recruitment — ⚠⚠ **the charter of the whole game** |
| Ask why machines DECAY, or why the map is littered with wrecks nobody killed | [`docs/SETTING.md`](docs/SETTING.md) § The pollen |

### Multiplayer and the shared world — ⚠ a DESTINATION, not a queue

| Goal | Start here |
|---|---|
| Ask whether MULTIPLAYER is next, or why § 20 is so long | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **its length is not its priority**; BUILDING the base game is what serves it |
| Ask when NETCODE gets built, or how multiplayer is ordered | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠ gated on demand, NOT on the critical path |
| Design MULTIPLAYER, or ask what may pass between players | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2i — ⚠⚠ **knowledge is not tradeable** |
| Ask whether CREW may be traded, or what identity a crew member has | `tests/d4_the_person.loft` (`@X290`, `@M067`) — ⚠⚠ **no trade**: a person cannot be COPIED, so `@X214`'s *you sold a copy* cannot apply.  ⚠ And a crew member has **no identity outside one run's roster**, so the RESCUE the design calls settled needs the same missing thing |
| Design PvP, a raid, or co-op between players | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **a base is impregnable to a PERSON** |
| Ask whether a player may HACK another player's tower | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠ only in a team MATCH |
| Design a TERRITORIAL or conquest mode, or ask how a leader is kept in check | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 |
| Design an ALLIANCE, betrayal or diplomacy mechanic | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **do not; the campaign already produces it** |
| Design a WORLD EVENT, or gate content on player history | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **a trigger system, and NEVER show the join** |
| Generate TERRAIN, or pick a seed for anything procedural | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **other players are the seed**; the GATES need it deterministic |
| Ask what a player LEAVES BEHIND on a planet, or design persistent world content | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 |
| Ask how the shared world is HOSTED, or what carries between servers | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠ knowledge crosses, assets do not |
| Ask whether the shared world CONVERGES, or add something exhaustible | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠⚠ **nothing may permanently close** |
| Ask what happens when players are at DIFFERENT campaign stages | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 |
| Judge a feature by WHICH PLAYERS it serves | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠ weighted early, thin tail |
| Ask whether a campaign feature should allow for MULTIPLAYER | [`docs/DESIGN.md`](docs/DESIGN.md) § 20 — ⚠ do not gold-plate it, do not foreclose it |

### Picking work, and running a plan

| Goal | Start here |
|---|---|
| Pick something CONCRETE to build | [`plans/BACKLOG.md`](plans/BACKLOG.md) — deliberately unordered |
| Pick next work to do | [`plans/ROADMAP.md`](plans/ROADMAP.md) § The critical path |
| Ask what the BIGGEST missing mechanic is | ⚠⚠ **It is no longer a MECHANIC — it is the SESSION** ([`plans/ROADMAP.md`](plans/ROADMAP.md) § THE SESSION IS THE GAP NOW, evaluated 2026-08-29).  Every verb in `DESIGN.md` § 2's pitch exists and is measured; what does not exist is a 15-25 minute sortie.  The best base plays **four of seven waves and falls at 320 ticks — 3.6 min**, the corpus's longest play is **490 ticks — 5.4 min**, and `numbers.json` targets **15-25 minutes**.  ⚠ Item 8 is blocked on that gap under another name, and `@X245`'s *first fifteen minutes* is a test the game cannot be run against.  ⚠⚠ **The fix is not a longer wave list** — duration should emerge from somewhere to GO, which is `plans/30`'s world (worth `@M085`'s 146 ticks) with nothing yet rewarding a look at it.  ⚠ Recommended order: **6 the landing flow** (a run cannot reach base 2), then **9 exploration finds BEFORE 8 the permit**, with the crew's VOICE alongside |
| Continue plan 01 work | [`plans/01-ground-editor/README.md`](plans/01-ground-editor/README.md) § Implementation status |
| TUNE a number | `examples/numbers.json` — ⚠ nothing LOADS it; edit the `.loft` constant too |
| Document a new public function, or point at a test as its EXAMPLE | [`docs/EXAMPLES.md`](docs/EXAMPLES.md) |
| Look up what a `src/` file owns before editing it | [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) |
| File an outbound loft request | [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) — ⚠ and file the GitHub issue |
| File a dryopea-internal bug | [PROBLEMS.md](PROBLEMS.md) (`@D<NNN>`) |
| Write/edit a `.loft` file | § Important conventions + loft's own `loft-write` skill |
| Run the editor | `make play` (⚠ not `loft src/main.loft` — the native backend is broken) |

### Playing it, and the three gates

| Goal | Start here |
|---|---|
| Ask what a KEY does, or what the game can do today | [`docs/PLAYING.md`](docs/PLAYING.md) — ⚠⚠ **the ONE controls list; do not write a second** |
| Change how much world the frame shows, or add anything that draws, measures or inverts a click | `src/editor_view.loft::view_ppm` — ⚠⚠ **the ONE door, and the base scale is PRIVATE** (BACKLOG C7, `@X285`).  Four paths read it and they must AGREE: the GL loop's frame, `snap`'s frame, `classify_world`, and `screen_to_hex`, which un-projects a pointer back to a hex |
| Play the game in a window | `make play SCRIPT=<name>` — ⚠ `script=`, never `--script` |
| Play one of the AUTHORED maps, or add one | [`maps/README.md`](maps/README.md) — ⚠ the `.keys` is the SOURCE, the `.json` is BUILT |
| Validate the GAME (not a function) | `scripts/validate.sh`, then [`plans/08-game-validation/README.md`](plans/08-game-validation/README.md) |
| Add a script to the gate | drop a `.keys` in `tests/scripts/` — ⚠ every file there must play GREEN |
| Add a validation scenario | a `.keys` + one test in `tests/08_v3_scenarios.loft` — ⚠ pin its check count |
| Add a regression test | `tests/01_*.loft` for patterns; `golden.loft::assert_golden` for images |
| Script a run of the editor | `tests/scripts/*.keys`; `script.loft::script_run_file`; `snap <name>` |
| Gate anything that is DRAWN by GL | `scripts/validate_gl.sh` over `src/gl_gate.loft`, [`docs/RENDERER.md`](docs/RENDERER.md) § R4 |
| Add a GL fixture, or ask why `other == 0` is a legal thing to ask | `tests/gl/*.keys` + a case in `src/gl_gate.loft` — ⚠ a fixture with no case is REFUSED |
| Ask what makes the GL gate's claim TOTAL rather than a share | `src/gl_gate.loft::gl_entity_pixels` |
| Check a change did not cost anything | `tests/11_f8_the_tick_budget.loft` — ⚠ a RATIO, and for an artefact COUNT it instead |
| Find out what the SUITE spends its time on, or optimise anything | [`docs/PROFILING.md`](docs/PROFILING.md) — ⚠ re-profile first and quote the DATE |
| Speed up frame measurement further | ⚠ it is no longer where the time goes — `classify_canvas` is ~5%, the field ~69% |
| Make the SIMULATION cheaper | [`plans/22`](plans/22-the-field-cache/README.md) — the field, not the roster |
| Judge a simulation-LOD idea (coarser away from the player) | [`plans/22`](plans/22-the-field-cache/README.md) § What this plan does NOT build — ⚠ granularity must NOT follow the CAMERA |

### Capturing, authoring and replaying a situation

| Goal | Start here |
|---|---|
| Turn a state you REACHED into a test | [`plans/18`](plans/18-scenario-capture/README.md) — built; wiring a key to it is § S5 |
| Write a situation down as a `.keys` file | `src/emit.loft::emit_keys` — ⚠ the ORDER it writes is load-bearing |
| Cut a captured situation down to the interesting part | `src/emit.loft::crop_keys` + `crop_fault` — ⚠ the refusals are necessary, not sufficient |
| Cut a fixture down to what a behaviour needs | `src/reduce.loft::reduce_keys` |
| Ask whether two runs are in the same STATE | `src/compare.loft::state_diff` — ⚠ its field list is hand-maintained |
| Author any part of a `WaveState` in a `.keys` file | `src/script.loft::script_author` |
| Author ONE enemy in a `.keys` file | `place <q> <r> <class> [heading]`, plus `stand` / `banked` / `dead` / `hit` |
| Author what a WAVE IS MADE OF | `schedule` then `compose` — ⚠⚠ **a wave is worth its FASTEST class**; `compose` after `schedule` |
| Add a `.keys` verb that takes a hex | `src/script.loft` **and** a row in `convert.loft::keys_schemas` — ⚠ a missing schema row is silent.  ⚠⚠ A verb with THREE hexes is what grew the third pair position (`@X345`) |
| Author a PLACE, a route or a mob in a `.keys` file | `src/script.loft` § THE PLACES + `@X345` — ⚠⚠ **`poi` / `route` / `mob` are the PLACE's and `routine` is the BODY's**, and neither is derivable from the other once a tick has run.  ⚠ A route says *how many* and `poi_enlist` seats them; a `mob` line is written only for a record a tick has changed.  ⚠⚠ **The FLAG is a second fact**: an ambient robot is `errand: true` with `ROLE_NONE`, so `routine` is emitted on the flag and never on the role |
| Write a mob's `slip` down, or convert one to seconds | `src/errand.loft::slip_units` / `slip_seconds` (`@D009`) — ⚠⚠ **it is the one duration in the game that may be NEGATIVE** (`poi_materialise` folds a seat offset in as a negative lateness), and `fixstep`'s authoring door clamps a negative to zero on purpose |
| Save something that outlives the process, or ask what a PLANET is | `src/persist.loft` — ⚠ read `@M052` before reaching for `store_persist_bind` |

### The simulation — movement, waves, combat, the clock

| Goal | Start here |
|---|---|
| Change how enemies move | [`docs/ENEMY_MOVEMENT.md`](docs/ENEMY_MOVEMENT.md); [`plans/11`](plans/11-flow-field/README.md) is what it cost |
| Step a hex coordinate | `lattice.loft::lat_neighbour` — ⚠ never a `+ 1` on a `q`, and there is no `(dq, dr)` table |
| Tell GEOMETRY from LABEL SPACE (any coordinate change) | ask what the site depends on; plan 09 is the worked example |
| Ask whether an enemy may MOVE somewhere | `src/passable.loft::can_step` (an EDGE) |
| Ask whether an enemy may BE somewhere | `src/passable.loft::can_occupy` (a POSITION) |
| Give a mover a climb that changes while it lives | `src/passable.loft::can_climb` |
| Ask what a hex's SURFACE is (vs what is painted on it) | `src/passable.loft::hex_ground`; `hex_surface_index` for the palette index |
| Raise a hex at runtime (bodies, broken walls) | `src/height.loft` — the rubble LAYER, never a repaint |
| Dig a MOAT, or ask what water's `drop` is for | `src/moat.loft` — ⚠⚠ **the depth decides ONE thing: how much it takes to FILL** |
| Ask what a besieger does at a trench, or why a moat is not permanent | `src/moat.loft` § What a besieger shovels — ⚠⚠ **a moat is a TIMER** (130 / 174 / 221), and a trench in front of a TOWER is what it is for |
| Add an obstacle a wave should be able to REMOVE | `src/flow.loft::sweep_ground` — ⚠⚠ **the desire field, never `wave_damage` alone**; a hex it does not admit is never named as a target |
| Ask why the crew cannot dig spoil back out | `src/vehicle.loft` § SPOIL IS NOT SALVAGE — ⚠⚠ **a clearer takes the WHOLE pile**, so it measured as an OFF SWITCH (`@M059`) |
| Ask whether the player or the crew can cross water | ⚠⚠ **Yes — they HOVER** (`@D006` closed by BACKLOG C10).  Flat sea is free; a trench they fall into wants a BOOST to leave and a `waterfall` never lets go; a cliff still refuses everybody |
| Ask whether a hex is free of enemies | `src/occupancy.loft` |
| Ask who on the PLAYER's side is standing on a hex | `src/occupancy.loft::blocker_at` |
| Ask what STARTS the wave list | `src/spawn.loft::wave_provoke_step` — ⚠ two thresholds, 10 and 12 |
| Ask why a fresh wave is not moving | `src/spawn.loft::enemy_standing` — the 8-tick pre-walk window |
| Ask how far an enemy moves in a tick, or make a class FASTER | `src/spawn.loft::enemy_speed` then `enemy_bank` — ⚠ a tick is no longer a hex |
| Ask what a blocked enemy attacks | `src/spawn.loft::enemy_target` over `flow.loft::flow_desire` |
| Hurt or kill an enemy | `src/spawn.loft::enemy_hurt`; `wave_deaths` is the ONE death path |
| Judge what a wave's COMPOSITION is worth | [`plans/24`](plans/24-the-siege-front/README.md) § W2 — ⚠⚠ **the siege front is the wall's WIDTH** |
| Materialise a POI's mobs, or ask what the TICK costs for a distant one | `src/poi.loft::poi_step` + `@X344` — ⚠⚠ **one subtraction**: a POI's whole bound folds ONCE to `poi_reach` = `max(lat_distance(poi, centre_i) + radius_i)`, so the tick asks `lat_distance(poi, player) <= reach + window` with no world read and no round built.  ⚠ `max(radius)` would be an UNDER-estimate — right on a straight out-and-back, a defect on every bend.  ⚠⚠ **And the saving is only visible as a COUNT** (`@M082`): deleting the cull changes no position and makes no extra body |
| Ask why the materialiser is not inside `wave_tick` | `src/play.loft::play_one_tick` — ⚠ it runs BEFORE the tick, for `errand_depart`'s own reason (a body made now must be one this tick's fields, occupancy and move order already know about), and OUTSIDE it because the sortie-long cache would otherwise be a parameter thirty test call sites had to carry |
| Cache anything for a whole sortie | ⚠⚠ **integers only** (`@X344`) — a `vector<Cycle>` or `vector<Bound>` in a long-lived field is [loft#974]'s shape, and a probe of exactly that shape read correctly four times of four while that gotcha's own warning is that **a green reading is not evidence** |
| Ask what a mob nobody can see COSTS, or propose remembering more about one | `src/poi.loft` § WHAT AN UN-MATERIALISED MOB IS + `@X343` — ⚠⚠ **one integer.**  The round is an index, the phase a derived SEAT, the hex `cycle_at`, and the BAG and the BANK are DERIVED (`errand_bag_for`, `cycle_carry`).  ⚠ Only `slip` accumulates, because **only a BODY can be pushed** — and a mob that HAS had one must keep its lateness or `@FR-E-Slip` is refunded where nobody can watch |
| Give a mob a body, or take one away | `src/poi.loft::poi_materialise` / `poi_release` — ⚠⚠ **four things go in and the fourth is the one nobody would think of**: the hex, the bag, the `slip`, and **the rule's BANK**.  A fresh body carries nothing where the rule is part-way through a hex, so it releases its next hex late by exactly `cycle_carry` — perfect when it appears, drifting from the tick after.  ⚠ At 1.5 hex/s the carry is always ZERO, so only a SCOUT can see it (`@M014`'s class, `@M081`) |
| Cull, evaluate or materialise a population | `src/poi.loft::poi_survey` — ⚠⚠ **the shape of that function IS the architecture**: the cull test is per POI and outside the loop, and it is all a distant POI ever costs.  ⚠ `PoiSurvey.asked` is the cost gate and it is a COUNT and never a clock (`@M029`) — 0 against 6 for a POI 200 hexes away |
| Bound a POI's population, or ask *could this ever be in this window?* | `src/poi.loft` + `@X342` — ⚠⚠ **the CLAIM is `bound_disc_holds(b, leg, h)` and the UNION is only the QUERY**: a union is SLACK, and against it a radius one hex short, a terminal leg with no disc, an off-by-one rim and a sidestep that breaks `@FR-E-Non-Increasing` ALL read green (`@M080`).  ⚠ Two doors, one implementation — the claim asks a body about its own leg, the query wants the slack |
| Add a POI, attach a population, or ask what a POI IS to its mobs | `src/poi.loft` § ANCHORS DERIVE FROM THE POI — ⚠⚠ **a KIND is a ROW and the table is INDEXED** (`@X333`'s rule, places as its second subject): the one column that matters is *which anchor this POI is standing on*.  ⚠ A route whose round never visits its place is REFUSED (`poi_route_fault`), which is R4b's lesson with a new subject |
| Ask how many mobs a bound costs, or propose a bound per mob | ⚠⚠ **The population is a set of ROUTES and never a list of BODIES** — `poi_bound` cannot see a body, so forty haulers on one route have the bound of one.  That IS `@X301`'s *one query instead of N*, and a bound that counted bodies would be a change of index rather than a collapse |
| Ask whether the BOUND is worth anything yet | `@M079` — ⚠⚠ **on an authored map it covers 1466 of 1467 standable hexes**, because a round that crosses its patch has legs as long as the patch.  ⚠ It is not a filter for the POIs a base lands among; what it excludes is the world OFF the patch, which dryopea has not got (`@X298`, `@X299`).  ⚠ `@M072`'s second reading with a new subject |
| Change a POI's state, or ask why one is never removed | `src/poi.loft::poi_state_set` — ⚠ the ONE door, and it writes `since` with the state because the closed form goes PIECEWISE at a change.  ⚠⚠ **A POI is never CULLED** (`@X304`): there is no verb that removes one, and *not materialised* is a different word |
| Add ambient life, or ask why a robot walks past instead of at you | `src/errand.loft` — ⚠⚠ **the bubble takes the errand, ONE WAY** |
| Draw a mob OFF its route, or add a thing that distracts one | `src/errand.loft` § THE LURE + `@X346` — ⚠⚠ **a lure is a fact about the GROUND and nothing here reads where a person is** (`@FR-E-Built-Not-Seen`).  ⚠⚠ **A hex walked AWAY from the anchor costs TWO** — the one it spent and the one it owes — because a detour is paid both ways and the return leg is indistinguishable from progress; charge one and the body is permanently short of its own rule, silently.  ⚠ A lure is a ROW (`errand_lures()`, indexed) and the REACH is its one column; the bound grows by exactly that much |
| Make a mob CARRY something it found, or ask why a theft does not flip the bag | `src/errand.loft::errand_lure_take` / `errand_lure_drop` — ⚠⚠ **the bag is the ROUND's state and `cycle_phase` reads the WALK**, so a pickup that flipped it would put the body on a different leg from its rule.  ⚠ The load is a `CarryObject` (`@X334` come due) keyed on `BLOCKER_MOB + PoiMob index`, **an identity that outlives a body** — a roster slot is not one, because three callers rebuild the roster |
| Ask what happens to the player's salvage when a robot takes it | `@M084` — ⚠⚠ **230.0 against 200.0**, one table column apart.  ⚠ Kill the thief and it is back on the ground as the same stuff; a spill that lost its SOURCE is the mutation that survived twelve others (`height.loft` names a pile by its NEWEST deposit, and a corpse is wreckage too) |
| Add a mob ROLE, or ask where a robot on a routine is going | `src/errand.loft` § THE ROUTINE + `@X333` — ⚠⚠ **a role is a ROW and the table is INDEXED, never a `role ==`**, and a test sweeps `src/` to say so.  ⚠ **The BAG steers and never a clock** (`@M073`): 4, 40 and 400 hexes all close; a period one column away gets 13 hexes out for ever |
| Ask where a mob is at an arbitrary MOMENT, or add a leg to a cycle | `src/errand.loft` § THE CYCLE + `@X335` — ⚠⚠ **one modulo, O(legs), one index, and never a step forward** (`@FR-E-Closed-Form`), so an un-tracked mob is COMPUTED and `plans/22`'s LOD refusal needs no exception.  ⚠⚠ **The bank does NOT restart at a leg boundary**: `walked(t) − walked(t₀)`, never `walked(t − t₀)`, and a **DWELL is a LEG with a length, not a pause**.  ⚠ A clock period must be a whole number of ticks and is refused otherwise (`@M074`) |
| Move a mob on a routine, or ask what `slip` pays for | `src/errand.loft` § THE MOVER + `@X336`, `@X337` — ⚠⚠ **`errand_step` is the ONE DOOR and nothing else may write a cycling mob's position** (`@FR-E-One-Door`).  ⚠⚠ **The position is exact in its PHASE and only conditionally in its HEX**: a mob whose first choice is taken walks another route of the same length and loses no time, so hex equality holds only where nothing can push a body while the DISTANCE holds everywhere.  ⚠⚠ **A DWELL is not a BLOCK** — charging `slip` while a guard waits out its clock freezes it **with every conformance count green**, so *liveness is a second gate*.  ⚠ A lost hex costs `BANK_WHOLE / rate` and a rate without a whole one is REFUSED |
| Give a round an ending it does not pass, or ask what `harvest` is | `src/errand.loft` § THE TERMINAL LEG + `@X341`, `@M078` — ⚠⚠ **the turn is a THIRD VALUE OF THE BAG and never a second clock**: the mover has no cycle, so *have I reached `T`* is unanswerable in the tick, while *my bag just emptied at the drop-off* is exactly it.  ⚠⚠ **The ending is compared in HEXES and never in TIME** (`@M077`) — 12 of 192 swept cases disagree and the failure is a WHOLE ROUND — and `cycle_fault` refuses a shift that is not a whole number of hexes at the mover's rate.  ⚠ `harvest` is a CATALOGUE ROW beside `haul` and not a change to it: one mechanism, two rows of data.  ⚠ **The refusal moved from the ROW to the CYCLE** — a row can promise an ending its geometry cannot keep, and only building the cycle finds out |
| End a mob's round, or ask where a robot goes when it is finished | `src/errand.loft` § HOME IS A PLACE + `@X338`, `@M076` — ⚠⚠ **HOME IS A LEG OF THE ROUND, never a place a finished mob walks to**: the plan's invariant is *three states and ONE exit*, so a mob breaking off its cycle would be a fourth state and a second exit.  A mob leaves the roster the tick its own cycle brings it home, and the mover, the cycle and the conformance gate are untouched.  ⚠ **A `shift` is a SPAN and not a count of rounds** — a count needs the cycle, `now − slip` is already there, and a shift chooses no ANCHOR so it cannot reproduce `@M073`.  ⚠⚠ **R4b MOVED THAT REFUSAL FROM THE ROW TO THE CYCLE** (`@X341`): a round whose home is off it grows a TERMINAL leg, so what `errand_row_fault` still refuses is a CLOCK-steered row with a shift, and `cycle_build` refuses the whole cycle when the walk home does not connect.  ⚠⚠ **The departure is at the TOP of the tick and that is the claim**: at the consequence stage a robot arrives at its nest and is removed in the same tick, so the last frame holding it has it ONE HEX SHORT — ***what the player cannot see the gate cannot see either*** |
| Give a mob a JOB, a route, a home or a reason to leave one | [`docs/ERRANDS.md`](docs/ERRANDS.md) — ⚠⚠ **the bag steers, not the calendar**, and a distraction must be caused by something the player BUILT, never by being seen |
| Simplify, drop or defer a piece of the worldgen / errand design | ⚠⚠ **READ `@X324` FIRST** ([`docs/WORLDGEN.md`](docs/WORLDGEN.md) § WHY IT IS AN OLD DESIGN) — it is aimed at a GAP, so **a piece dropped for convenience is a regression even when everything still works**: `@X320`'s edge restriction buys nothing visible alone and its absence makes the dither impossible; `@X321`'s residual looks like a detail and its absence takes the variant budget from a handful to thousands.  ⚠ **The design is not finished when it RUNS** — `@X303` and `@X323` are tests, not features |
| Judge ANY worldgen or terrain-derivation idea | ⚠⚠ **THE TEST** ([`docs/WORLDGEN.md`](docs/WORLDGEN.md) § THE THESIS, `@X323`): *express detail from a very compact BASE SET* — so **does this add data in proportion to the detail it produces?**  If it does, it is the wrong mechanism.  ⚠ Every rule is a COMPRESSION plus a decompression that is **local, deterministic and commutative**; take one away and it stops being usable.  ⚠⚠ dryopea has done this since plan 01 — `painted.loft`'s sea-default absence, `height.loft`'s layer, `entity_view.loft`'s *nothing is STATE* |
| Judge ANY mob-behaviour idea | [`docs/ERRANDS.md`](docs/ERRANDS.md) § WHY (`@X303`) — ⚠⚠ **does this make behaviour more BELIEVABLE, or does it only simulate MORE?**  Believability is owed where it can be OBSERVED; consistency everywhere |
| Add a POINT OF INTEREST to a scenario | [`docs/ERRANDS.md`](docs/ERRANDS.md) § FEW, AND EACH ONE LOAD-BEARING (`@X305`) — ⚠⚠ **two to four, and it earns its place only if REMOVING it moves the clock**.  A POI that changes the picture and not the play is scenery.  ⚠⚠ **Measured** (`@M085`): a base ON a robot road falls at **123** ticks against **269** beside one — but the same road is worth **0** against a wave big enough to saturate the siege front, so **price the supply against the capacity before believing a flat reading** |
| Ask what happens to a POI the player destroyed | [`docs/ERRANDS.md`](docs/ERRANDS.md) (`@X304`) — ⚠⚠ **nothing is culled**: the workers still come, find out, and try to fix it, and the swarm may send the machine that does |
| Ask why an off-screen mob is not simulated, or propose simulation LOD | [`docs/ERRANDS.md`](docs/ERRANDS.md) § The scenario GROWS (`@X299`) — ⚠⚠ **it is COMPUTED, not approximated**, so [`plans/22`](plans/22-the-field-cache/README.md)'s LOD refusal stands untouched |
| Ask how big the world behind a scenario is | [`docs/ERRANDS.md`](docs/ERRANDS.md) § The two scales (`@X298`) — ⚠ the economy's hex is **1.5 km against dryopea's 1.5 m**, so a whole scenario is ~1 % of ONE cell.  ⚠⚠ That is a CHOICE (`@X310`): `../crawler` compresses terrain 10× and thereby makes a level one whole tile |
| Pick a LANDING SITE, or ask what makes one good | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § LAND IN THE OVERLAP (`@X317`) — ⚠⚠ **a base goes where several areas' influence OVERLAPS, because the overlap is what makes a choice exist.**  One area in reach is a task list; several with time for fewer is `@X197` at sortie scale.  ⚠ It needs no new geometry — it is `@X300`'s bounds intersecting.  ⚠⚠ The anti-pattern is MEASURED: `@M058`'s base that stood at 378 with zero targets is *no overlap* by geography |
| Decide whether new worldgen/mob code is a LIBRARY or dryopea's | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § THIS IS LIBRARY WORK (`@X322`) — ⚠⚠ **three layers: LIBRARY (the mechanism), CATALOGUE (the game's rows), SCRIPT (the game's edge cases).**  ***The library never asks for a thing BY NAME; it asks the catalogue for one that FITS.***  ⚠⚠ **But extract on the SECOND consumer** — `plans/10`'s bar — because **a library with one consumer is a refactor with a version number**.  ⚠ Every NUMBER and every REFUSAL stays the game's |
| Compose block detail with coarse heights, or ask why shapes look pasted on | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) (`@X321`) — ⚠⚠ **a block contributes a RESIDUAL, never an absolute**: `height = coarse_field + block_residual`.  ⚠⚠ **The same block on a different slope is a different landform** — an outcrop on a gentle grade, a cliff band at 40° — which is the real answer to tiling.  ⚠ A residual tapering to zero at the edges satisfies `@X320` by construction.  ⚠ dryopea already does this one scale down: `height.loft` is *a LAYER, never a repaint* |
| Ask how many block VARIANTS a case needs, or why terrain looks tiled | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) (`@X319`, `@X320`) — ⚠⚠ **the budget belongs to the DULL cases**: a gorge seen once is a gorge, a grass shelf seen forty times is a pattern.  ⚠ Block SIZE is the lever and it trades quadratically (10-hex → ~64 co-visible; 41-hex → ~4).  ⚠⚠ **A block is a RULE APPLIED TO ANCHORS, not a stamp**, so instances already differ.  ⚠⚠ **And a variant may differ only in its INTERIOR — the edge band belongs to the EDGE**, which is exactly what makes variants substitutable.  ⚠ The dither must be a POSITION HASH, never a stream, or the goldens die |
| Author a BLOCK, or ask what a coarse cell admits | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § THE WORKED EXAMPLE (`@X318`) — ⚠⚠ **a block DECLARES WHAT IT NEEDS, never where it goes** (the trait seam `monsters.loft` and `ROBOT_ECONOMY.md` both use).  ⚠ A steep mountain side admits a LIMITED SET; flow admits the river members and flow-plus-a-drop the **waterfall** ones.  ⚠⚠ **The palette is already the output vocabulary** — `rock` 20, `steep_rock` 40, `rapids` 3, `waterfall` 8, priced against the 3.0 m boost since plan 01, **and nothing has ever placed a waterfall** |
| Derive fine terrain from a coarse cell | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § THE INHERITED SHAPE (`@X316`) — ⚠⚠ **the coarse map chooses the TABLE, the BLOCK chooses the cell, features overwrite in authority order.**  A mountain is not interpolated into existence, so **the ratio is never crossed by interpolation at all**.  ⚠ Anchors sample the field stack, so a block CANNOT contradict the big map — constructed, not checked |
| Ask where TREES, MINES, ROADS, RIVERS, COAST or CLIFFS come from | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § WHERE SCENARIO-SCALE DETAIL COMES FROM (`@X315`) — ⚠⚠ **three sources and the SIZE decides**: FIELDS (coarse only), FEATURES (world-coordinate records, exact at any scale), PROCESSES (fine only).  **Anything smaller than ~350 m cannot be read off the coarse map.**  ⚠⚠ *The coarse map decides WHERE and never WHAT at the scale you play at* — and **FIELD → density, FEATURE → instance**, so a 13 m tree is an instance and `wood` only says how many |
| Derive a fine hex from a coarse one, or add a terrain rule | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) (`@X313`) — ⚠⚠ **a hex's content is a function of its SIX NEIGHBOURS**: terrain, elevation, water flow.  The original design, never written down.  ⚠ Its output is a SHAPE, not a classification |
| Add a DISTANT VIEW, a vista, or anything on the horizon | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) § THE SECOND REASON (`@X312`) — ⚠⚠ **the default frame contains NO SKY** (`@M064`, 0.96° short), which already killed the battleship clock.  A view must be EARNED and **bring its own frame**; the horizon enters at exactly 30.0° against the camera's 30.96° |
| Judge a DERIVATION whose accuracy you cannot gate | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) (`@X311`) — ⚠⚠ **replace the classifier with a PROCESS whose conservation can be gated**; `../crawler` swept a slope threshold for zero gain and shipped a talus model instead |
| Generate anything, pick a SEED, or design the world map | [`docs/WORLDGEN.md`](docs/WORLDGEN.md) (`@X307`-`@X312`) — ⚠⚠ **dryopea has NO generation today**; the coarse map will be Ortler-shaped, and a planet has a GIVEN layer (author-free, because nobody designed a mountain range) and an ACCUMULATED one (`@X224`: other players are the seed).  ⚠ The gates need determinism and that is not a conflict: a seed is unknowable in advance but not un-recorded |
| Turn the core OFF, or ask what the jammer switch costs | `src/jammer.loft` — ⚠⚠ **it stops the SUPPLY and never the SIEGE** |
| Place a TRAP, or ask why re-arming one costs a trip | `src/trap.loft` — ⚠⚠ **a plate fired once is worth LESS than no plate**; the mechanic is the trip back (`@M057`) |
| Judge whether a TRENCH is worth digging | `@M059` — ⚠⚠ **130 / 174 / 221: it outlasts the wall the same price buys, and it is a TIMER** since BACKLOG C9.  ⚠ Alone it still earns nothing; with a TOWER behind it, **335 ticks and nine of thirteen dead** (`@M060`) |
| Advance the GAME | `src/play.loft` — ⚠ never call `wave_tick` directly, and never spell a count as `n * TICK_SECONDS` |
| Ask whether a session is LIVE, or start one | `src/play.loft::play_mode` — ⚠ it gates the CLOCK, never the seam |
| Advance the game by TIME, or change the tick's length | `fixstep::clock_advance` / `clock_step`; `TICK_STEP_UNITS` in `spawn.loft` |
| Pause, fast-forward, survive an alt-tab, or drive one clock from another | `fixstep` § THE POLICIES — ⚠⚠ **a cap DROPS the excess and must never DEFER it** |
| Add a one-shot DURATION, or ask why a timer needs no epsilon | `fixstep::Timer` — `timer_arm` then `timer_spend` |
| Ask whether a mover survives a SHORTER tick, or change `TICK_SECONDS` | `tests/26_l0_the_timestep_sweep.loft` — ⚠⚠ **the instrument is a CROSS-PRODUCT** |
| Ask whether a TIMER survives a shorter tick | `tests/26_l3_the_timers.loft` — ⚠ the GUARDED sites were the healthy ones |
| Ask whether the run is over | ⚠⚠ **`DESIGN.md` § 14 — the player LAUNCHES; there is no fail screen** (`@X292`).  `wallet_broke` terminates a run today as a **STEPSTONE**, and `plans/28` replaces it |
| START a base, or ask where a landing may GO | `src/landing.loft::landing_descend` + `landing_place` (`plans/32`) — ⚠⚠ **a landing MOVES the authored core rather than creating one**, so a map stays valid as authored and *a map cannot be checked against a pick nobody has made yet*.  ⚠ **The hashed offset is the START and the search is the RESCUE**, bounded from the PICK.  ⚠ Only **14-30 %** of an authored map takes a landing at all, dominated by the CLEARANCE |
| Author a LANDING in a `.keys` file | `land <q> <r> [seed]` (`plans/32` L3) — ⚠⚠ **the seed is written down because dryopea has NO RNG**: random is a HASH of position (`@FR-W-Position-Hash`), so a scenario replays exactly.  ⚠ It needed **no new emit verb and no `state_diff` row** — a landing's whole output is a POSITION, and markers and crew already round-trip |
| Judge whether a landing SITE is a real choice | ⚠⚠ **`@M091` — it is not yet, and that is the open question**: the pick is worth **58 ticks** across the band and the SEED alone is worth **59**, so *the dice are worth as much as the decision* and `@X317`'s *land in the overlap* cannot be felt.  ⚠ The cause is `DESIGN.md` § 15 step 6's **random direction** for the free starter tower, not the touchdown; aiming it at the nearest live spawn is one line and contradicts § 15, so it is the **owner's ruling** |
| End a run, or ask what a sortie was WORTH | `src/scramble.loft` + [`plans/28`](plans/28-the-scramble/README.md) — ⚠⚠ **the LAUNCH is the ending and the only one** (`@X293`).  Drive onto the core's own hex for six seconds; `launch <max>` / `launched <yes\|no>` are the verbs.  ⚠ 200.0 cut short against 225.3 played out (`@M068`) |

### Structures, towers, the crew and what they carry

| Goal | Start here |
|---|---|
| Build something, or ask what a build ORDER is | `src/build.loft` — ⚠ check `order_erase` before adding a thing to build |
| Ask how strong a wall hex is | `src/damage.loft::structure_max_hp` — ⚠ `numbers.json`'s 100 is the BRACED number, and since BACKLOG C6 it is **braced AND founded**: bracing holds a wall up sideways, FOOTING holds it up from below |
| Ask how much a wall has left | `src/damage.loft::structure_hp` — ⚠ 0.0 answers both *broken* and *never a structure* |
| Break a wall | `src/damage.loft::break_structure` — the one site, and it does both halves |
| Judge whether a CREW MEMBER should be told what to do | `@M070` — ⚠⚠ **+34 ticks where the work is out of earshot and 0 where it is not**, against the default's +44 and 0 the other way round.  Neither dominates, which is what keeps `DESIGN.md` § 9 a pillar |
| Judge whether a DEFENCE is worth building | [`plans/12`](plans/12-combat-resolution/README.md) § B7 — 69 / 112 / 128 ticks |
| Add a TOWER TYPE, or ask what an EMP / scrambler weapon does here | `DESIGN.md` § 7 § THE DISH (`@X348`) — ⚠⚠ **it is not a new type**: § Damage TYPE's `EMP` row is the BURNOUT rung and the dish is the UPSET rung of one ladder, and the core is already a scrambler.  ⚠⚠ **A scrambler has two forms and only one can be a tower** — a COIL is a near field, unaimable, spherical about itself, and its reach grows as **U^(1/6)**, so ***you cannot buy reach with power***; that version is already built as `src/trap.loft`'s plate.  ⚠ The dish's effect is a `Timer` (upset), so it adds no mechanism; its four costs are all existing rules, and firing it EARNS `SETTING.md`'s escalation rather than opening hostile |
| Ask what the opening 200 points buys, or judge a LANDING-LOADOUT idea | `tests/d2_the_landing_choice.loft` (`@X288`, `@M065`) — ⚠⚠ **200 over a 100-point beacon is exactly TWO and the third press is refused**, so the landing exclusion is the WALLET's and needed no rule.  ⚠ The scrambler is not the other half: it costs nothing and a base lands with it ON |
| Ask what a wall is MADE of, or why the ground matters | `src/damage.loft` § Footing (`@X284`) — ⚠⚠ **the palette's `slope` read at last**; 153 / 174 / 220 on sand, grass and rock, and the STURDIEST hex in reach wins |
| Ask whether a tower can HIT something | `src/tower.loft::tower_sees` — ⚠ never a "which kinds block" table |
| Ask why a tower is not shooting | `src/tower.loft::tower_sight_fault`; `tower_black` is the other answer |
| Bring a spent tower back | `src/tower.loft::tower_repair_tick` — ⚠ it refills the MAGAZINE, never the CHARGE |
| Take a tower's top off, move it, or evacuate it | `src/spawn.loft::wave_take` / `wave_drop` |
| Judge whether a TRANSPLANT is worth doing | [`plans/17`](plans/17-tower-hot-swap/README.md) § T3 — **+3 ticks at best, −50 if the donor was firing** |
| Run the suite without sitting and watching it | `scripts/gate.sh start` then `wait` in the BACKGROUND — [`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § Run the gates DETACHED.  ⚠⚠ **Polling a log costs a turn a minute and is wrong three ways** |
| Find out why a base cannot be played to its end | [`plans/16`](plans/16-the-wave-system/README.md) § W4 — the 30-shot magazine |
| Bring a lost crew member back | `src/spawn.loft::wave_drop` at the core — and NOTHING else does it |
| Take a crew member out of the run | `src/helper.loft::helper_wreck` |
| Judge whether fetching a lost crew member is worth it | [`plans/17`](plans/17-tower-hot-swap/README.md) § T3 — **+76 points** |
| Judge what another CREW MEMBER is worth | [`plans/14`](plans/14-helpers/README.md) § Status — 123 / 135 / 138 ticks |
| Add a crew SKILL, or ask what a detection radius detects | `src/skill.loft` — ⚠⚠ **check the number EXISTS first**; detection is two radii that interact |
| Ask why a crew member is slow, or add something that TIRES them | `src/endure.loft` — ⚠ tire by the RAW elapsed, and only when work LANDED |
| Pick something up, carry it, put it down | `src/carry.loft` — ⚠ never a "carried" field on the carrier as well |
| Add a new kind of carryable thing | a `CARGO_*` constant + a destination rule — ⚠ and NOTHING in the carrying path.  ⚠⚠ **It is FIVE sites, not three** (`plans/33` § The site count): the constant, the destination row *or a deliberate absence*, what arriving does at its call site, and **`emit.loft`'s name plus `script.loft`'s reader, which are a PAIR** (`@D007`).  ⚠ It is now DRAWN for free — `entity_bake_cargo` walks the ledger, so a new kind needs no rendering at all (`@X349`) |
| Add a value to a closed set a `.keys` file can carry | ⚠⚠ **the WRITER and the READER are a PAIR** (`@D007`) — `emit.loft` writes the name and `script.loft` reads it, and a kind added to one is invisible until a scenario produces it.  Three instances so far: `cargo`, `spoil`, `beacon` |
| Add a FIELD to `WaveState`, or ask what a new one owes | ⚠⚠ **three sites and a `.keys` verb**: `wave_state_empty`, `emit.loft::crop_state` (run-wide switches are carried WHOLE), and a `compare.loft::state_diff` row.  ⚠ Add the compare row **before it can differ** — `plans/30` R3 did, and it went RED the same hour on `tests/18_s2`'s round trip (`'now: 442000000 vs 0'`), which is what earned the `now <seconds>` verb |
| Clear rubble / collect after a tower | `src/vehicle.loft::salvage_at` — the shared chassis |
| Place or restore a marker of any kind | `src/markers.loft::place_marker` — the ONE dispatch |
| Add a marker kind | append a constant, bump `MARKER_KIND_COUNT`, add rows — ⚠⚠ **and the cycle grows in 47 places**: BACKLOG C4's fourth kind moved **33 `.keys` fixtures and 14 inline test scripts** by one `do cycle_kind` press.  ⚠ Each file's own `marker … spawn` assertion is what makes the omission loud  ⚠⚠ **RE-MEASURED 2026-08-30 (`plans/33` E0) AND THE 47 IS STALE: it is ~89 files today** — **70 `.keys` (266 `do cycle_kind` presses) and 23 `.loft` test files** — because the corpus doubled under a number nobody re-read.  ⚠ `@M044`'s rule with a new instrument: **re-measure before quoting a delta**.  ⚠⚠ And it is now a DESIGN lever rather than a cost note: a **CARGO** kind is ~5 sites, so `plans/33` buys a find as one cargo row and defers the marker row |
| Change what a key does | `src/bindings.loft::editor_actions` — the ONE table; never a `gl_key_pressed` |
| Add a PLAY action (a key that drives the game) | a row in `editor_actions` + a field on `EditorInput` + `play.loft::play_actions` |
| Ask why WASD does two different things | `src/bindings.loft::editor_input_from` — `playing` fills one set or the other |

### Drawing it — the camera, the mesh, the roster

| Goal | Start here |
|---|---|
| Change what a frame contains | `editor_view.loft::render_editor_frame` — ⚠ edit it there, not in `main.loft` |
| Draw the game in the WINDOW, or ask what a play frame costs | `src/play_view.loft` — ⚠ a play frame is never cached |
| Ask how the renderer knows the TERRAIN moved | `src/play_view.loft::mesh_watch_dirty` — it DIFFS the height layer |
| Add a GL draw call, or ask why the window goes BLACK after pressing P twice | the GL STATE — `play_view_draw` restores it, `gl_gate.loft` is the gate |
| Ask where the game's CAMERA lives, or why the editor's view is a mode of it | `src/render_camera.loft`, [`docs/RENDERER.md`](docs/RENDERER.md) § R1 |
| Ask where the game's camera is REMEMBERED between frames | `PlayState.cam` — a `CameraRig`; ⚠ two booms are two facts |
| Ask why the camera eases, or add a valve to it | [`docs/RENDERER.md`](docs/RENDERER.md) § R2b — ⚠⚠ **the approach is `1 − e^(−k·dt)`, never `k·dt`** |
| Ask what shortens the camera's boom, or add an occluder | `render_camera.loft::camera_boom_free` over `passable.loft::sight_first_block` |
| Point the camera at the vehicle, or ask which way it is facing | `render_camera.loft::camera_follow_vehicle` — ⚠ never paste moros's `270° − facing` |
| Put a hex into the CAMERA's world, or ask which way is up in 3-D | `render_camera.loft::lat_to_world` — ⚠⚠ **the camera's world is `+y` NORTH** |
| Ask what a frame draws BETWEEN two ticks, or why there is no interpolation | `fixstep::clock_alpha` / `play.loft::play_alpha` — the number ships, no policy does |
| Draw the GROUND, or ask why the terrain mesh does not blend | [`plans/25`](plans/25-the-terrain-mesh/README.md) § What was measured first |
| Add a face to the mesh, or ask why a wall's side is drawn once | `ground_mesh.loft::ground_side_faces`, [`plans/25`](plans/25-the-terrain-mesh/README.md) § M1 |
| Mesh a TILE, or ask which tiles an edit invalidates | `ground_mesh.loft::ground_chunk_mesh` over `src/mesh_chunks.loft` |
| Ask what draws SEA, or why the mesh is wider than the paint | `mesh_chunks.loft::mesh_hex_drawn` — the paint PLUS a one-hex ring |
| Ask what a mesh EDIT costs, or wire the GL path into play mode | [`plans/25`](plans/25-the-terrain-mesh/README.md) § M4 — a one-hex edit re-bakes ~4 000 hexes |
| Ask why the ground tile is 8x8 | `@X096` + `@M041` — the edit decides; the draw calls are free |
| Compare two builds of one mesh | `src/mesh_crc.loft::mesh_crc` — ⚠⚠ **an empty mesh folds to 0** |
| Ask what an entity IS, or add a socket to one | `src/part.loft`, [`docs/PARTS.md`](docs/PARTS.md) § D3 |
| Draw an ENTITY, or change what one looks like | [`docs/PARTS.md`](docs/PARTS.md) — ⚠ never a shape drawn inline in `editor_view.loft` |
| Draw the ROSTER, or ask why an entity is not in the frame | `src/entity_view.loft` — ⚠⚠ **nothing here is STATE** |
| Draw a thing lying on the GROUND, or ask why a beacon you set down is invisible | ⚠⚠ **It is not any more** — `entity_view.loft::entity_bake_cargo` (`plans/33` E1, `@X349`) draws every `CarryObject` that is not `CARGO_GONE`, which fixed the beacon and the tower top at the same time as the find.  ⚠ **Do not filter on `owner == BLOCKER_NONE`** — that is a SECOND rule about where cargo lives, and `carry.loft` made *on the ground* a VALUE of `owner` precisely so there would not be one.  ⚠ ONE drawn class for all five kinds, and the split is earned by a scenario where telling them apart changes what the player drives to |
| Ask which POSE an entity is drawn in, or add a joint the simulation drives | `src/pose.loft` — ⚠⚠ **read the sim, never a second flag** |
| Ask what a tower's top is, in the art | [`docs/PARTS.md`](docs/PARTS.md) § D3 — it is a SOCKET |
| Turn an entity to face somewhere | `part_mesh.loft::part_emit_facing` — a QUARTER TURN, not a scale |
| Ask why an entity draws NOTHING while every count looks right | the WINDING — `@D005` is the worked example |
| Pick a colour for anything that is DRAWN | `entity_view.loft::entity_colour` — ⚠⚠ **never a palette colour**, and distinct ≠ distinguishable |
| Pick a colour for anything DRAWN OVER the world, or add a colour RAMP | `@X098` — ⚠⚠ **a ramp is one colour per value it can reach** |
| Draw TEXT anywhere in dryopea | `src/font.loft` — ⚠⚠ **nothing else may call `graphics::draw_text`** |
| Ask whether a CREW REMARK, the debrief or the lore delivery is still blocked | ⚠ **no — `@X130` was lifted 2026-08-27** by BACKLOG B1 |
| Understand library extraction | § Loft consumer relationship — the `hex_*` family is published; `loft api --registry` |

## Branch policy

### Current phase — pre-game-shippable: commit + push directly to `main`

**Until a runnable game build exists, direct commits to `main`
are the normal flow.**  The repo is small, single-author, and
the cost of branching ceremony outweighs its benefit while the
foundation is being laid.

⚠⚠ **ALWAYS PUSH AFTER COMMITTING — the user's standing instruction, and
it is a SAFETY measure** (*"always push as a safety measure"*, restated
2026-08-26).  The remote is the **backup of record**, and work that
exists only in a local commit is work that can be lost.  ⚠ This line
previously said *"push when the user asks — no automatic pushes"*, which
was stale from the day the instruction was given.

⚠ **The override is about PUSHING only.**  Still never create a branch or
open a PR unprompted — that ceremony is unchanged by this.

**Trigger for switching to the formal flow below:** the moment
there's a runnable game — even a minimum-playable validation —
this section is retired and the **MANDATORY** rules below
become the policy.

### Future phase — once a runnable game exists — MANDATORY

**Direct commits to `main` will not be allowed.**

All changes — features, design updates, plan edits — must land
on a feature branch and reach `main` only through a pull
request.  CI gates each PR.  `main` becomes the release branch.

#### Rules (active once the policy switches)

1. **Never `git commit` directly on `main`.**  If you accidentally
   land on `main`, move the change to a feature branch before
   anything else.
2. **Pushing commits is OK by default — unless there's an open PR
   on the branch that the push would disturb.**  For a long-lived
   working branch with no open PR, push freely after each green
   commit.  When the branch has an open PR, do NOT push without
   an explicit user instruction.
3. **Never create a branch or open a PR unless the user
   explicitly asks.**  "Implement plan 01 phase E1" is *not* a
   PR instruction.  Only run `gh pr create` or `git checkout -b`
   after the user explicitly says "create PR", "open a PR",
   "merge", or "switch to a new branch".
4. Default branch name for general work: a GENERAL slug
   (`work`, `cleanup`, `housekeeping`).  ONLY a substantial plan
   earns a specific branch name.
5. Merging to `main` is via a GitHub pull request — not a local
   `git merge`.

## Git safety — MANDATORY

### Never use `git stash pop` or `git pull` with uncommitted changes

Both can produce unrecoverable working-directory states.  Always
commit before any operation that changes the working tree.  To
compare with main, use `git diff main -- <file>` or `git show
origin/main:<file>` — no branch switch needed.

### Never use `git bisect` or `git checkout HEAD -- <files>`

Both routinely destroy multi-session work-in-progress.  To
investigate a regression, read the relevant code paths directly
or use `git show <commit>` / `git diff <commit>^ <commit>`.

## Documentation validation

We **don't** have a loft-style `@P` tracker + `./scripts/idx`
indexer yet.  Triggers for adding one:

⚠⚠ **PARTLY BUILT 2026-08-28** (`@X325`): `scripts/tags.sh` runs inside
`scripts/test.sh` and fails on any `@X###` / `@M###` citation that does
not resolve to a row in `docs/DECISIONS.md` — **394 defined, 390 cited,
all resolve**.  ⚠ It is the *indexer* half of what this section
anticipates, and it exists because the third trigger below effectively
fired: four tags were cited from ten files (one of them `src/`) and
defined only inside a RANGE row nothing was expanding.

- First dryopea-side P-issue gets numerous enough that prose
  references stop being practical (PROBLEMS.md currently has
  one `@D` row; trigger fires somewhere around ~20).
- Documentation count crosses ~25 (**currently 26** — `docs/*.md`; it read
  "~12" until 2026-08-26 and "21" until 2026-08-27, so this trigger is
  closer than it looked and is still moving).
- A specific drift incident makes the manual scan painful.

Until then: keep cross-references prose-form (§ section names)
+ explicit relative-path markdown links.  Run `scripts/test.sh`
before committing — it's the only doc-adjacent automation we
have today (validates tests via assert_golden + the loft test
runner).
