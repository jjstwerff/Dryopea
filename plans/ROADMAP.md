<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea — roadmap

A logical-order list of remaining features.  **We will diverge
from it.**  The goal is to keep "what could we do next?"
answerable in 30 seconds rather than to lock a strict sequence.

Each row: short feature name, status, source-of-truth slot,
brief one-liner.  Status values:

- **shipped** — code landed, tests green.
- **partial** — some code landed; constrained or behind workarounds.
- **drafted** — design written, no code.
- **sketch** — referenced in docs, design not fully written.

Tiers are ordered by **player-impact-per-line-of-code**: Tier A
delivers a playable thing; subsequent tiers add depth on top.
Within a tier, ordering is a suggestion — you can pick any row.

---

⚠⚠ **For a CONCRETE, UNORDERED list of buildable things, see
[`BACKLOG.md`](BACKLOG.md)** (owner, 2026-08-26).  ⚠ This file keeps the
**dependency order**; that one keeps **what a person could sit down and
build**, grouped by what each item unblocks.  The two are complements,
and BACKLOG's lack of an order is deliberate rather than an omission.

## ⚠⚠ The critical path — the natural order to a full game

*(project owner, 2026-08-15: "create a natural order for game features to get to
the full game")*

⚠ **The tiers below are ordered by player-impact-per-line-of-code and say so.**
This section is the other axis: **what must exist before what can be judged.**
Where they disagree, this one is about dependency and the tiers are about value —
read both.

⚠⚠ **The organising principle is dryopea's own: every step must be MEASURABLE
when it lands.**  Twelve plans of simulation were gated headlessly with nothing
drawn, and that worked because the questions were clocks.  ⚠ The questions now
open are *feel* questions — does a sortie feel worth it, is the racing line real,
does a base read — and none of them can be answered by a number.  That is what
moves drawing up the list.

### The four gaps, in dependency order

| # | gap | why it comes here | state |
|---|---|---|---|
| **1** | **the four robot classes** | ⚠ the cheapest item in the whole design — *one row each in `numbers.json` plus one branch in the damage-to-wall lookup* — and the widest: `spawn.loft` said *"the validation tier still emits only regulars"*, so **every wave was the same wave**.  Until they exist, wave composition is a readout of one symbol (`@X023`) and no sortie can predict anything | ✅ **BOTH AXES SHIPPED** — [`plan 23`](23-the-small-robots/README.md) K0 built the WALL-DAMAGE axis and it was exactly that cheap (`@M011`: 23 / 35 / 50 / 96 / 454 ticks); K1 shipped the MIX (`compose 1 4 miner 8 scout`, size SUMMED from its parts, `@X055`); K2a rebuilt the mover on banked progress with the whole corpus as its gate; and K2b moved the numbers — scout 2.5 hex/s, miner 1.0, so nine hexes take **6 / 9 / 14** ticks (`@M016`).  ⚠⚠ **The estimate above missed the second axis and it cost three phases, not one row**: SPEED broke `TICK_SECONDS = 1 / ENEMY_SPEED_HEX_PER_SECOND`, which is `@X058`.  ⚠⚠ **K3 closed the plan and found the gap's real floor**: three waves of twelve fall at **94 / 126 / never** — composition is legible, and *loudly* — but every mix lands within four ticks of a PURE wave of its FASTEST class, because only **three** hexes of a wall are ever attacked and the quickest four robots own all of them (`@M018`).  So item 1 is done and item **1b** below is what stops a `compose` line meaning what it reads like |
| **1b** | **the equal-distance SIDESTEP** | ⚠ **newly PRICED, not newly known** — `ENEMY_MOVEMENT.md` has carried it since [`plan 11`](11-flow-field/README.md) F7 as *"a second steering rule and nobody has built it"*, and three phases judged it latent.  `@M018` is what makes it concrete: a wave's usable width is three, so a mixed wave collapses to one symbol again and `docs/ROBOT_ECONOMY.md`'s convoys cannot express anything.  ⚠ It also unlocks [`plan 12`](12-combat-resolution/README.md) B3's bracing consequence, which is exact and latent for the same reason.  ⚠⚠ Read [`plan 11`](11-flow-field/README.md) F7b before sizing it: the last steering rule judged latent moved every clock in the game (161/311/180 → 61/104/95) | ✅ **SHIPPED — [`plan 24`](24-the-siege-front/README.md), complete 2026-08-17.**  ⚠⚠ **And the estimate was wrong in the useful direction: it was not a second steering rule at all.**  W0 measured what the equal-distance sidestep actually offers at a wall face and it steps off the face as readily as along it (`@M019`) — the three-hex front was the DESIRE FIELD's shape, a ring around the core having ONE minimum on a straight face, so exactly three hexes lacked a legal closer step **whatever the wall's length**.  W1 fixed it with a PRECEDENCE — *arriving beats queueing* — which is one pre-pass plus a deletion.  ⚠⚠ `@M020`: the front is the WALL's width now (3 → 4 on five rows, 3 → **6** on seven) and `@M018` is retired — *4 scout + 8 miner* went from *never* to **126**, and a wave is worth its front class PLUS what the front cannot COVER.  **The screen is arithmetic — bodies against face width.**  ⚠ 16 assertions and 8 gate scripts re-priced (W2) |
| **2** | **the renderer, then entity art** | nothing of the running game is drawn.  ⚠ Everything after this is judged by eye | ✅ **DONE — THE GAME IS DRAWN IN THE WINDOW** ([`plan 19`](19-the-interactive-loop/README.md) P6, 2026-08-18): `make play`, press P, and the map editor becomes the game.  ⚠⚠ The renderer works out for itself what the terrain did, by DIFFING the height layer (`@X095`), and the ground tile went to 8x8 on the measurement plan 25 M4 deferred to it — **96 tiles draw as fast as 8** (`@X096`, `@M041`).  ⚠ **P7 added the HUD** and it is one number — the wallet — because `DESIGN.md` § HUD refuses everything else (`@X097`).  **[`plan 20`](20-entity-art/README.md) COMPLETE (A1-A5, 2026-08-18)** — every entity is a part-tree, the roster is walked off a `WaveState` with nothing stored, and `scripts/validate_gl.sh` photographs a base with a player, a crew member, four robots of three classes and two towers in it: **3 fixtures, 52 measurements**.  ⚠⚠ An entity colour is deliberately outside the palette (`@X092`), so the gate's claim is total — `unknown - entity pixels == 0`, every pixel a palette colour, an entity colour or the clear colour.  ⚠⚠ **A5 found `@D005`: half of every box in the catalogue wound INWARDS**, which draws nothing under `GL_CULL_FACE` and changed no count, no vertex, no normal and no `mesh_crc` — 42 tests across three phases passed over it.  ⚠ **The remaining piece is the WINDOW**: both plan 25 M3 and plan 20 A5 delivered a gate rather than a window, so `make play` still draws the software editor frame and nothing of the game.  **[`plan 21`](21-the-renderer/README.md) COMPLETE at R2** (2026-08-17) — the camera.  **[`plan 25`](25-the-terrain-mesh/README.md)** took its R3-R5 (the terrain mesh, the GL path, cost) and is **COMPLETE (M0-M4)** — the ground meshes as flat tops and vertical sides, tiled 32x32 and checksummed, with 0 of 654 measurements moved by any of the three.  ⚠⚠ **M2 answered what draws SEA** (`@X075`): the drawn region is the painted set plus a one-hex ring, because sea is stored as ABSENCE and meshing only what is stored leaves an erased region as a hole in the ground at the height of the land round it.  ⚠⚠ **This row said R3 was "the largest single item in the repo's history" and plan 25's opening probes MEASURED that it is not**: dryopea's ground is a flat plane with pillars on it — `height_override` is non-null on **two of twelve** palette kinds — so moros's corner-height mean and its halo are both no-ops, and `mesh3d::mesh_to_floats` + `graphics::GroupVboSet` already publish the whole GPU-side chunk cache.  *Sizing a port by the DONOR's line count is sizing it by a world you do not have.*  ⚠ Colour ships as a **uniform**, one mesh per palette kind (`@X074`), because a flat-unlit frame built that way can only contain palette colours — which is what keeps R0's exact classification alive.  ⚠⚠ **R1 corrected the design's own frame note and it is the transferable half**: dryopea's `+y` SOUTH is a CANVAS convention, left-handed once `+z` is up, and `mat4_look_at` MIRRORS it — no azimuth undoes it (`@M021`: one of eight works in the north frame, none in the south), so `lat_to_world` negates y and the camera's world is `hex_grid`'s own.  ⚠ **`camera_overview` at 89° IS the editor's view, measured**: 0.0014 rad of bearing and 0.56% of scale against the software rasteriser (`@M022`), so § R2's two-rasteriser plan holds and collapsing them later is a migration.  ⚠ moros's follow formula ported verbatim puts the camera **abeam** — tracking, easing, and wrong.  ⚠ 0 of 654 measurements moved, at R1 **and** at R2.  ⚠⚠ **R2's ease could not be ported**: moros's `f = k·dt` is frame-rate DEPENDENT and `play.loft` is built on the opposite property (`19-P0`), so the approach is `1 − e^(−k·dt)` — which composes exactly, one frame of a second and sixty of a sixtieth landing on the same bits (`@M023`).  ⚠⚠ **And the ease is load-bearing here for a reason moros does not have**: the vehicle is a LATTICE position and jumps 1.299 m on the tick it steps, so un-eased the camera moves on 12 frames of 240 and eased on 221.  ⚠ The camera now lives on `PlayState` (`@X014`) and its boom shortens behind the same sight walker the towers ask (`@X071`).  [21](21-the-renderer/README.md) → [20](20-entity-art/README.md) |
| **3** | ⚠⚠ **BUILDING** | **the biggest missing mechanic, and it gates three finished designs** — see below | ✅ **SHIPPED — [`plan 27`](27-building/README.md), C0-C5, 2026-08-27.**  Press **Q** and the hexes you drive over are ordered as walls your crew raise; press **E** at the core and 100 points becomes a tower beacon to carry out and plant.  ⚠⚠ **Measured: +44 ticks on a base that otherwise falls at 130** (`@M050`), from a scenario pair ONE token apart — which is `@X022`'s *pre-wave window is a budget* made measurable for the first time, and the wave CHEWED THROUGH the wall rather than going round.  ⚠⚠ **The wallet buys something at last** (`@M051`): points had been earned since `13-V3` and drained since `12-B6` and bought nothing.  ⚠ **Almost nothing here was a new number** — `numbers.json` already had build times, the beacon's 100 points and the `Q` binding, and the palette had carried an unread `buildable` flag since plan 01.  ⚠⚠ **Four findings the gates caught, and none was in the design**: the renderer could not SEE a structure appear (`@M048`, retiring `@X095` for a wider rule); a float build rate came up a tick short, which is `@D003`/`@D004`'s family again (`@M049`); a five-row band could not be SEALED because the trail never orders the hex you stand on (`@X272`); and a tower order must refuse ERASURE or the wall trail destroys a paid-for beacon (`@X274`).  ⚠ Deliberately NOT built: helper orders, the 8-walls wave trigger, bridges, `@X252`'s directed helpers |
| **4** | **the scramble** | the run's ENDING, and the mechanic the game is named after.  ⚠ Its ingredients all shipped: the carry model ([15](15-the-carry-model/README.md)), detachable tower tops ([17](17-tower-hot-swap/README.md) T2), the wallet, the core | ✅ **SHIPPED — [`plan 28`](28-the-scramble/README.md), S0-S5, 2026-08-28.**  Drive onto the core, stay six seconds, and the rocket goes.  ⚠⚠ **Measured: a sortie cut short carries 200.0 points and one played out carries 225.3** (`@M068`) — **the first measurement of what a SORTIE was worth**, where every other clock in the corpus counts down to a base FALLING.  ⚠⚠ **And the run gained its real ENDING**: `wallet_broke` had been called *the only end state* since plan 12 B6, and the owner ruled it a STEPSTONE (`@X292`) — *the actual game will have the design ending; never write the current implementation down as a rule*.  ⚠⚠ **The base never ends itself** (`@X293`): the wallet at zero is poverty, an empty sky is a PHASE, an expired permit costs the cargo, and a destroyed vehicle arms a countdown that exiting cancels — every candidate refused by name in `tests/28_s5`.  ⚠ **S0 found the design gap**: § 4's six faces and interior do not exist, so *beside the core you TRADE, on the core you LEAVE* (`@X294`) — and `vehicle_respawn` already puts the player there, so § 4's free *ready to leave?* prompt cost no code.  ⚠ Deliberately NOT built: the faces, the bottom pulse, helpers boarding themselves, the landing flow, and the next base |

### ⚠⚠ Why BUILDING is the load-bearing gap

⚠⚠ **And 2026-08-26 sharpened it** (`@X238`, `@X240`): the owner expects
**most players to play a couple of scenarios and never reach the end
game**.

⚠ **The same day also specified the finished game's OPENING** (`@X245`,
[`../docs/EXPLORATION.md`](../docs/EXPLORATION.md) § X0b) — the first
fifteen minutes are **scouting** in a world that is **alive**, with
robots in their normal work flow and insects on trees.  ⚠⚠ **That is a
TEST, not a task** (`@X251`): the owner's own note is *"this is the
experience a player has when the game is finished — we are not there
yet"*.  ⚠ It records one real fact worth knowing — **every robot in the
code today is a WAVE**, and there is no mover for a robot going about its
business — **but it does not move the critical path**, which is still
BUILDING.  ⚠⚠ **So the early game IS the product for almost everybody who
ever plays this** — and *building a base* is the first thing they will
try to do.

Walls and towers are placed in the **editor** today
([`plan 19`](19-the-interactive-loop/README.md) § What this plan does NOT build).
The player cannot make a base.  Three completed designs are written against that
missing verb and are inert without it:

| design | what it assumes | `@code` |
|---|---|---|
| the pre-wave window is a **budget** split between looking and building | that building competes for the window | `@X022` |
| a find is a **BUILD ACCELERANT** whose value collapses once you are busy | that there is building to accelerate | `@X024` |
| the base **layout** is the exam, and there is a racing line | that the player chooses the layout | `@X019` |

⚠ And it is what the **wallet** is for.  Points are earned (`13-V3`) and drained
(`12-B6`) and buy nothing — `DESIGN.md` § 13 prices tower orders and helper
orders at 100 points each, and neither exists.

⚠ **Its pieces are designed and named**: wall paint (§ Wall paint — trail outline
+ erasable), the **beacon ferry** (§ New towers via beacon ferry — carry a beacon
from the core to the site), and helper construction time.  ⚠ Nothing here needs
a new system; `plans/15`'s carry model already moves a beacon exactly as it moves
a tower-top.

### Then the run becomes a RUN

| # | feature | needs |
|---|---|---|
| 5 | **helper ORDERS** — commanding, not purchasing.  ✅ **SHIPPED — [`plan 29`](29-the-crews-own-work/README.md), O0-O4, 2026-08-28.**  The row's own diagnosis was exact and O0 measured it: `helper_drive`'s only caller was the script runner, and a crew member four hexes from a wall order stood there for sixty ticks with not one unit of work in it.  ⚠ O1 built the **semi-automatic default** — the nearest of the four jobs inside `detect_radius` — and the radius is the load-bearing number (`@X295`): asked at six hexes it moved **18 tests across 8 files** and made bases unkillable, because a default that absorbs the work `DESIGN.md` § 9 says growth is supposed to CREATE deletes the table the pillar stands on.  ⚠ O3 is the ORDER itself — one kind, base-wide, on key **G**, which CYCLES and so answers `@X289`'s RESET-or-STEP by construction (`@X297`).  ⚠⚠ **And the pillar comes out intact as ARITHMETIC** (`@M069`, `@M070`): the default is worth **+44** where the work is near and **0** where it is not, one order **+34** where it is far and **0** where it is near — neither dominates, which is `@X198`'s *does this make ONE axis dominate?* answered with numbers | building (so there is something to order them at) |
| 6 | **landing flow + map selection** — [`plan 04`](04-map-library/README.md) carries the map selection half.  ✅ **THE LANDING SHIPPED — [`plan 32`](32-the-landing/README.md), L0-L4, 2026-08-29.**  Where the base goes stops being AUTHORED and becomes a DECISION: `landing_of` is a pure function of the world, the pick and the seed, `land <q> <r> [seed]` is a `.keys` verb, and a landing MOVES the core rather than creating one — so a map stays valid as authored and `map_fault`'s reachability guarantee stays a statement about the MAP, because **a map cannot be checked against a pick nobody has made yet**.  ⚠⚠ **Three of `DESIGN.md` § 15's eight steps needed no code**: the core's six faces do not exist (`@X294`), close-spawn disable has read the core's CURRENT hex since plan 16, and step 8's wallet is `plans/31`'s `wallet_carrying`.  ⚠⚠ **AND IT CLOSED ON A PROBLEM** (`@M091`): the pick is worth **58 ticks** across the band it can be made in and the SEED alone is worth **59** — ***the dice are worth as much as the decision***, so `@X317`'s *land in the overlap* is not yet felt.  ⚠ The cause is § 15 step 6's **random direction** for the free starter tower; pointing it at the nearest live spawn contradicts § 15 as written and is the owner's ruling.  ⚠ **Still NOT built: the map selection / planet view** ([`plan 04`](04-map-library/README.md)) — UI over a list, and everything under it is testable headlessly | the scramble (a run needs an exit before it needs a second entrance) |
| 7 | **carryover** — what the rocket takes becomes the next base's start.  ✅ **N1-N4 SHIPPED — [`plan 31`](31-carryover/README.md), 2026-08-29.**  The wallet a base opens with stops being a constant: `Wallet` gains a `carried`, `wallet_budget` is the one door the budget is asked at, and `manifest_opens` is where the manifest's three columns are sorted into the one that crosses and the two that do not.  ⚠⚠ **Counting the re-assertion sites BEFORE writing any code is what earned the plan** (`@X347`): three are in `wallet.loft` and the fourth is `hud.loft::hud_ink_for`, which read the BASELINE for the wallet ramp's span — so a base opened with 200 carried would have sat at the full colour from 400 points down to 200, **the ramp dead for half the run on the one number the game shows**, with `tests/19_p7`'s exhaustive colour sweep still green because it sweeps *the span it is given*.  ⚠⚠ **And the carry COMPOUNDS** (`@M087`): **230.0 against 106.0** with the same 100 points left when the ferry ends — a hundred spent on two more towers came back as a hundred and twenty-four.  ⚠ The sweep is the finding: +124 points at the authored wave, **standing against falling** from 26 robots to 50, +11 ticks past 115, then nothing once the wave saturates the siege front.  ⚠⚠ **The first version read 198 against 198** because `fall` waits on `wallet_broke` and the wallet is what BUYS towers as well as what enemies drain | the scramble |
| 8 | **the permit clock** (`@X025`) | ⚠ **content long enough to clock.**  ⚠⚠ **That is § THE SESSION IS THE GAP NOW, under another name** — the corpus's longest play is **490 ticks ≈ 5.4 min** against a 15-25 minute target, so the window is derived from content and never chosen.  ⚠ It also needs `@X287`'s ruling: `@M064` falsified the battleship clock by 0.96° of camera pitch |

### ⚠⚠ THE SESSION IS THE GAP NOW — all the verbs, none of the duration

⚠⚠ **Evaluated 2026-08-29, after [`plans/31`](31-carryover/README.md) closed
item 7.**  Every mechanic in `DESIGN.md` § 2's pitch now exists and is
measured — land, build, defend, salvage, scramble, carry over.  **What does
not exist is a SESSION**, and that one gap is what silently blocks most of
what is left.

| | measured |
|---|---|
| `tests/16_w4::test_the_authored_seven_wave_list_is_not_survivable` | the best base the suite can build plays **four of seven waves** and falls at **320 ticks — 3.6 minutes** (`@M005`) |
| the longest play in all **58** gate scenarios | **490 ticks — 5.4 minutes** (`plans/31` N5), and that is a base that WINS and then idles |
| `examples/numbers.json` § `_doc` | *"Values target a single base session of **~15-25 minutes**"* — 1350-2250 ticks |
| the same `_doc`, about itself | *"a coherent placeholder set, **not a balanced one** — verify in play"* |

⚠⚠ **So the game is a factor of four to seven short of its own session
target**, and the consequences are already written down elsewhere as if they
were separate problems:

- **item 8** is blocked on *"content long enough to clock"* — that is this gap
  under another name;
- **`@X245`'s *the first fifteen minutes are scouting*** is a TEST the game
  cannot be run against, because there is no fifteenth minute;
- **`@X238`'s *most players play a couple of scenarios and never reach the end
  game*** makes that missing session the entire product.

### ⚠⚠ And the fix is NOT to lengthen the wave list

⚠ The session is short not because the content runs out but because **there is
nothing to do but hold the wall.**  The design's own answer for what fills
fifteen minutes is SCOUTING (`EXPLORATION.md` § X0b) and TRIPS
(`MATERIALS.md` § The governing rule) — so duration should EMERGE from having
somewhere to go, and a tuned wave list would be balancing a placeholder:
`numbers.json` calls its list *"Authored content (placeholder for the eventual
economy-driven model)"*, which is item **10**.

⚠⚠ **The largest built-and-unconsumed asset in the repo is the one that fills
it.**  [`plans/30`](30-the-mob-routine/README.md) built a world worth scouting
— places, routes, populations, rounds, lures — and `@M085` measured that it
moves the clock by **146 ticks**.  ⚠ The player has no way to see that, learn
it, or act on it.

### ⚠ The recommended order, and where it diverges from the table above

| # | do this | why here |
|---|---|---|
| **1** | ~~**the landing flow** (item 6)~~ ✅ **DONE — [`plan 32`](32-the-landing/README.md), 2026-08-29.**  ⚠⚠ **And it did NOT make the session longer, which was never its claim**: a landed base plays the same base from a different hex, so § THE SESSION IS THE GAP NOW stands untouched and **2 is now 1**.  ⚠ It left one thing for the owner — `@M091`'s *the dice are worth as much as the decision* | ⚠⚠ `plans/31` made a run possible and **nothing in the game can reach it** — there is no way to play base 2.  Smallest change that turns *a base* into *a run* in the player's hands, and most of the mechanism exists (close-spawn disable, the markers, the crew, `wallet_carrying`).  ⚠ It also lands the first real decision of a sortie — `@X317`'s **land in the overlap**, whose geometry is `plans/30`'s POI bounds, already built |
| **2** | **exploration finds** (item 9) | ⚠⚠ **This is the divergence: 9 BEFORE 8.**  A permit clock bounds a session that has a middle; today it would bound holding a wall.  This is what cashes `plans/30` and makes the session long by giving the player somewhere to go.  ⚠⚠ **PRICED 2026-08-30 with no code written** (`@M092`, `EXPLORATION.md` § The order of work phase 1): a stranded crew member on the road is worth **+74 ticks taken in passing and +116 fetched at wave three**, against a base that lives **248** — so a sortie PAYS and the ordering stands.  ⚠⚠ **But the two are in the WRONG ORDER**: the find is worth MORE late, because a body is spent by being used, and `@X024`'s *go now, while going is cheap* has no pressure behind it while `@X022`'s recon window is free and unlimited.  **So item 8 is what turns exploration from *worth doing* into *worth doing NOW*** — 9 before 8 for the CONTENT, 8 for the INCENTIVE.  ⚠⚠ **AND [`plans/33`](33-exploration-finds/README.md) IS OPEN ON IT, WITH E0 COMPLETE** (2026-08-30, `@M093`, again no code): the sortie's PRODUCT is a build decision, and a build decision is now worth **+37 ticks** — 170 tight against 207 wide with the defences held equal.  ⚠⚠ **And the null is the finding**: the same move with the towers deleted is worth **102 against 102, to the tick**, so ***the layout is not the wall — it is how far from the core the fight happens***, and `PROGRESSION.md` § P7's racing line is answered |
| **3** | **the crew's voice**, alongside 2 | ⚠ Not on this path at all, and load-bearing anyway: no tutorial (`@X137`), one HUD number, and the crew as the **only** lore channel (`@X152`).  A longer session with more to find is a longer session of confusion without it.  ⚠ Text shipped (BACKLOG B1); only the remark design is outstanding |
| **4** | **the permit clock** (item 8) | now it has something to bound.  ⚠ Still needs the owner's ruling — `@M064` falsified the battleship clock by **0.96° of camera pitch** and `@X287` recommends the passing SHADOW |
| **5** | **the economy** replaces the authored list (item 10) | the wave list stops being a placeholder, and only then is there something to balance |

⚠⚠ **What this evaluation says NOT to do next**: tiers 2 and 3 and the bosses
(item 11), the coarse world map (BACKLOG F8, deliberately last), bridges (item
12, which re-reads 1 094 tests), and any tuning pass over `numbers.json`.  All
of them pour more content into a three-minute container.

⚠ **Multiplayer is not in this ordering and `DESIGN.md` § 20 already says
why**: its length is not its priority, and BUILDING the base game is what
serves it.


### Then it gets DEEP

9. **exploration finds + intel that persists** (`@X020`-`@X029`) — needs 1 and 3.
10. **the robot economy graph** ([`ROBOT_ECONOMY.md`](../docs/ROBOT_ECONOMY.md)) —
    *"the replacement for plan 16's authored list"*; needs 1.
11. **tiers 2 and 3** (insects, elementals) and the **bosses** — Tier C below.
12. **bridges the robots walk under** (`@X052`) — ⚠ a movement change
    (`Hex` → `(Hex, layer)`), deliberately last because it re-reads 1 094 tests.

### Cross-cutting, pulled in by need rather than by turn

- [`plan 22`](22-the-field-cache/README.md) — **cost.**  ⚠ Not scheduled: it
  fires when the world grows or the roster does.  `flow_sweep` is **~75 %** of
  the suite (`@M001`) and is computed over ground nobody reads.
- [`plan 18`](18-scenario-capture/README.md) `S0`-`S4` — **built and idle.**
  Capturing a live session is [`plan 19`](19-the-interactive-loop/README.md) `P5`,
  which is one small phase and unblocks turning any played moment into a test.

### ⚠ What this order deliberately does NOT do

**It does not put the renderer first.**  Item 1 is cheaper by two orders of
magnitude and makes every later measurement legible; doing it after the renderer
would mean drawing four classes that are one class.

**It does not wait for a "full" renderer.**  [`plan 20`](20-entity-art/README.md)
A1-A4 are metres and turns — the part model, the catalogue, the poses — and are
buildable and gateable while `21-R3` is still open.

⚠ **It does not schedule the layer change.**  `@X052` is real and wanted, and it
is last for a reason: it changes the flow field's node type, and every item above
it is cheaper to build against one surface per hex and re-read once.

---

## Tier A — Validation playable

One map, one mission, one tower type, one enemy type — but
end-to-end and *fun-shaped*.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Ground editor (sparse paint, sea default) | **shipped** | [plan 01 E1-E3](01-ground-editor/README.md) | Hex grid + camera + palette + click/drag paint |
| Save/load MapFile JSON | **partial** | [plan 01 E4](01-ground-editor/README.md) | 6-field schema; expanded once loft JSON-cast bugs land.  The marker sidecar round-trips all three kinds (spawn / target / tower) |
| Integration smoke test (cold-start cycle) | **shipped** | [plan 01](01-ground-editor/README.md) | Part of the 691-test suite under `scripts/test.sh` |
| Interactive GL editor (E1-live) | **shipped** | [plan 01 E1-live](01-ground-editor/README.md) | `src/main.loft`; human playtest pending |
| 3D solver-validation viewer | drafted | [plan 02](02-solver-validation-viewer/README.md) | Painted layer + height-solved mesh overlay, 40% transparent |
| Marker layer + spawn points | **shipped** | [plan 03](03-marker-layer-and-spawns/README.md) | M1-M5 done; sidecar JSON, mode toggle, placement + rotation, render overlay, wave engine + spawn director |
| Map library + browser (planet-view UI) | drafted | [plan 04](04-map-library/README.md) | MapFile schema (L1), map index, content, selector |
| Enemy flow field (route round walls, per class) | **shipped** | [plan 11](11-flow-field/README.md) | F0-F8 done.  Distance field per climb limit, arrows computed not stored, enemies spread rather than stack, passability as a height STEP over a runtime layer, and a desire field for besieging a sealed perimeter.  Does NOT replace the straight-line `enemy_tick` — that is approach mode, and the two modes hand off at the scrambler bubble |
| Combat resolves (damage, death, rubble, towers) | **done** | [plan 12](12-combat-resolution/README.md) | Walls have structural HP and break into climbable rubble, enemies have HP and die leaving bodies that ramp a kill zone shut, towers fire at 15 hex with a sight line and a 30-shot budget, and a wallet the nibblers drain is the only end state.  ⚠ **B7's clock half-falsified the goal:** a sealed wall takes an unattended base from 61 ticks to 104, a wall with a GATE in it buys one tick, and adding a tower cuts it back to 95 — its own dead ramp over the wall it defends.  (B7 first measured 161/311/180; its findings motivated plan 11 F7b's sidestep, and all three conclusions survived the rebaseline.)  **Answered by [plan 13](13-the-vehicle/README.md):** a crew that clears the ramp takes the towered base from 95 ticks to 121 — past the bare wall's 104 — so the tower is finally worth its own dead.  ⚠ Only a BOOSTING crew can reach it: the ramp forms outside the wall and an idle hover unit climbs 0.4 m |
| A player in the world (drive, boost, clear, earn, be destroyed) | **done** | [plan 13](13-the-vehicle/README.md) | V0-V5.  A hover unit at 2 hexes a tick, boosting over its own 3 m wall, clearing the body ramp that beats a tower and getting paid 20 points a metre for it — which takes the towered base from 95 ticks to 145.  Blocking a wave with nowhere to go round costs 5 HP/s and twenty seconds |
| NPC helpers (the crew becomes a cooperative) | **done** | [plan 14](14-helpers/README.md) | H0-H3 done: a crew on the player's chassis at 2.5 hex/s — ⚠ the first mover whose speed does not fit the tick, so it BANKS fractional hexes (not "the tick becomes a timestep"; plan 11 F8's budget trigger does not fire).  They clear and earn on the shared chassis, and a base with two fronts goes 77 → 214 → 242 ticks as the crew grows to cover them — ⚠ a roster buys COVERAGE, not throughput: a second helper beside the first is worth NOTHING.  A crew member can also be lost for good: the blocker rule covers the whole player's side and a helper that dies WRECKS where it stood while the player respawns.  H4 (retrieve → recover) shipped as [plan 15](15-the-carry-model/README.md) C2, where the CARRY model it was blocked on lives: a lost crew member is picked up, carried to the core and rejoins the roster after exactly 90 ticks — and ⚠ nothing else brings one back, which the scenario gates by having its retrieval cut out |
| End-to-end validation scenario | drafted | [plan 05](05-validation-scenario/README.md) | The "minimum playable thing" spec |

When Tier A is done the game is **playable** — a player can
land, paint a base, defend through some waves, scramble.  Not
deep yet; just real.

---

## Tier B — Combat depth + content pipeline

Skilled play hooks **and** the editor-to-entity content
pipeline.  Two parallel arcs that don't strictly depend on
each other but both gate the depth tiers above them.

### Combat depth (DESIGN § 7)

Skilled-play opt-in mechanics; an entry-level player can
ignore the lot and still complete missions.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Tower attack-count decay + repair | drafted | DESIGN § 7 | 30-shot budget; goes black; refill on repair |
| Repair rule (firing tower can't be repaired) | drafted | DESIGN § 7 | Engineering realism: power-down before maintenance |
| Boost (timed, fire-and-forget) | drafted | DESIGN § 7 | Pink, held-key activation; validation ships free |
| Strain system (shot-density wear) | drafted | DESIGN § 7 | Per-shot wear scaled by output level |
| Boost cooldown + active-maintenance bypass | drafted | DESIGN § 7 | Pickup-drop-repair cycle doubles boost frequency |
| Overload (presence-locked input) | drafted | DESIGN § 7 | High-output mode; player must remain |
| Hot-swap cycle (two-top alternation) | drafted | DESIGN § 5 + § 7 | Sustain overload via swap-pit cycling |
| Swap-pit wall pattern | drafted | DESIGN § 5 | Authored indentation: spare top + safe parking + clear path |
| Tower variants (anti-insect / area / etc.) | drafted | DESIGN § 7 future-tower-types | Unlocked via scouting (Tier D) |
| **⚠ Speed decoupled from the tick** | **designed 2026-08-13** | DESIGN § 10 § Speed must NOT be tied to the tick | Owner instruction. Speed varies by role, by tier and by CONDITION (a damaged robot moves slower) — the scout is quite a bit faster and the BOSS quite a bit slower, the latter deliberately so the player has time to strategize rather than react, which is § 6's pre-walk-visibility principle applied inside the fight. So the tick stops being "the time to cross one hex" and becomes a simulation timestep; every enemy banks movement progress the way a tower banks its fire interval (plan 12 B5a, epsilon included). ⚠ Turns two assertions in `tests/12_b0_probe.loft` into an inverted gate, and makes "the tick got shorter" a THIRD trigger for plan 11's incremental field rebuild — the per-tick budget shrinks in direct proportion, and a from-scratch rebuild that fits at 667 ms does not fit at 100 ms |
| **Builders repair the boss** | **designed 2026-08-13** | DESIGN § 7 § Builders repair the BOSS | Closes a mutual-repair knot with the boss's existing heal-nearby-regulars: the player picks which end to break. Three counters, and TWO are architectural — "near" and "room to reach" are decided by where the walls went, not by DPS. ⚠ Check the arithmetic before it ships: a couple of builders can out-heal one laser, which is what splash and artillery are for |
| **Small-robot roles (scout / harvester / builder / miner)** | **designed 2026-08-13** | DESIGN § 10 § Small robots: four roles, one AI | Four economic roles that differ ONLY in how fast their working tools chew a wall — a miner cuts rock for a living, a scout does not. Same routing, same targeting, same retaliation: one `numbers.json` row each plus one branch in `spawn.loft`'s damage-to-wall lookup, which already carries the note saying it becomes one. Makes wave composition a threat the player can READ without a HUD, and sharpens the bracing rule (a strong attacker at a weak hex is what decides a perimeter). The scout is also notably FASTER and has no real weapon — so it is the unit artillery cannot hit and the one not worth hitting, while the miner is what artillery is for: role composition and tower composition become a matching problem. ⚠ The harvester is the one role with no stated mechanical distinction yet; a hauler's obvious axis is what it CARRIES |
| **Damage types + armour + traverse time** | **designed 2026-08-13** | DESIGN § 7 § Damage TYPE is the axis | A triangle, not a ladder: laser is poor vs armour and vaporises salvage; artillery is good vs armour but single-target and can MISS a mover; explosive splashes onto the player's OWN walls; EMP destroys the high-value electrics, blocks longest, and barely scratches insects. Cross-cutting: a tower TURNS to aim, so switching targets costs damage — and a shot that has become impossible is not fired at all, while one already in flight can be dodged by a fast enemy without trying. A flame thrower is short-ranged and excellent against a swarm of SMALL enemies; a sniper is the heaviest gun, slowest to aim, best far and especially bad up close — so range becomes a PROFILE rather than a number, and the two bookend both axes. Two class properties fall out — armour and size — and size is needed by the corpse-blocking rule anyway. Placement becomes a real decision: a sniper is bad inside the base and excellent on an outer ridge (LOS read as a HEIGHT gives elevation for free). Retaliation is an INFORMATION rule — an enemy attacks a tower that hurt IT while the scrambler is up, and one that hurt anyone's companions once it is down — but never overrides ROUTING, so a tower behind a closed perimeter is unreachable and the wave keeps going for the core. A ridge tower is exposed because it is reachable, and costs the player a drive out through a live wave to service it. The ONE exception under scrambling is the BOSS — and NOT as a second AI: bosses run the same rules with different data (2x2, so it cannot fit a one-hex entrance and is the unit most likely to be stuck outside being shot; plus the option to share what is hurting it). The event is emergent. Needs tower HP and routing for a unit wider than one hex |
| Ammo for variant weapons | drafted | DESIGN § 7 | Per-shot consumable, distinct from decay |
| Tactical type-swap mid-combat | drafted | DESIGN § 7 | Different-type spare in swap pit |
| **The CARRY model (one slot, one pickup/drop key)** | **done** — [plan 15](15-the-carry-model/README.md) C0-C3 | DESIGN § 11 § E + § Carry visibility | ⚠ **A shared blocker, named here because three separate features wait on it**: helper retrieval + recovery ([plan 14](14-helpers/README.md) H4), the tower-top repair / hot-swap arc, and the beacon ferry below.  One slot per vehicle (`numbers.json` § helper.carry_slot_count is 1 for player and helper alike), one context-resolved key (empty hands = pickup, carrying = deposit), and everything carried renders above the carrier.  Building it inside any one of the three would serve that case alone and be re-derived by the next.  **Built by [plan 15](15-the-carry-model/README.md)**: one record per object with an `owner`, so an object is on the ground XOR in exactly one carrier's slot and duplication is unrepresentable rather than prevented.  ⚠ It is the ONE runtime layer that is not a hash keyed by hex — two carry objects share a hex when a loaded carrier is destroyed, and a hash deletes one of them silently.  Helper retrieval is done (C2); **tower-tops and beacons are the two consumers still open**, and the contract they arrive under is a kind row plus a destination rule and NO new carrying code.  ⚠ **C3 measured what retrieval is worth and it is nothing yet** — 85/79/79 ticks on one base, because a 60 s recovery is priced against § wave_system's SEVEN waves with 15 s lulls and dryopea plays one wave at a time.  The wave system is the named trigger for re-measuring it; shortening the recovery would be tuning a number to fit a harness.  ⚠⚠ **That trigger FIRED and the answer did not move** — [plan 16](16-the-wave-system/README.md) W4 measured 247/248/248 on a 248-tick base where the crew member genuinely does come back (tick 187), so it is worth ONE tick.  The reason changed: the job is gone by the time they return, because nothing lets a base RECOVER between waves.  **The next trigger is tower-top repair / hot-swap below** — this model's own second consumer.  ⚠⚠ **That trigger fired too**: [plan 17](17-tower-hot-swap/README.md) T3 measured the retrieval at **+76 POINTS** on a base with upkeep, where nothing falls — so the currency changed from the clock to the wallet, and the answer is finally yes |
| New tower order via beacon ferry | drafted | DESIGN § 7 | Carry beacon from core to build site |
| **Wreck decay, blocking + damage types** | **designed 2026-08-13** | ENEMY_MOVEMENT § Bodies are terrain | ONE decay clock driving TWO things: salvage value (a fresh wreck is harvestable, an old one is rubbish) and PASSABILITY (a big robot's body seals its hex until it settles; a small one never does). A plugged chokepoint makes the wave attack the WALL instead, so the player shoots corpses to reopen it — at the cost of shots and salvage, and only while standing at the tower. Damage TYPE picks the trade: laser vaporises, explosive splashes onto the player's own walls, EMP maximises obstruction and destroys the high-value electrics (and barely scratches insects). Needs a decay clock on the rubble layer, per-class body height + breakdown rate, tower damage types, and plan 06 S1's contents layer — see [`plans/12`](12-combat-resolution/README.md) § Wreck decay, blocking, and damage types |

Plan-shaped candidate: `plan-future-XX — Tower mechanics
depth` covering the strain / boost / overload / hot-swap
arc together — they share mechanics and graphics.

⚠⚠ **The UPKEEP half SHIPPED as [plan 17](17-tower-hot-swap/README.md)**
(complete 2026-08-15): the authored seven-wave list is playable, and a
retrieval pays.  ⚠ **What is left here is what makes the HOT-SWAP pay** —
T3 measured a transplant at +3 ticks with an idle donor and −50 with a
firing one, because a tower close enough to donate is close enough to be
shooting.  Its payoff needs swap pits (authored room for a spare) and
STRAIN (a reason to pull a top BEFORE it is spent), which is exactly the
arc below.  Plan 17 was:
rebuild a black tower, and make the top a carry object that can be
transplanted onto a spent one.  ⚠ **Strain / overload / type-swap /
ammo / swap pits stay here** — `DESIGN.md` § Tower overload marks
them deferred to a later tier, and several need tower variants that
do not exist.  ⚠ **Boost stays here too**, and deliberately: it is
about OUTPUT where plan 17 is about RECOVERY, and W4's finding does
not name it.

[Plan 16](16-the-wave-system/README.md) W4 is what named the upkeep
half.  Measured, two ways:

- **The authored seven-wave list is unplayable.**  205 robots is
  6150 HP and a tower is 300 HP of ammunition *for the whole run*,
  so the list needs 21 perfectly-aimed towers.  The best band the
  suite can build plays FOUR waves and falls at 321 with every
  tower black.
- **Nothing else can pay while that is true.**  A retrieval is
  worth one tick, because a base spends its magazines and its wall
  and has no way back up — so the lull is a pause rather than a
  repair window, and every mechanic priced against "lose somebody
  on wave 2, get them back for wave 5" has nowhere to land.

⚠ This is a HOT-SWAP need specifically, not a strain/boost one:
what is missing is carrying a fresh top out to a black tower, which
is the carry model's second consumer and needs a kind row plus a
destination rule and no new carrying code.

⚠ **And plan 17's own first finding sharpens it**: a tower's
magazine is keyed by HEX today (`hash<TowerCharge[q, r]>`), so
making a top carriable is an OWNERSHIP move — the budget becomes a
property of the top — and *detach-and-remount must not refill*, or
two keypresses bypass the whole loop.

#### ⚠ From the 2026-08-26 seed hand-over

Three mechanics arrived with the owner's seed notes that pass
[`DESIGN.md`](../docs/DESIGN.md) § What kind of game this is without
argument, because each puts something in the player's hands **at a
moment when using it costs them something**.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| **The jammer can be switched OFF** | drafted | [SETTING § The recruitment](../docs/SETTING.md) | ⚠⚠ *"It is possible to turn off the Jammer tower.  Has an impact on attack waves."*  The core is the jammer, so switching it off **stops the waves — and stops the salvage**, since scattered robots are what the contract pays for (`DESIGN.md` § 13).  ⚠ It also re-coordinates the swarm, so it is a decision with a downside on both sides.  Cheap: the core already owns the bubble |
| **Traps that do not auto-reset** | ✅ **built** 2026-08-27 | [`src/trap.loft`](../src/trap.loft), BACKLOG C4, `@X281` | ⚠ From the 2023 catalogue (*spike trap*, *demolition*).  Placed in advance, fires once, then somebody must **drive out mid-wave to re-arm it** — mechanically a black tower restored by a standing vehicle (`plans/17` T1), with the cost moved to the front.  ⚠⚠ **Worth the TRIP and not the blast** (`@M057`): one blast nobody re-arms is **−3 ticks**, the same plate re-armed three times is **+106**.  ⚠ Still authored rather than orderable in a run — a trap beacon wants a cargo kind and a key |
| **Moat — a trench that fills with water** | ✅ **built** 2026-08-27, **filled** 2026-08-28 | [`src/moat.loft`](../src/moat.loft), BACKLOG C5 + C9, `@X282`, `@X283` | ⚠ It was the cheapest of the three and stayed cheap: `order_work_units` gained one NAME, `hex_height` one term, `hex_ground` one threshold.  ⚠⚠ **The palette's DROP is read at last** (sea 0, water 1, rapids 3, waterfall 8) — as a LOCAL depth, so [`plans/02`](02-solver-validation-viewer/README.md)'s drainage chain is not foreclosed.  ⚠⚠ **Worth the whole run and it earns NOTHING** (`@M058`): 378 ticks still standing against a wall's 174, on exactly the opening 200 points.  ⚠ A probe killed the feature's headline — **`walk_vehicle` was read by nothing** (`@D006`) — and ⚠⚠ **BACKLOG C10 gave it back on 2026-08-28** (`@X286`): the chassis hovers, so the player falls INTO a trench and BOOSTS out, and *the depth is the cost* is true after all.  ⚠ The depth's other job stands either way: a pile is a surface once it clears the water, which is what *"a trench allowed to fill"* means as arithmetic |  ⚠⚠ **AND BESIEGERS SHOVEL IT SHUT** (BACKLOG C9) — the title's *fills with water* is now also *fills with spoil*, so a moat is a **TIMER**: 130 / 174 / **221** (`@M059`).  ⚠⚠ **What it is really FOR turned out to be the kill zone**: a besieger has to stand at a fixed distance and dig, so a tower behind one is **335 ticks and nine of thirteen dead** (`@M060`)
| **Drawbridge** | drafted | [MATERIALS § Defensive structures](../docs/MATERIALS.md) | Seals or opens the perimeter on demand — and `plans/13` § V4 measured that a sealed base can only be left by boosting, so closing it is a bet on your own flying |
| **Rock kind decides wall strength** | ✅ **built** 2026-08-28 | [`src/damage.loft`](../src/damage.loft) § Footing, BACKLOG C6, `@X284` | ⚠⚠ **153 / 174 / 220 — the same wall on sand, grass and rock** (`@M061`), and the ground turned out to be worth four times itself end to end.  ⚠⚠ **Granite / sand rock / volcanic DO NOT EXIST in the palette** — what does is the SLOPE ladder nothing had ever read, and an angle of repose is exactly the hardness this row wanted.  ⚠ The footing is the ground AROUND the wall rather than under it, because a wall's strength has to survive a save and *under it* was overwritten by the paint; the crew do not haul, so the stone comes from where they stand.  ⚠ **The map chooses, not the player** — until a map puts the good ground somewhere awkward |

### Editor-to-entity content pipeline

The whole stencil-from-editor arc — multi-layer painting,
bridges, stencil authoring mode, mesh baker, mesh
composition, entity runtime.  Brings the suite into
**rapid prototyping** posture: think → paint → bake → drop
into a map → run.

Strategic positioning — **two shipping paths from one
pipeline**:

- **Polish path** (big-studio): the developer never waits
  on art to reach a solid playable state; the final art
  push lands at the end of development on shape-correct
  stencils that are already in every position.  The polish
  artist refines what's there.
- **Strike path** (indie / starting devs): the stencil
  output IS the shipped aesthetic.  No polish layer.
  Clean geometric / art-deco / low-poly / block-layout
  art-direction works as a final shipped style, exactly
  as it does for many indie successes.  This expands the
  suite's addressable audience to indies who don't have
  or need an art team — they ship full games on stencils
  alone.

See [`plan 06`](06-editor-stencil-pipeline/README.md)
§ Who this serves for the three-audience breakdown
(dryopea team + studios + indies).

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Multi-layer painting (moros-house style) | drafted | [plan 06 S1](06-editor-stencil-pipeline/README.md) | Stacked layers per hex; vertical structure |
| Bridges as a primitive | drafted | [plan 06 S1](06-editor-stencil-pipeline/README.md) | Multi-layer connecting spans |
| Stencil authoring mode | drafted | [plan 06 S2](06-editor-stencil-pipeline/README.md) | Same editor, bounded region, separate save format |
| Mesh baker (stencil → static mesh) | drafted | [plan 06 S2](06-editor-stencil-pipeline/README.md) | Scales down to entity size; per-stencil colour |
| Mesh composition (mount + pivot) | drafted | [plan 06 S3](06-editor-stencil-pipeline/README.md) | Tower-base + rotating-top; swivel turrets |
| Entity runtime (baked meshes as units) | drafted | [plan 06 S3](06-editor-stencil-pipeline/README.md) | Spawn, tick, render baked entities |
| World-dressing prefabs (place a stencil into a map) | drafted | [plan 06 § 1](06-editor-stencil-pipeline/README.md) | Old habitats / huge trees / bridges / factories / ruins |
| Jointed / leg movement extensions | sketch | plan 06 S4 (deferred) | Walking robots, insect locomotion — triggers with tier-2 plan |

Plan slot: [`plan 06 — Editor-to-stencil pipeline`](06-editor-stencil-pipeline/README.md)
covers all of the above as a single coherent initiative,
with phases S1-S4.  S4 (joints / legs) explicitly deferred
until tier-2 insects trigger it.

---

## Tier C — Enemy diversity

Make tier 1 fully alive, then extend to tier 2 + tier 3.
Currently all enemies render as the same placeholder magenta
cuboid.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Tier 1 economic-bot vs combat-bot wave split | drafted | DESIGN § 10 + SETTING § Combat bots | Typed wave mixes; combat-bots dormant by default |
| Combat-bot wake klaxon (diegetic activation cue) | drafted | SETTING § Combat bots | Audible signal when an AI reactivates its military |
| Boss = mobile repair platform (2×2) | drafted | DESIGN § 7 Boss | Industrial unit, not a soldier; phase 3 |
| **Boss 2 — the big COMBAT robot** | drafted | [DESIGN § There are TWO boss kinds](../docs/DESIGN.md) | ⚠ **The first real challenge.**  Built to fight; crystal for core AND power weapons; comes from a woken military stockpile, not a factory line.  ⚠⚠ It SHOOTS TOWERS — the first enemy that makes the player poorer rather than closer to losing, and the first that invalidates a learned optimum (a tight funnel denies a 2×2, but not something shooting from outside).  Answer is depth, dispersal, LOS-breaking and EVACUATING tower-tops under fire |
| Tier 2 — insects + sap | drafted | SETTING § Insects | Passive fauna; smell-tracking; `wall_high` blocks |
| Tier 3 — elementals + stones | drafted | SETTING § Elementals | Dormant; gem-keyed activation; 4 sub-kinds |
| Hacking helpers (subvert robot units) | drafted | DESIGN § 9 Helpers | Coordinator bots = highest-value target |
| Robot diversity — typed wave compositions | drafted | SETTING § Robot diversity | Workers / haulers / scouts / coordinators / etc. |

Likely candidate for two plan slots: `plan 07 — Tier 2 insects`
(largest mechanical novelty: passive fauna + smell-tracking) and
`plan 08 — Tier 3 elementals + stones` (gem mechanic, dormancy).
Tier-1 typing + boss + hacking probably fit into plan 03 or
plan 06 expansions.

---

## Tier D — Between-missions meta

Multi-mission play actually feels like a campaign.  Currently
each mission is independent; persistence isn't shipped.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Central space station hub | drafted | SETTING § Between missions | Rented bay; persistent state; pre-mission UI |
| Persistent inventory (tops, materials, points) | drafted | DESIGN § 13 | Carries across runs |
| Q4 loadout closure | drafted | DESIGN § 7 + § 13 | Pick towers from inventory before each sortie |
| Scouting unlocks new tower types | drafted | DESIGN § 7 + § 13 | Scouted intel persists; variants become orderable |
| **Crew skills + statistics — the practice LOOP** | drafted | [PROGRESSION § P2a](../docs/PROGRESSION.md) | ⚠⚠ **The RPG layer, adopted 2026-08-26** (`@X111`, `@X124`) — twelve skills over six statistics, per-character, and **the arrow runs both ways**: practice raises the skill fast and its two statistics slowly, and each statistic lifts four skills, so a crew generalises on its own (diameter 2).  ⚠ Gains bank at the scramble for whoever boarded (`@X126`).  ⚠⚠ **Two curves stack** — diminishing EFFECT per skill (`@X191`, no numerical ceiling) and a global slowdown on TOTAL learning that makes **specialisation beat breadth** (`@X193`), which is what keeps the roster heterogeneous.  ⚠ Its landing rule makes it incremental: **a skill scales a number that already exists** (`@X112`), so ship the three the owner named — `build` scales helper-seconds (the bottleneck DESIGN § 13 names), `repair` the 20 s standing clock, `scout` the detection radius.  Each has a constant to multiply and a gate that already measures it |
| **Endurance pools + living quarters** | drafted | [PROGRESSION § P2d](../docs/PROGRESSION.md) | Work spends a pool, rest restores it — the crew-side version of the tower's charge (`@X113`).  ⚠ It is what turns [MATERIALS § The crew are people](../docs/MATERIALS.md)' *living quarters* into a building a long sortie must actually put up |
| **Post-mission debrief** | drafted | [PROGRESSION § P2f](../docs/PROGRESSION.md) | ⚠⚠ **How the dominant knowledge axis actually accumulates** (`@X144`) — optional, at the station, PULL where in-mission remarks are push.  ⚠ The fence relaxes because the base is finished (`@X145`), so the crew may conclude; what the player keeps is **rules**-knowledge.  ⚠⚠ **The roster is a sensor loadout** (`@X146`) — no scout, no account of where the wave came from — and a helper left behind leaves **an empty chair**.  ⚠⚠ **The crew are UNRELIABLE NARRATORS** (`@X147`) — coloured by position, expertise, stake and salience, never lying — which is what keeps the relaxed fence from becoming an oracle, and which makes **calibrating your own crew** a progression nobody had to author.  ⚠⚠ **Nothing they say is false** — the skew is selection, emphasis and omission (`@X148`), so the build is a filter over a true event list.  ⚠ They **report, never prescribe** (`@X149`): the fence relaxes on the INSTANCE and not on the RULES.  ⚠ Never a scoring screen: § 14 has no fail screen |
| **Crew chatter — the window into the universe** | drafted | [PROGRESSION § P2g](../docs/PROGRESSION.md) | ⚠⚠ **The only delivery channel `SETTING.md` has** (`@X152`) — ~2 300 lines of setting with no tutorial, no overlay and no on-planet discovery path.  ⚠ Novices about Dryopea, **natives of the setting**: they explain where you all came from, never where you are.  ⚠ Overheard, never delivered; sparse, and it **yields to the nudge** (`@X154`).  ⚠ Backgrounds become a **vantage on the universe** — a hive dweller and a spacer see different outer worlds |
| **Ask a crew member mid-sortie** | drafted | [PROGRESSION § P2g](../docs/PROGRESSION.md) | ⚠⚠ **Passes DESIGN § What kind of game this is outright** (`@X153`) — the player DRIVES to them and stays a moment, so it costs the clock and the position.  A POSITION, not a keypress, the same shape as [plan 17](17-tower-hot-swap/README.md) § T1's repair.  ⚠⚠ **UI-LIGHT: presence IS the interaction** (`@X156`) — no topic list, no dialogue panel, no `talk` key.  The player picks WHO and WHEN; the crew pick WHAT, so this and the idle remark are **one mechanism with two selectors** |
| **Stranded helpers build a LIFE** | sketch | [PROGRESSION § P2e](../docs/PROGRESSION.md) | ⚠⚠ **Being left behind is not necessarily grim** (`@X171`) — shelter and a garden from what the wildlife killed, running the natives' own playbook.  ⚠ A lone human with no jammer is not a target, so the fiction was already ready.  ⚠⚠ **How well they do is a readout of the crew you built** — the practice loop's verdict delivered back as a place.  ⚠ Changes the TONE, not the cost; opens a third ending (**they stay**).  ⚠⚠ Other players hunting them is **content that TEACHES** (`@X172`) |
| **Player-built caches** | sketch | [DESIGN § 20](../docs/DESIGN.md) | ⚠⚠ **Abandonment as a PLAN** (`@X173`) — bury what the rocket cannot lift, as a kickstart near the same place.  ⚠ Sharpens the scramble: *what do I take, what do I hide, can I get back to it*.  ⚠⚠ Other players may crack one open — **PvP with no combat and no netcode**.  ⚠ Hard to find, hard to crack, and **cracking should be LOUD** |
| **The crew section — cross-player rescue and defection** | sketch | [PROGRESSION § P2e](../docs/PROGRESSION.md) | ⚠⚠ **Another player picks up who you left, and a frankly bad relation lets them stay** (`@X168`) — half of it is already in DESIGN § 9 § Stranded helpers, and the retrieval path is **shipped** in [plan 14](14-helpers/README.md).  ⚠⚠ **Crew move between players but never as GOODS** — a choice, not a transaction.  ⚠ **Default must be RETURN** (`@X169`), so returning somebody is a favour with no reward that buys **reputation**.  ⚠ A **second asynchronous channel**, and it degrades to single player with no second design (`@X170`) |
| **Crew relationships — shared history** | drafted | [PROGRESSION § P2e](../docs/PROGRESSION.md) | ⚠⚠ **A ledger of what happened, never an affinity bar** (`@X131`) — the same mechanism as the practice loop, one word changed: the crew get closer through what they WENT THROUGH.  ⚠ The history already exists (`@X132`) — who retrieved whom, who was left at force-launch, who boarded — so this adds a record, not a simulation.  ⚠⚠ Turns § 9's **stranded helper** into the campaign's best story beat with nothing written.  ⚠ Keep counts + landmarks, never a log (`@X134`).  **Blocked on per-campaign persistence (this tier) and on text (`@X130`)** |
| **Crew remarks on landing — THE ONBOARDING** | drafted | [PROGRESSION § P2c](../docs/PROGRESSION.md) | ⚠⚠ **There is no tutorial** (`@X137`), so this is not a convenience: controls answer *how* and the recon window answers *is it safe to try*, but neither answers **what to do first** or **why keep going** (`@X141`).  ⚠⚠ **WHO speaks is the helper with NO TASK** (`@X142`) — no stall detector and no tuned window; it reads the order list the player already fills, and it self-calibrates.  ⚠⚠ **They know no more than the player — first time on this planet too** (`@X150`), so a past job gives trained ATTENTION and never world knowledge, and the no-concluding rule holds by fiction rather than by discipline.  ⚠ They **learn alongside** the player over a campaign, which is audible progression (`@X151`).  ⚠ **The crew comment on the world in their own domain** (`@X129`) — an ex-miner sees ore, an ex-security officer sees approach lanes, an ex-handyman sees what the pollen is eating, an ex-scout sees distance.  ⚠ Each is right about their half and blind to the rest, so the synthesis stays the player's; a remark **points, never concludes**.  ⚠⚠ **They fire in OFF-TIME only** (`@X135`) — a player getting on with it hears nothing, a stalled one hears a line, so `@X120`'s *the tutorial cannot be a wall* is satisfied with no skip button.  ⚠⚠ **Silence must be a real outcome** (`@X136`) or stalling becomes the way to get answers.  ⚠ The trigger is absence of PROGRESS, never idleness — a parked player is repairing.  ⚠⚠ **BLOCKED on text** (`@X130`) — the first designed feature that needs `draw_text`, which `@X097` says dryopea cannot do; prerequisite is a font in the repo plus `draw_text` under `loft test` |
| **Crew personality — a voice per class** | drafted | [PROGRESSION § P2h](../docs/PROGRESSION.md) | ⚠⚠ **Class decides what they notice, voice decides how they say it** (`@X157`) — and it reaches wording and emphasis, never the facts.  ⚠⚠ **Author it as a FILTER, not a matrix** (`@X158`): domain picks a true thing, voice colours the phrasing, so cost is additive rather than (class × personality × situation).  ⚠ It is also **what makes the empty chair hurt** — you do not miss *a scout*, you miss the one who reported in three words |
| **The office MARKET — sell salvage** | drafted | [PROGRESSION § P2i](../docs/PROGRESSION.md) | ⚠⚠ **Closes the contract's loop** (`@X162`) — SETTING § The recruitment makes salvage the pay and there was nowhere to be paid; it is the materials→points exchange MATERIALS § Open questions 1 recommended.  ⚠⚠ **Never a way to earn without going down**, and **prices stay stable and knowable** — one that fluctuates on entry is a refresh button on the economy |
| **The TRADER role** | sketch | [PROGRESSION § P2i](../docs/PROGRESSION.md) | ⚠⚠ **A legitimate alternative role, and it must be POLITICAL** (`@X165`) — permits, competitors, factions, the cordon's officials, all already in SETTING.  ⚠⚠ Stable prices remove arbitrage, so **the only lever is who you deal with** — which is what makes *not a spreadsheet role* achievable.  ⚠ Cannot hollow out the game: salvage comes from sorties and nothing else (`@X166`).  ⚠⚠ **Honestly not a tower defence** (`@X167`) — the boundary is that it must not change the tower-defence game for anyone playing it.  ⚠ Belongs AFTER the shared market: single player would need an economic simulation ROBOT_ECONOMY already refuses |

#### ⚠⚠ Multiplayer — a lot of design, and NONE of it is near-term  `@X187`

Owner, 2026-08-26: *"that should not bother us too much in the short
term.  It is an old idea that I would love to see come to fruition, but
**it needs an active community of people that only the base game can
provide**.  Where we reuse a lot of the assets and game mechanics in a
new format."*

⚠⚠ **The dependency is an AUDIENCE, not a technology.**  Every row below
is cheap *because* it reuses the base game's assets and mechanics — and
that reuse only works if the base game is finished and coherent first.

⚠ **So read these rows as a destination, not as a queue.**  ⚠⚠ **The
volume of design in [`DESIGN.md`](../docs/DESIGN.md) § 20 is not a signal
of its priority** — it accumulated in one sitting because the ideas
compose, not because the work is next.  § The critical path above is
unchanged, and **BUILDING is still the biggest missing mechanic**.

⚠⚠ **What serves the multiplayer modes TODAY is building the base game
well** — nothing here needs anything built for it, and a mechanic bent
early to suit a mode that has no players yet is the way to lose both.

⚠⚠ **And that is not a consolation — the campaign builds the PIECES**
(`@X188`).  **Per-planet persistence** (this tier) is the *sole*
prerequisite of five of the rows below; the **robot economy** (Tier E) is
the territorial mode's neutral structures; **hacking** (Tier C) is the
whole aggressive route; the **crew layer** is cross-player rescue entire.
⚠ So the distance shrinks as a **side effect**, and the only rule for
today is: **do not gold-plate a campaign piece for multiplayer, and do
not design one that forecloses it** — persistence per-planet-per-player
rather than per-session, and code that asks *which* player rather than
assuming *the* player.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| **Co-operative multiplayer — friendly towers** | sketch | [DESIGN § 20](../docs/DESIGN.md) | ⚠⚠ **Co-op is nearly FREE** (`@X178`) — every action is position-triggered and none asks who you are, so a guest can repair towers, clear bodies and deliver salvage the moment the owner sets the towers friendly.  ⚠ Permission is a **TOWER COLOUR** (§ HUD already speaks that language), revocable — so **betrayal is possible**, which makes trust worth something |
| **Territorial mode — contested installations** | ⚠ **deferred behind the same trigger** | [DESIGN § 20](../docs/DESIGN.md) | ⚠⚠ **A rearrangement, not a new game** (`@X184`) — ROBOT_ECONOMY's six installation types as neutral structures, `@X176`'s hacking to take them, towers and walls to hold them.  ⚠⚠ **Balance rule: a big area is harder to guard** (`@X185`), so the leader is exposed to **precise** attacks — no rubber band, and **already measured** by `@M020`, plan 12 B7 and plan 17 T3.  ⚠ Installations **cannot be moved**, so the geometry enforces it.  ⚠ Collides with ROBOT_ECONOMY's *no underground level*; the separate-mode container resolves it |
| **Team matches — tower hacking and base takeover** | ⚠ **deferred behind a trigger** | [DESIGN § 20](../docs/DESIGN.md) | ⚠⚠ **2v2 / 3v3 only** (`@X179`) — hacking a tower is the one thing that would break `@X175`, and team scoping makes it safe through **consent plus a counter**: both sides signed up, and a teammate can drive to a hacked tower.  ⚠⚠ A takeover is a contest of **PRESENCE** — hacking and repairing are the same verb reversed — so the noncombatant player survives.  ⚠⚠ **The ONLY multiplayer feature needing netcode** (`@X180`, `@X182`); everything else here ships on persistence alone.  ⚠⚠ **Must not risk campaign bases or crew** (`@X181`) |
| **Combative multiplayer — point the planet at them** | sketch | [DESIGN § 20](../docs/DESIGN.md) | ⚠⚠ **A base is impregnable to a PERSON** (`@X175`), so PvP is never raiding-by-violence: the aggressive route is **hacking enough robots to overwhelm somebody** (`@X176`), which keeps the enemy robots and the defence towers.  ⚠ Campaign-scale expensive, so hostility is earned; and the attack **arrives as content** because the defender's answer is the one they always had.  ⚠⚠ Default PvP is a **RACE** — and it settles ROBOT_ECONOMY's open question as **per-planet** (`@X177`) |
| **Asynchronous multiplayer — trade through the station** | sketch | [PROGRESSION § P2i](../docs/PROGRESSION.md) | ⚠⚠ **Where § 20's multiplayer ambition finally has an INTERFACE** (`@X163`) — a shared market needs none of the netcode a shared sortie would, players never meet, and it fits § 14's bounded sessions exactly.  ⚠⚠ **Knowledge is NOT tradeable** (`@X164`): things may cross, answers may not |
| **The office — recruit / dismiss / station tasks** | drafted | [PROGRESSION § P2i](../docs/PROGRESSION.md) | ⚠⚠ **The between-missions half of the roster, which has never existed** (`@X159`) — DESIGN § 9 only orders helpers in-mission.  ⚠ A helper who **stays up** runs the store, processes salvage or analyses AI cores, and the practice loop applies unchanged.  ⚠⚠ **Makes the roster an allocation problem every mission**: who goes down is labour + sensor loadout, who stays up is paid work + safety.  ⚠ A PLACE, not a roster screen (`@X161`); dismissal can send somebody to a competitor (`@X160`) |
| **Crew templates — backgrounds and classes** | drafted | [PROGRESSION § P2b](../docs/PROGRESSION.md) | ⚠⚠ **Templates, never rerolls** (`@X125`) — the player picks a known profile and optimises it by giving the NPC the work.  ⚠ The slots exist in the 2023 schema and are **empty**: eight backgrounds and sixteen classes carry descriptions and no numbers, so the job is authoring them against the pair table so that no template is dominant |
| **Defection — crew join competitors when neglected** | sketch | [PROGRESSION § P2d](../docs/PROGRESSION.md) | From the 2023 data.  ⚠ Neglect becomes a run-level consequence with a face on it, and it lands the player's crew on [SETTING § The competitors](../docs/SETTING.md)' payroll |
| **The perspective rule — switch human at the terminal** | sketch | [PROGRESSION § P8](../docs/PROGRESSION.md) | ⚠ `@X115` — see the world through a person, switch only at the main communication terminal.  ⚠⚠ Without it a skilled crew is a spreadsheet the player watches; with it the switch costs a drive back to the core mid-wave.  A change to the control model (DESIGN § 8, § 9), so it needs a decision before a plan |
| Helper rescue quests | drafted | DESIGN § 9 Helpers | Stranded helpers from past missions, rescuable |
| Static planet-view map selector | drafted | [plan 04 L3](04-map-library/README.md) | Clickable markers per available map |
| Rotating planet-view UI (future UX) | sketch | SETTING § Future UX | Day/night terminator, overlay state |
| Bounded sessions + mission chaining | drafted | DESIGN § 14 | Time-windowed run shape |

Likely candidate for `plan 10 — Station hub + persistence`
covering hub UI + inventory + Q4 loadout + scout-unlock.  Mission
chaining (DESIGN § 14) may want its own slot once the persistent
inventory lands and its trigger fires.

---

## Tier E — Narrative arcs (deep content)

The world the validation mechanics live in.  Deliberately
gated behind player demonstrated competence — see
[SETTING.md § Future contact](../docs/SETTING.md#future-contact--humans-ais-and-the-no-shortcut-rule)
for the no-shortcut design rule.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| **Robot economy — natural wave patterns** | drafted | [docs/ROBOT_ECONOMY.md](../docs/ROBOT_ECONOMY.md) | Six installation types + transport routes; traffic replaces the authored wave list and spawn markers.  ⚠ **Retires [plan 16](16-the-wave-system/README.md)**, which the owner has committed to removing before ship |
| **The knowledge tree — ~55 facts in arcs** | drafted | [SETTING § The knowledge tree](../docs/SETTING.md) | ⚠⚠ The structure § Future contact's no-shortcut rule has been describing without one, recovered from the 2023 data: natives → sap/insects → robots → aliens/portal → the old ones, each arc a chain of sentences the player learns.  ⚠ Knowledge must change what the player can **attempt**, never how well they do it — a knowledge item granting *+10% hacking* belongs on [PROGRESSION § P2](../docs/PROGRESSION.md)'s crew axis instead.  ⚠⚠ And it must be **found, never sold** (§ P6a: knowledge IS the answer, and an upgrade may only buy friction) |
| **Competitors — the other permits** | sketch | [SETTING § The competitors](../docs/SETTING.md) | Other operators, some unapproved.  ⚠ Start with the **traces** (a stripped wreck field, a half-built wall) — authored terrain, no mechanism.  A raid is a wave that wants your cargo rather than your jammer |
| **The four endings** | sketch | [SETTING § The endings](../docs/SETTING.md) | Reprogram / shut down / keep mining / set to defend the natives.  ⚠ All four resolve *what do you do with a machine a person lives inside*, and **destroying it in battle is not among them** |
| **The six biomes** | sketch | [SETTING § The biomes](../docs/SETTING.md) | Moors / forest / mountains / caves / coast / swamp, as **authoring briefs** — each states the problem a base built there must solve.  ⚠ Caves stay geography, never a level (`ROBOT_ECONOMY.md`) |
| Faction territory awareness | drafted | SETTING § Robot diversity | Maps tagged with AI faction; affects compositions |
| Side quests — underground human contact | drafted | SETTING § Future contact | Breadcrumb discovery → first contact → trade |
| **The buried city + the portal** | sketch | [SETTING § The buried city](../docs/SETTING.md) | An ancient city the underground humans FOUND, its statues of the OLD ONES AND THEIR SERVANTS kept under cloth, and a portal beneath it to a benevolent being that accepts people at the price of their individuality.  ⚠ The builders took that portal, so the statues are documentation and the cloth is denial — and the statue gallery is the end-game BESTIARY, seen before it is ever met |
| **End-game: enemies change, defence does not** | sketch | [DESIGN § The end game, and why it is still this game](../docs/DESIGN.md) | The old one's other servants attack; robots and insects become co-belligerents; the player still BUILDS BASES.  ⚠ Humans cannot attack an old one and the other two factions inherently can, so the human contribution is logistics and architecture — the genre's strongest justification, arriving last.  The ROBOT_ECONOMY map keeps its geometry and reverses its meaning |
| **Portal warriors — the world's immune response** | sketch | [SETTING § The warriors are the world's immune response](../docs/SETTING.md) | Humans and other beings arrive when an old one wakes too far.  ⚠ Answers the OLD ONE, never the player — a summonable version would be a superweapon.  Makes the end-game a BATTLE rather than a doom |
| Direct contact with an AI | drafted | SETTING § Future contact | Deep-lore: meet the girl-hacker AI as a person |
| Truth discovery — government cordon paradox | drafted | SETTING § Future contact | Off-planet leverage |
| Player-faction alignment | drafted | SETTING § Future contact | Ally with one AI vs another |
| Off-planet meta — orbital banking, vendors | sketch | DESIGN § 13 Future expansion | Shop at the station hub |
| Multi-player disruption missions | sketch | SETTING § How mechanics fit | Coordinate against the AI economy |
| **The old ones — world event** | sketch | [SETTING § The old ones are a Lovecraft reference](../docs/SETTING.md) | Mythos-register looming danger; wakes only through COORDINATED MANY-PLAYER action, never a solo sortie.  ⚠ Its escalation is COMMAND for tier 3, not units — nothing gets stronger, the elementals simply start wanting something.  Present as signs long before it is a mechanic |

Likely candidate for `plan 11 — Future contact arcs`, but this
tier is **deferred** by design — it's the cap on the skill
ceiling, not the floor.  Authoring of breadcrumbs sits inside
the maps from plan 04 + plan 07-08, so the *content* lives in
the per-map authoring, while the *triggers + state machine*
deserve a plan when the trigger to start lands.

---

## Persistence destination — path-backed `Store` (deferred, and now on MEASURED grounds)

Today the editor saves to `dryopea_save.json` via `text as
MapFile` round-trip.  The eventual destination is **the hash IS
the file**: each in-game data structure (painted hexes, marker
layer, stencil instances, …) is a `Store` mmap'd from a path —
lookups are direct memory reads, mutations are durable on the
next OS msync, no explicit save/load loop.

The Rust side already supports this (`Store::open(path)`,
`Store::open_durable(path, mode)`; @PLAN38 phase 01 shipped in
loft commit `d494edc`).  The integrity bracket
(`store_durable_check` / `store_durable_seal`) is exposed to
`.loft` user code as of phase 01b (`8bc4b08`).  ~~**What's missing
is the language surface for binding a user-data `Store` to a path
at program startup**~~ — ⚠⚠ **IT SHIPPED**, and
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) has it under
Resolved.  See § What B3 measured below.

Until that lands, we **stay on JSON** for the world file —
the manual binary `file()` + `#read` cursor-IO route is a worse
stopgap than the existing JSON path (still requires hand-rolled
ser/deser, still doesn't get us mmap).  Hybrid split when the
surface lands:

- **Store-backed mmap** for bulk runtime state — painted hexes,
  marker layer, stencil instance lists (everything that mutates
  during play).
- **JSON** for human-edited content — `examples/palette.json`,
  per-map metadata + objective + waves list, stencil library
  definitions.  Diffable, hand-editable, git-friendly.

~~When the upstream surface lands the dryopea migration is a
**one-line annotation** on `PaintedWorld` / the marker-layer
wrapper / etc.; the rest of the codebase doesn't change.~~

### ⚠⚠ What B3 measured — the one-line prediction is FALSIFIED

`@M052`, 2026-08-27.  `store_persist_bind` **works**: 63 painted hexes
bound in one process and read back in another, every value intact, with
no save call.  The destination is real.  Four measurements say dryopea
cannot take it yet:

1. ⚠ **A store does not say what it is.**  A MARKER file bound into a
   `PaintedWorld` returns `true`, reports the right count, and reads as
   a patch of **water** — `MarkerEntry` and `PaintedHex` share `q`, `r`
   and a `u8`, so a marker's kind is read as a palette index and a spawn
   marker (kind 0 = sea) silently vanishes.
2. ⚠⚠ **dryopea's world is a FIELD, so the file is `EditorState`.**
   Binding `s.pw.painted` writes the whole container's store — loft
   advises `persist-bind-through-field` at the call — so the undo
   HISTORY rides along (12 entries in, 12 out, which is Ctrl+Z undoing
   an edit from a previous session) and the on-disk LAYOUT becomes the
   editor's working struct's.  **Add a field to `EditorState` and every
   saved world is silently misread.**
3. ⚠ **A binding does not survive being handed over.**  `s.pw = bound`
   COPIES (`CLAUDE.md` § a struct stored in a FIELD is a copy), so
   binding a small stable container and giving it to the editor writes
   nothing — measured at 2 hexes painted and **0** read back.
4. ⚠ A missing directory is a silent `false`.

**So the migration is not an annotation: it needs `EditorState`
restructured so the world lives in a store-owning container of its
own.**  That is a change to the editor's central seam, and it is now a
decision somebody can take on numbers rather than on a prediction.
BACKLOG B3 shipped the per-planet KEY over the JSON path instead
(`@X275`).

---

## How to use this

- **"What could we do next?"** — scan the table for the nearest
  drafted row whose dependencies are shipped.  Pick whichever
  seems most appealing.
- **"Is X in the plan?"** — search for X here.  If it's not
  here, either it's not yet design-thought or it lives in a
  lib_plan (loft library scope — outside dryopea).
- **"What's the dependency between X and Y?"** — the tier
  ordering is a hint but not a strict gate.  Concrete
  dependencies live in each plan's `## Dependencies` section.

Diverging from the order is expected.  The dogfood loop
(per [CLAUDE.md](../CLAUDE.md) dev cadence) often pulls a
later-tier feature forward when it sharpens an earlier-tier
demo.  Update this file when something ships or when a new
candidate is added.

---

## See also

- [`README.md`](README.md) — plans admin (workflow, file layout)
- [`DEFERRED.md`](DEFERRED.md) — parked plans
- [`../docs/DESIGN.md`](../docs/DESIGN.md) — master design
- [`../docs/SETTING.md`](../docs/SETTING.md) — fiction
- [`../QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) — outstanding loft-side asks
