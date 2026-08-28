<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# BACKLOG — concrete things to build, in NO fixed order

⚠⚠ **This list is deliberately UNORDERED** (project owner, 2026-08-26).
[`ROADMAP.md`](ROADMAP.md) carries the *dependency* order and the tiers;
this carries **what a person could sit down and build**, so that picking
work does not require re-deriving it from four documents.

⚠ **Grouped by what each item UNBLOCKS, not by priority.**  A group is a
statement about consequences, never about sequence.

⚠ Rough size is `XS / S / M / MH / H`, the same letters plans use.

---

## A. Unblocks PLAYING the game today

⚠⚠ **Nothing here is a feature — it is the difference between a game that
runs and a game you can sit down with.**

| # | What | Size | Why |
|---|---|---|---|
| **A1** | ✅ **DONE 2026-08-26** — load a `.keys` scenario into a live session: `make play SCRIPT=a-defended-base` | **S** | ⚠⚠ Was *the single biggest blocker on playing today*.  `src/scenario.loft` joins `script_run_on` to `main.loft`, and the **36 `.keys` files in `tests/scripts/` + `tests/gl/` are now 36 playable starting positions**.  ⚠⚠ **A scenario opens as its PREFIX, cut at the first `tick` / `fall`** (`@X263`) — its question, not its answer.  ⚠ The launcher's flag is `script=<name>`; `--script` never reaches `arguments()` (`@X264`).  ⚠ Measured in `@M044`; gated by `tests/a1_the_scenario.loft` |
| **A2** | ✅ **DONE 2026-08-27** — three authored maps in `maps/`: `starter_01`, `crossroads_02`, `the_gap_03` | S | `make play MAP=starter_01` now opens a base.  ⚠⚠ **The `.keys` beside each `.json` is the SOURCE and both are committed** (`@X265`); `make maps` rebuilds and **refuses a map nobody could play** (`@X266`).  ⚠ Each teaches one MEASURED thing (`@M045`) — where to stand (+61 ticks), that parking is the wrong answer, terrain instead of masonry (wave 5).  ⚠ A map still holds only the GROUND and the MARKERS: no crew, no wave list, so every one is played solo.  Gated by `tests/a2_the_maps.loft` |
| **A3** | ✅ **DONE 2026-08-27, both halves, and neither was new work** | S | ⚠⚠ **The `save` verb already existed**: `do save` is an ACTION in the key table, so a `.keys` script has been able to write a map since plan 09 (`tests/scripts/round-trip.keys` is the proof).  ⚠ **The exporter is `scripts/build_maps.sh`** (A2) — it plays a `.keys` source and writes the map pair, which is *scenario → map* exactly.  ⚠ What is left is the OTHER direction for a live session: `emit.loft` writes a situation as `.keys`, but nothing wires a key to it — that is `plans/18` § S5 |
| **A4** | ✅ **DONE 2026-08-27** — [`docs/PLAYING.md`](../docs/PLAYING.md) | XS | Every key, what happens without pressing one, and what is not there yet.  ⚠⚠ **GATED against `bindings.loft::editor_actions`** (`@X267`) — because `main.loft`'s hand-kept copy was **off by one for twenty-five plans** (`@M046`), and a comment is compiled by nothing.  ⚠ The entry's header now points at it instead of keeping a second list |

## B. Unblocks a LOT of other work

| # | What | Size | Why |
|---|---|---|---|
| **B1** | ✅ **DONE 2026-08-27** — `assets/DejaVuSans-Bold.ttf` + [`src/font.loft`](../src/font.loft) | M | ⚠⚠ **Half the blocker was STALE**: `draw_text` already worked headless under `loft test` — measured at **1324 pixels** on the pinned `graphics` 0.5.2 (`@M047`).  The font half was real and is paid.  ⚠⚠ **The seam owns the ABSOLUTE path**, because a relative one means different things in 0.5.2 and 0.8.0, and a font that failed to load **draws in whatever font loaded first** rather than drawing nothing (`@X268`).  ⚠ The five designed things (`@X130`) are unblocked; the wallet stays seven-segment by CHOICE now.  ⚠⚠ **What the first consumer must know**: 774 of a glyph line's 1324 pixels are BLENDED, so text drawn into a classified frame breaks `@X077` and `@X092`.  Gated by `tests/b1_the_font_seam.loft` |
| **B2** | ✅ **DONE 2026-08-27** — [`plans/27`](27-building/README.md), C0-C5 | H | ⚠⚠ **The player can build.**  Press **Q** and every hex you drive over is ordered as a wall your crew raise; press **E** at the core and 100 points becomes a tower beacon to carry out and plant.  ⚠⚠ **Measured worth: +44 ticks on a base that otherwise falls at 130** (`@M050`) — and that is `@X022`'s *pre-wave window is a budget* made measurable for the first time.  ⚠ **Four findings the gates caught**: the renderer could not SEE a structure appear (`@M048`, and it retired `@X095`); a float build rate came up a tick short (`@M049`); a five-row band could not be SEALED (`@X272`); and a tower order had to refuse ERASURE or the wall trail destroys a paid-for beacon (`@X274`).  ⚠ Still unbuilt and deliberately out of scope: **helper orders**, the 8-walls wave trigger, bridges, and `@X252`'s *directed* helpers |
| **B3** | ✅ **DONE 2026-08-27** — [`src/persist.loft`](../src/persist.loft), `make play PLANET=kepler` | MH | ⚠⚠ **A planet is a place that REMEMBERS** (`@X275`): land, build a wall, save, come back, and it is where you left it.  Keyed `<planet>/<player>` from the first day (`@X188`) though dryopea has one player, because a shared world cannot be retrofitted into a path with no room for a name.  ⚠ A planet holds the GROUND and the MARKERS — the pair a MAP holds — and a RUN is not in it.  ⚠⚠ **The mmap destination was PROBED and deferred on measured grounds** (`@M052`): `store_persist_bind` shipped and works across processes, but dryopea's world is a FIELD of `EditorState`, so a bind writes the EDITOR's store — the undo history rides along, and any new editor field silently invalidates every saved world.  `ROADMAP.md`'s *"one-line annotation"* is falsified.  Gated by `tests/b3_the_planet.loft` |
| **B4** | ✅ **DONE 2026-08-27** — [`src/errand.loft`](../src/errand.loft), `traffic <rate>` | M | ⚠⚠ **It needed no mover and no second AI**: `enemy_walk_heading` has walked a robot along its business since plan 11 F5b, and the fiction already said a spawn marker's direction is *what they were going before*.  What was missing was at both ends — a robot's business never ENDED (walk into a cliff and it stood there for the run) and nothing ever STARTED one the wave schedule had not (`@X276`).  ⚠⚠ **The bubble is the whole mechanic**: a robot that enters it loses its errand, one way, so **the same traffic is scenery or a wave and where you landed decides** — 200 of 200 points past a road that runs by, drained by one that runs through (`@M053`).  ⚠ Defaults off, so the 679 existing measurements did not move |

## C. Self-contained — buildable without anything else moving

| # | What | Size | Why |
|---|---|---|---|
| **C1** | ✅ **DONE 2026-08-27** — [`src/skill.loft`](../src/skill.loft), `skill <i> <build> <repair> <scout>` | M | ⚠⚠ **Two of the three scaled a constant that existed; the third did NOT.**  `build` (helper-seconds) and `repair` (the standing clock) landed as `@X112` predicted — **a wall goes from 15 ticks to 10 at level 5** (`@M054`).  ⚠⚠ **`scout` had no number to scale**: there is no detection radius anywhere in `src/` or `numbers.json`, which is the case `@X112` itself says to push back on.  The owner ruled what it is (`@X277`): **two radii that INTERACT, and the SUBJECT owns half** — a heap announces itself, the motors inside a wreck do not, so a trained scout notices *quieter* things rather than seeing further.  ⚠ The curve is CHOSEN and says so — the archive names the twelve skills and gives no numbers.  ⚠ Level 0 is bit-for-bit the old game, so 687 measurements did not move |
| **C2** | ✅ **DONE 2026-08-27** — [`src/endure.loft`](../src/endure.loft), `tired <i> <seconds>` | S | ⚠⚠ **Work spends, rest restores, and a tired person works LESS rather than stopping** (`@X279`) — `PROGRESSION.md` refuses the cliff, so the pool bottoms out at a floor.  ⚠ **Rest is CONTINUOUS**, which is what lets the three jobs spend independently with nothing to co-ordinate.  ⚠⚠ **Skill makes you PRODUCTIVE, not TIRELESS**: a site tires by the RAW elapsed, so a trained builder gets more done for the same tiredness.  ⚠ The pool is sized from the design's own sortie (`@M055`): ~675 ticks to tire, against a corpus whose longest base is **321**, so 687 measurements did not move — and the arithmetic is asserted, so a longer base goes red first.  ⚠ `MATERIALS.md`'s living quarters is the multiplier when it lands |
| **C3** | ✅ **DONE 2026-08-27** — [`src/jammer.loft`](../src/jammer.loft), `jammer on` / `jammer off`, key **J** at the core | S | ⚠⚠ **It stops the SUPPLY and never the SIEGE**, and that one line is what keeps the SCRAMBLE the answer to being overrun (`@X280`): losing an errand is one way, so a player who hits the switch with a wave on the perimeter has bought nothing for that wave.  ⚠⚠ **Measured worth: two waves bought, sixty points given up** (`@M056`) — and the scenario is the claim with one line of script, `a-core-switched-off.keys` being `a-base-that-plays-its-list.keys` plus `jammer off`, four waves down to **one** and finishing on exactly 200.0 points with ten points of wreckage lying uncollected.  ⚠ **The salvage stops as INCOME, never as WORK**: the heap still clears, you are just working for free — which is also why toggling cannot be farmed.  ⚠ The field stores OFF so [loft#914]'s silent default is today's game; 687 measurements did not move.  ⚠ `DESIGN.md` § Shutting down the scrambler's END-GAME job — the swarm turning on the ancient ones, the bubble as a LURE — needs a tier 3 that does not exist |
| **C4** | ✅ **DONE 2026-08-27** — [`src/trap.loft`](../src/trap.loft), the fourth marker kind, `trap <q> <r> <on\|off> <rearm>` | S | ⚠⚠ **A plate fired once is worth LESS than no plate at all, and the mechanic is the TRIP BACK** (`@M057`): −3 ticks for one blast nobody re-arms, **+106** for the same plate re-armed three times — and the control says it is not the crew member standing there (parked all run: +20).  ⚠⚠ **ONE HEX decides it and it fails two different ways**: in the gate the servicing hex is the hex the wave comes through and the crew member is wrecked; two hexes out it never gets through its own gate at all and the run is the baseline to the tick.  ⚠⚠ **The trigger is a CROSSING, never a standing position** (`@X281`) — a scout strides TWO hexes in two ticks out of three, so a plate asking `occupancy_taken` is walked over by the one class fast enough to matter, and **no test using a regular robot could see it**; `occupancy.loft` grew a `visits` count, one field and one line.  ⚠ The blast is `hex_disc_radius_1` and two derivations land on it — a crosser cannot out-stride it, and it is the one footprint `numbers.json` names.  ⚠ It does not fire on the player: the vehicle HOVERS.  ⚠ Adding the fourth marker kind moved **33 `.keys` fixtures and 14 inline test scripts** by one `do cycle_kind` press — the churn `CLAUDE.md` § Add a marker kind warns about |
| **C5** | ✅ **DONE 2026-08-27** — [`src/moat.loft`](../src/moat.loft), the trench trail on key **F**, `dig on` / `dig off` | S | ⚠⚠ **A PROBE KILLED THE FEATURE'S HEADLINE BEFORE IT SHIPPED** (`@X282`).  It was designed as *the depth is the COST, because the crew and the player HOVER and fall in* — and **`walk_vehicle` is read by NOTHING** (`@D006`): `can_climb` refuses a step whose either end fails `hex_walkable`, which answers `walk_ground` for everybody, so nobody has ever been able to enter water and a boost does not cross a 1 m ditch.  ⚠⚠ **So the drop's whole job is the WATERLINE**: a pile is a surface only once it clears the water, which makes the depth *how much a moat swallows* — `water`'s 1 m is **two bodies** — and is `MATERIALS.md`'s *"a trench allowed to fill"* as arithmetic.  Without it the drop would decide nothing.  ⚠⚠ **Worth the whole run and it EARNS NOTHING** (`@M058`): the third of plan 27 C5's pair, one token apart — 130 / 174 / **still standing at 378**, on exactly the opening 200 points with 13 robots alive and **zero targets**.  A wave that cannot reach you cannot die, and salvage is the only income.  ⚠ Three costs, all asserted where they happen: nothing to chew, **the crew dug themselves in**, and the player cannot follow the wave back across.  ⚠ `water` and only `water` may be ordered — a `waterfall` trench wants sixteen bodies and nothing can put them there.  ⚠⚠ **AND *nothing fills one* IS CLOSED** — BACKLOG C9 (2026-08-28) makes a besieger shovel it shut, so the trench falls at **221** rather than standing at 378, and `@M058`'s *earns nothing* stalemate is dissolved by a TOWER behind it (`@M060`).  Gated by `tests/c5_the_moat.loft` |
| **C6** | ✅ **DONE 2026-08-28** — [`src/damage.loft`](../src/damage.loft) § Footing, `footing_of` / `ground_footing` | S | ⚠⚠ **The ground is worth four times itself end to end: 153 / 174 / 220** (`@M061`) for the same wall on sand, grass and rock — `@M050`'s pair grown into a triple, one token apart.  ⚠⚠ **THE ROCK KINDS THIS ROW ASKS FOR DO NOT EXIST** — no granite, no sand rock, no volcanic — which is `@X112`'s *check the number exists first* and the C1 `scout` case again.  What the palette has is a SLOPE ladder (2 / 6 / 12 / 20 / 40) carried since plan 01 and **read by nothing**, exactly as `drop` was before C5: an angle of repose IS how well a material holds itself up, so C6 reads the column that exists.  ⚠⚠ **The footing is the ground AROUND the wall and PERSISTENCE decided that**: *under it* was overwritten by the paint and `MapFile` cannot remember it ([loft#876]), and a runtime layer cannot either — a wall's strength must be recoverable from the SAVED world.  The crew do not haul, so the stone comes from where they stand.  ⚠ **The STURDIEST in reach wins, for MONOTONICITY**: a mean or a minimum would make a standing wall weaken when the wall beside it broke.  ⚠ One hex of sand changes nothing (measured: **174, to the tick**) — brittleness is a property of a REGION.  ⚠⚠ **And the honest limit: the MAP chose, not the player** — it is a map-authoring lever until a map puts the good ground somewhere awkward.  Gated by `tests/c6_the_footing.loft` + the two `a-wall-cut-from-*.keys` |
| **C7** | ✅ **DONE 2026-08-28** — [`src/editor_view.loft`](../src/editor_view.loft)`::view_ppm`, `@D002` closed | XS | ⚠⚠ **The wheel now changes the picture: 0.207 / 0.052 / 0.013 of the frame at z1 / z2 / z4** (`@M062`), a quarter each doubling because a share is an AREA.  ⚠ The behavioural half is one line (`VIEW_PPM / zoom`); **the half that keeps it fixed is making `VIEW_PPM` PRIVATE** (`@X285`) — a test cannot stop the next caller reaching for a base scale that looks like the answer, and a constant it cannot NAME can.  ⚠⚠ **The fix plan said ONE place and there are FOUR** — and the fourth is not a drawing: `screen_to_hex` inverts a pointer back to a hex, so fixing only the frame would have made every click land on the wrong hex at any zoom but 1.  ⚠ **The rebaseline it warned about was an EMPTY SET**: all 16 goldens are drawn at zoom 1, so none moved and neither did any of the 827 measurements — which is also why it survived twenty-plus plans.  Gated by `tests/c7_the_zoom.loft` + `the-wheel-changes-the-view.keys` |
| **C9** | ✅ **DONE 2026-08-28** — [`src/moat.loft`](../src/moat.loft) § What a besieger shovels, [`src/flow.loft`](../src/flow.loft)`::sweep_ground` | S | ⚠⚠ **A moat is a TIMER: 130 / 174 / 221** (`@M059`) — the third of plan 27 C5's triple, one token apart, where it used to be *still standing at 378*.  ⚠⚠ **THIS ROW'S OWN MECHANISM WAS WRONG AND A PROBE SAID SO** (`@X283`): it read *a besieger's target lands nothing*, which assumes a target is NAMED — none is.  Water fails `hex_walkable`, so a moat hex is not a node in the DESIRE field either and `enemy_target` answers the besieger's own hex, so *one branch at `wave_damage`* would have been **dead code**.  The work is in the desire SWEEP, and it is the rule that sweep always followed said out loud: **an obstacle the wave can REMOVE is passable in it** — a wall (lift the climb) and now a trench (widen the node rule).  ⚠ A trench is **a wall that cannot be UNBRACED**, so the rate is the full `WALL_HP` per metre, and it is metres per DAMAGE and never per depth — which keeps the drop the timer: a `waterfall` wants 1200 ticks of one regular against a 320-tick corpus.  ⚠⚠ **AND THE ROW'S COUNTER-PLAY MEASURED AS AN OFF SWITCH**: `salvage_at` takes the WHOLE pile whenever it is smaller than one bite, so one helper in reach holds a trench open for ever — 300 ticks of siege left **0.0133 m**.  Spoil is refused there, and the counter-play is the one the trench was always for — ⚠⚠ **a tower behind it is 335 ticks and NINE of thirteen dead** (`@M060`), because a besieger has to stand at a fixed distance and dig.  Gated by `tests/c9_the_fill.loft` + `a-tower-behind-a-moat.keys` |
| **C10** | ✅ **DONE 2026-08-28** — `@D006` closed: [`src/passable.loft`](../src/passable.loft)`::hex_hoverable` / `can_hover`, and `drive_along` asks the second door | M | ⚠⚠ **The chassis FLOATS, and C5's falsified headline comes back true** (`@X286`): a hovering mover crosses flat sea for free, **falls INTO a trench** — a drop always is free — and then owes a climb out that 0.4 m has not and a 3.0 m boost has.  So *boost is the only way out of a base you have sealed* is true of trenches again, and the palette's 0-1-3-8 is priced against the boost for the first time.  ⚠⚠ **IT MOVED NOT ONE OF 833 MEASUREMENTS** (`@M063`) — this row said *not a patch* and `passable.loft` predicted `tests/11_f6` would go red; **neither held**.  It is ONE rule with two doors (`can_climb` takes a CLIMB, not a kind) and **the flow fields are built for ENEMIES only**, so the vehicle was never in one.  ⚠ And 11_f6 compares two WALKING kinds, so its warning never pointed at the hazard it named — `@M025`'s shape.  ⚠⚠ **What DID need a decision was invisible to every gate**: `steep_rock` carried `walk_vehicle: true`, so reading the column made a massif drivable and `the_gap_03`'s *the gap is the only way through for anybody* stopped being true — with no measurement able to see it.  One palette line fixes it, and it is a CORRECTION: a cliff has no HEIGHT to be stopped by until plan 02, so `walk_ground: false` was carrying the whole of *this is a cliff*.  Gated by `tests/c10_the_hover.loft` + `a-trench-you-fall-into.keys` |
| **C8** | ✅ **DONE 2026-08-26** — `CLAUDE.md`'s architecture table had a stray double-pipe | XS | The `part.loft` row merged with `catalogue.loft` **and** `part_mesh.loft`, so the table rendered **two** rows short rather than one.  Fixed while adding the `scenario.loft` row (A1).  ⚠ Pipes break a cell even inside a code span |

## D. Needs a DECISION before it can be built

⚠ Each is designed; each has one open question the owner owns.

| # | What | The open question |
|---|---|---|
| **D1** | The **permit clock** | ⚠ `@X250` — with the wallet as the only number, do the **battleships overhead** become the clock? |
| **D2** | The **landing loadout** | ⚠ `@X247` — is *"scrambler **or** two towers"* exclusive? |
| **D3** | **Helper remit** | ⚠ `@X253` — can a specialised helper be **widened again**? |
| **D4** | **Crew across servers** | ⚠ `@X164` open question — settled for rescue (`@X168`), open for trade |
| **D5** | The **starting pair** | ⚠ `@X260` makes them the game's main characters; their two profiles and voices are unwritten |

## E. MEASUREMENTS owed — the repo's own doctrine

⚠⚠ *A design is a hypothesis*, and these are claims currently resting on
argument rather than a number.

| # | What to measure | Why it matters |
|---|---|---|
| **E1** | ⚠⚠ **How fast the statistic layer rises** vs the skill layer | Load-bearing in **two** places: too slow and a veteran is not distinguishably better than a fresh hire, which reopens the churn exploit (`@X193`, `@X160`) |
| **E2** | **A scenario varying LAYOUT with defences held equal** | `PROGRESSION.md` § P7 says **none exists** — it is what would say whether the racing line is real |
| **E3** | **A scenario varying the CREW profile** | Nothing in the corpus varies it at all, and § P7 now claims a second axis |

---

## How to use this

⚠ **Pick anything.**  The groups say what an item *unblocks*, so a group
is a consequence rather than a queue.

⚠⚠ **Two things are worth knowing before choosing**, and neither forces
an order:

- ~~**A1 is small and it is what lets the game be played at all today.**~~
  **Group A is done** (A1 2026-08-26, A2 + A3 2026-08-27) — `make play
  MAP=starter_01` opens an authored base, `SCRIPT=<name>` opens any of
  the 36 scenarios, and [`docs/PLAYING.md`](../docs/PLAYING.md) says
  what every key does.
- ~~**B1 has the widest blast radius of anything on the list.**~~  **B1 is
  done** (2026-08-27) — text draws headless, so the onboarding nudge, the
  debrief, crew chatter, `SETTING.md`'s delivery and faction feedback are
  all unblocked.
- ⚠⚠ **Group B is now DONE** (B1 text, B2 building, B3 persistence, B4
  ambient life — all 2026-08-27), so `@X245`'s three things for the
  finished game's opening all exist: a world to look at, somebody to
  point at it, and somewhere to go.
- ~~**B2 (BUILDING) is the biggest missing mechanic.**~~  **B2 is done**
  (2026-08-27, [`plans/27`](27-building/README.md)) — so `@X022`, `@X024`
  and `@X019` are no longer inert, and `ROADMAP.md` § The critical path
  item **3** is closed.  ⚠ The next item on that path is **4, the
  SCRAMBLE** — the run's ending, and the mechanic the game is named
  after.  Its ingredients all shipped years of plans ago.

⚠ **Most of this is not a plan** (`README.md` § Pick the lightest
workflow that fits).  Only **B2** is clearly multi-phase; the rest are
tasks.

## See also

- [`ROADMAP.md`](ROADMAP.md) — the dependency order and the five tiers,
  including § The critical path.  ⚠ Its ordering is deliberate; this
  file's absence of one is equally deliberate.
- [`README.md`](README.md) — plan conventions, and when work earns a
  plan directory.
- [`../docs/DECISIONS.md`](../docs/DECISIONS.md) — every `@X` cited here.
- [`../PROBLEMS.md`](../PROBLEMS.md) — dryopea-internal defects.
