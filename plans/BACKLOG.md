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
| **C1** | **Helper skills: `build`, `repair`, `scout`** | M | ⚠ `@X112` — each **scales a constant that already exists** (helper-seconds, the 20 s standing clock, the detection radius), so each lands alone with a gate that already measures it |
| **C2** | **Endurance pools** (`@X113`) | S | Work spends, rest restores — the crew-side of the tower's charge, and what makes quarters a building |
| **C3** | **The jammer switch** (`@X102`'s seed) | S | ⚠ Turning the core off stops the waves **and** the salvage — a decision with a downside on both sides, over a bubble that already exists |
| **C4** | **Traps that do not auto-reset** (`@X108`) | S | Place in advance, fire once, then **drive out mid-wave to re-arm** — mechanically a black tower restored by a standing vehicle |
| **C5** | **A moat** (`@X173`'s neighbour) | S | ⚠ `GROUND_TYPES.md` already carries water's **DROP** and nothing reads it; `plans/25` § M2 names that as plan 02's call |
| **C6** | **Rock kind decides wall strength** | S | ⚠ Granite sturdy / sand rock brittle — over a palette and a `structure_max_hp` that both exist.  The cheapest material rule available |
| **C7** | ⚠ **`@D002` — `cam.zoom` changes no pixel** | XS | An open defect in `PROBLEMS.md` |
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
