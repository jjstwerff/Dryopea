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
| **A4** | A short **"how to play what exists"** note | XS | ⚠ There are bases now (A1, A2), so the note can be written against something real: `make play MAP=starter_01`, what the keys do, what you will see, and what is not there yet.  ⚠ [`maps/README.md`](../maps/README.md) covers the maps half already; what is missing is the CONTROLS half |

## B. Unblocks a LOT of other work

| # | What | Size | Why |
|---|---|---|---|
| **B1** | ⚠⚠ **TEXT: a font file + `draw_text` reachable under `loft test`** | **M** | ⚠⚠ `@X130` — it blocks **five designed things**: the onboarding nudge, the debrief, crew chatter, the whole delivery of `SETTING.md`, and faction feedback.  ⚠ `@X097` is why the wallet is seven-segment rectangles.  **Nothing else in the design is close to this leverage** |
| **B2** | ⚠⚠ **BUILDING** — wall paint (Q), the order record, helper construction, the wall appearing | **H** | ⚠⚠ `ROADMAP.md` § The critical path: **the biggest missing mechanic**, and it gates three finished designs (`@X022`, `@X024`, `@X019`).  ⚠ The design is complete — `DESIGN.md` § 11 § Wall paint, § 13's *helper-seconds is the bottleneck*, and `@X252` settles who builds |
| **B3** | **Per-planet persistence** | MH | ⚠⚠ The **sole** prerequisite of five multiplayer features (`@X182`), and wanted by Tier D's own inventory and chaining anyway |
| **B4** | ⚠ **A mover for a robot going about its business** | M | ⚠⚠ **Every robot in the code is a WAVE** (`@X245`).  The opening needs ambient life, and this is the smallest slice of `ROBOT_ECONOMY.md` that provides it |

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
  the 36 scenarios.  Only A4, the controls note, is left.
- **B1 has the widest blast radius of anything on the list.**

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
