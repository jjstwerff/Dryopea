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
| **A2** | Ship **authored maps** in `maps/` | S | `make play MAP=starter_01` is documented in `main.loft`'s own header and **there is no `maps/` content**, so the option does nothing.  ⚠ A2 is the durable version of A1's shortcut — ⚠⚠ and A1 half-built it: a scenario saves to `maps/<stem>.json` on exit, so *opening one and pressing Esc writes a map*.  What is still missing is that a map keeps only the GROUND and the MARKERS — the crew, the wave list and the wallet live on the run and are not saved |
| **A3** | A **`save` verb** for `.keys`, or a scenario→map exporter | S | ⚠ Would let a scenario become a map once, rather than being replayed every launch.  ⚠ Note `emit.loft` already writes a situation *as `.keys`* — this is the other direction |
| **A4** | A short **"how to play what exists"** note | XS | ⚠ There is a base now (A1), so the note can be written against something real: `make play SCRIPT=a-base-that-plays-its-list`, what the keys do, what you will see, and what is not there yet |

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
  **Done 2026-08-26** — `make play SCRIPT=<name>` is the way in.
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
