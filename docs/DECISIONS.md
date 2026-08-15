<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# DECISIONS — the greppable index of what was decided and what was measured

⚠ **This file is an INDEX, never a source.**  Every row points at the document
that owns the decision; the row is one line and the reasoning stays where it
was written.  A second copy of a decision is the copy that drifts.

## The code scheme

Four namespaces, one letter each, three digits, **never reused** — the same
convention `PROBLEMS.md` already uses for `@D001`.

| code | is | index | grep |
|---|---|---|---|
| `@X###` | a **design decision** | this file § Decisions | `grep -rn '@X014' docs/ plans/` |
| `@M###` | a **measurement of record** — a number other documents quote | this file § Measurements | `grep -rn '@M001' .` |
| `@D###` | a **dryopea defect** | [`PROBLEMS.md`](../PROBLEMS.md) | `grep -rn '@D002' .` |
| `@P###` | a **loft** issue (upstream) | loft's own tracker | — |

### ⚠⚠ Plan phases are NOT unique, and must be written `<plan>-<phase>`

A bare phase code is ambiguous today and grep cannot resolve it:

| bare code | plans that use it |
|---|---|
| `S0` | **18** (scenario capture) and **22** (the field cache) |
| `C2` | **09** (lattice conversion) and **15** (the carry model) |
| `V0` | **08**, **13** and **19** |
| `R0` | **20** and **21** |

⚠ So cite a phase as **`19-P3`**, **`22-S0`**, **`12-B7`** — plan number, hyphen,
phase.  That form is unique, greppable, and reads the same as the prose already
does (*"plan 12 B7"*).  ⚠ Existing prose that says "plan 12 § B7" is fine and is
not being rewritten; what matters is that a **code written on its own** carries
its plan.

### Categories

Every `@X` carries one, so the index can be filtered:

| category | what it governs |
|---|---|
| `world` | the lattice, terrain, heights, passability |
| `sim` | the tick, movement, damage, waves, cost |
| `render` | cameras, geometry, frames, art |
| `play` | the loop, input, what the player does |
| `run` | the shape of a session and a run |
| `method` | how the project builds and gates things |

---

## Decisions

### render — entity art ([`PARTS.md`](PARTS.md))

| code | decision | owner |
|---|---|---|
| `@X001` | ⚠ **REVISED 2026-08-15** — ~~a dryopea-native part model~~ → **reuse `hex_body`** (published: rigs, revolute joints with limits, pure poses, per-bone OBBs, canonical text) and **enhance it** with an out-of-plan hinge axis.  ⚠ The first draft read `hex_part` (unpublished) and never checked the registry.  ⚠ `hex_part` itself stays out — trigger unchanged | `PARTS.md` § D1 |
| `@X002` | ⚠ **REVISED 2026-08-15** — ~~box/disc primitives~~ → **hex CELLS WITH HEIGHTS**, authored at the part's own scale.  ⚠ The roundness objection dissolves on `@X044`'s corner smoothing; boxes would have forfeited six published libraries | `PARTS.md` § D2 |
| `@X003` | **the tower's socket is the SIMULATION's** — plan `17-T2`'s detachable top already is moros's socket model, so what is drawn asks `TowerState` and never a second flag | `PARTS.md` § D3 |
| `@X004` | a part emits **GEOMETRY**; there is no sprite and no baked scale.  ⚠ Replaced an earlier sprite design — a sprite sheet does not degrade under a free camera, it *lies* | `PARTS.md` § D4 |
| `@X005` | a **PNG is an ARTEFACT**, never a runtime input.  ⚠ `imaging::Pixel` has no alpha, but a captured frame is opaque, so the hole does not bite | `PARTS.md` § D5 |
| `@X006` | **the durable artefact is the SIZE**, and a test compares a part's footprint to the simulation's constant | `PARTS.md` § D6 |
| `@X007` | the **hover unit**: fixed base, four booms, front rotors r 0.36, **rear r 0.52**, canopy on a rear lateral hinge `0 → 0.30` turns | `PARTS.md` § D7 |
| `@X044`-`@X047` | the terrain mesh's construction, copied from moros — corner heights are a MEAN; **one predicate** (`faced_between`) both stops the blend and draws the wall; emit a face only from the side that stands; the mesh tile is not the store tile | [`RENDERER.md`](RENDERER.md) § R3 |
| `@X048` | ⚠⚠ **a cell is a COLUMN OF LAYERS**, each with an absolute height — that is where vertical detail comes from.  ⚠ Parts get columns now; the TERRAIN keeps one surface until something must walk UNDER something | `PARTS.md` § D2 |
| `@X051` | **BRIDGES span HOLES, not walkable ground** — `DESIGN.md` makes a cavern a *"hole in the surface map, a non-walkable kind the way sea already is"*, so a span is just a hex at height and ONE surface still answers.  ⚠ The layer trigger fires only if a bridge must cross walkable ground; that is a MOVEMENT change (`Hex` → `(Hex, layer)`), not an art one.  ⚠⚠ A span *"cannot be walled and cannot be flanked"* — terrain that invalidates the learned optimum `@M004` teaches | `PARTS.md` § D2 |
| `@X052` | ⚠⚠ **base bridges robots walk UNDER fire `@X048`'s trigger** — two walkable surfaces over one hex, so the flow field's node becomes `(Hex, layer)` and `hex_height` / `can_step` / occupancy / 1 094 tests are all in scope.  ⚠ **Undesigned, and flagged as such by the project owner.**  `DESIGN.md` already names the feature (`cy`-layer decks, second-phase).  ⚠ Three seeds: a tower above its own body ramp, a crew route the wave does not share, somewhere to park that is not in the way | `PARTS.md` § D2 |
| `@X049` | a **PLACEMENT layer** — where another form MAY go (`SOCK`/`FITS`, `hex_fit`'s doorstep).  ⚠ A refusal must NAME its restriction, because an off-grid value is otherwise **silently snapped**.  ⚠ dryopea has one already: the tower socket | `PARTS.md` § D2 |
| `@X050` | **attachment with LIMITS** — `hex_body`'s `Joint {value, lo, hi}` plus `joint_fits` / `joint_offer` (clamped, not refused) / `joint_residual` (how far past), and `rig_admissible` for a whole pose | `PARTS.md` § D2 |
| `@X038`-`@X043` | the build pipeline — a script in, a `.glb` and validation frames out.  ⚠ `mesh3d` + `glb` are published, so the emitter is a MAPPING; the glb is a baked POSE not a rig; validation is measured **and** a cold read | `PARTS.md` § D9 |
| `@X008` | the **crew is the same part** as the player (*"same chassis"*), and the four robot classes are **one part with per-class DATA** | `PARTS.md` § D8 |

### render — the camera and the pipeline ([`RENDERER.md`](RENDERER.md))

| code | decision | owner |
|---|---|---|
| `@X009` | the camera is **moros's `RenderCamera`**, ported not depended on.  ⚠⚠ `camera_overview` at elevation **89° IS dryopea's editor view**, so it is one camera with two presets | `RENDERER.md` § R1 |
| `@X010` | **geometry is shared, rasterisation is not** — GL for the game, the software rasteriser keeps the editor and its 520 measurements.  ⚠ Trigger to collapse: GL reproduces the top-down view | `RENDERER.md` § R2 |
| `@X011` | the **terrain becomes 3-D**, meshed per dirty chunk.  ⚠ The largest single item; probably its own plan | `RENDERER.md` § R3 |
| `@X012` | the gate chain is **xvfb → GL → `gl_screenshot` → `imaging::png` → exact classification** | `RENDERER.md` § R4, `@M002` |
| `@X013` | the gate renders **FLAT UNLIT** — shading turns one palette colour into a range and `unknown` stops meaning *fault*.  ⚠ Never loosen to nearest-colour | `RENDERER.md` § R4 |
| `@X014` | the game's camera lives on **`PlayState`**, never `EditorState.cam` | `RENDERER.md` § Open 4 |
| `@X015` | **no indoor camera modes** — moros's SNUG / CUTAWAY / EYES answer an interior dryopea's roofless bases do not have | `RENDERER.md` § R1 |

### play — progression ([`PROGRESSION.md`](PROGRESSION.md))

| code | decision | owner |
|---|---|---|
| `@X016` | **the progression is SKILL, not stats** — it lives in the player's hands, and passes the genre test in its purest form (there are no stats to resolve into) | `PROGRESSION.md` § G1 |
| `@X017` | ⚠⚠ **the player's vehicle must not get faster.**  The moment speed is a purchase, skill stops separating a good run from a bad one | `PROGRESSION.md` § G1 |
| `@X018` | the **landscape is the school** and the fiction already made it safe — both non-human tiers open as maintenance | `PROGRESSION.md` § G2 |
| `@X019` | the **base layout is the exam**, and a racing line exists because the measured-best defence is one only a good pilot can live in | `PROGRESSION.md` § G3, `@M004` |

### run — exploration and the session clock ([`EXPLORATION.md`](EXPLORATION.md))

| code | decision | owner |
|---|---|---|
| `@X020` | **exploration IS scouting** — already *the* progression activity; this design assembles rather than adds a pillar | `EXPLORATION.md` banner |
| `@X021` | **the run already opens with a sortie** (`wave_provoke_step`), so "explore earlier" is CONTENT on a trip the player already takes | `EXPLORATION.md` § X2 |
| `@X022` | ⚠⚠ **the game WAITS** — an unlimited recon window the player ends deliberately by poking a marker | `EXPLORATION.md` § X2b |
| `@X023` | **the four robot classes gate the intel layer** — until they exist every wave is the same wave and a sortie predicts nothing.  ⚠ One row each in `numbers.json` + one branch in the damage-to-wall lookup.  ⚠ **HALF DONE** — the wall-damage axis shipped as `23-K0` and was exactly that cheap; the SPEED axis is `23-K2` and is not a row (see `@X053`) | `EXPLORATION.md` § X2b, `plans/23` |
| `@X053` | ⚠ **the harvester's role is what it CARRIES** — its body is `RUBBLE_CARGO` and cargo pays **3×**, so the one hole `DESIGN.md` § Small robots left open is filled by `body_source` → `loot_rate`, two lookups that already composed.  ⚠ Chosen against the genre test: at 90 points a body, letting one through and collecting it later is a real alternative to killing it at the wall | owner, 2026-08-15; `plans/23` § Open questions 1 |
| `@X054` | ⚠ **`robot` is not retired and is not one of the four** — the small robots APPEND as kinds 3-6 and the baseline keeps 1.0 HP/s, which is what let four classes land without moving one of 1 094 tests or 520 measurements | `plans/23` § K0 |
| `@X024` | **a find is a BUILD ACCELERANT**, and what decays is *the opportunity to use it* — so it must be found early | `EXPLORATION.md` § X2c, `@M006` |
| `@X025` | **the PERMIT is the run clock**, and the cordon's teeth are at the exit.  ⚠ Expiry costs the **CARGO**, never the run — § 14 has no fail screen | `EXPLORATION.md` § X2d |
| `@X026` | a find is **one marker row + one cargo row**, and nothing else.  ⚠ A find that needs a subsystem has become an economy | `EXPLORATION.md` § X4 |
| `@X027` | **every find opens a fight**, and the fight is a wave that already exists — no new mover | `EXPLORATION.md` § X5 |
| `@X028` | **the first find is a stranded helper** — it needs no code at all, and it is a build accelerant whose value compounds | `EXPLORATION.md` § X7 |
| `@X029` | **what persists is INTEL** — a per-node `found` flag, not revealed pixels.  ⚠ No fog-of-war layer | `EXPLORATION.md` § X8 |

### sim — cost and scale ([`plans/22`](../plans/22-the-field-cache/README.md))

| code | decision | owner |
|---|---|---|
| `@X030` | the field is **cached by EPOCH**, and the epoch is bumped **inside** the two mutators (`height_raise` / `height_clear`) — never at a call site | `22-S1` |
| `@X031` | the sweep is **bounded by the ROSTER**, by **PATH** distance never straight-line — the bubble is a sphere and a route can be far longer | `22-S3` |
| `@X032` | **invalidate on EFFECT, not on EVENT** — the field changes only if an edge flipped | `22-S4` |
| `@X033` | ⚠⚠ **simulation granularity must NOT follow the CAMERA.**  If it does, where the player looks changes the outcome.  The boundary is the interaction radii | `plans/22` § What this plan does NOT build |
| `@X034` | **incremental field repair stays deferred** — an incrementally wrong field routes enemies through a wall.  ⚠ Its equality gate is already written and green | `22-S5`, `11-F8` |

### method

| code | decision | owner |
|---|---|---|
| `@X035` | **one caller of `wave_tick`** — asked by COUNT (`play_ticks`) or DURATION (`play_advance`), never one folded into the other | `19-P1`, `@M003` |
| `@X036` | the **mode gates the CLOCK, never the seam** — `in_playing` says what keys mean, `PlayState.playing` says whether time reaches the sim | `19-P3` |
| `@X037` | **probes are kept when their answer is load-bearing** — [`probe/`](../probe/README.md), with their own `loft.toml` so an experiment does not become a dependency | `probe/README.md` |

---

## Measurements

⚠⚠ **A measurement AGES, and the stale one gets quoted** — `CLAUDE.md`
§ Profiling records that *"58% is `canvas()`"* was cited three plans after it
stopped being true.  **Every row here carries a date.  Quote the code, not the
number, and re-measure before optimising.**

| code | measurement | date | source |
|---|---|---|---|
| `@M001` | **`flow_sweep` and the lookups under it are ~75%** of the suite's interpreted time; `classify_canvas` is 7.5% | 2026-08-15 | [`PROFILING.md`](PROFILING.md) |
| `@M002` | a GL frame captured under `xvfb` and decoded classifies with **`other` = 0** over 76 800 px — zero colour drift | 2026-08-15 | [`probe/r0`](../probe/r0/), `RENDERER.md` § R0 |
| `@M003` | `n × TICK_SECONDS` through an accumulator is **one tick short for 602 of the first 1000 `n`** — and an exact product does not save it (`n = 12` gives exactly 8.0 and answers 11) | 2026-08-15 | `19-P1` |
| `@M004` | a **sealed** wall nearly **doubles** the fall clock; a wall with a **GATE buys nothing**; a tower is **+16 ticks** since the pre-walk window (69 / 112 / 128) | 2026-08-15 | `12-B7`, `16-W2` |
| `@M005` | the authored **seven-wave list plays FOUR** and falls at **321** with every tower black — a tower is 300 HP of ammunition against a 6 150 HP list | 2026-08-14 | `16-W4` |
| `@M006` | the same retrieval is worth **one tick** when the job is gone, and **+76 points** when it is not | 2026-08-14 / -15 | `16-W4`, `17-T3` |
| `@M007` | seven towers and **two SHUTTLING helpers clear all 205 robots**; the same two **parked** reach 5/7 and the base falls | 2026-08-15 | `17-T3` |
| `@M008` | binding a `FlowField` to a local **copies the heap value** — **2250×** the cost of reading it in place, live for four phases across 490 green tests | 2026-08-13 | `11-F8` |
| `@M009` | the crew-size spread is **123 / 135 / 138 ticks** (was 77 / 214 / 242 before the pre-walk window) — a roster buys COVERAGE, not throughput | 2026-08-15 | `14-H2`, `16-W2` |
| `@M010` | a **mirrored base is not a symmetric one** — 112 vs 211 ticks on a map that looks mirror-symmetric, until both walls were extended past the walkable band (214 vs 211) | 2026-08-14 | `14-H2` |
| `@M011` | four of a class into a sealed band breach at **20 / 35 / 50 / 96 / 456 ticks** (miner / builder / robot / harvester / scout) — **23× between the two ends**, from a mover that cannot tell them apart | 2026-08-15 | `23-K0` |
| `@M012` | ⚠ the scout's first rate was **wrong by measurement, not by arithmetic**: 0.2 HP/s breached at **231** ticks, inside the 321 a real base lasts (`@M005`), because the sum had been done against the 100 HP BRACED figure while the siege chews the 30 HP END | 2026-08-15 | `23-K0`, `12-B3` |

---

## How to use this

```bash
grep -rn '@X025' .          # everywhere the permit decision is cited
grep -rn '@M001' .          # everywhere the 75% figure is quoted
grep -rn '@X0[0-9][0-9]' docs/ | wc -l
```

⚠ **Adding a row is cheap and removing one is not.**  A code is permanent even
after the decision is reversed — a reversed decision keeps its code and gains a
`⚠ SUPERSEDED by @X###` line, exactly as `PROBLEMS.md` keeps `@D001` after the
fix.  That is what makes an old commit message still resolvable.

⚠ **This index does not cover the older docs yet** — `DESIGN.md`, `SETTING.md`,
`ENEMY_MOVEMENT.md`, `ROBOT_ECONOMY.md` and `GROUND_TYPES.md` carry many
decisions with no codes.  Backfilling them is worth doing when something needs
to cite one, not as a sweep: a code nobody references is a code nobody
maintains.
