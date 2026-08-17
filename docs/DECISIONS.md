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
| `@X055` | ⚠⚠ **a wave's SIZE is SUMMED from its parts, never stored** — a wave is a flat `vector<WavePart>` keyed by wave index, so *the parts sum to the count* is the definition rather than an invariant.  ⚠ This **DELETES** `plans/23`'s own named negative control (*"a mix whose parts do not sum to the wave's count is refused at parse"*): the check only exists if the count is a second fact beside the parts, which is the shape `carry.loft` refuses.  ⚠ `wave_schedule_of(vector<integer>)` keeps its meaning — N waves of REGULARS — which is what let composition land with **569 measurements unchanged** | `plans/23` § K1, `waves.loft` § A wave is PARTS |
| `@X056` | **`schedule` arms and `compose` fills** — two `.keys` verbs rather than one grammar, because `schedule 12 12` already means *two waves of twelve* and a form that also took class names could not tell a wave boundary from a group boundary.  ⚠ `compose` REPLACES a wave, so re-running a fixture reaches the same state; and a `schedule` line AFTER a `compose` wipes it, which is why `emit.loft`'s order is a requirement | `plans/23` § K1 |
| `@X057` | ⚠ **`WaveFile` is deliberately NOT given composition** — nothing in `src/` loads it, and the shape it would need is `vector<Struct>` through a `text as`, which is the cast that hangs and miscompiles to zero entries on native.  Composition is authored in the `.keys` vocabulary, where dryopea authors everything else about a run | `plans/23` § K1, `plans/23` § Open questions 3 |
| `@X058` | ⚠⚠ **the tick's duration is no longer DERIVED, it is CHOSEN** — an enemy banks `speed × tick_seconds` and steps when a whole hex is due, so `TICK_SECONDS = 1/ENEMY_SPEED_HEX_PER_SECOND` is now what HOLDS the timestep at one regular's hex rather than what forces it.  ⚠ The expression stays because changing it moves all 569 measurements for a reason that is not a defect | `plans/23` § K2a, `spawn.loft` § What a tick is worth |
| `@X059` | ⚠ **a hex the ground refuses is SPENT, not re-banked** — the OPPOSITE of `helper_bank`, and deliberate: a helper parked at a wall keeps accumulating so its average stays a rate, but an enemy stopped by a wall is BESIEGING, and storing the hexes would let a robot that chewed a breach for ten ticks cross the base the moment it opens | `plans/23` § K2a |
| `@X060` | ⚠ **`enemy_bank` takes its SPEED as an argument**, where `helper_bank` reads a constant — because 1.5 hex/s is one of the few speeds at which the rounding guard cannot fire, so a bank that read the constant could only ever be tested at the value that hides its own guard | `plans/23` § K2a, `@M013` |
| `@X061` | ⚠ **the class lookup lives at the CALL SITE, not in the bank** — `enemy_tick` asks `enemy_speed(e.kind)` and hands the number over, so `enemy_bank` still knows only a rate.  The reason has outlived `@X060`'s: `DESIGN.md` § Speed must NOT be tied to the tick wants speed to vary *within a life* (*"a damaged robot moves slower"*), which makes the number a property of an enemy's CONDITION rather than of its class — so the one line that decides pace has to be somewhere a condition can also be read | `plans/23` § K2b, `spawn.loft` § How fast each class arrives |
| `@X062` | ⚠ **harvester and builder get NO speed row**, and that is `numbers.json`'s DELTAS shape rather than an omission: a role that lists no field takes the regular's.  Giving all four a row would invent two tunables to say *unchanged* | `plans/23` § K2b, `NUMBERS.md` |
| `@X063` | ⚠⚠ **2.25 and 3.0 hex/s were refused for a ROUNDING reason, not a design one** — both are *"quite a bit faster"* and both make `speed × tick_seconds` exact, hiding `ENEMY_PROGRESS_EPSILON` exactly as 1.5 does (`@M013`).  Picking one would have left the whole roster unable to see its own rounding.  ⚠ The general rule: **when a constant's guard is invisible at the shipped value, the next value shipped should be one that can see it** | `plans/23` § K2b, `@M013`, `@M017` |
| `@X064` | ⚠⚠ **the equal-distance SIDESTEP is refused inside plan 23 and priced instead** — `@M018` measures a mixed wave collapsing to its fastest class because only three robots can ever reach the wall, and the fix is a besieger spreading ALONG the face.  ⚠ It is a second steering rule, so it is a PLAN; and a measurement phase that changed the thing it was measuring would have measured nothing.  ⚠ The general rule: **when a measurement finds the cause of what it measured, its job is to price the fix, not to apply it** | `plans/23` § K3, `@M018`, `ENEMY_MOVEMENT.md` § The siege front is three hexes wide |
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
| `@M005` | the authored **seven-wave list plays FOUR** and falls at **320** with every tower black — a tower is 300 HP of ammunition against a 6 150 HP list.  ⚠ It was **321** from plan 16 W4 until plan 24 W1 took one tick off it (`@M020`); prose quoting 321 is quoting the older reading | 2026-08-17 | `16-W4`, `24-W1` |
| `@M006` | the same retrieval is worth **one tick** when the job is gone, and **+76 points** when it is not | 2026-08-14 / -15 | `16-W4`, `17-T3` |
| `@M007` | seven towers and **two SHUTTLING helpers clear all 205 robots**; the same two **parked** reach 5/7 and the base falls | 2026-08-15 | `17-T3` |
| `@M008` | binding a `FlowField` to a local **copies the heap value** — **2250×** the cost of reading it in place, live for four phases across 490 green tests | 2026-08-13 | `11-F8` |
| `@M009` | the crew-size spread is **123 / 135 / 138 ticks** (was 77 / 214 / 242 before the pre-walk window) — a roster buys COVERAGE, not throughput | 2026-08-15 | `14-H2`, `16-W2` |
| `@M010` | a **mirrored base is not a symmetric one** — 112 vs 211 ticks on a map that looks mirror-symmetric, until both walls were extended past the walkable band (214 vs 211) | 2026-08-14 | `14-H2` |
| `@M011` | four of a class into a sealed band breach at **23 / 35 / 50 / 96 / 454 ticks** (miner / builder / robot / harvester / scout) — **20× between the two ends**, from a mover that cannot tell them apart.  ⚠ Read at K0 as 20 / 35 / 50 / 96 / 456; the miner's and the scout's moved in `23-K2b` and **not because a rate changed** — a breach clock counts from placement, so it contains the WALK, and those are the two classes whose speed moved.  The three unmoved rows are what say the rates are untouched | 2026-08-15 (re-measured `23-K2b`) | `23-K0`, `23-K2b` |
| `@M012` | ⚠ the scout's first rate was **wrong by measurement, not by arithmetic**: 0.2 HP/s breached at **231** ticks, inside the 321 a real base lasts (`@M005`), because the sum had been done against the 100 HP BRACED figure while the siege chews the 30 HP END | 2026-08-15 | `23-K0`, `12-B3` |
| `@M013` | ⚠⚠ **1.5 hex/s is a speed at which the rounding guard CANNOT FIRE.**  Over sixty ticks of the 1/1.5 s timestep the epsilon is worth a whole hex at **1.0, 1.2, 1.8, 2.0 and 2.5** hex/s and worth **nothing** at 0.5, 0.75, 1.5, 2.25 and 3.0 — and it is worth a hex to a TENTH-length tick too.  So a class K2b gives 1.0 hex/s would lose a hex every forty | 2026-08-15 | `23-K2a`, `17-T1` |
| `@M014` | ⚠ **the guard's invisibility, measured rather than argued**: with `ENEMY_PROGRESS_EPSILON` set to 0.0, all **1128 pre-K2a tests and all 569 measurements stay green** and only the three assertions written to look for it fail.  This is what *an epsilon whose removal leaves the suite green* looks like when you check | 2026-08-15 | `23-K2a`, `17-T1` |
| `@M015` | **banking at 1× is the identity, bit for bit** — the mover was rebuilt on `speed × tick_seconds` with every number held, and the corpus did not move: **1128 → 1138 tests** (the ten new ones are K2a's own) and **30 scripts / 569 measurements unchanged** | 2026-08-15 | `23-K2a` |
| `@M016` | **the three arrival clocks**: nine hexes of identical corridor take a scout **6** ticks, a robot **9** and a miner **14** — 2.5 / 1.5 / 1.0 hex/s.  ⚠ A miner stands STILL on one tick in three and a scout takes TWO hexes on two in three, which is what a rate looks like once a tick is no longer a hex (`@X058`) | 2026-08-15 | `23-K2b` |
| `@M018` | ⚠⚠ **a wave is as dangerous as its FASTEST class, and no more.**  Twelve robots into one sealed band: **12 miner falls at 94**, *4 builder + 8 miner* at **104** (12 builder = 100), *4 robot + 8 miner* at **119** (12 robot = 115), *4 harvester + 8 miner* at **164** (12 harvester = 161), *4 scout + 8 miner* **never** (12 scout = never).  Every mix lands within **four ticks of a PURE wave of its front class** and none of them near the eight miners that are two thirds of it.  ⚠ The direction inverts the intuition — the hardest-biting class in the game, added BEHIND a weaker one, makes the wave *slower*.  ⚠ Cause: only **three** hexes of the wall are ever attacked (the approach fan's width, not the wall's — a seven-row wall does not add a fourth), an enemy blocked by a COMPANION attacks nothing (`11-F7b`), and since `23-K2b` the fastest class arrives first — so four robots own the whole front.  ⚠ It is a CLIFF: the first scout swapped into twelve miners costs the wave nothing at all, the fourth costs it the base.  ⚠ **The ROSTER order is worth nothing** — scouts first, scouts last and scouts alternated all land on the same tick, so `23-K0`'s *order is worth 20x* is now a statement about POSITION.  The fix is `@X064`.  ⚠⚠ **SUPERSEDED by `@M020`** (plan 24 W1, 2026-08-17): the cause was not the fan's width but the desire field's shape (`@M019`), and one precedence retired the headline — *4 scout + 8 miner* went from *never* to **126** | 2026-08-15 | `23-K3`, `12-B3`, `11-F7b`, `23-K2b` |
| `@M020` | ⚠⚠ **what *arriving beats queueing* is WORTH — `@M018` retired.**  Plan 24 W1's one precedence, and the same five waves of twelve: **12 miner 94** (unmoved), *4 builder + 8 miner* **104 → 101**, *4 robot* **119 → 116**, *4 harvester* **164 → 122**, *4 scout* ***never* → 126**.  ⚠ The front went **3 → 4** hexes on a five-row wall and **3 → 6** on a seven-row one, so it is the WALL's width now rather than the approach fan's.  ⚠⚠ **The new rule: a wave is worth its front class PLUS whatever the front class cannot COVER** — four screens against a five-hex face leak exactly ONE miner, and a soft screen loses 39 ticks to it while a hard one loses nothing (the builder row is the negative control).  ⚠ The cliff is still a CLIFF — the first three scouts are worth nothing and the fourth thirty-two ticks — but it no longer buys immunity.  ⚠ **The ROSTER order is still worth nothing and the test got STRONGER**: it used to compare three bases that never fell, and three zeroes are equal for any reason at all; all three now fall on the same tick.  ⚠ Corpus cost: **16 measurements moved**, `@M005` 321 → **320**, and a wider front makes most bases LAST LONGER (`a-base-on-two-fronts` 123 → 132) because a besieger that stops at the wall is not walking on to drain the wallet | 2026-08-17 | `24-W1`, `24-W2`, `@M018`, `@M019` |
| `@M019` | ⚠⚠ **the three-hex front is the DESIRE FIELD's shape, and the rule five documents name is one dryopea already has.**  K3's band, core at `(0,0)`, wall across `q = 6`: the face column `q = 7` reads **8 / 8 / 7 / 8 / 8** — a ring around the core has ONE minimum on a straight face, so only the minimum and the two hexes where the lateral step runs out have no legal closer step.  ⚠ That is **three for any wall length** — `(7,±3)` steps to `(7,±2)` exactly as `(7,±1)` steps to `(7,0)` — which is why a seven-row wall does not add a fourth.  ⚠⚠ **All five face hexes TOUCH the wall and two of them walk away from it**: an enemy attacks only when it cannot walk (`enemy_target`: *"only when NOTHING down it is legal"*), and the equal-distance sidestep offers `(7,-1)` a choice between `(7,-2)` along the face and `(8,0)` **back off it**.  ⚠ So `@X064`'s *equal-distance sidestep* is misnamed: the missing rule is a PRECEDENCE — *arriving beats queueing* — and it must land in `enemy_walk_desire` AND `enemy_target` or they disagree.  ⚠ `enemy_target` is occupancy-blind by SIGNATURE, so the rule has to be phrased *"I am beside an attackable wall"* rather than *"my closer steps are held"* | 2026-08-17 | `24-W0`, `@M018`, `23-K3`, `12-B3`, `11-F7b` |
| `@M017` | ⚠⚠ **the guard `@M014` measured as unreachable now FIRES, on BOTH gates** — with `ENEMY_PROGRESS_EPSILON` at 0.0: **7 of 1149 tests fail** and `scripts/validate.sh` goes RED (581 measurements, not 597), where the same experiment at K2a left 1128 tests and 569 measurements entirely green.  ⚠ Only **3** of the 7 are the assertions written to look for it; the other four are ordinary behaviour — a scenario round trip, a per-enemy-rate gap, a bank total and the `.keys` file itself.  ⚠ It takes **three ticks**, not sixty: 3 × 2.5 hex/s over a 1/1.5 s tick sums to **4.999999999999999**, so the fifth hex exists only because the guard releases it.  The lesson is `@X063` — an invisible guard is a defect waiting for whoever changes the number, and the cheapest fix is to ship a value that can see it | 2026-08-15 | `23-K2b`, `23-K2a`, `17-T1` |

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
