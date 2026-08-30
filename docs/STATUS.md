<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# What exists today

⚠ **Each plan's own `## Status` is the SOURCE OF TRUTH** and
[`plans/README.md`](../plans/README.md) indexes them.  This table is a
one-line-per-shipped-phase reading of those, in the order they landed — it is
for orientation, not for citation.  [`CLAUDE.md`](../CLAUDE.md) § Status
carries the gate numbers and the operational rules; this file carries the
history.

| What works | Plan |
|---|---|
| A hex editor: camera, palette, click/drag paint, markers, undo, save/load | [01](../plans/01-ground-editor/README.md) + [03](../plans/03-marker-layer-and-spawns/README.md) |
| Every editor action driven headlessly through ONE seam; `.keys` scripts that replay a run, photograph it and MEASURE the frame | [08](../plans/08-game-validation/README.md) |
| Pointy-top odd-r offset throughout, delegated to `hex_grid`; the axial layer is deleted | [09](../plans/09-lattice-conversion/README.md) |
| Enemies that spawn, route round walls per class, spread rather than stack, and besiege a sealed perimeter | [11](../plans/11-flow-field/README.md) |
| Rubble: a runtime layer with a source, climbable at 2.0 m, clearable back to the authored ground | [12](../plans/12-combat-resolution/README.md), B0 + B1 shipped |
| A besieged wall loses HP, breaks into a heap of masonry, and the breach is a way IN | [12](../plans/12-combat-resolution/README.md), B2 shipped |
| Enemies have HP, die, and leave a body that raises its hex — so a kill zone ramps itself shut | [12](../plans/12-combat-resolution/README.md), B4 shipped |
| A wall's HP is STRUCTURAL — an end is worth 30% of a braced hex, a lone stub 15% — and a perimeter unzips from a breach | [12](../plans/12-combat-resolution/README.md), B3 shipped |
| Towers: a third MARKER kind, range 15 by `lat_distance`, two shots every three ticks | [12](../plans/12-combat-resolution/README.md), B5a shipped |
| A tower SEES: one straight line from its eye over what `hex_height` says is in the way, and thirty shots before it goes black | [12](../plans/12-combat-resolution/README.md), B5b shipped |
| A wallet: an enemy standing on the core drains 1 pt/s off 200, and zero ends the run — the core stays invulnerable | [12](../plans/12-combat-resolution/README.md), B6 shipped |
| An unattended base falls on a measured clock — and a sealed wall nearly doubles it while a tower CUTS it | [12](../plans/12-combat-resolution/README.md), B7 shipped — plan **complete** |
| A PLAYER: a hover unit that parks, drives at two hexes a tick, and is stopped by the same height rule everything else is | [13](../plans/13-the-vehicle/README.md), V0-V1 shipped |
| A CREW: it clears rubble it stands on or beside at one body a second — and that turns a tower from a liability into an asset (95 → 121 ticks; ⚠ **128 → 140** since plan 16 W2) | [13](../plans/13-the-vehicle/README.md), V2 shipped |
| BOOST: four hexes a tick and a 3.0 m climb for three ticks, so a crew leaves a sealed base and comes home | [13](../plans/13-the-vehicle/README.md), V4 shipped |
| LOOT: clearing wreckage pays 20 points a metre, so the wallet can rise for the first time — and a crew that clears AND collects takes the towered base from 95 ticks to 145 | [13](../plans/13-the-vehicle/README.md), V3 shipped — plan **complete** |
| The player can be DESTROYED — but only by blocking a wave with nowhere to go round, which is a property of the map rather than of parking | [13](../plans/13-the-vehicle/README.md), V5 shipped |
| HELPERS: an NPC crew on the player's chassis, moving at 2.5 hex/s — the first mover whose speed does NOT fit the tick | [14](../plans/14-helpers/README.md), H0-H1 shipped |
| A helper WORKS: it clears and it earns, on one shared chassis — and a base with two fronts goes 77 → 214 → 242 ticks as the crew grows to cover them | [14](../plans/14-helpers/README.md), H2 shipped |
| A helper can be LOST: the blocker rule covers the whole crew, and a helper that dies WRECKS where it stood while the player respawns | [14](../plans/14-helpers/README.md), H3 shipped |
| CARRY: one slot per vehicle, one record per carryable thing — an object is on the ground, in exactly one carrier's slot, or spent, and a lost helper leaves something to fetch | [15](../plans/15-the-carry-model/README.md), C0-C1 shipped |
| RETRIEVAL: a lost crew member is carried to the core and rejoins the roster after EXACTLY 90 ticks — and nothing else brings one back | [15](../plans/15-the-carry-model/README.md), C2 shipped — closes [14](../plans/14-helpers/README.md) H4, so plan 14 is **complete** |
| ⚠ What a retrieval is WORTH: nothing yet — 85/79/79 ticks (⚠ **93/87/87** since plan 16 W2), because a 60 s recovery is priced against a SEVEN-wave base and dryopea plays ONE wave | [15](../plans/15-the-carry-model/README.md), C3 shipped — plan **complete** |
| WAVES ARRIVE ON THEIR OWN: an authored list, a lull that is COUNTED, and a schedule that advances on a CLEAR — so a base can be more than one wave long | [16](../plans/16-the-wave-system/README.md), W0-W1 shipped |
| PRE-WALK VISIBILITY: a wave stands 8 ticks at its marker and steps on the 9th — and because it stands INSIDE tower range, the kills pile up out there and plan 12 B7's tower inverts from -9 ticks to **+16** | [16](../plans/16-the-wave-system/README.md), W2 shipped |
| A run STARTS ITSELF: driving onto a spawn marker 12+ hexes out wakes the list — and a marker at 11, which really does send the wave, is safe to stand on | [16](../plans/16-the-wave-system/README.md), W3 shipped |
| ⚠ What a base is worth AT ITS REAL LENGTH: the authored seven-wave list plays **FOUR** and falls at 321 with every tower black, and a retrieval is worth **one tick** on a base where the crew member does come back | [16](../plans/16-the-wave-system/README.md), W4 shipped — plan **complete** |
| **No wall trigger, no ordering, no beacons and no scramble** | [plans/ROADMAP.md](../plans/ROADMAP.md) |
| UPKEEP: 20 s of standing at a black tower rebuilds it — so the lull is a REPAIR WINDOW and a base can outlive its own wave list | [17](../plans/17-tower-hot-swap/README.md), T0-T1 shipped — T2 next |
| HOT-SWAP: a tower's top is a CARRY object — take it off (the tower stops firing), transplant it onto a spent tower (red instantly), or evacuate it at the core.  ⚠ The magazine travels WITH the top, so detach-and-remount is not a free repair | [17](../plans/17-tower-hot-swap/README.md), T2 shipped |
| ⚠ **The authored SEVEN-WAVE list is playable**: seven towers and two SHUTTLING helpers clear all 205 robots.  ⚠ Parked on their towers the same two reach 5/7 and the base falls — upkeep is a POSITIONING problem, not a resource | [17](../plans/17-tower-hot-swap/README.md), T3 shipped — plan **complete** |
| THE GAME HAS A DOOR: `play.loft` owns the only call to `wave_tick`, asked by COUNT (`play_ticks`) or by DURATION (`play_advance`).  ⚠ **They are not interchangeable** — `n × TICK_SECONDS` through the accumulator is one tick SHORT for 602 of the first 1000 `n` | [19](../plans/19-the-interactive-loop/README.md), P0-P1 shipped |
| AND A KEYBOARD: WASD / Shift / E are rows in the ONE key table, and a `.keys` script presses them.  ⚠ WASD is SHARED with the camera pan — `editor_input_from(…, playing)` fills one set or the other, never both.  ⚠ W is TRUE north via a metre heading, measured at zero drift | [19](../plans/19-the-interactive-loop/README.md), P2 shipped |
| FOUR ROLES, ONE AI: scout / harvester / builder / miner — the same wave size at the same wall breaches at **23 / 35 / 50 / 96 / 454** ticks, and a harvester's body pays TRIPLE.  ⚠ `robot` keeps its rate, so **no existing measurement moved** | [23](../plans/23-the-small-robots/README.md), K0 shipped |
| A WAVE HAS COMPOSITION: `schedule 4 12` arms the list and `compose 1 4 miner 8 scout` fills a wave of it, in the order written.  ⚠ A wave's SIZE is **summed** from its parts and never stored, which DELETES the plan's own negative control (`@X055`).  ⚠ 569 measurements unchanged — a `vector<integer>` still means N waves of regulars | [23](../plans/23-the-small-robots/README.md), K1 shipped |
| SPEED IS NO LONGER THE TICK: an enemy BANKS `speed × tick_seconds` and steps when a whole hex is due, so the timestep is a CHOICE (`@X058`) — and nothing moved (1128 tests, 569 measurements).  ⚠⚠ **1.5 hex/s is a speed at which the rounding guard cannot fire**: zero the epsilon and the whole corpus stays green (`@M014`), while 1.0 / 1.2 / 1.8 / 2.0 / 2.5 hex/s each lose a hex without it (`@M013`) | [23](../plans/23-the-small-robots/README.md), K2a shipped |
| THE SCOUT IS FASTER: 2.5 hex/s against a miner's 1.0 and a robot's 1.5, so nine hexes of one corridor take **6 / 9 / 14** ticks (`@M016`) — one lookup, no new mover.  ⚠⚠ **The guard that could not fire now DOES**: 2.25 and 3.0 were refused *because* they hide it as 1.5 does, so zeroing the epsilon today turns the suite RED (`@M017`, `@X063`) | [23](../plans/23-the-small-robots/README.md), K2b shipped |
| ⚠ Composition is legible: three waves of twelve fall at **94 / 126 / never**.  ⚠⚠ Its headline — *a wave is as dangerous as its FASTEST class and no more* (`@M018`) — is **RETIRED by plan 24** | [23](../plans/23-the-small-robots/README.md), K3 shipped — plan **complete** |
| ⚠⚠ **THE SIEGE FRONT IS THE WALL'S WIDTH**: a besieger attacks the hex it is TOUCHING, so 3 → **4** hexes on a five-row wall and 3 → **6** on a seven-row one — and a wave is worth its front class PLUS what the front cannot COVER (`@M020`).  ⚠ Four screens against a five-hex face leak exactly ONE miner, so *4 scout + 8 miner* went from **never** to **126**.  ⚠ The rule five documents asked for was one we already had (`@M019`) | [24](../plans/24-the-siege-front/README.md), W0-W2 shipped — plan **complete** |
| **AND IT OPENS**: `make play`, press **P**, and waves arrive because TIME PASSED — the crew lands at the core and WASD drives it.  ⚠ **Nothing of the game is DRAWN yet** (P4), so the console echo is the only way to see it.  ⚠ The mode gates the CLOCK and never the seam | [19](../plans/19-the-interactive-loop/README.md), P3 shipped — P4 next |
| A CAMERA that comes to the vehicle: an orbit camera whose azimuth is the VELOCITY's and whose elevation and boom are the player's.  ⚠⚠ **`camera_overview` at 89° IS the editor's view** — measured against the software rasteriser at **0.0014 rad of bearing and 0.56% of scale** (`@M022`), so it is one camera with two presets.  ⚠⚠ The 3-D world frame is **+y NORTH** and `lat_to_world` is the ONE negation | [21](../plans/21-the-renderer/README.md), R1 shipped |
| AND IT EASES: the camera lives on `PlayState`, steps on every frame, and shortens its boom behind a wall.  ⚠⚠ The approach is **`1 − e^(−k·dt)`** and moros's linear `k·dt` was REFUSED — `play.loft` is frame-rate independent and the linear form is not (`@M023`).  ⚠⚠ **The ease is what makes a LATTICE look like a moving world**: un-eased the camera moves on 12 frames of 240 and jumps a whole hex, eased on 221 with a worst frame nine times smaller | [21](../plans/21-the-renderer/README.md), R2 shipped — plan **complete** |
| ⚠⚠ **THE GROUND IS NOT MESHED YET** — and the job is HALF what plan 21 sized it at.  dryopea's ground is a flat plane with pillars on it (`height_override` non-null on **2 of 12** palette kinds), so moros's corner-height MEAN is a no-op at every hex and the mesher does not blend (`@X072`); `mesh3d::mesh_to_floats` + `graphics::GroupVboSet` already publish the whole GPU-side chunk cache.  ⚠ Colour is a **UNIFORM**, one mesh per palette kind (`@X074`) — a flat-unlit frame built that way can only contain palette colours, which is what keeps the exact classification alive.  ⚠⚠ **A reversed fan changes no count, no height and no vertex position — and draws NOTHING under `GL_CULL_FACE`**, so M0 gates the winding as DATA three phases before anything is drawn | [25](../plans/25-the-terrain-mesh/README.md), M0 shipped |
| A COLUMN HAS SIDES: one vertical quad per edge where a hex stands above its neighbour, emitted **once**, by the side that STANDS.  ⚠⚠ **Both halves of `hh <= nh` fail invisibly** — no guard draws every faced edge twice and the copy is back-facing; `<` grows a zero-area sliver at every hex boundary in the world — so it is gated as four COUNTS on four fixtures (**6 / 10 / 0 / 5+6**).  ⚠⚠ A quad's NORMAL (from the two centres) and its WINDING (from the corner ring) are two facts that can disagree, and the test asserts they AGREE | [25](../plans/25-the-terrain-mesh/README.md), M1 shipped |
| THE WORLD, IN TILES: 32×32 hexes meshed one palette kind at a time, compared by folding the geometry to an integer, and the mesher's reach **measured** against `MESH_HALO_K` rather than restated — so plan 02's blend goes red here.  ⚠⚠ **The drawn region is the painted set plus a ONE-HEX RING** (`@X075`) — sea is stored as ABSENCE, so meshing what is stored leaves an erased region as a hole in the ground at the height of the land round it.  ⚠⚠ **M2's headline gate could not fail and its own control said so**: loft's keyed collections iterate in KEY order, not insertion order (`@M025`) | [25](../plans/25-the-terrain-mesh/README.md), M2 shipped |
| **AND IT IS DRAWN, AND GATED**: `make validate-gl` meshes a `.keys` world, draws it flat-unlit through real GL under `xvfb`, captures it and counts every pixel with **`classify_canvas` itself** — `other == 0` over 691 200 px.  ⚠⚠ **A per-kind COUNT cannot see a MIRRORED world** — every band stays green while the world is reflected, and only a LANDMARK against `camera_screen` sees it, at **490.8 px** (`@M027`, `@X078`).  ⚠ A landmark must be FLAT: a column draws its sides in the same colour, 29 px off | [25](../plans/25-the-terrain-mesh/README.md), M3 shipped |
| AND IT IS PRICED: a one-hex edit re-bakes **~4 000 hexes' worth** of geometry, because a tile is re-meshed whole (`@M028`).  ⚠⚠ The gate counts **FLOATS**, not seconds — the clock could not carry it (two identical calls differed **5.4x**) — and it prices M1's *invisible* breaks at last: a zero-area sliver **triples the world's geometry** and draws no pixel (`@M029`) | [25](../plans/25-the-terrain-mesh/README.md), M4 shipped — plan **complete** |
| ⚠⚠ **A RATE IS A RATE — EXCEPT THE PLAYER'S**: every banked mover holds its speed at seven tick lengths and the vehicle read **180 / 120 / 180 / 0 / 0 / 0 / 0** hexes a minute against a true 180, because `vehicle_hexes_this_tick` TRUNCATED and `Vehicle` had no carry (`@M030`, `@D003`).  ⚠⚠ Three accidents hid it, and the third is a **new shape of blind gate**: the one shortened timestep in the repo that would have caught it banks an ENEMY | [26](../plans/26-the-fixed-step/README.md), L0 shipped — fixed at L2 |
| THE CLOCK IS EXACT: simulation time is an integer count of a chosen step, so `advance(n × step) == step(n)` for **all** of 1..100000 where the float bank was wrong for **602 of the first 1000** — and the old arithmetic was **quadratic** as well as inexact.  ⚠⚠ The base unit is **1/3 µs**, because 2/3 of a second is not a whole number of microseconds and the step the plan recommended moves **17 tests** while the 654 measurements cannot see it (`@M031`, `@X079`) | [26](../plans/26-the-fixed-step/README.md), L1 shipped |
| A RATE IS EXACT TOO, and `@D003` is closed: `src/tick_bank.loft` is the ONE *do-not-lose-a-fraction* in the game, all three movers take integer base units, and **both mover epsilons are DELETED rather than zeroed** — where `@M017` measured that zeroing the float one turned 7 of 1149 tests red.  ⚠⚠ A `Bank` holds the CARRY alone (`@X080`): the rate is `@X061`'s and `whole` is a parameter, because a nested struct's silent zero-default would freeze every mover built from a partial literal.  ⚠ The one-shot TIMERS are still float seconds — L3 | [26](../plans/26-the-fixed-step/README.md), L2 shipped |
| AND SO ARE THE ONE-SHOT TIMERS, WHERE THE GUARDS TURNED OUT TO BE THE HEALTHY SITES: `src/tick_timer.loft` holds `{spent, total}` in integer base units and `timer_left` is `total − spent`, so **there are no longer two directions to disagree** and all three timer epsilons are DELETED.  ⚠⚠ Swept at seven tick lengths BEFORE converting anything, every timer WITH an epsilon was exact at all seven and the two WITHOUT one ran a tick long (`@M033`, `@D004`) — `@D003`'s shape in the other family.  ⚠⚠ And the plan's own gate (*"UP and DOWN fire on the same tick"*) could not fail: the two float directions **agree while both being wrong at four of six durations** | [26](../plans/26-the-fixed-step/README.md), L3 shipped |
| AND THE POLICIES A GAME WOULD REBUILD ARE DOORS BESIDE THE ARITHMETIC — a backlog cap, a rational rate (pause / slow-mo / fast-forward) and clock composition, **none of them consumed by dryopea** and all of them things moros or the next consumer would hand-roll (`@X083`, `@X084`).  ⚠⚠ A cap **DROPS** the excess: the version a driver writes by mistake clamps the answer and keeps the backlog, which answers fewer ticks on the stalled frame too and then runs the simulation behind the wall for ever — **4** ticks against **24** over one stream.  ⚠⚠ And the phase's own gate could not fail for the third phase running: *a 1 Hz clock driven by a 30 Hz clock* is commensurate, so **the vacuity was in the NUMBERS rather than in the assertion** (`@M034`) | [26](../plans/26-the-fixed-step/README.md), L4 shipped |
| AND THE ALPHA IS THE CLOCK'S WHILE THE POLICY IS THE RENDERER'S — `clock_alpha` ships and **none of the three draw policies does**, because their prices sit on three different axes and which one wins is a function of how long the STEP is: interpolate is smooth and **one whole step behind for ever** (2.598 m), extrapolate is exact to **9.5e-16 m** and jumps a whole step whenever the velocity changes, an ease needs no previous state and lags between them (`@M035`, `@X085`).  ⚠⚠ And the headline is about the CAMERA: an eased follow camera does not remove a lattice mover's jump, it **moves it off the world and onto the mover** — 14.9 px of ground becomes **96.1 px** of mover — so `@M023` was blind by construction, having measured the thing being eased.  ⚠ Drawing the mover at alpha takes that to 14.1 px and only a camera following the DRAWN point takes it to 0 | [26](../plans/26-the-fixed-step/README.md), L5 shipped |
| AN ENTITY IS A PART-TREE, AND WHERE ITS PIECES ARE IS COMPUTED — `src/part.loft` holds the `Socket` a part offers and the `Binding` that fills it, over `hex_body::Rig`, refusing a socket filled twice and a part that contains itself.  ⚠⚠ **It re-implements no rig at all** — `Rig`, `Joint`, `rig_world_seg` and `rig_admissible` arrive as a published dependency, and `part_fault` asks the library's doorstep first.  ⚠ **A binding carries neither a coordinate nor a pose**: move a tower and its top follows, and which pose it draws in is the simulation's to answer.  ⚠ Nothing is DRAWN yet — that is A2 (triangles) and A5 (pixels) | [20](../plans/20-entity-art/README.md), A1 shipped |
| AND EVERY ENTITY NOW HAS A SIZE THE SIMULATION AGREES WITH — `src/catalogue.loft` holds the hover unit, the robot and the tower as LIMB tables, and `part_size` DERIVES each footprint from them.  ⚠⚠ **The vehicle is 2.28 m x 2.05 m because that is where its rotors are**, not because somebody typed it: the project owner settled § D7's numbers over `numbers.json`'s on 2026-08-18, which made the vehicle WIDER than the hex it stands on — a simulation change, with `blocker_at` unchanged.  ⚠ Move one rotor 0.1 m and the gate fails naming both numbers.  ⚠ Still nothing DRAWN — that is A2 (triangles) and A5 (pixels) | [20](../plans/20-entity-art/README.md), A3 shipped |
| AND A PART IS TRIANGLES — `src/part_mesh.loft` poses every vertex through `hex_body`'s world FRAME, one per bone: a box is 12 triangles with per-face corners, a disc and a cone are four per segment, and **a quarter turn moves the canopy's far edge 1.0 m**, which is the assertion that tells TURNS from DEGREES.  ⚠ It contains no forward kinematics — `rig_world_frame3` was added upstream (`hex_body` 0.3.0) rather than re-derived.  ⚠⚠ The phase spent a session declared BLOCKED on a heap corruption a rebuild had already fixed, which `loft --version` could not reveal (`@M038`).  ⚠ Still nothing on SCREEN — that is A5 | [20](../plans/20-entity-art/README.md), A2 shipped |
| AND WHAT IS DRAWN NOW FOLLOWS THE SIMULATION — `src/pose.loft` reads the three joints the game has out of state it already keeps: a tower's top from `tower_has_top`, the canopy from `cargo_carrying` (open exactly while laden), the rotors' rate from `vehicle_speed`.  ⚠⚠ **A DISC CANNOT SHOW THAT IT IS TURNING** under the flat-unlit colour every gate rests on (`@M039`): a 12-gon at a twelfth of a turn is its own silhouette, so a rotor is now two crossed BLADES — and the footprint is unchanged to the last decimal.  ⚠⚠ And `socket_world` was PLANAR while the one socket the game has is **6 m up**, which A3's own gate could not see because it asked a different function for the height.  ⚠ Still nothing on SCREEN — that is A5 | [20](../plans/20-entity-art/README.md), A4 shipped |
| AND THE ROSTER IS ON THE SCREEN — `src/entity_view.loft` walks a `WaveState` into triangles and `src/entity_gl.loft` draws them through the ground's own flat-unlit shader; `scripts/validate_gl.sh` photographs a base with a player, a crew member, four robots of three classes and two towers standing in it.  ⚠⚠ **An entity colour is deliberately NOT a palette colour** (`@X092`), so the gate's claim is total and exact: `unknown - entity pixels == 0` — every pixel is a palette colour, an entity colour or the clear colour.  ⚠⚠ It found **half of every box wound INWARDS** (`@D005`), invisible to counts, vertices, normals and `mesh_crc` alike, and it moved three of `PROXY_ART.md`'s colours — one that was the WALL's red to the bit, and two the player could not tell from `waterfall` and `rock` (`@M040`).  ⚠ **Nothing is drawn in the WINDOW yet** — `make play` is unchanged, exactly as plan 25 M3 left the ground | [20](../plans/20-entity-art/README.md), A5 shipped |
| AND THE WINDOW DRAWS IT — `make play`, press **P**, and the map editor becomes the GAME: the ground as triangles, the roster as part-trees, through the session's own eased follow camera.  ⚠⚠ **The renderer works out for itself what moved** (`@X095`): a live session changes the terrain three ways that never go near `paint`, so `MeshWatch` keeps a SNAPSHOT of the height layer and diffs it — exact only because *every terrain change a tick can make moves the height layer*, which is asserted against a played base rather than quoted.  ⚠⚠ **The tile is now 8x8**, settling plan 25 M4's deferred decision on the measurement it could not make: **96 tiles draw as fast as 8**, so twelve times the draw calls is free and a one-hex terrain change went from 54-112 ms to a constant **7 ms** (`@X096`, `@M041`).  ⚠ And a GL state leak that drew a **black window** on the second P press now has a gate of its own | [19](../plans/19-the-interactive-loop/README.md), P6 shipped |
| AND THE CORNER SHOWS THE WALLET — one number, in seven-segment digits over the 3-D frame, ramping AMBER to RED as it drains (`@X098`), and `docs/DESIGN.md` § HUD says that is the WHOLE HUD: no wave counter, no countdown, no minimap, no boost bar.  ⚠⚠ **The digits are RECTANGLES and that is forced** (`@X097`): `graphics::draw_text` is `#native` and needs a font file, so a text HUD is one no test and no `snap` could see.  ⚠ A wrong row in the segment table draws a good-looking digit that reads the wrong number, so the gate is an INDEPENDENT ORACLE — the lit-segment counts 6 2 5 5 4 5 6 3 7 6 — plus two GL cases for the two ways a transparent overlay fails silently, each fired against its own break (`@M042`).  ⚠⚠ **A ramp is not a colour, it is 201** — and the sweep of them caught one that was exactly the SCOUT's, at 134 points, which the GL gate would have counted as a robot (`@M043`) | [19](../plans/19-the-interactive-loop/README.md), P7 shipped |
| AND THERE IS A BASE TO PLAY — `make play SCRIPT=a-base-that-plays-its-list`, and any of the **50 `.keys` files** in `tests/scripts/` + `tests/gl/` opens as a live session: the ground, the walls, the towers, the crew and the armed wave list, with the clock stopped.  `maps/` was empty and `.keys` had no `save` verb, so until this landed `make play` opened a paint brush and there was no base to press P on.  ⚠⚠ **A scenario opens as its PREFIX**, cut at its first `tick` / `fall` (`@X263`) — its QUESTION, not its answer, since a scenario's tail is a fallen base or an exhausted list.  ⚠ The claim that `tick` and `fall` are the whole advancing set is gated by a sweep asserting **t0 over every `.keys` file in the tree**, paired with what those positions AUTHORED because an empty one also reads t0 — and both halves fire (`@M044`).  ⚠⚠ **`script=<name>`, never `--script`**: loft strips a leading `--` argument as its own, so the flag form opened a MAP of that name, silently (`@X264`) | [BACKLOG A1](../plans/BACKLOG.md), shipped 2026-08-26 |
| AND THREE OF THEM ARE AUTHORED AND SHIPPED — `make play MAP=starter_01`.  `maps/` was empty for twenty-five plans while `main.loft`'s own header documented the option, so it did nothing; it now holds three bases, each teaching one MEASURED thing (`@M045`): **starter_01** where to stand (**+61 ticks and a whole wave** for parking between its towers), **crossroads_02** that parking is the wrong answer (**shuttling t250 against t222** holding one lane), and **the_gap_03** terrain instead of masonry (**wave 5**, the strongest base the repo can build).  ⚠⚠ **The `.keys` is the SOURCE and the `.json` is BUILT** (`@X265`), both committed, `make maps` rebuilds — a map is ~460 ground entries and a diff over one says nothing a reviewer can act on.  ⚠⚠ **`map_fault` refuses a map nobody could play** (`@X266`): the silent failure is a map that loads, draws, lands the crew and never sends a wave, because no spawn is 12+ hexes from the core to poke.  ⚠ Two negatives came out of authoring them — **the funnel's width is worth 2 ticks in 233** because the drain saturates at the core's footprint, and **two towers are the worst of one, two and four** because kills leave bodies and bodies are terrain | [BACKLOG A2](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THERE IS A NOTE THAT SAYS HOW TO PLAY IT — [`docs/PLAYING.md`](PLAYING.md): every key, what happens without pressing one (repair, salvage — most of this game has no key), and an honest list of what is not there yet.  ⚠⚠ **It is GATED against the key table** by `tests/a4_the_controls.loft` (`@X267`), and that is not belt-and-braces: `main.loft`'s hand-kept copy had been **off by one for twenty-five plans**, claiming *5=grass, 9=wall* when key `5` paints **sand** and `wall` is on `0` (`@M046`).  ⚠⚠ Two of the five breaks exposed the GATE rather than the note — a bare substring search called an action documented because the word appeared in prose — so the check is a name **in backticks on a table row**.  ⚠ The entry's header now POINTS instead of restating | [BACKLOG A4](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND IT CAN DRAW WORDS — `assets/DejaVuSans-Bold.ttf` ships and [`src/font.loft`](../src/font.loft) is the ONE seam to `graphics::draw_text`.  ⚠⚠ **Half the blocker was STALE**: *`draw_text` answers "native function not loaded" under `loft test`* had aged against a binary that moved under it — measured, it draws **1324 pixels** on a headless `Canvas` with the pinned `graphics` 0.5.2 (`@M047`).  The missing font was the real half.  ⚠⚠ **The seam owns an ABSOLUTE path**, because `gl_load_font` reads a relative one against the process CWD in 0.5.2 and against `source_dir()` in 0.8.0 — so `loft install graphics` would have stopped the font loading **silently**, and silently is the whole problem: a null handle collapses to **0**, a live handle, so a font that failed to load draws **the wrong typeface** rather than nothing.  ⚠ `loaded` is a second field for one fact on purpose — under [loft#914] a partial literal FAILS CLOSED that way.  ⚠⚠ **`@X130`'s five blocked features are unblocked**; what they still owe is COMPOSITING, since 774 of a glyph line's 1324 pixels are BLENDED and `classify_canvas` looks colours up exactly | [BACKLOG B1](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THE PLAYER CAN BUILD — press **Q** in play mode and every hex you drive over is ordered as a wall; the crew raise it at 10 s a hex, and driving over an outline again rubs it out until somebody has started on it.  ⚠⚠ **Measured worth: +44 ticks on a base that otherwise falls at 130** (`@M050`), from a scenario pair ONE token apart — and the wave CHEWED THROUGH the wall rather than going round, which is what makes that a defence.  ⚠⚠ **The measurement caught a defect before it shipped**: the trail toggles every hex ENTERED and you never enter the one you are standing on, so a five-row band kept its first row open — and `plans/12` § B7 had already priced a wall with one gate at **+1 tick**.  ⚠⚠ **`@X095` is RETIRED on the way** (`@X269`): building is a terrain change that moves the painted layer and not the height layer, so the renderer now notices any change to the ground it draws.  ⚠⚠ **AND THE WALLET BUYS SOMETHING AT LAST** (`@M051`): press **E** at the core and 100 of the opening 200 points becomes a tower beacon — carry it out, plant it, and the crew raise a tower over 30 s.  Points had been earned since `13-V3` and drained since `12-B6` and bought nothing.  ⚠ A tower order and a wall order are ONE queue with a discriminant (`@X273`), and the fourth branch between them was found by PROBING rather than by reading the design: a tower order must refuse erasure, or the wall trail destroys a paid-for beacon (`@X274`) | [plan 27](../plans/27-building/README.md) C0-C5, shipped 2026-08-27 |
| AND A PLANET REMEMBERS IT — `make play PLANET=kepler` opens `dryopea_planets/<planet>/<player>/world.json`, so a wall you built is there when you come back (BACKLOG B3, `@X275`).  ⚠ Keyed by planet AND player from the first day though dryopea has one player (`@X188`), because a shared world cannot be retrofitted into a path with no room for a name.  ⚠ A planet holds the GROUND and the MARKERS — the pair a MAP holds — and a RUN is not in it.  ⚠⚠ **The mmap destination was probed and DEFERRED on measured grounds** (`@M052`): `store_persist_bind` shipped and round-trips across processes, but dryopea's world is a FIELD of `EditorState`, so a bind writes the EDITOR's store — the undo history rides along, and any new editor field silently invalidates every saved world.  `ROADMAP.md`'s *"one-line annotation"* is falsified and now says so | [BACKLOG B3](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THE WORLD HAS TRAFFIC ON IT — `traffic <rate>` puts robots on a road, and they walk their heading across the map and go on their way (BACKLOG B4, `@X276`).  ⚠⚠ **It needed no mover and no second AI**: `enemy_walk_heading` had walked a robot along its business since plan 11 F5b and the fiction already said a spawn marker's direction is *what they were going before* — what was missing was that a business never ENDED and nothing ever STARTED one the wave schedule had not.  ⚠⚠ **The bubble is the whole mechanic**: enter it and a robot loses its errand, one way, so **the same traffic is scenery or a wave** — 200 of 200 points past a road that runs by, drained by one that runs through (`@M053`).  ⚠ And that gate passed on TIMING until `a-road-that-passes-by.keys` caught it at 136.67 points gone: *a road far away that leads here is not a road that passes by* | [BACKLOG B4](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THE CREW ARE BETTER OR WORSE AT THINGS — `build`, `repair` and `scout` per helper (BACKLOG C1, `@X278`).  ⚠ **A wall goes from 15 ticks to 10 at level 5**, and a trained mechanic brings a black tower back sooner, while the PLAYER's own repair stays a flat 30 — the crew get better at working, nobody gets better at flying (`@X119`).  ⚠⚠ **Two of the three scaled a constant that existed and the third did not**: there is no detection radius anywhere in the code, which is the case `@X112` itself says to push back on.  The owner ruled what it is (`@X277`) — **two radii that INTERACT, and the SUBJECT owns half**, so a trained scout notices *quieter* things rather than seeing further, and an INTERNAL (brain, motors, weapons) is the quietest thing in the design.  ⚠ The curve is CHOSEN and says so; level 0 is bit-for-bit the old game, so 687 measurements did not move | [BACKLOG C1](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THEY GET TIRED — endurance is spent by work and restored continuously, and a tired crew member works LESS rather than stopping (BACKLOG C2, `@X279`).  ⚠⚠ **Rest is CONTINUOUS**, which is what lets salvage, build and repair each spend independently with nothing to co-ordinate; the alternative needs a per-tick scratch flag on a person.  ⚠⚠ **Skill makes you PRODUCTIVE, not TIRELESS** — a site tires by the RAW elapsed, so a trained builder gets more done for the same tiredness.  ⚠ **You tire from work you actually DID**, and the gate caught the version that did not: spending inside the composed door charged every helper for repairing nothing, **1.67x a tick's worth per tick**.  ⚠ Sized from the design's own sortie (`@M055`) — ~675 ticks to tire against a corpus whose longest base is **321** — so 687 measurements did not move, and the arithmetic is asserted so a longer base goes red first | [BACKLOG C2](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND THE PLAYER CAN TURN THE CORE OFF — the jammer switch (BACKLOG C3, `@X280`): `jammer on` / `jammer off` in a scenario, key **J** at the core in a live session.  ⚠⚠ **It stops the waves AND the salvage**, which is a decision with a downside on both sides — the core IS the jammer, so it is why there are waves and why there is anything to salvage, and the switch turns both off in one act.  ⚠⚠ **But it stops the SUPPLY, never the SIEGE**: losing an errand is one way, so everything already cut off is still walking to the core and the SCRAMBLE is still the answer to being overrun.  ⚠ **Worth two waves and sixty points** (`@M056`), and `a-core-switched-off.keys` is `a-base-that-plays-its-list.keys` plus one line — four waves down to **one**, finishing on exactly 200.0 points with ten points of wreckage lying uncollected.  ⚠ The salvage stops as INCOME and never as WORK: the heap still clears, you are just working for free | [BACKLOG C3](../plans/BACKLOG.md), shipped 2026-08-27 |
| AND A TRAP CAN BE BURIED ON THE APPROACH — a plate that fires ONCE and stays dead until somebody drives out to it (BACKLOG C4, `@X108`, `@X281`): the fourth marker kind, `flag <q> <r> trap`, and **no key at all** — re-arming is a POSITION, like clearing rubble and rebuilding a tower.  ⚠⚠ **The trigger is a CROSSING, not a standing position**: a scout strides two hexes in two ticks out of three, so a plate asking who is standing on it is walked over by the one class fast enough to matter — and no test using a regular robot could have seen it.  ⚠⚠ **What it is worth is the TRIP, not the blast** (`@M057`): one blast nobody goes back for is **−3 ticks**, because the bodies it drops are a permanent terrain change; the same plate re-armed three times is **+106**, and the control says it is not the crew member standing there.  ⚠ **One hex decides it**, failing two different ways — in the gate the crew member is wrecked servicing it, two hexes out it never gets through its own gate at all | [BACKLOG C4](../plans/BACKLOG.md), shipped 2026-08-27 |

⚠⚠ **AND THE GROUND DECIDES WHAT A WALL IS WORTH** (BACKLOG C6,
[`src/damage.loft`](../src/damage.loft) § Footing, `@X284`).  A wall is
as strong as the ground it was cut from: `structure_max_hp` is the
kind's figure scaled by **bracing and footing**, where bracing is what
holds a wall up sideways and footing is what holds it up from below.
The same wall falls at **153 / 174 / 220** on sand, grass and rock
(`@M061`) — the ground is worth four times itself end to end.

⚠⚠ **The rock kinds the design names do not exist, and did not need
to.**  There is no granite, no sand rock and no volcanic rock in the
palette; what it carries is a **slope** ladder — sand 2, grass 6, hill
12, rock 20, steep_rock 40 — read by nothing since plan 01, exactly as
`drop` was before C5.  An angle of repose *is* how well a material
holds itself up.

⚠ The footing is the **sturdiest ground the wall touches**, not the
ground under it: that was overwritten by the paint, and a wall's
strength has to be recoverable from the SAVED world.  ⚠ A maximum is
what keeps it monotone — a mean would make a standing wall get weaker
when the wall beside it broke — so **one hex of sand changes nothing**
and brittleness is a property of a REGION.  ⚠ **The map chooses, not
the player**, until a map puts the good ground somewhere awkward.

⚠⚠ **AND THE WHEEL CHANGES THE PICTURE** (BACKLOG C7, `@D002` closed,
`@X285`).  `cam.zoom` had been moved by the wheel, saved, and reported
in `snap`'s state line since plan 01 while **no file that drew anything
read it** — the picture was identical at `z1` and at `z6`.  It is
`VIEW_PPM / zoom` now, and a grass patch covers **0.207 / 0.052 /
0.013** of the frame at z1 / z2 / z4 (`@M062`) — a quarter each
doubling, because a share is an area.

⚠ **The fix plan said one place and there are four**, and the fourth is
not a drawing: `screen_to_hex` inverts a pointer back to a hex, so
fixing only the frame would have made every click land on the wrong hex
at any zoom but 1.  ⚠ **`VIEW_PPM` is now PRIVATE**, which is the half
that keeps it fixed — a test cannot stop the next caller reaching for a
base scale that looks like the answer.  ⚠ The rebaseline the defect
warned about was an EMPTY SET: all 16 goldens are drawn at zoom 1.

⚠⚠ **AND THE CREW FIND THEIR OWN WORK** ([`plans/29`](../plans/29-the-crews-own-work/README.md)
O1, `src/task.loft`, `@X295`).  `DESIGN.md` § 9 calls assignment a
PILLAR and until this there was none: `helper_drive`'s only caller in the
tree was the `.keys` script runner, so **a crew member four hexes from a
wall order stood there for sixty ticks with not one unit of work in it**,
and all four jobs they can do reach exactly one hex.  A crew member
nobody has told anything now takes the nearest of the four **inside their
own senses** — 3 hexes untrained, 4 at scout 5.

⚠⚠ **The radius is the load-bearing number and it was measured twice.**
Asked as the full `detect_sees`, a heap's own `notice_of_heap` widened it
to six hexes and **18 tests across 8 files moved one way**: bases that
used to fall stood for 800 ticks with 0 enemies alive, and a wallet that
ended at 40 ended at 299 of 300.  ⚠ `@X277`'s two radii answer *is this
worth driving to*, which is the player's question; the crew's is *what is
under my nose*.  ⚠⚠ **And a default that absorbs the work § 9 says growth
is supposed to CREATE deletes the table the pillar stands on.**

⚠ **`helper_drive` is an ORDER and the search does not overrule one**
(`@X296`) — a verb that says *go here* has to be honoured or the `.keys`
vocabulary lies.  ⚠ No key sends a crew member to a hex, so in a played
session nobody is ever ordered and every crew member is semi-automatic.

⚠⚠ **AND YOU CAN TELL SOMEBODY WHAT TO DO** ([`plans/29`](../plans/29-the-crews-own-work/README.md)
O3, `@X297`).  Press **G** beside a crew member and their remit moves one
step round a cycle — **anything → build → clear → repair → re-arm →
anything**.  ⚠⚠ **A remit trades BREADTH for REACH**: they will do the
one job only, and they will cross the whole map to find it.  ⚠ The cycle
is what makes widening cost what narrowing cost — a trip — which answers
`@X289`'s RESET-or-STEP by construction.

⚠⚠ **The pillar comes out intact as ARITHMETIC** (`@M069`, `@M070`):

| | work NEAR the crew | work FAR from the crew |
|---|---|---|
| **nobody told them anything** | 174 | **140** |
| **told, one trip each** | 174 | **174** |

The default buys **+44** in the left column and nothing in the right; an
order buys **+34** in the right and nothing in the left.  ⚠ **Neither
dominates**, which is `DESIGN.md` § 9's *does this make ONE axis
dominate?* answered with numbers instead of an argument.

⚠⚠ **AND A ROBOT CAN HAVE A ROUTINE — five anchors, and the BAG steers**
([`plans/30`](../plans/30-the-mob-routine/README.md) R1, `@M073`,
`@X333`, `@X334`).  `Errand { role, home, work, alt, carry, slip }` sits
on every `Enemy`, and where it is going is **DERIVED** from those anchors
every time it is asked — there is no waypoint list, so a deviation costs
nothing to resume.

⚠⚠ **The bag steers and never a clock**, and that is one measurement
rather than a preference: the shipped `haul` row closes its round trip at
**4, 40 and 400 hexes — three bags each**, while a row one column away
with a period gets **13 hexes out and delivers nothing, for ever**.  That
is `../crawler`'s own measured defect reproduced in dryopea's code, and
`@X298` makes dryopea's version worse — a route crosses a 1.5 km cell, so
any period you could pick is shorter than the trip.

⚠ **A role is a ROW and the table is INDEXED, never compared** (`@X333`)
— `tests/30_r1_the_errand.loft` sweeps every `src/*.loft` and fails on a
code line comparing a `ROLE_` constant, because crawler has `role == 7`
in eight places and no compiler can refuse one.  ⚠⚠ **And the bubble's
one-way door is enforced in the READER**: `errand_role` answers
`ROLE_NONE` for any robot whose link `wave_cutoff` has cut, so a cut-off
hauler cannot go on running a cycle it has lost and there is no second
write to forget.

⚠⚠ **AND THE CYCLE IS EVALUABLE AT ANY MOMENT WITHOUT STEPPING TO IT**
([`plans/30`](../plans/30-the-mob-routine/README.md) R2, `@M074`,
`@X335`).  `cycle_at` is one modulo, O(legs) to find the leg and one
index — so an un-tracked mob is **computed** rather than approximated,
and `plans/22`'s LOD refusal needs no exception.  ⚠ **0 hexes and 0 legs
of 8 920 swept moments disagree with a stepped body**, over four speeds ×
three step lengths, on a world whose leg bends: **15 steps against a
straight line of 12**.

⚠⚠ **A leg boundary in TIME lands inside a step and one in DISTANCE
cannot** — a guard's clock reads **8 of 60 moments adrift** at a step its
period does not divide while the bag holds at **0 of 60** over the same
step, which is a second and independent reason for *the bag steers*, and
why a clock period that is not a whole number of ticks is **refused at
construction**.

⚠ **It is INERT and that is the phase**: nothing in the tick reads any of
it, `ROLE_NONE` is 0, and every `Enemy { … }` literal in the suite goes
on meaning a robot with no cycle at all.  The mover is R3.

⚠⚠ **AND THE MOB MOVES — ONE DOOR, AND `slip` IS WHAT A DEVIATION
COSTS** ([`plans/30`](../plans/30-the-mob-routine/README.md) R3 + Rc,
`@M075`, `@X336`, `@X337`).  `errand_step` descends a field toward the
anchor the mob's BAG names, steps BESIDE a companion and stands for the
ground — F7b's own rule with the destination changed — and **nothing
else may write a cycling mob's position**, which collapses four of the
twelve sites that could otherwise forget the bookkeeping.  ⚠ A mob whose
one strictly-closer step is held by the player arrives at **t = 22 000 000
against 20 000 000 and carries exactly 2 000 000 of slip**, and a blocker
that is not in the way changes nothing.

⚠⚠ **The claim splits in two, and only the PHASE half is total**
(`@X336`): a mob whose first choice is taken walks a **different route of
the same length and loses no time at all**, so hex equality holds where
nothing can push a body — **0 hexes, 0 phases and 0 legs of 320
mob-ticks** — while the field distance to the anchor equals the rule's
under every deviation.  That is `@FR-E-Slip`'s *re-converges on the same
hex* read exactly, and it is R6's own correction arriving three phases
early.

⚠⚠ **And a DWELL is not a BLOCK** (`@X337`): charging `slip` for the
ticks a guard stands at its post freezes its clock for ever **with every
conformance count green**, because a frozen rule agrees with a frozen
body.  ***Conformance is an equality between two things that can stop
together, so liveness has to be asked separately*** — and two of the six
gates now do.  ⚠ Still INERT: no scenario has a routine, so
`errand_fields` builds nothing and the **920 measurements did not move**.

⚠⚠ **AND A ROUND HAS AN ENDING** ([`plans/30`](../plans/30-the-mob-routine/README.md)
R4, 2026-08-28) — a gatherer takes rounds for a 120 s **shift** and then
**leaves the roster at its nest**, at 384 000 000 units against a
360 000 000 shift, with the wallet unmoved and nothing dropped
(`@M076`).  ⚠⚠ **Home is a LEG of the round and never a place a finished
mob walks to** (`@X338`): the plan's invariant is *three states and ONE
exit*, so a mob breaking off its cycle would be a fourth state and a
second exit.  ⚠⚠ **And the departure moved to the TOP of the tick** —
removed at the consequence stage a robot arrives at its nest and is gone
inside one tick, so the last frame that holds it has it **one hex
short**: *what the player cannot see the gate cannot see either*.
⚠ `@D008` came out of the same fixture: `errand_fields` built one field
per DESTINATION where it needed one per ANCHOR, and the lost hexes went
to `slip` with **every conformance count green** — ***`slip` is a
currency that can pay for a defect***.  ⚠ Still INERT: **920
measurements unmoved**.

⚠⚠ **AND A ROUND CAN END SOMEWHERE IT DOES NOT PASS**
([`plans/30`](../plans/30-the-mob-routine/README.md) R4b, 2026-08-29,
`@X341`) — the world's commonest robot dumps its load in one place and
is serviced in another (`@X339`, `@X340`), so its round never touches
home and R4's ending could not reach it.  A **terminal leg** closes it:
the working legs repeat until the shift, then ONE walk to the repair
point and off the roster.  ⚠⚠ **The turn stays closed-form** —
`T = ceil(S / period) × period` — **and the BODY reaches it without
knowing what `T` is**: the mover has no cycle, but the moment its bag
empties at the drop-off IS the turn, so the ending is a third value of
the bag (`ERRAND_BAG_HOMEWARD`) rather than a second clock.  ⚠⚠ **And
it is compared in HEXES, never in time** (`@M077`): 12 of 192 swept
cases disagree and the failure is a whole round.  ⚠ Eight mutations,
seven caught, and the fixture's LEG is what catches the mover
(`@M078`).  ⚠ Still INERT: **920 measurements unmoved**.

## Two rules the table above rests on

⚠ **A robot climbs 2.0 m** (`CLIMB_REGULAR`, plan 12 B1), and the number
is derived rather than picked: **a single-hex body ramp onto a structure
`H` high needs a climb of `H / 2`**, so half a 3 m `wall` is 1.5 and 2.0
is the interior of four constraints — see `src/passable.loft` § Why a
robot climbs 2.0 m.  It was 0.0 until B1, which meant no rubble height a
robot could walk onto existed at all.

⚠ **Rubble is a LAYER, never a repaint** (`src/height.loft`).  A pile
makes its hex's SURFACE `rubble` (palette 11) while the authored ground
underneath is untouched, so clearing restores exactly what was authored.
That is what dissolves the sea trap: the painted layer is sea-default, so
a breach that ERASED its hex would be *less* passable than the wall it
replaced, while "the wall broke" asserted true.

⚠⚠ **A MOAT IS A HEX THE CREW DUG, AND ITS DEPTH IS THE PALETTE'S OWN
`drop`** (BACKLOG C5, [`src/moat.loft`](../src/moat.loft), `@X282`).
Press **F** and every hex you drive over is ordered as a trench, at a
wall's 10 s each; the crew dig it, and it becomes `water` — 1 m below
the ground around it, which is the first time anything read the drop
`examples/palette.json` has carried since plan 01.

⚠⚠ **The depth decides ONE thing: how much it takes to FILL.**  A pile
is a surface only once it clears the water
([`src/passable.loft`](../src/passable.loft) § A PILE UNDER THE
WATERLINE), so `water`'s metre swallows exactly two bodies.  On land the
threshold is 0.0, so it is the old `rise > 0` everywhere anybody has
ever painted — 745 gate measurements did not move.

⚠⚠ **AND THE CHASSIS FLOATS** (BACKLOG C10, `@D006` closed, `@X286`).
`walk_vehicle` had been read by NOTHING since plan 01, so the player and
the crew were stopped by flat sea exactly as a robot is.  `drive_along`
asks `can_hover` now, and **BACKLOG C5's falsified headline comes back
true**: a hovering mover crosses flat sea for free, **falls INTO a
trench** — a drop always is free — and then owes a climb out that 0.4 m
of clearance has not and a 3.0 m boost has.  ⚠ So *boost is the only way
out of a base you have sealed* is true of trenches again, and the crew,
who share the chassis and have no boost, are in one for the run.

⚠⚠ **It moved not one of 833 measurements** (`@M063`), where `@D006`
called itself *not a patch*: it is ONE rule with two doors, and the flow
fields are built for ENEMIES only.  ⚠ What DID need deciding was
invisible to every gate — `steep_rock.walk_vehicle` went FALSE, because
a 0.4 m clearance does not clear a cliff and a cliff has no HEIGHT to be
stopped by until plan 02.

⚠⚠ **AND BESIEGERS SHOVEL IT SHUT, SO IT IS A TIMER** (BACKLOG C9,
`@X283`).  A wave stopped at a trench fills it in at the rate it would
otherwise chew a wall, priced at the full `WALL_HP` per metre — *a
trench is a wall that cannot be UNBRACED* — so the triple now reads
**130 / 174 / 221** (`@M059`) where C5 measured it *still standing at
378*.  ⚠⚠ **The desire SWEEP is where that lives, not `wave_damage`**:
a moat hex was not a node in it, so no besieger ever named one, and the
rule that came out is *an obstacle the wave can REMOVE is passable in
the desire field*.  ⚠⚠ **And what a trench is FOR turned out to be the
kill zone rather than the barrier**: a besieger has to stand at a fixed
distance and dig, so a tower behind one is **335 ticks and nine of
thirteen dead** (`@M060`) — `@M058`'s *a wave that cannot reach you
cannot die*, inverted.  ⚠ `salvage_at` refuses SPOIL, because a clearer
takes the whole pile whenever it is smaller than one bite and one helper
in reach would hold a trench open for ever (`@M059`).

**`plans/30` R5 — a PLACE owns its mobs, and its reach is a region you
can ask about before anything moves** (2026-08-29).  `src/poi.loft`: a
`Poi { kind, q, r, state, since }`, a population that is a set of ROUTES
rather than a list of BODIES, and the BOUND — which turns *could this
ever be in this window?* from `N` queries into ONE.  ⚠⚠ **The claim is
per-LEG and the union is only the QUERY** (`@X342`): a union is slack,
and against it a radius one hex short, a terminal leg with no disc, an
off-by-one rim and a sidestep that breaks `@FR-E-Non-Increasing` all read
green — four of nine mutations, and the four that matter (`@M080`).
⚠⚠ **And the probe chose the fixture**: on an authored map the bound
covers **1466 of 1467** standable hexes, because a round that crosses its
patch has legs as long as the patch — so it is not a filter for the POIs
a base lands among, and what it excludes is the world OFF the patch,
which dryopea has not got (`@M079`).  ⚠ Still INERT: nothing builds a
`PoiWorld`, and the 920 gate measurements did not move.

**`plans/30` R6a — a mob nobody can see costs one integer, and giving it
a body back changes nothing** (2026-08-29).  `src/poi.loft`:
`PoiMob { route, seat, slip, gone }` and the three tiers.  ⚠⚠ **Only
`slip` accumulates** (`@X343`) — the round is an index, the phase a
derived SEAT, the hex `cycle_at`, and the BAG and the BANK are derived,
because only a BODY can be pushed.  ⚠⚠ **The bank is the one nobody would
think of**: a fresh body carries nothing where the rule is part-way
through a hex, so it releases its next hex late by exactly `cycle_carry`
— and at 1.5 hex/s that is always ZERO, so the shipped robot cannot see
it (`@M014`'s class, a fifth instance).  ⚠ Ten mutations, nine caught,
and **both survivors were faults in the GATE**: a test comparing
`poi_bound_from` with `poi_bound`, which *is* `poi_bound_from` over
`poi_cycles` — ***a wrapper is a golden of its own delegate*** — and a
redundancy whose two conditions cannot both hold (`@M081`).  ⚠ Still
INERT: nothing builds a `PoiWorld`, and the 920 gate measurements did not
move.

**`plans/30` R6b — the game materialises its own mobs, and two window
sizes give one answer** (2026-08-29).  `WaveState.pois` carries the
places, `PlayState.reach` one integer apiece, and `poi_step` runs in
`play.loft` before `wave_tick`.  ⚠⚠ **A POI's whole bound folds to ONE
integer** (`@X344`), so the tick's per-POI question is
`lat_distance(poi, player) <= reach + window` — one subtraction, no world
read, no round built.  ⚠⚠ **And the pair holds**: identical where nothing
can push a body, differing by exactly `slip` where something can — which
is `@X299`'s claim in the narrower form that survives it, with the
boundary's one-hex collar stated rather than assumed.  ⚠ Eleven
mutations, seven caught, and **all four survivors were things the gate
could not see** (`@M082`) — headed by ***a saving is not a behaviour***:
deleting the cull changed no position and made no extra body, so
`poi_step` grew a fourth answer and the far run reads **0 against 160**.
⚠ Still INERT: nothing builds a `PoiWorld`, and the 920 gate measurements
did not move.

**`plans/30` R7a — a PLACE is four verbs in a `.keys` file** (2026-08-29,
`@X345`, `@M083`, `@D009`).  `poi` / `route` / `mob` say what a PLACE
issues and `routine` says what one BODY holds — and neither is derivable
from the other once a tick has run, because `errand_arrive` flips the bag
and `errand_step` adds to `slip` without asking any place.  ⚠ **A route
ENLISTS its own population** (`poi_enlist`), so a file says *how many*
and never *which ones*; that works only because a record is never
removed.  ⚠⚠ `routine` is the first command in the vocabulary carrying
THREE hexes, so `KeysSchema` grew a third pair position.  ⚠⚠ **Twelve
mutations, twelve caught — and all THREE real defects came from a gate
laid earlier** (`@M083`): the FLAG is a second fact (an ambient robot is
`errand: true` with `ROLE_NONE`), **`slip` is the one duration that may
be NEGATIVE** while the authoring door refuses one on purpose (`@D009`),
and **R1's `src/` sweep caught both new verbs comparing a role** —
`errand_role_named` cannot say no, so `errand_role_known` is what a
refusal asks.  ⚠ **No longer inert**:
`tests/scripts/a-place-that-sends-robots.keys` authors a face, a depot
and two haulers and places no robot — the game does — and it is the
first fixture in which `slip` is VISIBLE.  Gates **51 scripts / 932
measurements**.

**`plans/30` R7b — a hauler turns for what you left** (2026-08-29,
`@X346`, `@M084`).  `@FR-E-Built-Not-Seen` in code: a heap the player
left on a hauler's route is picked up and carried off, and the scenario
pair reads **230.0 against 200.0** of a 200-point wallet.  ⚠⚠ **The
finding is the accounting**: a hex walked AWAY from the anchor costs
**two** — the one it spent and the one it owes — because a detour is
paid both ways and the return leg is indistinguishable from progress.
Then the rule and the body agree in DISTANCE at every moment of the
detour **with nothing remembered**, which is what `@FR-E-Place-State`
requires.  ⚠⚠ **The BAG is untouched**: the stolen heap is a
`CarryObject` keyed on `BLOCKER_MOB + PoiMob index` — an identity that
outlives a body — so killing the thief gives it back, as the same stuff.
⚠ The negative control is in the fixture's own opening half: the player
in plain sight for thirty ticks, and the robot on its rule's hex to the
tick.  ⚠ Thirteen mutations, twelve caught; the survivor was the load's
MATERIAL.  Gates **53 scripts / 956 measurements**.

**`plans/30` R8 — a robot road is worth 146 ticks** (2026-08-29,
`@M085`), and it CLOSES the plan.  `a-base-on-a-robot-road.keys` falls in
**123** ticks against **269** for `a-base-beside-a-robot-road.keys` —
the same map, wall, waves and painted road, differing in the four
coordinates that put the round inside the scrambler bubble or 36 hexes
out of it.  Four haulers nobody sent at the player lose their link
crossing it and join the siege: seven alive against three.  ⚠⚠ **And the
sweep is the finding**: the road is worth 196 / 146 / 10 / **0** ticks
against waves of 2 / 3 / 5 / 8, because the siege front is the wall's
WIDTH (`@M020`) and a wave of eight already saturates it — *the first
version of the pair ran the authored 5 + 8 and read 118 against 118 with
four extra besiegers plainly on the map.*  ⚠ `@X303`'s test is answered
the right way: the routine makes behaviour more believable **and** moves
the clock.  Gates **55 scripts / 974 measurements**.

**`plans/31` N1-N4 — a base opens with what the last one carried**
(2026-08-29, `@X347`, `@M086`).  `ROADMAP.md` item **7**: `plans/28` built
`manifest_of` and `@M068` measured it, and for a plan and a half nothing read
it.  `Wallet` gains a `carried`, `wallet_budget` is the one door the budget is
asked at, `wallet_carrying` is the only place a ratio could ever be applied,
and `scramble.loft::manifest_opens` is where the manifest's three columns are
sorted into the one that crosses and the two that do not.  ⚠⚠ **The field is
spelled as the CARRY and never as the BUDGET** — [loft#914] takes an omitted
field's default silently, so a defaulted `budget` would open every base
already FALLEN while a defaulted `carried` reproduces the game that shipped.
⚠⚠ **Counting the re-assertion sites before writing any code found a fourth
nothing could have seen**: `hud.loft::hud_ink_for` read the BASELINE for the
wallet ramp's span while clamping above it, so a base opened with 200 carried
would have sat at the full colour from 400 points down to 200 — the ramp dead
for half the run on the one number the game shows — with `tests/19_p7`'s
exhaustive colour sweep still green, because *it sweeps the span it is given*.
⚠ Measured: **four beacons where the first base bought two** (`@M086`), so
`@M065`'s landing exclusion moves with the wallet — and **the carry
COMPOUNDS** (`@M087`): **230.0 against 106.0** on the same base with the same
100 points left when the ferry ends, so a hundred points of extra towers came
back as a hundred and twenty-four.  ⚠⚠ **The first version of that pair read
198 against 198** — `fall` waits on `wallet_broke`, and the wallet is what
BUYS towers as well as what enemies drain, so a base that spends its budget on
defence is broke before a robot arrives.  ⚠ The sweep is the finding: +124
points at the authored wave, **standing against falling** from 26 robots to
50, +11 ticks past 115, then nothing once the wave saturates the siege front.
Gates **1790 green / 145 files**, **58 scripts / 1011 measurements**.

**`plans/32` L0-L4 — where the base goes stops being AUTHORED and becomes a
DECISION** (2026-08-29, `@M091`).  `ROADMAP.md` item **6**, and the step
§ THE SESSION IS THE GAP NOW put first because `plans/31` made a run possible
and **nothing in the game could reach it**.  `src/landing.loft` turns a pick,
a map and a seed into a landed base — core moved, free starter tower down,
two crew out — and `land <q> <r> [seed]` is a `.keys` verb.  ⚠⚠ **A landing
MOVES the authored core rather than creating one** (`@FR-L-Map-Stays-Valid`):
`map_fault` validates a map AGAINST its core, so a map stays playable as
authored, `make maps` keeps refusing a map nobody could play, and the
authored core is simply where the rocket lands if the player does not choose
— *a map cannot be checked against a pick nobody has made yet*.  ⚠⚠ **Three
of `DESIGN.md` § 15's eight steps needed no code and saying so was half the
plan**: the core's six faces do not exist (`@X294`), close-spawn silencing
has read the core's CURRENT hex since plan 16, and step 8's wallet shipped as
`wallet_carrying`.  ⚠ **Random is a HASH of position** (`@FR-W-Position-Hash`,
given its **first code** by a plan that is not worldgen's) — dryopea has no
RNG and this plan introduced none.  ⚠⚠ **Both of the plan's own defects were
one rule written twice**: L0's probe restated `landing_ground_ok` with a disc
of ONE where the real door needs THREE, reporting 210 / 395 / 196 landable
hexes against a truth of **80 / 159 / 71**, and L1's search bound was derived
cleanly from `LANDING_PICK_EDGE_BUFFER` but measured from the **hashed
start**, itself 3 hexes out, so 3 + 5 = 8 reached past the very buffer the
derivation rested on.  ⚠ Only **14-30 %** of an authored map takes a landing,
dominated by the CLEARANCE and not the spawn rule, and `crossroads_02` loses
nothing only because its two markers sit at opposite ends — **so the content
rule is *spawns at opposite ends***.  ⚠⚠ **AND IT CLOSED ON A PROBLEM RATHER
THAN A NUMBER** (`@M091`): the pick is worth **58 ticks** across the band and
the SEED alone is worth **59** — ***the dice are worth as much as the
decision***, so `@X317`'s *land in the overlap* cannot be felt.  The cause is
§ 15 step 6's **random direction** for the free starter tower, not the
touchdown; aiming it at the nearest live spawn is one line and contradicts
§ 15 as written, so it is left OPEN as the owner's ruling.
⚠⚠ **AND A SORTIE IS PRICED — `EXPLORATION.md` § The order of work phase 1,
2026-08-30, `@M092`, with NO CODE WRITTEN.**  `a-find-nobody-fetched.keys` /
`a-find-fetched-on-the-way.keys` / `a-find-fetched-late.keys`: a stranded crew
member authored at (14, 0) on the road to the spawn marker, and the only
difference between the three is when the player picks them up — **248 never
fetched, 322 taken in passing, 364 fetched at wave three**.  ⚠⚠ **So a
sortie PAYS and `@X024` is inverted**: the find is worth more LATE, and it buys
that with **half the life** — ~80 ticks alive for +74 delivered early against
~40 for +116 delivered late, because a body is *spent by being used* and what it
is worth is the **pressure it stands against**, not the time it stands.  ⚠⚠ **And four of eight swept fetch ticks FAILED** — the
player is killed carrying the wreck and the carry is cleared — so the late
fetch's price is a RISK rather than a discount.  ⚠ The BUILD half of `@X024`
cannot be priced at all while `@X022`'s recon window is free and unlimited,
which makes the permit (`ROADMAP.md` item 8) the missing INCENTIVE rather than
a follow-on.
⚠⚠ **AND THE LAYOUT IS WORTH SOMETHING — `plans/33` E0, the same page's
phase 2, 2026-08-30, `@M093`, and again with NO CODE WRITTEN.**
`a-base-drawn-in-tight.keys` / `a-base-drawn-out-wide.keys` and their two
no-tower nulls: eight wall hexes, two towers, two crew, a ramping list and a
24-hex spawn-to-core walk held identical in all four, and only how far from the
core the defence sits moves — **170 tight against 207 wide, +37 ticks and
+22 %**, so `PROGRESSION.md` § P7's racing line has a number under it at last.
⚠⚠ **And the NULL is the finding**: the same nine hexes with the towers
deleted read **102 against 102, to the tick** — ***the layout is not the wall;
it is how far from the core the FIGHT happens, and a base with nothing that
shoots has no such distance to sell***.  ⚠ All four cells burn **all 30 shots
each**, so the sixty shots are identical and only their geometry moved.
⚠⚠ **AND THE SWEPT FACTORIAL FOUND SOMETHING BIGGER** (`@M094`): with the walls
and the towers deleted in turn the same two bases read **neither 95 / 95, wall
only 102 / 102, towers only 250 / 324, wall AND towers 170 / 207** — ***a wall
standing in front of a tower costs more than it buys***, −80 tight and −117
wide, and `plans/12` B7's DEAD RAMP is falsified as the reason by a `pile` sweep
that reads 0.0 at all thirteen hexes of the approach.  ⚠ SIGHT is the leading
hypothesis and it is not gated.

⚠⚠ **AND A FIND IS A CARGO ROW, AND ANYTHING ON THE GROUND CAN BE SEEN —
`plans/33` E1, 2026-08-30, `@X349`, and this one IS code.**
`object <q> <r> find <points> <owner>` is a `.keys` verb and delivering one at
the core credits the wallet by exactly `<points>`, so `plans/33` E2 sweeps a
number rather than rebuilding a mechanism.  ⚠⚠ **And the visibility question
is what decided the marker row**: `entity_view.loft` is *the ROSTER, as
triangles* and a `CarryObject` was never in it, so a beacon set down and a
tower top on the ground were INVISIBLE — one catalogue row and one walk fixed
all three, against ~96 files for a marker kind.  ⚠ The walk filters on
`CARGO_GONE` and nothing else, so the frame's count and `cargo_count` are one
number by construction.

⚠⚠ **AND E2 PAID THE OTHER HALF OF `@M092`'S RULE — `plans/33` E2,
2026-08-30, `@M095`, and no code again.**  One base, a hundred-point find:
**209 never cashed, 239 cashed in passing, 245 cashed at tick 100**, and
**flat across seven late fetch ticks** — so ***what decays is the find, never
the opportunity***.  ⚠⚠ **And the fourth column inverted the phase**: the same
hundred points as a TOWER reads **199, minus ten** — it fires a full 30-shot
magazine and its base is **poorer, not weaker** (73.3 points against 106.0),
because upkeep is paid out of the crew time that produces the wallet's only
income.  ⚠ More hands make it worse (−37 / −91 / −96), and `plans/12` B7's
dead ramp is falsified as the reason for the second time in one plan.

⚠⚠ **AND TAKING A FIND OPENS A SPAWN SOURCE — `plans/33` E3, 2026-08-30,
`@X350`, and this one IS code.**  `EXPLORATION.md` § X5 built as written:
`wave_taken` plants a spawn marker on the hex the find lay on, and `spawn_wave`
already round-robins across the active ones, so **a find never makes a wave
bigger**.  ⚠⚠ **And it measures BACKWARDS** (`@M096`): **209 / 240 / 211** —
a third source is worth **+31 ticks TO THE PLAYER** on the front the tower is
on and **+2** on the front it is not, so ***a new spawn source is worth
something only where something can shoot at it***.  ⚠ § X5 needs a ruling
rather than a tune, and it is named rather than taken.

Gates **1852 green / 152 files**, **78 scripts / 1383 measurements**.
