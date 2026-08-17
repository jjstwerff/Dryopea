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

**Active implementation.**  Each plan's own `## Status` is the source of
truth and [`plans/README.md`](plans/README.md) indexes them; this is what
exists today.

| What works | Plan |
|---|---|
| A hex editor: camera, palette, click/drag paint, markers, undo, save/load | [01](plans/01-ground-editor/README.md) + [03](plans/03-marker-layer-and-spawns/README.md) |
| Every editor action driven headlessly through ONE seam; `.keys` scripts that replay a run, photograph it and MEASURE the frame | [08](plans/08-game-validation/README.md) |
| Pointy-top odd-r offset throughout, delegated to `hex_grid`; the axial layer is deleted | [09](plans/09-lattice-conversion/README.md) |
| Enemies that spawn, route round walls per class, spread rather than stack, and besiege a sealed perimeter | [11](plans/11-flow-field/README.md) |
| Rubble: a runtime layer with a source, climbable at 2.0 m, clearable back to the authored ground | [12](plans/12-combat-resolution/README.md), B0 + B1 shipped |
| A besieged wall loses HP, breaks into a heap of masonry, and the breach is a way IN | [12](plans/12-combat-resolution/README.md), B2 shipped |
| Enemies have HP, die, and leave a body that raises its hex — so a kill zone ramps itself shut | [12](plans/12-combat-resolution/README.md), B4 shipped |
| A wall's HP is STRUCTURAL — an end is worth 30% of a braced hex, a lone stub 15% — and a perimeter unzips from a breach | [12](plans/12-combat-resolution/README.md), B3 shipped |
| Towers: a third MARKER kind, range 15 by `lat_distance`, two shots every three ticks | [12](plans/12-combat-resolution/README.md), B5a shipped |
| A tower SEES: one straight line from its eye over what `hex_height` says is in the way, and thirty shots before it goes black | [12](plans/12-combat-resolution/README.md), B5b shipped |
| A wallet: an enemy standing on the core drains 1 pt/s off 200, and zero ends the run — the core stays invulnerable | [12](plans/12-combat-resolution/README.md), B6 shipped |
| An unattended base falls on a measured clock — and a sealed wall nearly doubles it while a tower CUTS it | [12](plans/12-combat-resolution/README.md), B7 shipped — plan **complete** |
| A PLAYER: a hover unit that parks, drives at two hexes a tick, and is stopped by the same height rule everything else is | [13](plans/13-the-vehicle/README.md), V0-V1 shipped |
| A CREW: it clears rubble it stands on or beside at one body a second — and that turns a tower from a liability into an asset (95 → 121 ticks; ⚠ **128 → 140** since plan 16 W2) | [13](plans/13-the-vehicle/README.md), V2 shipped |
| BOOST: four hexes a tick and a 3.0 m climb for three ticks, so a crew leaves a sealed base and comes home | [13](plans/13-the-vehicle/README.md), V4 shipped |
| LOOT: clearing wreckage pays 20 points a metre, so the wallet can rise for the first time — and a crew that clears AND collects takes the towered base from 95 ticks to 145 | [13](plans/13-the-vehicle/README.md), V3 shipped — plan **complete** |
| The player can be DESTROYED — but only by blocking a wave with nowhere to go round, which is a property of the map rather than of parking | [13](plans/13-the-vehicle/README.md), V5 shipped |
| HELPERS: an NPC crew on the player's chassis, moving at 2.5 hex/s — the first mover whose speed does NOT fit the tick | [14](plans/14-helpers/README.md), H0-H1 shipped |
| A helper WORKS: it clears and it earns, on one shared chassis — and a base with two fronts goes 77 → 214 → 242 ticks as the crew grows to cover them | [14](plans/14-helpers/README.md), H2 shipped |
| A helper can be LOST: the blocker rule covers the whole crew, and a helper that dies WRECKS where it stood while the player respawns | [14](plans/14-helpers/README.md), H3 shipped |
| CARRY: one slot per vehicle, one record per carryable thing — an object is on the ground, in exactly one carrier's slot, or spent, and a lost helper leaves something to fetch | [15](plans/15-the-carry-model/README.md), C0-C1 shipped |
| RETRIEVAL: a lost crew member is carried to the core and rejoins the roster after EXACTLY 90 ticks — and nothing else brings one back | [15](plans/15-the-carry-model/README.md), C2 shipped — closes [14](plans/14-helpers/README.md) H4, so plan 14 is **complete** |
| ⚠ What a retrieval is WORTH: nothing yet — 85/79/79 ticks (⚠ **93/87/87** since plan 16 W2), because a 60 s recovery is priced against a SEVEN-wave base and dryopea plays ONE wave | [15](plans/15-the-carry-model/README.md), C3 shipped — plan **complete** |
| WAVES ARRIVE ON THEIR OWN: an authored list, a lull that is COUNTED, and a schedule that advances on a CLEAR — so a base can be more than one wave long | [16](plans/16-the-wave-system/README.md), W0-W1 shipped |
| PRE-WALK VISIBILITY: a wave stands 8 ticks at its marker and steps on the 9th — and because it stands INSIDE tower range, the kills pile up out there and plan 12 B7's tower inverts from -9 ticks to **+16** | [16](plans/16-the-wave-system/README.md), W2 shipped |
| A run STARTS ITSELF: driving onto a spawn marker 12+ hexes out wakes the list — and a marker at 11, which really does send the wave, is safe to stand on | [16](plans/16-the-wave-system/README.md), W3 shipped |
| ⚠ What a base is worth AT ITS REAL LENGTH: the authored seven-wave list plays **FOUR** and falls at 321 with every tower black, and a retrieval is worth **one tick** on a base where the crew member does come back | [16](plans/16-the-wave-system/README.md), W4 shipped — plan **complete** |
| **No wall trigger, no ordering, no beacons and no scramble** | [plans/ROADMAP.md](plans/ROADMAP.md) |
| UPKEEP: 20 s of standing at a black tower rebuilds it — so the lull is a REPAIR WINDOW and a base can outlive its own wave list | [17](plans/17-tower-hot-swap/README.md), T0-T1 shipped — T2 next |
| HOT-SWAP: a tower's top is a CARRY object — take it off (the tower stops firing), transplant it onto a spent tower (red instantly), or evacuate it at the core.  ⚠ The magazine travels WITH the top, so detach-and-remount is not a free repair | [17](plans/17-tower-hot-swap/README.md), T2 shipped |
| ⚠ **The authored SEVEN-WAVE list is playable**: seven towers and two SHUTTLING helpers clear all 205 robots.  ⚠ Parked on their towers the same two reach 5/7 and the base falls — upkeep is a POSITIONING problem, not a resource | [17](plans/17-tower-hot-swap/README.md), T3 shipped — plan **complete** |
| THE GAME HAS A DOOR: `play.loft` owns the only call to `wave_tick`, asked by COUNT (`play_ticks`) or by DURATION (`play_advance`).  ⚠ **They are not interchangeable** — `n × TICK_SECONDS` through the accumulator is one tick SHORT for 602 of the first 1000 `n` | [19](plans/19-the-interactive-loop/README.md), P0-P1 shipped |
| AND A KEYBOARD: WASD / Shift / E are rows in the ONE key table, and a `.keys` script presses them.  ⚠ WASD is SHARED with the camera pan — `editor_input_from(…, playing)` fills one set or the other, never both.  ⚠ W is TRUE north via a metre heading, measured at zero drift | [19](plans/19-the-interactive-loop/README.md), P2 shipped |
| FOUR ROLES, ONE AI: scout / harvester / builder / miner — the same wave size at the same wall breaches at **23 / 35 / 50 / 96 / 454** ticks, and a harvester's body pays TRIPLE.  ⚠ `robot` keeps its rate, so **no existing measurement moved** | [23](plans/23-the-small-robots/README.md), K0 shipped |
| A WAVE HAS COMPOSITION: `schedule 4 12` arms the list and `compose 1 4 miner 8 scout` fills a wave of it, in the order written.  ⚠ A wave's SIZE is **summed** from its parts and never stored, which DELETES the plan's own negative control (`@X055`).  ⚠ 569 measurements unchanged — a `vector<integer>` still means N waves of regulars | [23](plans/23-the-small-robots/README.md), K1 shipped |
| SPEED IS NO LONGER THE TICK: an enemy BANKS `speed × tick_seconds` and steps when a whole hex is due, so the timestep is a CHOICE (`@X058`) — and nothing moved (1128 tests, 569 measurements).  ⚠⚠ **1.5 hex/s is a speed at which the rounding guard cannot fire**: zero the epsilon and the whole corpus stays green (`@M014`), while 1.0 / 1.2 / 1.8 / 2.0 / 2.5 hex/s each lose a hex without it (`@M013`) | [23](plans/23-the-small-robots/README.md), K2a shipped |
| THE SCOUT IS FASTER: 2.5 hex/s against a miner's 1.0 and a robot's 1.5, so nine hexes of one corridor take **6 / 9 / 14** ticks (`@M016`) — one lookup, no new mover.  ⚠⚠ **The guard that could not fire now DOES**: 2.25 and 3.0 were refused *because* they hide it as 1.5 does, so zeroing the epsilon today turns the suite RED (`@M017`, `@X063`) | [23](plans/23-the-small-robots/README.md), K2b shipped |
| ⚠ Composition is legible: three waves of twelve fall at **94 / 126 / never**.  ⚠⚠ Its headline — *a wave is as dangerous as its FASTEST class and no more* (`@M018`) — is **RETIRED by plan 24** | [23](plans/23-the-small-robots/README.md), K3 shipped — plan **complete** |
| ⚠⚠ **THE SIEGE FRONT IS THE WALL'S WIDTH**: a besieger attacks the hex it is TOUCHING, so 3 → **4** hexes on a five-row wall and 3 → **6** on a seven-row one — and a wave is worth its front class PLUS what the front cannot COVER (`@M020`).  ⚠ Four screens against a five-hex face leak exactly ONE miner, so *4 scout + 8 miner* went from **never** to **126**.  ⚠ The rule five documents asked for was one we already had (`@M019`) | [24](plans/24-the-siege-front/README.md), W0-W2 shipped — plan **complete** |
| **AND IT OPENS**: `make play`, press **P**, and waves arrive because TIME PASSED — the crew lands at the core and WASD drives it.  ⚠ **Nothing of the game is DRAWN yet** (P4), so the console echo is the only way to see it.  ⚠ The mode gates the CLOCK and never the seam | [19](plans/19-the-interactive-loop/README.md), P3 shipped — P4 next |
| A CAMERA that comes to the vehicle: an orbit camera whose azimuth is the VELOCITY's and whose elevation and boom are the player's.  ⚠⚠ **`camera_overview` at 89° IS the editor's view** — measured against the software rasteriser at **0.0014 rad of bearing and 0.56% of scale** (`@M022`), so it is one camera with two presets.  ⚠⚠ The 3-D world frame is **+y NORTH** and `lat_to_world` is the ONE negation | [21](plans/21-the-renderer/README.md), R1 shipped |
| AND IT EASES: the camera lives on `PlayState`, steps on every frame, and shortens its boom behind a wall.  ⚠⚠ The approach is **`1 − e^(−k·dt)`** and moros's linear `k·dt` was REFUSED — `play.loft` is frame-rate independent and the linear form is not (`@M023`).  ⚠⚠ **The ease is what makes a LATTICE look like a moving world**: un-eased the camera moves on 12 frames of 240 and jumps a whole hex, eased on 221 with a worst frame nine times smaller | [21](plans/21-the-renderer/README.md), R2 shipped — plan **complete** |
| ⚠⚠ **THE GROUND IS NOT MESHED YET** — and the job is HALF what plan 21 sized it at.  dryopea's ground is a flat plane with pillars on it (`height_override` non-null on **2 of 12** palette kinds), so moros's corner-height MEAN is a no-op at every hex and the mesher does not blend (`@X072`); `mesh3d::mesh_to_floats` + `graphics::GroupVboSet` already publish the whole GPU-side chunk cache.  ⚠ Colour is a **UNIFORM**, one mesh per palette kind (`@X074`) — a flat-unlit frame built that way can only contain palette colours, which is what keeps the exact classification alive.  ⚠⚠ **A reversed fan changes no count, no height and no vertex position — and draws NOTHING under `GL_CULL_FACE`**, so M0 gates the winding as DATA three phases before anything is drawn | [25](plans/25-the-terrain-mesh/README.md), M0 shipped |
| A COLUMN HAS SIDES: one vertical quad per edge where a hex stands above its neighbour, emitted **once**, by the side that STANDS.  ⚠⚠ **Both halves of `hh <= nh` fail invisibly** — no guard draws every faced edge twice and the copy is back-facing; `<` grows a zero-area sliver at every hex boundary in the world — so it is gated as four COUNTS on four fixtures (**6 / 10 / 0 / 5+6**).  ⚠⚠ A quad's NORMAL (from the two centres) and its WINDING (from the corner ring) are two facts that can disagree, and the test asserts they AGREE | [25](plans/25-the-terrain-mesh/README.md), M1 shipped — M2 next |

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

**Suite: 1211/1211 green under `scripts/test.sh`** (~177 s re-measured
2026-08-17 — the `frame` measurements classify full 960x720 frames, the
cost gate ticks a radius-40 world twice, and since plan 13 a dozen tests
run whole scenarios to their fall.  ⚠ This line carried "~35 s" from
plan 12 until H2 re-measured it and "~150 s" until plan 23 K3; the
figure grows with the SCENARIO tests, not with any one phase.  ⚠ The
two most expensive files are both closing measurements and they are
**35 s (plan 23 K3)** and **13 s (plan 16 W4)** — where a file that
runs no simulation costs 3.8 s, which is the compile baseline every
single-file run pays.  ⚠ **The corpus went 1161 → 1156 test
FUNCTIONS with no assertion lost** — five were folded into siblings
because each was re-deriving an expensive value a neighbour had already
computed, and the assert counts are byte-identical before and after.
That cut the suite **10.1%** (5 983 456 → 5 377 562 samples);
[`docs/PROFILING.md`](docs/PROFILING.md) has the per-file table and the
one refactor that measured as FREE).
**Gate: 33 scripts green under `scripts/validate.sh`** (~14 s, 654
measurements).  ⚠ Plan 24 W2 moved **8 of the 33** — a steering change
re-prices scenarios rather than breaking them, and the numbers of record
are `@M020`.  ⚠ Plan 21 R1 **and** R2 moved **none of them**, which is
the point: a camera is not a simulation, and the day it re-prices a
scenario is the day something is reading it that should not be.

⚠ **[loft#939](https://github.com/loft-lang/loft/issues/939) is FIXED
and CLOSED** (loft `ac8fb1dc`, *"A vector field assigned from a view
frees what it only names"* — which is exactly `crop_state`'s
`cs_out.crew = state.crew`).  For about a day it made
`tests/18_s3_the_crop.loft` fail and the suite SIGSEGV: returning a
large struct by value poisoned the store, and the next unrelated call
read a plain `integer` field back as a pointer.  ⚠ **`18_s3` is the
detector for it** — it is not something to "fix" if it ever goes red.
⚠ It closed labelled `both-backends`, so *"`--native` looked clean"* was
wrong at the time and the tell was in the reading: native emitted 255
characters where the interpreter emitted 1017, i.e. it never ran the
same workload.  **A backend answering differently on a different
workload is not a backend answering correctly.**

⚠ Do not run two `scripts/test.sh` at once — both pre-clean
`tests/actual/`, so they clobber each other and fail for no reason.

⚠⚠ **And the suite's WALL CLOCK is not yours alone.**  Plan 25 M1 timed
it at **293 s** twice against a documented ~177 s and nearly rewrote the
figure; `ps` showed a `rustc` at 336% CPU, a `dotnet` at 101% and
another project's `loft` probe, all from unrelated sessions on the same
box.  The figure above was NOT changed on the strength of those
readings.  ⚠ Before believing any timing here, look at what else is
running — this is the same rule § Profiling the suite gives for the
profiler (read the SAMPLE COUNT, never the seconds), arriving one level
up.

⚠⚠ **Both gates can be taken out by the `graphics` cdylib, and it is a
TOOLCHAIN fault every time — but the cause is NOT pinned, so do not
trust a tidy story about it.**  ⚠ **Not reproducing as of 2026-08-17**:
both gates run clean with no flags.  It has come and gone twice, each
time around a fresh `loft` install, so treat this as a thing to
RECOGNISE rather than a thing that is currently broken.  Symptoms, all
seen 2026-08-15/16:
every PNG/GL test failing with *"native function not loaded"*; a
`[timeout] hard-kill after 300s` in an unrelated file's PARSE phase (a
cdylib build in flight); a `SIGABRT` at the end of an otherwise green
run; and `validate.sh` refusing to start with

```
rust-lld: error: unable to find library -lloft_graphics_native
```

which is a DIFFERENT library's auto-cdylib (`hex_grid`) linking against
`graphics` while `libloft_graphics_native.so` is absent.  loft rebuilds
graphics 2-3 times in a single run and the artefact ends up missing.

⚠ **Two explanations were tried and FALSIFIED, so skip them**: it is not
simply *two loft binaries sharing `~/.loft/build-cache`* (it reproduces
with one binary, installed and in-tree byte-identical), and it is not
the stamped loft-ffi fingerprint alone — pinning `.loft-build-fp` to the
expected value and setting `LOFT_NO_AUTO_REBUILD=1` do not stop the
rebuild.  ⚠ A fresh `loft` install is what has triggered it each time.

⚠ **What has worked, when it works**: build the cdylib by hand and
re-run, checking the `.so` actually survives.

```bash
(cd ~/.loft/registry/graphics-<ver>/native && \
   CARGO_TARGET_DIR=~/.loft/build-cache/graphics-<ver> cargo build --release)
ls -l ~/.loft/build-cache/graphics-<ver>/release/libloft_graphics_native.so
```

⚠ It is a loft-side problem and belongs upstream, not in a dryopea
workaround.

⚠ **Both gates run INTERPRETED**, and that is not a preference.  On the
NATIVE backend `load_palette` answers 0 entries — a silent `text as
vector<Struct>` miscompile, filed in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md) — which no test could
see, because `loft test` runs the interpreter only.

## Hard-won rules

Every rule here cost a real defect to learn, and most of them describe a
test that CANNOT see the thing it appears to test.  They are grouped by
what they protect.

### Movement + passability

**How an enemy moves today.**  `wave_tick` rebuilds the distance field
ONCE per tick before anybody moves — one field per climb limit in the
roster — and `enemy_tick` steps down it.  Two steering modes hand off at
the **scrambler bubble**: inside 25 hexes the field steers, outside it the
spawn heading does.  An enemy with no route at all follows the DESIRE
field instead and besieges what it cannot climb.

⚠ **The bubble is a STRAIGHT-LINE distance, never a route length** — it is
a jamming sphere, so an enemy with no route whatsoever is still inside it.

⚠ **The SURFACE is not always the painted kind.**  A hex carrying a
pile stands on `rubble`; `passable.loft` therefore has two lookups —
`painted_ground` for the HEIGHT (which ADDS the layer to the authored
structure) and `hex_ground` for the SURFACE.  Answer the height off the
surface and rubble's null `height_override` swallows the wall under it,
so piling debris onto a wall would LOWER it.

⚠ **Passability is TWO questions, and they filter different things.**
The field filters its NODES by the surface (`can_stand`) and its EDGES
by the step (`can_step`).  Filtering nodes by `can_occupy` instead is
**vacuous** — `can_occupy(x)` means `height(x)` is within a climb of its
LOWEST standable neighbour, so between two adjacent occupiable hexes the
step is legal in both directions by construction, and the height rule
could be deleted with no test moving.  It compiles, reads well and makes
F6 a no-op.

⚠ **A drop is free and a climb is not**, so `wall` hexes beside reachable
ground are IN a robot's field: it cannot get up there, but one standing
there could step down and walk home.  Nothing routes onto them —
`flow_steps` checks the step as well as the distance.  This is also why
`flow_build`'s BFS asks `can_step(n, a)` and not `can_step(a, n)`: the
sweep runs outward and the enemy walks inward.

**The siege.**  `flow_desire` is the same sweep with the climb lifted, so
walls are passable; an enemy follows it and attacks where the height rule
refuses the next step.  `enemy_target` names that hex.
⚠ **The spread is by APPROACH and by ARRIVAL** (plan 24 W1): enemies
from different directions meet the wall at different hexes, and one down
a single corridor now stops at the first wall hex it touches rather than
walking on to the desire gradient's minimum.  That is what makes a
front as wide as the wall's face.  ⚠ It is **not** the equal-distance
sidestep — dryopea has one and it steps off a face as readily as along
it (`@M019`).

⚠ **You attack what you could STAND on and cannot climb** — a target is
always a walkable surface, so an enemy at the water's edge besieges
nothing.

⚠ **ONE AI, per-class DATA — and it is a design rule, not an accident.**
*Bosses are not special in their AI; their size and their options are
different, and that is what makes them special events* (project owner,
2026-08-13).  `climb_limit` already says the same thing about movement —
a class's climb is "its WHOLE contribution to passability", which is what
lets one distance field serve several classes — and every later class
property (armour, size, a boss's 2x2 footprint, its option to share what
is hurting it) belongs in the same shape.  A class that needs its own
mover has broken it.

Its first concrete payoff, and the reason it is a rule rather than a
preference: the small robots are **scout, harvester, builder and
miner**, they differ a lot in how fast they chew a wall, and they
differ in **nothing else**.  Four enemy types for one row each in
`numbers.json` plus one branch in `spawn.loft`'s damage-to-wall lookup
— no new mover, no new targeting, no new code path.

⚠ **An enemy blocked by a COMPANION steps BESIDE it; one blocked by
the GROUND stands and attacks** (plan 11 F7b).  `flow_sidesteps` offers
the equal-distance neighbours and the mover reaches for it only after
every strictly-closer step came back OCCUPIED.  The condition is the
whole rule: sidestep on a terrain block and a besieger jitters along a
wall face for ever, attacking a different hex each tick and finishing
none.
⚠ **It was missing for three phases and it was the whole BALANCE.**
F5c, F7 and plan 12 B3 each recorded the gap as latent; B7 measured it
— thirteen robots reached an undefended core and exactly TWO ever
nibbled it, so the drain did not scale with the wave and a base's width
and roster were both scenery.  Building it moved every clock in the
game (161/311/180 → 61/104/95) and made B3's falsified claim true: a
wall the front SPANS now breaks at its 30 HP end.  ⚠ The fan has a
width, so a LONGER wall still hides its ends and bracing still pays.

⚠ **The siege chews where the ROUTE meets the wall, never where the
wall is weakest** — measured in plan 12 B3, and it falsifies what
`ENEMY_MOVEMENT.md` § A wall's HP is structural used to claim.  Six
robots at a six-hex fence land NOTHING on either end and everything on
the braced middle, because the approaches converge on the hexes their
routes cross.
⚠⚠ **And plan 24 W1 did NOT change that, which is the surprise.**  B3's
test was authored to go red the day somebody built the missing steering,
and it stayed green: each of its six robots already touches the fence
where its own route meets it, so a precedence about touching changes
nothing for them.  ⚠ **A tripwire aimed at the RULE you expect to build
is not one aimed at the BEHAVIOUR you want** — what W1 widens is a front
arriving down ONE approach, which is a different fixture.

⚠⚠ **THE SIEGE FRONT IS THE WALL'S WIDTH — a besieger attacks the hex
it is TOUCHING** (plan 24 W1, `@M020`).  Twelve robots from one spawn
marker besiege **four** hexes of a five-row wall and **six** of a
seven-row one, so widening the perimeter widens the front.  Everybody
past it is blocked by a companion, and a companion-blocked enemy
attacks nothing (F7b).
⚠⚠ **It was THREE for any wall length until plan 24, and the diagnosis
is the lesson** (`@M019`).  An enemy attacked only when it could not
WALK, and a desire field is a ring around the CORE — so a straight face
has ONE minimum and exactly three hexes lack a legal closer step,
whatever the wall's length.  ⚠ All five face hexes TOUCH the wall and
two of them walked away from it.  ⚠⚠ **Five documents called the fix
*the equal-distance sidestep*, and dryopea has had one since F7b** — at
the face hex `(7,-1)` it offers `(7,-2)` along the face and `(8,0)`
**back off it**, so the named rule was as likely to empty the front as
to widen it.  The fix is a PRECEDENCE, in `enemy_walk_desire`'s pre-pass
and `enemy_target`'s siege branch, asked in identical words.
⚠ **`enemy_target` takes no `Occupancy`**, so the rule must be phrased
*"a wall is between me and the core"* rather than *"my closer steps are
held"* — which needs no memory and cannot jitter, because an enemy that
stops never moves again.
⚠⚠ **A WAVE IS WORTH ITS FRONT CLASS PLUS WHAT THE FRONT CANNOT
COVER.**  Four screens against a five-hex face leak exactly ONE miner:
worth nothing behind a hard-biting builder (101 vs a pure 100) and
thirty-nine ticks behind a soft harvester (122 vs 161).  **The screen is
arithmetic — bodies against face width** — where `@M018` had it as
positional immunity, four scouts making a base unbreakable.  It is
still a CLIFF (the first three scouts are worth nothing, the fourth
thirty-two ticks) but it no longer buys immunity: *4 scout + 8 miner*
went from **never** to **126**.
⚠ **A wider front makes most bases last LONGER** — a besieger that
stops at the wall is not walking on to drain the wallet, so
`a-base-on-two-fronts` went 123 → **132** and `@M005` 321 → **320**.
⚠ **So a `compose` line's ORDER still decides nothing** (K0's 20x was
measured on POSITIONS, before classes had speeds): scouts first, scouts
last and scouts alternated all land on the same tick, because they
overtake.  ⚠ That test got STRONGER — it used to compare three bases
that never fell, and three zeroes are equal for any reason at all.
⚠⚠ **The tripwire written for this day did NOT fire.**  Plan 12 B3's
fence test was authored to go red when this steering landed and stayed
green: its six robots come from six directions and each already touches
the fence where its route meets it.  **A tripwire aimed at the RULE you
expect to build is not one aimed at the BEHAVIOUR you want.**

### Cost

**The tick's budget comes from the design's own numbers** — 80 enemies
(the largest authored wave), a radius-40 world (the haze bound) and
1.5 hex/s, so a tick has **~667 ms**.  Plan 11 F8 measured it at ~125 ms
and it has not been re-measured since; plan 12 B1 added a hash lookup to
every surface question and B2 an `enemy_target` per enemy per tick, so
treat 125 ms as a floor rather than a reading.

⚠ **Do not reach for a standalone stopwatch to check that.**  A probe
that ticks a radius-40 world under `loft --interpret --lib src` answers
173 ms, 737 ms and 754 ms on three runs of an UNCHANGED file, and a
`flow_build` called three times in one process climbs 323 ms → 1006 ms →
1407 ms.  Discarded structs are not freed, so the process degrades as it
measures, and a long enough probe exhausts the store table outright
("store table exhausted: 65535 stores live").  `tests/11_f8_the_tick_
budget.loft` runs inside `loft test`, is a RATIO as well as an absolute,
and is the number of record.

⚠ **NEVER bind a `FlowField` (or any struct with a big hash) to a
local in a per-enemy path.**  A whole-value bind COPIES the heap value,
and an accessor that returned the field did it once per enemy per
lookup — 2250x the cost of reading it in place, and it had been there
since F5.  Loop the fields and pass `cf.field` straight into a `const`
parameter; there is deliberately no accessor to reach for.

⚠ **A copy changes no behaviour, only cost**, so 490 green tests sat
over a tick 25% past its budget for four phases.  `tests/11_f8_the_tick_
budget.loft` is the gate that can see cost, and it is a RATIO (16x the
enemies over one world, <200%) rather than a stopwatch — 115-125%
healthy vs 316% copying, stable to +-2% under a full suite run.

⚠ **The incremental rebuild is deliberately NOT built.**  The budget
gate is green with room to spare, and an incrementally wrong field
routes enemies through a wall the player just built.  Its equality gate
is already written and green against the from-scratch reference; the
trigger for revisiting is the budget test going red, `numbers.json`
raising the wave list or the world radius — or **the TICK getting
shorter**, which is the one nobody would look for.

⚠ **That third trigger is now ARMED, and it is a design decision rather
than a regression.**  A tick was 667 ms because it was *defined* as the
time an enemy takes to cross one hex; plan 23 K2a broke that definition
— every enemy banks `speed × tick_seconds` and steps when a whole hex is
due, so the timestep is a free choice and `TICK_SECONDS` is now what
HOLDS it at 667 ms rather than what forces it (`@X058`).  Nothing has
shortened it yet.  ⚠ The moment something does, the per-tick budget
shrinks in direct proportion — the rebuild that fits at 667 ms does not
fit at 100 ms — so [`plans/22`](plans/22-the-field-cache/README.md) is
the prerequisite for the shorter tick, not a follow-up to it.
⚠ And the epsilon travels with it: a **tenth-length tick loses a whole
hex** without `ENEMY_PROGRESS_EPSILON` (`@M013`).  See `spawn.loft`
§ What a tick is worth.

### Testing something that moves

⚠ **A 1-hex-wide corridor cannot tell a flow field from a fixed
heading** — both give the identical path, so every enemy test dryopea
had was blind to the field when it landed.  A scenario that means to
exercise routing needs
a route that leaves the heading's line: a heading of 4 is `(-1, 0)`,
so `enemy 0 3 -1` is a hex no heading can reach.  That is the shape
to reach for when gating a movement change.

⚠ **A corridor cannot see F5c either, and the reason is a number: on
a hex AXIS the field offers ONE closer neighbour, off the axis TWO.**
So "an enemy whose step is taken moves BESIDE" has no beside in a
corridor, and a blocked enemy can only wait.  Gate a spreading change
on an OPEN world — `tests/11_f5c_spread.loft` paints rows `r = 0..4`
over `q = 0..8`, where the distance to the core is exactly `q + r`.

⚠ **A wave spawns STACKED** — `spawn_wave` emits the whole wave onto
one marker hex — and leaves it one enemy per tick.  So `range` over a
walking wave is a SPAN, not a point (`range 4 7`, not `range 4 4`),
`enemies distinct` is red until they have walked, and "the wave
arrived" means one enemy per hex packed against the core, never N on
`(0, 0)`.

⚠ **Route every step through `lat_neighbour`** (§ Hex convention has the
rule).  It is what let the whole lattice convert with **233 measurements
unchanged** — the one table converted, so no distance could move.  A `+ 1`
on a `q` or `r` anywhere else is the bug, and it is how moros#10 sheared
every reach computation.

⚠ **A walking test must paint the ground it walks on.**  An unpainted
hex IS sea, so a wave over a blank map does not move at
all, and `enemies passable` over one is red.  Every scenario that
walks enemies drags a corridor first; that is the game's rule, not a
harness quirk.

⚠ **A world where every source hex is at 0 m cannot tell "the step is a
RISE" from "the step is the destination's height".**  A world like that
lets the whole height rule change with the suite green.  The case that
discriminates is an enemy walking ALONG raised ground it could never have
climbed onto — level steps all the way, and a drop at the end.

⚠ **"N enemies attack N hexes" does NOT gate the desire field.**  Six
enemies released on six sides spread across six wall hexes with the
steering disabled too — their spawn headings already take them to
different places.  Measured.  It gates the TARGETING; what gates the
steering is a corridor that BENDS, because a straight one gives a field
and a heading the identical path.

⚠ **A MIRRORED base is not a symmetric one, and the artefact reads
exactly like a finding.**  Plan 14 H2's two-front base measured 112
ticks with a helper on the east front and 211 with one on the west — a
99-tick spread on a map that looks mirror-symmetric, and none of it was
the crew's.  A wall's END is worth 30% of a braced hex, the siege chews
where the ROUTE meets the wall (both plan 12 B3), and odd-r rows are
offset — so one approach fan included an end hex and the other did not.
Extending both walls two hexes PAST the walkable band braces every
reachable hex and brings the fronts to within three ticks (214 vs 211).
⚠ So a scenario that compares two sides of a base must control for
BRACING first; `q -> -q` is not a symmetry of this lattice.

⚠⚠ **A gate that reads PERFECT is as suspect as one that reads wrong,
and it is much easier to miss** (plan 21 R1).  The camera gate compares
a ring of twelve hexes projected two ways and reported a worst bearing
disagreement of **exactly 0.0 rad** — twice, for two unrelated reasons:
the ring was an empty `const vector` ([loft#955](https://github.com/loft-lang/loft/issues/955)),
so twelve null hexes all landed on the screen centre; and once fixed,
the bearings were compared in **NDC**, where the aspect ratio is baked
into the projection and the space is anisotropic by construction.
⚠ **The tell was the exactness** — an integer-pixel-versus-float
comparison over arbitrary hexes cannot produce a true zero.
⚠ **The missing control is generic and costs two lines**: *can this
gate produce a non-trivial reading at all?*  Assert the disagreement is
`> 0`, and assert the fixture is not degenerate.  Any gate that compares
two computations of one thing can agree by both being empty.

⚠⚠ **SEVERAL COUNTS IN ONE TEST FUNCTION ARE RANKED, NOT INDEPENDENT**
(plan 25 M1).  loft abandons a test function at its FIRST failed
assertion, so a function asserting four counts can only ever report the
earliest one that breaks — and a break that moves two of them reports
one.  Measured: the bundled version of `tests/25_m1_the_sides.loft`
named a single failure where the split version names **three** for the
identical break.
⚠ The three quiet ones are not merely unhelpful; they are unmaintained.
Nothing ever prints them, so nothing ever checks that they can fire.
⚠ **The test is cheap and it is the same one M0 used**: falsify, and see
whether the assertion SPEAKS.  A count that can never be the diagnosis
is decoration, and splitting it into its own function is the whole fix.

⚠ **A gate whose reading is already saturated cannot see the thing you
built.**  H2's plan said the crewed base's clock would rise again with
helpers on it; measured, it does not move by a tick, because one tower
makes 0.03 m of body a tick and one vehicle clears 0.33 — **ten times
the capacity the front demands**.  Nothing was wrong with the crew; the
base could not express a second one.  Before believing a flat reading,
price the SUPPLY against the CAPACITY — the same move `12_b5b`'s
LOS-versus-alternative pricing makes.

⚠ **A cost gate over a world with none of the thing you changed is not
a gate.**  `tests/11_f8_the_tick_budget.loft` ticked a MARKERLESS world,
so `wave_fire` returned immediately and the budget would have stayed
green through any line-of-sight cost whatever — B5b could have put a
15-hex trace per enemy per shot into the tick unseen.  It now ticks a
defended world too.
⚠ And even that is not enough on its own: LOS is **3% of a tick with
3.8x headroom above it**, so the budget cannot see a 20x regression in
it.  What can is pricing the ALTERNATIVE and comparing — twelve shots
the shipped way against ONE roster-wide `tower_sees` pass, 51% measured
against 1200% for the naive shape.  Reach for that whenever the thing
you changed is a small share of a gate that has room to spare.

### Profiling the suite — and why the wall clock cannot do it

`LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` gives one merged
per-function + per-line + call-path report over the whole suite.  The
method, the numbers of record and the compilation half of the wall
clock live in [`docs/PROFILING.md`](docs/PROFILING.md); these three are
the ones that mislead a reader who skips it:

- ⚠ **The report goes to STDERR.**  A plain `> out.txt` keeps the test
  results and silently drops the profile, which reads as "the profiler
  says there is nothing to see".
- ⚠ **Read the SAMPLE COUNT, never the seconds.**  The op counter is
  deterministic (two runs of an unchanged suite agree exactly); the wall
  clock has **~3.5 s of variance on a ~33 s suite** and has pointed the
  wrong way twice on real improvements.
- ⚠⚠ **A profile AGES, and the stale one gets quoted.**  *"58% is
  `canvas()`"* was true on 2026-08-12 and was still being cited three
  plans later; re-profiled 2026-08-15, **the distance field is ~75%**
  and `classify_canvas` 7.5%.  Re-profile before optimising, and quote
  the date.
- ⚠ **Re-profiled again 2026-08-17 and the SHAPE held**: the field
  family is **~69%** (`flow_sweep` 17.6% self) and every one of the
  eight hottest paths is `wave_tick → … → flow_sweep`.  So a third
  reading agrees and [`plans/22`](plans/22-the-field-cache/README.md) is
  still the win.
- ⚠ **A test that RE-DERIVES an expensive value a sibling already
  computed is the cheapest thing to find.**  `18_s4` asked `reduced()`
  eight times for one answer (−51% once bound), `23_k3` re-ran the
  twelve-miner control per class (−25%); together **−10.1% of the
  suite**, with the assert counts byte-identical.  ⚠ And the refactor
  that LOOKS identical — hoisting `state_diff` out of an assert message
  — measured as **free**, because loft evaluates assert messages
  lazily.  Measure before tidying, both ways.
- ⚠⚠ **The SIZE doubled — 2 780 440 → 5 983 456 samples on twelve
  more tests — and most of it is UNATTRIBUTED.**  Plan 23's files are
  only ~1.1 M of the +3.2 M; the first hypothesis to test is K2a's
  banked mover, which put an `enemy_bank` per enemy per tick into every
  scenario under a gate that only asked whether behaviour moved
  (`@M015`).  ⚠ `11_f8_the_tick_budget` cannot see it either — a RATIO
  divides a uniform increase out.  [`docs/PROFILING.md`](docs/PROFILING.md)
  has the per-file table.

### Timers and epsilons

⚠⚠ **A COUNT asked for in SECONDS comes back SHORT** (plan 19 P1), and
it is the epsilon family's worst member because the arithmetic looks
harmless: `n * TICK_SECONDS` fed to an accumulator that spends whole
ticks answers `n - 1` for **602 of the first 1000 `n`**.
⚠ **The product being exact does not save it** — `n = 12` gives exactly
`8.0` and still answers 11, because it is the SUBTRACTION chain that
loses the tick, not the multiplication.
⚠ **And an epsilon does not save it either** — it moves WHICH 602 are
wrong, not how many.  A count is exact because it is counted, which is
why `play_ticks` and `play_advance` are two verbs (`src/play.loft`).
⚠ The failure is invisible where you would look for it and loud where
you would not: one tick short, at the END, so a scenario reports *a
wave that never arrived* rather than a clock a hair off.

⚠ **A banked timer's DIRECTION is half the epsilon rule** (plan 17 T1).
Plan 15 C0 said a timer loses a tick exactly when its duration divides
the tick length exactly — true, and incomplete.  Counting DOWN from `T`
and counting UP to `T` do not accumulate the same way: over a 1/1.5 s
tick, 20.0 s counting up lands on **exactly** 20.0 (no epsilon needed)
while counting down leaves a residue; 10.0, 40.0 and 60.0 counting up
all fall SHORT.  ⚠ **Neither direction is safe** — measure the pair.
⚠ And the direction is not free to choose: `repair` counts UP because
zero has to mean *nobody is working on this*, which is the same
zero-neutral rule as *damage taken* and *shots fired*.
⚠ **An epsilon whose removal leaves the suite green is a guard that
cannot fire** — `tests/17_t1_the_rebuild.loft::test_the_guard_can_fire`
is the shape that fixes it: exercise the branch directly (bank a hair
under the target) instead of relying on the tick arithmetic.

⚠⚠ **And the branch test is NECESSARY, not SUFFICIENT — the third
member of this family is a guard invisible at the value you shipped**
(plan 23 K2a).  An enemy banks `speed × tick_seconds`, and at today's
1.5 hex/s over a 1/1.5 s tick the product is **exactly 1.0 to the bit**,
so the carry is 0.0 for ever and no scenario can reach the epsilon at
all: set `ENEMY_PROGRESS_EPSILON` to 0.0 and **1128 tests and 569
measurements stay green** (`@M014`).
⚠ **1.5 is one of the FEW speeds with that property**, which is what
makes it a trap rather than a curiosity — swept over sixty ticks, the
epsilon is worth a whole hex at **1.0, 1.2, 1.8, 2.0 and 2.5** hex/s and
nothing at 0.5, 0.75, 1.5, 2.25 and 3.0 (`@M013`).  ⚠ It is worth a hex
to a **tenth-length tick** too, so this is the shorter tick's problem as
much as the faster class's.
⚠ **So sweep the NEIGHBOURS of the value you shipped.**  A guard that
cannot fire at one speed is not dead code; it is a defect waiting for
whoever changes that number, and it will report as *a wave that arrives
a tick late* rather than as rounding.  ⚠ That is also why `enemy_bank`
takes its speed as an ARGUMENT where `helper_bank` reads a constant
(`@X060`) — a bank that read the constant could only ever be tested at
the value that hides its own guard.

⚠⚠ **And then SHIP a value that can see it, if the design leaves you a
choice** (plan 23 K2b, `@X063`).  The scout had to be *"quite a bit
faster"* and five speeds said that; 2.25 and 3.0 hide the guard exactly
as 1.5 does, 2.5 does not, so the tie was broken on testability.  Zero
`ENEMY_PROGRESS_EPSILON` today and the suite goes **red** (`@M017`)
where at K2a it stayed entirely green — the same experiment, the same
constant, and the difference is one number in `numbers.json`.  ⚠ A
guard nobody can reach is a guard nobody maintains.

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

dryopea uses the **installed** `loft` binary (`loft` on PATH —
`/usr/local/bin/loft`).  There is no local loft build step: the
libraries it depends on resolve from the loft package registry
via `loft.toml` + `loft.lock`, so no `--lib` path is passed
anywhere.

```bash
# Run dryopea's test suite (canonical entry — DO NOT run `loft test` directly)
scripts/test.sh

# Play every tests/scripts/*.keys and gate on what they measure —
# the SECOND gate (plan 08 V4).  Prints each measurement beside its
# band, writes a PNG per `snap` into shots/, exits non-zero on a
# reading out of band.  `make validate` is the same thing.
scripts/validate.sh                  # all of them (~11 s)
scripts/validate.sh paint-a-base     # just one, while iterating

# Run the game / editor (opens a 960x720 GL window; P toggles play).
# Use `make play` — it passes --interpret, and the NATIVE backend is
# broken for dryopea today: it panics on the marker load, and where it
# does not panic it silently loads an EMPTY palette (both filed in
# QUESTIONS_FOR_LOFT.md).  `loft src/main.loft` is `make play-native`,
# kept for testing the eventual fix.
make play
make play MAP=starter_01

# Parse-check a single .loft file without running it
loft --native-emit /tmp/check.rs src/<file>.loft

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

⚠ **The full listing — what each file owns, and the trap in it — is
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)**, together with the
key data structures.  Read it before editing any file below; each
`.loft` file's own header is the source of truth and the listing is a
navigational summary of it.

| File | Owns |
|---|---|
| `dryopea.loft` | the library aggregator — `use dryopea;` brings every submodule into scope |
| `main.loft` | the interactive entry — the GL shell (NOT in the aggregator; parse-check it by hand).  ⚠ Since plan 19 P3 it runs the GAME and owns the CLOCK: it measures the frame and hands the seconds over, and decides nothing else |
| `editor_step.loft` | **the editor's input seam** — `EditorState` + `EditorInput` + `editor_step`.  Every editor action runs through it |
| `play.loft` | **the game's seam** (plan 19 P1) — `PlayState` + `play_ticks` / `play_advance` / `play_step`, and the ONE call to `wave_tick`.  ⚠ Also the MODE (P3): `play_mode` / `play_set_mode` / `play_begin` / `play_frame_seconds`, and since plan 21 R2 the game's CAMERA, stepped LAST and on every frame |
| `bindings.loft` | **the ONE key table** — keys → actions → `EditorInput`.  Never add a `gl_key_pressed`.  ⚠ Since plan 19 P2 it carries the PLAY actions too, and `editor_input_from`'s `playing` argument decides whether WASD pans or drives.  ⚠ And since P3 one SHELL action (`toggle_play`, P), filled in BOTH branches — fill it in one and there is no way out of play mode |
| `script.loft` | the `.keys` script runner and its whole vocabulary — commands name ACTIONS, never keys |
| `validate.loft` / `validate_main.loft` | the second gate: sweep `tests/scripts/`, sum the measurements, report the FIRST failure |
| `editor_view.loft` | `render_editor_frame` — what the player sees, composed ONCE for both the GL loop and `snap` |
| `measure.loft` | frame measurement — `classify_canvas` / `classify_world` → `FrameCounts` |
| `golden.loft` | `assert_golden` — write `tests/actual/`, compare bytes to `tests/golden/` |
| `compare.loft` | `state_diff` — are two runs in the same state? (plan 18 S0) |
| `emit.loft` | write a situation down as `.keys`, plus `crop_keys` (plan 18 S2/S3) |
| `reduce.loft` | cut a fixture to what a behaviour needs, against a predicate (plan 18 S4) |
| `lattice.loft` | **THE lattice** — pointy-top odd-r offset, `Hex`, and every `lat_*` verb.  Delegates to `hex_grid` |
| `relabel.loft` / `convert.loft` | plan 09's old-label → new-label bijection, and the `.keys` converter |
| `camera.loft` | `EditorCamera` + `camera_update`.  ⚠ pan NORTH is `r += 1` |
| `render_camera.loft` | **the GAME's camera** (plan 21 R1) — `RenderCamera`, the two presets, and `lat_to_world`; and since R2 the EASE — `CameraRig`, `camera_rig_step`, `camera_boom_free`.  ⚠⚠ Its world is `+y` **NORTH** with `+z` up, which is NOT dryopea's `+y`-south canvas frame: that one is left-handed once z points up, and `mat4_look_at` MIRRORS it.  ⚠ Assert on `camera_eye_of_view`, never on the struct.  ⚠⚠ The approach is `1 − e^(−k·dt)`, never `k·dt` |
| `ground_mesh.loft` | **the GROUND, as triangles** (plan 25 M0-M1) — `ground_top_face`, a six-triangle fan per hex in the CAMERA's world, and `ground_side_faces`, one vertical quad per faced edge.  ⚠⚠ There is no blend and that is measured, not lazy (`@X072`): the corner mean is a no-op at every hex in both directions.  ⚠ HEIGHT off `hex_height`, COLOUR off `hex_surface_index` — two lookups, and swapping them makes debris LOWER a wall.  ⚠ Colour is a UNIFORM, so it emits one mesh per palette kind (`@X074`); putting it on the vertex throws away the exact classification.  ⚠⚠ A side face is emitted ONCE, by the column that STANDS (`if hh <= nh { continue; }`) — and **both halves of that guard fail invisibly**, so they are gated as COUNTS |
| `painted.loft` | `PaintedHex` / `PaintedWorld` — sparse, sea-default ground |
| `palette.loft` | `GroundType` + `load_palette` + `GROUND_RUBBLE` |
| `markers.loft` / `marker_file.loft` / `marker_render.loft` | the marker layer, its save format and its drawing.  `place_marker` is the ONE dispatch |
| `map_file.loft` / `save.loft` | the save record (6 fields — see § Known constraints) and the save/load path |
| `render.loft` | the software rasteriser over `graphics::Canvas` |
| `picker.loft` / `hud.loft` / `editor_mode.loft` / `chunks.loft` / `history.loft` | palette UI, HUD, the mode flag, the dirty-chunk set, undo/redo |
| `spawn.loft` | **the tick** — `WaveState`, `wave_tick`, enemy movement, targeting, deaths, the schedule, `TICK_SECONDS`, and since plan 23 K2a the banked `enemy_bank` / `enemy_step` pair the mover is built on |
| `waves.loft` | the authored wave list, its lull, and what a wave is MADE OF — `WavePart` / `wave_schedule_compose`.  ⚠ A wave's size is SUMMED from its parts, never stored |
| `flow.loft` | the distance field — `flow_build` / `flow_step` / `flow_steps` / `flow_desire` |
| `passable.loft` | may a class MOVE here? — `can_stand` / `can_step` / `can_occupy`, and `hex_height`.  ⚠ Since plan 21 R2 also the SIGHT line: `sight_first_block`, the ONE walker, shared by `tower_sees` and the camera's boom |
| `occupancy.loft` | who is standing where this tick — enemy counts, and the separate `BlockerMap` |
| `height.loft` | the RUBBLE layer — metres piled at runtime, and what they are made of |
| `damage.loft` | what a structure has TAKEN, bracing, and `break_structure` |
| `tower.loft` | towers — range, the banked charge, the 30-shot magazine, LOS, repair, the detachable top |
| `wallet.loft` | the run's budget and the ONLY end state (`wallet_broke`) |
| `vehicle.loft` | the PLAYER — drive, boost, salvage.  `salvage_at` is the shared chassis |
| `helper.loft` | the NPC crew — banked movement, wrecking, and the 60 s recovery |
| `carry.loft` | one record per carryable thing, with an `owner` — conservation is STRUCTURAL |

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

By name, so you know when to go and read it:

- A struct **RETURNED from a function is a COPY** — mutating it is a
  silent no-op ([loft#894](https://github.com/loft-lang/loft/issues/894)).
- A struct **stored in a FIELD** of another struct is a copy too; loft
  says so as `advice[avoidable-copy]`.  A struct passed as a PARAMETER,
  or read out of a field into one, does alias.
- A struct returned through **TWO nested tail calls** loses what its
  loop wrote ([loft#880](https://github.com/loft-lang/loft/issues/880));
  bind the inner call to a local.
- **Never index a call's result in TAIL position** (loft#877) — but
  binding a call whose callee is declared LOWER in the file **panics the
  parser** ([loft#918](https://github.com/loft-lang/loft/issues/918)),
  so the fix for the two above triggers a third.
- **Never interpolate a struct with a `hash` field** — SIGSEGV
  ([loft#873](https://github.com/loft-lang/loft/issues/873)).
- A struct literal that **omits a field takes that field's default
  silently** ([loft#914](https://github.com/loft-lang/loft/issues/914))
  — build from `*_empty()`, never a partial literal.
- ⚠⚠ A file-scope **`const vector` holding a NEGATIVE number is EMPTY**
  ([loft#955](https://github.com/loft-lang/loft/issues/955), filed
  2026-08-17, both backends) — `len()` 0, every index `null(oob)`, and
  no diagnostic anywhere.  The SIGN is the whole trigger: `[10, 9, 5,
  0]` is fine, `[10, -5, 9]` and `[-1, 2, 3]` and `[1.0, -2.0]` are
  empty.  ⚠ A **local** with the same literal is correct, so bind it
  inside the function.  ⚠⚠ **A loop over an empty vector runs zero
  times, so every assertion inside it holds VACUOUSLY** — it made plan
  21 R1's camera gate report perfect agreement while iterating over
  nothing.
- **Loop variable names** must keep one type per function scope and
  OUTLIVE their loop ([loft#915](https://github.com/loft-lang/loft/issues/915))
  — prefix them per function.
- A **missing `use`** reports as `Expect token ;` on a later `.0`, and
  the whole aggregator goes red naming the importer.
- The JSON cast **HANGS** on ≥8 declared fields with a `vector<Struct>`
  — `MapFile` is capped at 6 — and **ignores declared defaults**
  ([loft#876](https://github.com/loft-lang/loft/issues/876)).
- `graphics::KEY_*` need **explicit qualification**.
- ⚠ `ticks()` is loft's clock builtin — **never shadow it**, not even
  as a parameter name.  A probe that did compiled clean and reported a
  tick 4x cheaper than it was.
- ⚠⚠ **Never add a `vector<Struct>` local to `script_command`**
  ([loft#935](https://github.com/loft-lang/loft/issues/935)) — give it a
  helper function, as `compose_parts` does.  A ~700-line function with
  one in it corrupts the interpreter heap **at compile time**, and the
  abort (`realloc(): invalid next size`) lands in an unrelated test file
  that never reaches the branch.  ⚠ Bisected at full-suite scale in plan
  23 K1: the nested data structure is innocent and the function's SIZE
  is the ingredient, so the same local is fine anywhere smaller.  ⚠ And
  a green suite cannot see the violation — the damage is latent until
  the allocator trips over it, so unrelated code can wake it up.
  ⚠ **FIXED and CLOSED upstream 2026-08-16**, so this is now a
  historical note rather than a live rule.  The split into
  `compose_fault` / `compose_parts` / `script_compose` **stays** — it
  reads better than the inline version and `script_command` is already
  at complexity 255 — but it is no longer load-bearing, and a future
  `vector<Struct>` local in a big function is not by itself a defect.
  ⚠ Retiring the split to re-test the fix is open work nobody needs.

### Save path

The interactive editor saves to `dryopea_save.json` in the
cwd.  Tests write to `tests/actual/*.json` (also gitignored).
Both paths are blown away between runs by `scripts/test.sh`.

**Eventual destination:** path-backed mmap'd `Store` (the hash
IS the file — no save loop).  Rust side ships; loft `.loft`
language surface for binding user-data Stores to a path is
missing.  Filed in [`QUESTIONS_FOR_LOFT.md` § Path-backed
user-data Store binding](QUESTIONS_FOR_LOFT.md); strategy in
[`plans/ROADMAP.md` § Persistence destination](plans/ROADMAP.md).
**Don't take the manual binary `file()` + `#read` detour** —
it's strictly worse than the JSON we have today.

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
                README.md carries the conventions + the index (value,
                effort, lifecycle, one line each); _TEMPLATE.md starts a
                new one; ROADMAP.md carries the feature ordering across
                5 tiers; DEFERRED.md parks them.
                ⚠ Each plan's own `## Status` is the SOURCE OF TRUTH.
                Never keep a second copy of per-phase state here or in
                the index — it drifts, and the copy is what gets read.

docs/
  DESIGN.md             — master design (mechanics, towers, walls,
                          combat dynamics, scramble loop, run shape)
  SETTING.md            — fiction (AI-driven robots, faction lore,
                          surface-vs-underground, future contact gates)
  DESIGN_HISTORY.md     — 2023 prototype design seeds
  PARTS.md              — entity art: the part-tree model and what an entity
                          IS, in metres and turns (plan 20)
  RENDERER.md           — the camera, the pipeline and the gate chain
                          (plan 21).  ⚠ Retires DESIGN.md § 12
  EXPLORATION.md        — scouting, assembled: the recon window the game
                          already gives you, why a find must arrive EARLY,
                          and the PERMIT that clocks it all
  PROGRESSION.md        — skill, not stats; the landscape is the school and
                          the base is the exam
  DECISIONS.md          — ⚠ the greppable index: @X decisions, @M measurements
  EXPLORATION.md        — scouting, assembled: skill as the progression, the
                          recon window the game already gives you, and why a
                          find has to arrive EARLY
  EXPLORATION.md        — scouting, assembled: the four rings, the sortie the
                          run already opens with, and what is out there
  GROUND_TYPES.md       — 11-type palette (water + land + structure)
  NUMBERS.md            — tunable values
  PROXY_ART.md          — placeholder shapes for entities

PROBLEMS.md             — dryopea-internal bugs (@D-prefixed; @D002 open — `cam.zoom`
                          changes no pixel; @D001 fixed)
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

| File | Topic |
|---|---|
| [README.md](README.md) | Public-facing project intro |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | ⚠ **The full `src/` layout** — what each file owns, the trap in it, and the key data structures.  `CLAUDE.md` § Architecture is a one-line index of this |
| [docs/EXAMPLES.md](docs/EXAMPLES.md) | ⚠ **The worked-example convention** — a public function is documented by the TESTS that show how to use it, pointed at by an index tag `@XXX-###` (an `@`, a THREE-LETTER acronym, a hyphen, three digits) in loft's own family (`@P367` / `@X072`), so ONE indexer carries them all; the hyphen is what keeps the families apart.  ⚠⚠ **The abbreviation namespace is the ECOSYSTEM's** — the indexer covers the registered libraries too, so `@XXX-001` must mean one test everywhere.  ⚠⚠ **A tag is not only an API example**: a first-class program tags a test because the ALGORITHM is worth reading, so a citation is any reference — a `// Example:` line OR prose in a doc.  ⚠ **NEW work only** (project owner, 2026-08-17): no retroactive sweep of the 387 existing public functions, and a file opts in with `// #examples`.  ⚠⚠ The gate carries an eight-control `--self-test`, and it earned its keep at once — `grep -r --exclude-dir='.*'` applies to the command-line directory too, so **any checkout under a hidden path scanned zero files and reported `ok`**, and every registered library lives under `~/.loft/` |
| [docs/PROFILING.md](docs/PROFILING.md) | How to profile the suite, the numbers of record and their date, and why the wall clock cannot see a real improvement |
| [docs/LOFT_GOTCHAS.md](docs/LOFT_GOTCHAS.md) | Every loft behaviour dryopea works around — ⚠ almost all of them compile clean and fail silently |
| [docs/DESIGN.md](docs/DESIGN.md) | Master design — towers / walls / waves / scramble / camera / HUD / economy / run shape |
| [docs/SETTING.md](docs/SETTING.md) | Fiction — autonomous AIs (girl-hacker imprint), faction wars dormant, surface-vs-underground, future contact gates, crew-doesn't-walk justification, combat-bot escalation |
| [docs/DESIGN_HISTORY.md](docs/DESIGN_HISTORY.md) | 2023 prototype seeds |
| [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) | ⚠ DESIGN, not built — the six robot installations (mines, factories, transport routes, military stockpiles, repair points, carbon plants) whose traffic is what waves are made of; the replacement for plan 16's authored list.  ⚠ Also § Crystal (the boss supply, and the only input with one product) and § The vertical dimension (a withered TREE is the shaft that reaches it) |
| [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) | Enemy movement — two steering modes, passability as a height step, bodies as terrain, sealing punished not forbidden, structural wall HP, retaliation, the tick resolving once |
| [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) | Palette spec — 11 painted types plus `rubble`, which the runtime deposits and nobody paints |
| [docs/NUMBERS.md](docs/NUMBERS.md) | Guide to `examples/numbers.json` — what is in it, what reads it, and ⚠ that nothing LOADS it yet |
| [loft_repros/README.md](loft_repros/README.md) | Minimal reproducers for loft bugs — filed, and ready to file |
| [docs/DECISIONS.md](docs/DECISIONS.md) | ⚠ **The greppable INDEX** — `@X###` design decisions, `@M###` measurements of record, each one line pointing at the doc that owns it.  ⚠⚠ **A bare plan phase is NOT unique** (`S0` is plans 18 AND 22, `C2` is 09 AND 15, `R0` is 20 AND 21) — write a code as `<plan>-<phase>`: `19-P3`, `22-S0`, `12-B7`.  ⚠ Every `@M` carries a DATE, because a measurement ages and the stale one gets quoted |
| [docs/PROGRESSION.md](docs/PROGRESSION.md) | ⚠ **The player gets better, the vehicle does not** (`@X016`-`@X019`).  Skill, not stats — which passes the genre test in its purest form.  The landscape is the school, the base is the exam, and there is a racing line because the measured-best defence is one only a good pilot can live in |
| [docs/PARTS.md](docs/PARTS.md) | ⚠ **Entity art — every entity is a PART-TREE and its GEOMETRY is derived** (plan 20).  The moros model (limbs on joints, three limb kinds, scale derived, hitbox a subset of the skin) and where dryopea deviates.  ⚠ Decisions D1-D8; moros's own `doc/claude/PARTS.md` § P9.0 is the authority on the model.  ⚠ § D4 replaced a SPRITE design — read it before quoting anything about pixels |
| [docs/EXPLORATION.md](docs/EXPLORATION.md) | ⚠⚠ **§ X0 is the load-bearing one: the progression is SKILL, not stats** — the landscape is the low-stakes school where flying is learned, and the BASE is where it cashes out.  ⚠ The measurements already agree: a sealed wall nearly doubles the clock, a gate buys nothing, and boost is the only way out of a sealed base — so **the best layout is one only a good pilot can live in**.  ⚠ Exploration IS scouting, which `DESIGN.md` § 13 already calls *the* progression activity — this doc ASSEMBLES the pieces rather than adding a pillar.  ⚠⚠ The run ALREADY opens with a sortie (`wave_provoke_step` needs a vehicle 12+ hexes out), so "explore earlier" is content on a trip the player already takes, not a new phase.  ⚠ A find is ONE marker row + ONE cargo row; the first scouting scenario needs **no code at all** |
| [docs/EXPLORATION.md](docs/EXPLORATION.md) | ⚠⚠ **Exploration IS scouting** — `DESIGN.md` § 13 already ranks it *the* progression activity, so this doc ASSEMBLES rather than adding a pillar.  ⚠⚠ **§ X0: the progression is SKILL, not stats** — the landscape is the school, the base is the exam, and the measurements agree (a sealed wall doubles the clock, a gate buys nothing, boost is the only way out of a sealed base, so **the best layout is one only a good pilot can live in**).  ⚠⚠ **§ X2b: the game already WAITS** — `wave_provoke_step` means an unlimited free recon phase the player ends deliberately.  ⚠⚠ **§ X2c: a find accelerates BUILDING, so its value collapses once you are busy** — measured twice already (plan 16 W4: one tick; plan 17 T3: +76 points) |
| [docs/RENDERER.md](docs/RENDERER.md) | ⚠ **The camera and the pipeline** (plan 21) — moros's `RenderCamera`, FOLLOW behind the facing, and ⚠⚠ **`camera_overview` at 89° IS the editor's view**, so it is one camera with two presets.  ⚠ § R0 MEASURED that a GL frame survives `xvfb` → `gl_screenshot` → `imaging::png` → exact classification with **zero** colour drift — which is what makes going 3-D affordable at all.  ⚠ Retires `DESIGN.md` § 12 |
| [docs/PROXY_ART.md](docs/PROXY_ART.md) | Placeholder shapes.  ⚠ Its SIZES stay and become a gate (`PARTS.md` § D6); its SHAPES retire entry by entry as plan 20's catalogue covers them |
| [plans/README.md](plans/README.md) | Plan conventions (moros-style) + index |
| [plans/_TEMPLATE.md](plans/_TEMPLATE.md) | Template for a new plan |
| [plans/ROADMAP.md](plans/ROADMAP.md) | Comprehensive feature roadmap (5 tiers) |
| [PROBLEMS.md](PROBLEMS.md) | Dryopea-internal bugs (`@D<NNN>`) |
| [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) | Outbound queue to loft |

## Reading by goal

| Goal | Start here |
|---|---|
| Understand the game | [README.md](README.md) → [docs/DESIGN.md](docs/DESIGN.md) § What kind of game this is, then § 2 The pitch |
| Design a base site that is not flat ground | [docs/DESIGN.md](docs/DESIGN.md) § Trees as terrain — a 10-hex tree stem is a plateau, so the perimeter IS the terrain and no wall is needed.  ⚠ The catch is the whole design: it is impregnable until you start killing, because bodies ramp and your own kills build the only staircase up |
| Understand the END GAME | [docs/DESIGN.md](docs/DESIGN.md) § The end game, and why it is still this game — enemies change, robots and insects become co-belligerents, and the player still BUILDS BASES because humans cannot attack an old one at all.  ⚠ The scrambler changes JOB rather than switching off: it is a LURE (robots converge on it), so where you put one decides where the swarm fights — but it ATTRACTS and DEGRADES in one act, so you can have them coordinated or where you want them, never both.  ⚠ And every ROBOT_ECONOMY lever inverts its sign: feed the factories, keep crystal flowing, wake the stockpile you spent the mid-game avoiding |
| Judge whether DEEP-LORE content belongs | [docs/DESIGN.md](docs/DESIGN.md) § And the DEEP layers are what keep it a tower defence — the second test: *does it resolve into a statement about position, terrain or timing?*  If it resolves into the player's STATS or ABILITIES it is off-genre, however good the story is.  ⚠ The deep layers are load-bearing for the GENRE, not decoration on it |
| Judge whether a new MECHANIC belongs | [docs/DESIGN.md](docs/DESIGN.md) § What kind of game this is — the test is *does this put something in the player's hands at a moment when using it costs them something?*  ⚠ Second test since 2026-08-14: [docs/SETTING.md](docs/SETTING.md) § Nobody is attacking anybody — yet.  Both non-human tiers OPEN as maintenance (robots think they are repairing, insects guard a wound), so a mechanic that opens with hostility is off-fiction; aggression has to be EARNED by the player's accumulated pressure |
| Judge a PROGRESSION idea (upgrades, unlocks, XP) | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X0 — ⚠ **the progression is the player's own skill with the controls**, which passes `DESIGN.md`'s genre test in its purest form (there are no stats to resolve into).  ⚠⚠ **The player's vehicle must not get faster** — the moment speed is a purchase, skill stops separating a good run from a bad one.  (`DESIGN.md` § 9's *"Scouting — faster movement"* is a HELPER skill and is unaffected) |
| Design EXPLORATION, or judge a scouting idea | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) — ⚠ it is not a new pillar: `DESIGN.md` § 13 § Scouting already ranks it *the* progression activity, and § X2 shows the run already opens with a sortie.  ⚠ **The cost of leaving is already MEASURED** — plan 17 T3 priced parked-vs-shuttling helpers at two waves of the authored list — so exploration needs no new cost mechanic.  ⚠ The first scenario is a `.keys` file, not a feature |
| Judge a PROGRESSION idea (upgrades, unlocks, XP) | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X0 — ⚠ **the progression is the player's own skill with the controls**, which passes `DESIGN.md`'s genre test in its purest form: there are no stats to resolve into.  ⚠⚠ **The player's vehicle must not get faster** — the moment speed is a purchase, skill stops separating a good run from a bad one.  (§ 9's *"Scouting — faster movement"* is a HELPER skill and is unaffected) |
| Design EXPLORATION, or judge a scouting idea | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) — ⚠ not a new pillar; § X2 shows the run already opens with a sortie and § X2b that the game waits until you poke a marker.  ⚠ **The cost of leaving is already MEASURED** (plan 17 T3: parked vs shuttling helpers = two waves of the authored list), so exploration needs no new cost mechanic.  ⚠ The first scenario is a `.keys` file, not a feature |
| Ask why a find has to be found EARLY | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2c — a find is a BUILD ACCELERANT, and what decays is **the opportunity to use it**, not the thing itself.  ⚠ Already measured twice by accident: the same retrieval is worth **one tick** when the job is gone (plan 16 W4) and **+76 points** when it is not (plan 17 T3) |
| Author what a WAVE IS MADE OF | `schedule 4 12` arms the list, `compose 1 4 miner 8 scout` says what one wave of it is made of ([plan 23](plans/23-the-small-robots/README.md) K1, `@X056`).  ⚠ **`compose` REPLACES a wave and a later `schedule` line WIPES it**, so the order `emit.loft` writes is a requirement, not a style.  ⚠ A wave's SIZE is SUMMED from its parts and never stored (`@X055`), so `schedule 12` + `compose 0 3 miner 2 scout` is a wave of **five** — there is no total to disagree with.  ⚠⚠ **The ORDER you write is worth NOTHING** (plan 23 K3, `@M018`) — it sets the departure order, and since K2b the faster class overtakes, so four scouts first, four scouts LAST and four scouts alternated all land on the same tick.  K0's *"order is worth 20x"* was measured on enemies PLACED at different distances, before classes had speeds.  ⚠⚠ **What a mix IS worth is its FASTEST member and nothing else** — four harvesters in front of eight miners behaves like twelve harvesters, not like anything in between — so write compositions expecting the quickest class to decide the outcome.  ⚠ `examples/waves.json` is NOT the place — `WaveFile` deliberately has no composition (`@X057`) |
| Judge a PROGRESSION idea (upgrades, unlocks, XP) | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X0 — ⚠ **the progression is the player's own skill with the controls**, which passes `DESIGN.md`'s genre test in its purest form: there are no stats to resolve into.  ⚠⚠ **The player's vehicle must not get faster** — the moment speed is a purchase, skill stops separating a good run from a bad one.  (§ 9's *"Scouting — faster movement"* is a HELPER skill and is unaffected) |
| Design EXPLORATION, or judge a scouting idea | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) — ⚠ not a new pillar; § X2 shows the run already opens with a sortie and § X2b that the game WAITS until you poke a marker.  ⚠ The cost of leaving is already MEASURED (plan 17 T3: parked vs shuttling helpers = two waves of the authored list).  ⚠ The first scenario is a `.keys` file, not a feature |
| Ask what CLOCKS a run, or why the player must be efficient | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2d — the **permit**.  `DESIGN.md` § 2 hires the player on a *"permit-bound sortie"*, `SETTING.md` § History calls them *"limited-time sorties"*, and § The quarantine puts the teeth at the exit: *"orbital exit is the chokepoint … permit on file = pass; permit missing = destroyed"*.  ⚠ Expiry must cost the CARGO, never the run — § 14 has no fail screen, and a bad run is one with *"meagre carryover"*.  ⚠ It also turns `NUMBERS.md`'s ungateable *"15-25 minutes"* into a tunable — but today's longest base falls at **321 ticks (~3.5 min)**, so the window is derived from content, not chosen |
| Ask why a find has to be found EARLY | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2c — a find is a BUILD ACCELERANT, and what decays is **the opportunity to use it**, not the thing itself.  ⚠ Already measured twice by accident: the same retrieval is worth **one tick** when the job is gone (plan 16 W4) and **+76 points** when it is not (plan 17 T3) |
| Design where WAVES eventually come from | [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) — six installation types and the routes between them.  ⚠ Its governing rule is the enemy rule again: ONE system, per-type DATA, so a new installation costs a row and no new behaviour |
| Cite a design decision, or find where one was made | [`docs/DECISIONS.md`](docs/DECISIONS.md) — `grep -rn '@X025' .` finds every mention of a decision, `@M001` every quote of a number.  ⚠ **Never cite a bare plan phase in a code** — `S0` is two plans and `C2` is two more; write `22-S0`.  ⚠ A code is permanent even after its decision is reversed (it gains a `SUPERSEDED by` line, like `@D001`) |
| Find a mechanic that is designed but NOT built | [docs/DESIGN.md](docs/DESIGN.md) (the mechanics) and [plans/ROADMAP.md](plans/ROADMAP.md) (the index).  ⚠ `plans/12` § Design recorded during this plan POINTS at them rather than restating — a second copy is the one that drifts |
| Understand the fiction | [docs/SETTING.md](docs/SETTING.md) |
| Pick next work to do | [plans/ROADMAP.md](plans/ROADMAP.md) § **The critical path** — the natural order by DEPENDENCY (the 5 tiers below it are ordered by impact-per-line instead, and say so).  ⚠ Its organising principle: **every step must be measurable when it lands**, and the questions now open are FEEL questions, which is what moves drawing up the list |
| Ask what the BIGGEST missing mechanic is | ⚠⚠ **BUILDING.**  Walls and towers are placed in the EDITOR; the player cannot make a base, and the wallet buys nothing.  Three finished designs are inert without it — `@X022` (the pre-wave window is a budget), `@X024` (a find accelerates building), `@X019` (the layout is the exam).  ⚠ Its pieces are all designed and named: wall paint, the beacon ferry, helper construction time — and the carry model already moves a beacon exactly as it moves a tower-top |
| TUNE a number | `examples/numbers.json`, then run `scripts/test.sh`.  ⚠ `tests/numbers_design_targets.loft` gates five of `docs/NUMBERS.md` § Design targets against the running sim, so a tuned value fails there naming the promise it broke.  ⚠ And nothing LOADS numbers.json: every value is hand-copied into a `.loft` constant, so edit BOTH — that test pins them together |
| Continue plan 01 work | [plans/01-ground-editor/README.md](plans/01-ground-editor/README.md) § Implementation status |
| Document a new public function, or point at a test as its EXAMPLE | [`docs/EXAMPLES.md`](docs/EXAMPLES.md) — put an index tag in a comment above the test (`// @XXX-001 — a frame loop spends its backlog`) and cite it from the function (`// Example: @XXX-001`).  ⚠ A tag is an INDEX TAG in a comment, never part of an identifier, and it binds to the `fn` that FOLLOWS it.  ⚠⚠ **Anchoring to the comment is the better contract**: deleting the test makes the citation DANGLE (real breakage), while renaming it for clarity leaves the example valid.  ⚠ A snippet in prose ROTS (nothing compiles it) and an unlinked test is INVISIBLE — the pair is the deliverable.  ⚠ Write a NEW test if no existing one is clear: a test that proves a function works is not automatically one that shows how to use it |
| Add a regression test | `tests/01_*.loft` for patterns; `golden.loft::assert_golden` for image tests |
| Script a run of the editor | `tests/scripts/*.keys` for the vocabulary; `script.loft::script_run_file` to play one; `snap <name>` for a picture |
| Add a validation scenario | a new `tests/scripts/<name>.keys` + one test in `tests/08_v3_scenarios.loft` (pin its check count — a scenario with its measurements deleted still reports ok) |
| Ask whether two runs are in the same STATE | `src/compare.loft::state_diff` — the first difference, NAMED, or `""` (plan 18 S0).  ⚠ It reads the state field by field and knows nothing about the emitter: define equality as "emit both and compare the text" and S2's round-trip gate is CIRCULAR — green precisely where the tool is broken.  ⚠ Layers are compared by KEY, because hash iteration order is not part of the state.  ⚠ Floats EXACTLY — an epsilon would hide the ulp of drift the gate exists to catch.  ⚠ Its field list is hand-maintained: a new field on a state struct needs a row in `tests/18_s0_the_comparison.loft` or nothing covers it |
| Write a situation down as a `.keys` file | `src/emit.loft::emit_keys` (plan 18 S2) — the ground, the markers and the whole runtime state, as an authored STARTING POSITION with no `tick` in it.  ⚠ Order is load-bearing: `flag` before `tower`, `crew` before an `object` it owns, `place` before `hit`/`stand`/`dead`, `schedule` before `pending`.  ⚠ Gated by capture → emit → replay over all 28 real scenarios, comparing the WORLD as well as the state — terrain is not in `WaveState`, so a state-only comparison is green for an emitter that lost the map |
| Cut a captured situation down to the interesting part | `src/emit.loft::crop_keys` + `crop_fault` (plan 18 S3).  ⚠ **The refusals are NECESSARY and not SUFFICIENT** — dropping the core and cutting under a tower's 15-hex reach are refused, but a LEGAL crop can still change the answer: measured, a radius-15 crop of a band whose spawn marker sits at 18 silently stops every wave, because `SPAWN_DISABLE_RADIUS` and `WAVE_1_PROVOCATION_HEXES` are distances from the CORE.  ⚠ Only running both and comparing certifies a particular crop |
| Cut a fixture down to what a behaviour needs | `src/reduce.loft::reduce_keys` (plan 18 S4) — greedy line removal against a PREDICATE, which is just `.keys` text appended to the fixture: the measurement vocabulary is the predicate language.  ⚠ **A predicate that holds over an EMPTY fixture is refused** — otherwise every line is removable and the reducer is a delete button.  ⚠ The result is 1-MINIMAL (removing any one line breaks it), not minimal: a pair of lines that only matter together survives |
| Play the game in a window | `make play`, pan to the base, press **P** (plan 19 P3).  The crew lands at the core, WASD drives it, and waves arrive on the wall clock.  ⚠ **Nothing of the game is drawn yet** — P4 — so the console echo (a line per tick) is the whole of what you can see, and the camera does not follow the vehicle |
| Turn a state you REACHED into a test | [plans/18](plans/18-scenario-capture/README.md) — the tool is BUILT (S0-S4); the loop to capture FROM now exists (plan 19 P3), and wiring a key to it is plan 19 P5.  ⚠ Emits `.keys` and never a state blob: a saved `WaveState` is a golden of the simulation and *a golden agrees with a shear*.  ⚠ The work is making the vocabulary TOTAL over `WaveState` — enemies, towers, wallet and cargo have no setters today.  ⚠ A crop has a MINIMUM radius set by the mechanics (the core, the 25-hex bubble, a tower's range 15), so a naive one silently changes enemy steering |
| Change what a frame contains | `editor_view.loft::render_editor_frame` — the GL loop and `snap` both draw it, so edit it there, not in `main.loft` |
| Draw an ENTITY, or change what one looks like | [`docs/PARTS.md`](docs/PARTS.md) — a part-tree, and the GEOMETRY is derived from it (plan 20).  ⚠ **Never a shape drawn inline in `editor_view.loft`**: that is the *"second renderer that happens to live in the test harness"* its own header refuses, one layer down.  ⚠ The SIZE is the durable artefact and § D6 gates it against the simulation's constant |
| Ask where the game's CAMERA lives, or why the editor's view is a mode of it | `src/render_camera.loft` (built, plan 21 R1) and [`docs/RENDERER.md`](docs/RENDERER.md) § R1 — moros's `RenderCamera`, ported.  ⚠ `camera_overview` at elevation 89° reproduces the editor's top-down view **to 0.08° of bearing and 0.56% of scale** (`@M022`), so there is ONE camera with two presets.  ⚠ The game's camera belongs on `PlayState`, never on `EditorState.cam` (that is `EditorCamera`, and its zoom is `@D002`) — ⚠ **not built yet**: `@X014` stands and lands in R2, where an eased boom gives the session something to remember |
| Put a hex into the CAMERA's world, or ask which way is up in 3-D | `src/render_camera.loft::lat_to_world` — and it is the ONE place that may negate y.  ⚠⚠ **The camera's world is `+y` NORTH**, where every other metre in dryopea is `+y` SOUTH: that is a CANVAS convention, it is left-handed once `+z` is up, and `mat4_look_at` builds a right-handed basis — so carrying it into 3-D **mirrors the world** and no azimuth undoes it (`@M021`: one of eight azimuths works in the north frame, none in the south).  ⚠ The negation cancels `lat_to_metres`', so the camera's frame is `hex_grid`'s own — a library frame is a WORLD frame and dryopea's is a SCREEN frame |
| Ask why the camera eases, or add a valve to it | `src/render_camera.loft` § The ease (plan 21 R2) and [`docs/RENDERER.md`](docs/RENDERER.md) § R2b.  ⚠⚠ **The approach is `1 − e^(−k·dt)` and moros's `f = k·dt` is REFUSED** — the linear form is frame-rate dependent and `play.loft` is built on the opposite property (`19-P0`), so a linear camera would put a frame-rate dependence into the artefact a gate photographs.  ⚠⚠ **THREE valves ease, not the boom alone**: the vehicle is a lattice position and jumps 1.299 m on the tick it steps, so the target and the azimuth are what make the picture move at all (`@M023`: 12 of 240 frames un-eased, 221 eased).  ⚠ The azimuth eases the SHORT way — A then A+S is a real **−300°** swing otherwise (`@M024`).  ⚠ Rest SNAPS: an asymptote stopped by a tolerance rests wherever the frames fell |
| Ask what shortens the camera's boom, or add an occluder | `src/render_camera.loft::camera_boom_free` over `passable.loft::sight_first_block` — **the same walker `tower_sees` asks** (`@X071`).  ⚠ It answers WHERE rather than whether, because a boom needs a distance and a shot needs a yes/no.  ⚠ The camera reads a HEIGHT and never a kind: a `wall` at the far cell lends the whole boom and a `wall_high` there does not, while ONE HEX OUT both stop it because the ray is only 1.6 m up (`@M024`).  ⚠ The free length is quantised to hex steps and smoothed in TIME; the trigger for a sub-hex march is terrain elevation (plan 02) |
| Ask where the game's camera is REMEMBERED between frames | `PlayState.cam` — a `CameraRig`, which is the live `RenderCamera` plus the boom the PLAYER asked for (`@X014`, `@X070`).  ⚠ **Two booms are two facts**: occlusion lends the eye less, it never rewrites the ask, or a wall the vehicle drove past would shorten the camera for the rest of the run.  ⚠ `play_step` steps it LAST and on EVERY frame — inside `play_advance`'s tick loop it would run on one frame in forty at 60 fps and stutter with the right average |
| Point the camera at the vehicle, or ask which way it is facing | `src/render_camera.loft::camera_follow_vehicle` over `vehicle_facing` — the bearing comes from the **VELOCITY** (`metres(to) − metres(here)`), because a hover unit has no stored facing (`@X067`).  ⚠ It answers a PAIR: plan 19 P2 spells *stop* as `vehicle_drive(v, v.q, v.r)`, so a parked vehicle's velocity is zero and `atan2(0, 0)` would swing the camera east on every key release.  ⚠⚠ **Never paste moros's `azimuth = 270° − facing_deg`** — correct in moros's frame, and in dryopea's it puts the eye exactly ABEAM at all four cardinal headings, where it still tracks and still eases and still looks like a working camera |
| Draw the GROUND, or ask why the terrain mesh does not blend | [`plans/25`](plans/25-the-terrain-mesh/README.md) § What was measured first — ⚠⚠ **the corner-height MEAN is a no-op in dryopea** (`@X072`): `height_override` is non-null on two of twelve palette kinds, so the ground is a flat plane with pillars and the mean changes nothing across ground *or* across a structure's edge.  ⚠ It is honest rather than cheap — the sim asks `can_step`, a height DIFFERENCE, so a sloped mesh would draw a ramp the vehicle cannot climb.  ⚠ The corner↔direction relation is `lattice.loft::lat_edge_corners` over `hex_grid`, delegated and never tabulated (`@X073`) — and it takes no `Hex`, because unlike the neighbour LABEL delta the corner relation is parity-independent.  ⚠ The corner ring winds **counter-clockwise** in the camera's world (two negations cancel), so `GL_CULL_FACE` needs no reversal — and M3 turns culling ON so a reversed winding fails loudly.  ⚠ The trigger to add the blend is [`plans/02`](plans/02-solver-validation-viewer/README.md), and M2's halo gate is the tripwire |
| Add a face to the mesh, or ask why a wall's side is drawn once | `src/ground_mesh.loft::ground_side_faces` and [`plans/25`](plans/25-the-terrain-mesh/README.md) § M1 — one quad per edge where a column stands above its neighbour, `if hh <= nh { continue; }` (`@X046`).  ⚠⚠ **Both halves of that guard fail INVISIBLY**: no guard draws every faced edge twice and the second copy is back-facing (pixel-identical, twice the mesh); `<` instead of `<=` grows a zero-area sliver at every hex boundary in the world (also pixel-identical).  So it is gated as four COUNTS — **6** for a lone wall, **10** for two adjacent, **0** for flat ground, **5 and 6** across a step — and the step fixture is the only one that can see the face drawn by the WRONG side.  ⚠ **Absent is zero**: a sparse sea-default world means a wall at the painted region's edge has a 0 m neighbour and gets its quad.  ⚠⚠ A quad's NORMAL comes from the two hex CENTRES and its WINDING from the corner RING — two facts, computed differently, and the test asserts they AGREE, because normals-out-triangles-in draws nothing under `GL_CULL_FACE` with every normal reading healthy |
| Gate anything that is DRAWN by GL | [`docs/RENDERER.md`](docs/RENDERER.md) § R0 + § R4 — `xvfb` → GL → `gl_screenshot` → `imaging::png` → `classify_world`, measured at **zero** colour drift.  ⚠ Render FLAT UNLIT for the gate: a shaded frame turns one palette colour into a range and `unknown` stops meaning "fault".  ⚠ Never loosen to nearest-colour — that discards the property R0 measured |
| Ask what a tower's top is, in the art | `docs/PARTS.md` § D3 — it is a SOCKET, and the simulation has had one since plan 17 T2 (`tower_detach_top` / `tower_mount_top`, which refuses an occupied tower).  ⚠ Which pose a tower draws in is ASKED of `TowerState`, never a second flag beside it |
| Write/edit a `.loft` file | Loft language conventions: see § Important conventions above + loft's own `loft-write` skill |
| Run the editor | `loft src/main.loft` |
| Author any part of a `WaveState` in a `.keys` file | `src/script.loft::script_author` (plan 18 S1b) — `tower` / `object` / `spent` / `player` / `member` / `pending` / `cursor` cover the layers and the condition fields no play verb reaches, so the vocabulary is TOTAL over the state.  ⚠ They AUTHOR and never simulate: a tower authored black fires nothing and an object authored into the player's hands checks no reach.  ⚠ SEVEN command words rather than one `set` with a subject, because `keys_schemas` keys a coordinate's position on the FIRST token — one `set` row would silently rewrite `set member 0 on 0.5` as if `0 on` were a hex |
| Author ONE enemy in a `.keys` file | `place <q> <r> <class> [heading]` (plan 18 S1a), plus `stand <i> <secs>`, `banked <i> <hexes>` and `dead <i>` for the three fields a placement leaves neutral; `hit <i> <hp>` is the fourth and already existed.  ⚠ A bare `place` is HEALTHY, WALKING and carrying NOTHING — `taken`, `stand` and `progress` are all zero-neutral, and getting any of them backwards spawns a corpse that has not finished arriving while every "the wave is there" assertion stays green.  ⚠ `dead` deposits NO body: it authors the ledger and never the consequence, so `wave_deaths` stays the one death path.  ⚠ `banked` arrived in plan 23 K2b, because `18_s2`'s round trip went red the moment a class walked at a speed whose carry is not zero — until then the field had no setter and nothing in the repo could tell |
| Add a `.keys` verb that takes a hex | `src/script.loft`, AND a row in `src/convert.loft::keys_schemas` + the vocabulary list in `tests/09_c5a_converter.loft`.  A missing schema row is silent: the converter passes an unknown command through untouched |
| Place or restore a marker of any kind | `src/markers.loft::place_marker` (and `history.loft::place_marker_and_record`) — the ONE dispatch.  ⚠ Sidecar load, undo and redo each used to fall through to SPAWN, so a kind they had not learned about arrived as a wave source with a heading |
| Add a marker kind | append a constant in `markers.loft`, bump `MARKER_KIND_COUNT`, add a row to `place_marker` + `marker_kind_name`.  ⚠ The editor's place-kind CYCLE grows, so every `.keys` script that cycles back to spawn needs another press — B5a paid that for nine scenarios |
| Change what a key does | `src/bindings.loft::editor_actions` — the ONE table.  Both the GL loop and every `.keys` script read it, so a change is visible to the gate.  Never add a `gl_key_pressed` |
| Add a PLAY action (a key that drives the game) | a row in `src/bindings.loft::editor_actions`, a field on `EditorInput`, a line in `editor_input_from`'s `playing` branch, and the effect in `src/play.loft::play_actions`.  ⚠ **Never in `editor_step`** — the editor's seam has no roster, and `tests/19_p2_the_keys.loft` § The editor seam is blind to the play fields is what keeps it that way.  ⚠ `tests/09_i1_bindings.loft` pins the table's ROW COUNT, so a new row goes red there by design |
| Ask why WASD does two different things | `src/bindings.loft::editor_input_from` — `playing` fills the pan set OR the drive set (plan 19 P2).  ⚠ `DESIGN.md` § 11 gives movement to WASD and § 12 has the play camera locked, so the two never coexist.  ⚠ Merge them and every `at` in the gate drives whatever vehicle its scenario parked.  ⚠ Who ANSWERS it is `play_mode(ps)` in the window (P3) and the ACTION NAME in a script — a `.keys` file has no mode, which is why `do toggle_play` is refused |
| File an outbound loft request | [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) |
| File a dryopea-internal bug | [PROBLEMS.md](PROBLEMS.md) (`@D<NNN>` convention) |
| Understand library extraction | The `hex_*` family is published — `loft api --registry` |
| Change how enemies move | [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) — the whole spec.  [plans/11](plans/11-flow-field/README.md) is what it costs to build |
| Step a hex coordinate | `lattice.loft::lat_neighbour`.  ⚠ Never a `+ 1` on a `q` or `r`, and never a constant `(dq, dr)` table either — odd-r deltas depend on row parity, so no such table exists |
| Tell GEOMETRY from LABEL SPACE (any coordinate change) | Ask what the site depends on. **Geometry** ("where on screen?") depends on the lattice alone. **Label space** ("which cell?") is only meaningful relative to how the DATA is labelled — `paint_line`, `enemy_tick`, the flow BFS and every `.keys` literal are label-space.  Plan 09 is the worked example: the two had to move in separate phases (C3, then C5), and converting one label-space site alone turns `scripts/validate.sh` red for a reason that is not a defect |
| Ask whether an enemy may MOVE somewhere | `src/passable.loft::can_step` — the rule, an edge.  Never `walk_ground` on its own, and never the destination's height on its own |
| Ask whether an enemy may BE somewhere | `src/passable.loft::can_occupy` — what a position can say with no history.  The measurement's rule; never the field's node filter |
| Raise a hex at runtime (bodies, broken walls) | `src/height.loft` — the rubble layer: a rise above what the palette paints, plus what it is made of.  Lives on `WaveState`, never saved.  ⚠ Shrinking a pile to nothing REMOVES its entry — a 0.0 m pile would still read as a rubble surface |
| Ask what a hex's SURFACE is (vs what is painted on it) | `src/passable.loft::hex_ground` — rubble where a pile stands, the painted kind otherwise.  `painted_ground` is the other half and is what `hex_height` adds the layer to.  ⚠ Need it as a KEY rather than as an entry (grouping, a mesh per kind)?  `hex_surface_index` is the same question given back as the palette index (plan 25 M0) |
| Ask whether a hex is free of enemies | `src/occupancy.loft` — a separate question from passability, and a count rather than a flag |
| Ask who on the PLAYER's side is standing on a hex | `src/occupancy.loft::blocker_at` over the map `spawn.loft::wave_blockers` builds each tick — it answers WHICH vehicle, because the blocker damage has to land on the one in the way.  ⚠ Never a per-vehicle predicate: `vehicle_on` was deleted for being the second door |
| Bring a lost crew member back | `src/spawn.loft::wave_drop` at the core — and NOTHING else does it (`DESIGN.md` § 9: *"retrieval is the only way back"*).  ⚠ The clock is exactly 90 ticks and the epsilon in `helper_recover_tick` is what keeps it 90 rather than 91 |
| Take a crew member out of the run | `src/helper.loft::helper_wreck` — and the tick is the only caller, at the end, beside the deaths and the breaks.  ⚠ It is TWO effects at one site since plan 15 C1: the helper goes down AND a carryable wreck appears where it stood.  ⚠ Nothing brings it back yet: retrieval is plan 15 C2 |
| Pick something up, carry it, put it down | `src/carry.loft` — one record per object with an `owner`, so conservation is structural.  ⚠ Never add a "carried" field to a vehicle beside it: a slot on the carrier and an owner on the object are two facts that can disagree |
| Add a new kind of carryable thing | a `CARGO_*` constant plus what a valid destination is and what arriving there does — and NOTHING in the carrying path.  ⚠ A kind that needs new carrying code has broken `plans/15` § C0.4.  ⚠ **The second consumer showed the contract's edge** (plan 17 T2): a tower-top has TWO destinations and `cargo_destination_ok`'s `(kind, at, core)` can only state one, so the tower-mount half lives in `spawn.loft::wave_drop` where the markers are.  Look in both places |
| Take a tower's top off, move it, or evacuate it | `src/spawn.loft::wave_take` / `wave_drop` (plan 17 T2) — `tower.loft::tower_detach_top` / `tower_mount_top` are the primitives.  ⚠ The magazine is the TOP's, carried as `CarryObject.subj`, so a round trip is not a repair.  ⚠ `tower_mount_top` REFUSES an occupied tower; the hot-swap is COMPOSED out of a detach and a mount at the call site, which is what conserves the count.  ⚠ A loose object on the ground beats detaching, and mounting beats evacuating — both ambiguous presses decided in `plans/17` § T2 |
| Ask what a blocked enemy attacks | `src/spawn.loft::enemy_target` over `flow.loft::flow_desire` — per route, never a global "nearest wall" |
| Ask whether a tower can HIT something | `src/tower.loft::tower_sees` — one straight line from the eye over `hex_height`.  ⚠ Never a "which kinds block" table: a `wall_high` beside the tower does not block and a `wall` near the target does |
| Ask why a tower is not shooting | `src/tower.loft::tower_sight_fault` names the hex, the two heights and how far along the line it sits; `tower_black` is the other answer |
| Bring a spent tower back | `src/tower.loft::tower_repair_tick` — 20 s of a vehicle standing within one hex, banked ON THE TOWER so a relief crew finishes what a lost one started (plan 17 T1).  `spawn.loft::wave_repair` is the tick's turn, at the END beside the salvage.  ⚠ No key is pressed — repair is a POSITION, so a player parked beside a tower is working on it whether it meant to or not.  ⚠ A FIRING tower REFUSES (`DESIGN.md` § 7), which is what makes upkeep a timing decision.  ⚠ It refills the MAGAZINE and never the CHARGE — get that wrong and the base reads exactly like an undefended one |
| Ask how much a wall has left | `src/damage.loft::structure_hp` — max minus taken.  ⚠ 0.0 answers BOTH "broken" and "never a structure"; ask `structure_breakable` first if you need to tell them apart |
| Ask how strong a wall hex is | `src/damage.loft::structure_max_hp` — the kind's figure scaled by `brace_of`.  ⚠ `numbers.json`'s wall_hp (100) is the BRACED number; a lone plug in a corridor is a STUB and gets 15 |
| Break a wall | `src/damage.loft::break_structure` — the one site, and it does both halves.  The tick calls `damage_resolve` AFTER every enemy has moved, so a breach belongs to the NEXT tick |
| Clear rubble / collect after a tower | `src/vehicle.loft::salvage_at` — the rule, taking a HEX, so the player and every helper read one implementation (`vehicle_salvage` / `helper_salvage` are the two doors).  The counter-play to `ENEMY_MOVEMENT.md` § Bodies are terrain.  ⚠ A crew inside a SEALED base can only reach the ramp by BOOSTING out (V4): the ramp forms outside the wall and an idle vehicle climbs 0.4 m — and no helper has a boost |
| Give a mover a climb that changes while it lives | `src/passable.loft::can_climb` — the rule with the climb passed rather than looked up.  ⚠ Never widen `climb_limit(kind)`: it is a CLASS lookup and a convenience for callers that have a kind.  `vehicle_climb` is the worked example |
| Ask what STARTS the wave list | `src/spawn.loft::wave_provoke_step` — a live vehicle standing on a spawn marker `WAVE_1_PROVOCATION_HEXES` (12) or more from the core, read at the TOP of the tick and fired ONCE (plan 16 W3).  ⚠ Two thresholds: under 10 a marker is silenced entirely, 10–11 it sends enemies and cannot be poked, 12+ it does both — the middle band is what makes the distance test a rule rather than a restatement of "is this marker active".  ⚠ Never an occupancy test: a wave spawns ON its marker, so "is anybody here" lets wave 1 provoke wave 1 |
| Ask how far an enemy moves in a tick, or make a class FASTER | `src/spawn.loft::enemy_speed` for the CLASS's rate (plan 23 K2b — scout 2.5, miner 1.0, everybody else 1.5), then `enemy_bank` for what a timestep owes it: `speed × tick_seconds` banked per enemy, whole hexes released to `enemy_step`.  ⚠ **A tick is no longer a hex**: `TICK_SECONDS` HOLDS the timestep at one regular's hex, it does not force it (`@X058`).  ⚠ **Pick a new speed against `@M013`** — 1.5, 2.25 and 3.0 are values at which the rounding guard cannot fire, and 1.0 / 1.2 / 1.8 / 2.0 / 2.5 each lose a hex every forty ticks without the epsilon; 2.5 was picked partly for that (`@X063`).  ⚠ The lookup is at the CALL SITE and not in the bank, because *"a damaged robot moves slower"* makes speed a property of a CONDITION (`@X061`).  ⚠ A hex the ground refuses is SPENT, not re-banked — the opposite of `helper_bank`, and deliberate (`@X059`) |
| Ask why a fresh wave is not moving | `src/spawn.loft::enemy_standing` — the pre-walk window (plan 16 W2), 8 ticks at the marker.  ⚠ Spent ONCE per tick by `wave_stand`, at the END beside `helper_recover_tick`; the predicate only asks.  ⚠ A standing enemy does not move, attacks nothing and blocks nobody — but is NOT immune, which is what "stand visible" means |
| Advance the GAME | `src/play.loft` — `play_ticks(ps, s, n)` for a COUNT, `play_advance(ps, s, seconds)` for elapsed time, `play_step(ps, s, input, seconds)` for a whole frame.  ⚠ Never call `wave_tick` directly: `play_one_tick` is its one caller, and a second one is a second game with the same numbers on it.  ⚠ And never spell a count as `n * TICK_SECONDS` — it is one tick short for 602 of the first 1000 `n` |
| Ask whether a session is LIVE, or start one | `src/play.loft::play_mode` / `play_set_mode` (plan 19 P3).  ⚠ **It gates the CLOCK and never the seam**: `EditorInput.in_playing` says what the KEYS mean this frame, `PlayState.playing` says whether wall time reaches the simulation.  Gate `play_step`'s seconds on either and P1/P2 go red — a scripted frame's time is the SCRIPT's business.  ⚠ The window spends it through `play_frame_seconds`, which is a function rather than an `if` in `main.loft` because an entry point is compiled by nothing |
| Ask whether the run is over | `src/wallet.loft::wallet_broke` — the wallet at zero, and the ONLY end state.  ⚠ Never `core.hp`: it is `null` by design |
| Understand the DIFFICULTY CURVE's shape | [docs/DESIGN.md](docs/DESIGN.md) § It shoots TOWERS — the first real challenge.  Early = a RUSH (volume).  Then the combat boss, which is the first enemy that makes the player POORER rather than merely closer to losing — and the first that invalidates a LEARNED OPTIMUM (the tight funnel that denied a 2×2 repair platform is worthless against something that shoots from outside) |
| Judge what a wave's COMPOSITION is worth | [plans/24](plans/24-the-siege-front/README.md) § W2 and [`docs/ENEMY_MOVEMENT.md`](docs/ENEMY_MOVEMENT.md) § The siege front is the WALL's width — **94 / 101 / 116 / 122 / 126** for twelve robots screened by four of a faster class (`@M020`).  ⚠⚠ **A wave is worth its front class PLUS what the front cannot COVER**: the front is the wall FACE's width, so four screens against five hexes leak exactly ONE miner — worth nothing behind a builder, thirty-nine ticks behind a harvester.  **The screen is arithmetic: bodies against face width.**  ⚠ So price a wave by what is in front of it AND by how much of the face that front can actually cover.  ⚠ [plans/23](plans/23-the-small-robots/README.md) § K3's `@M018` (*a mix is worth its fastest class and no more*, four scouts buying outright immunity) is the SUPERSEDED reading — quote `@M020` |
| Judge whether a DEFENCE is worth building | [plans/12](plans/12-combat-resolution/README.md) § B7 — three scenarios that differ only in their defences, and the measured clock (69 / 112 / 128 since plan 16 W2).  ⚠ A sealed wall nearly doubles it; a wall with a GATE buys nothing at all; and a tower now ADDS 16 ticks where it used to cost 9 — because the pre-walk window moved its kills off the wall's foot, so the ramp that used to bury it no longer forms there |
| Judge whether fetching a lost crew member is worth it | [plans/17](plans/17-tower-hot-swap/README.md) § T3 — **+76 POINTS** over the errand control, on a base with upkeep where nothing falls (~45 / ~41 / ~117 points left).  ⚠ The currency is the WALLET, not the clock: a base that can recover stops falling, so the clock saturates and *points left* is what "how well did you do" means.  ⚠ Earlier readings are history rather than alternatives: [plans/16](plans/16-the-wave-system/README.md) § W4 — **247 / 248 / 248** on a base where the crew member genuinely does come back (tick 187), so it is worth ONE tick.  ⚠ The reason is no longer "the base ends first" (that was [plans/15](plans/15-the-carry-model/README.md) C3's 93/87/87): the JOB is gone by the time they return — the gate is worth 53 ticks while the wave is outside and nothing while it is on the core.  ⚠ The middle run is the control that keeps the drive and the carry apart |
| Judge whether a TRANSPLANT is worth doing | [plans/17](plans/17-tower-hot-swap/README.md) § T3 — **+3 ticks at best, −50 if the donor was firing.**  ⚠ A tower close enough to donate cheaply is close enough to be shooting, which is `DESIGN.md` § The opportunity-cost layer measured.  Its payoff needs swap pits and STRAIN — pulling a top BEFORE it is spent — and neither is built |
| Find out why a base cannot be played to its end | [plans/16](plans/16-the-wave-system/README.md) § W4 — **the tower's 30-shot magazine**.  The authored list is 205 robots = 6150 HP and a tower is 300 HP for the whole run, so seven waves needs 21 perfectly-aimed towers; the best band the suite can build plays FOUR and falls at 321 with every tower black.  ⚠ Nothing lets a base RECOVER between waves, so the lull is a pause rather than a repair window — and that is what blocks retrieval, hot-swap and every mechanic priced across waves.  [plans/17](plans/17-tower-hot-swap/README.md) is the fix |
| Judge what another CREW MEMBER is worth | [plans/14](plans/14-helpers/README.md) § Status — three scenarios that differ only in their crew lines, and the measured clock (⚠ **123 / 135 / 138** since plan 16 W2, from 77 / 214 / 242).  ⚠ A roster buys COVERAGE, not throughput — but the base can now barely express it: the pre-walk window means far fewer ramps form for a crew to clear, so the whole spread is 15 ticks where it was 165 |
| Hurt or kill an enemy | `src/spawn.loft::enemy_hurt` lands damage and never kills; `wave_deaths` (the tick's, after the move loop) is the ONE death path, so B5's tower and a script's `hit` cannot drift.  ⚠ A fatal hit is followed by one last STEP — the tick moves before it kills, so the body lands one hex down the route from where the shot landed |
| Validate the GAME (not a function) | `scripts/validate.sh` — then [plans/08-game-validation/README.md](plans/08-game-validation/README.md) |
| Check a change did not cost anything | `tests/11_f8_the_tick_budget.loft` — a RATIO gate, because a copy changes no behaviour and no other test can see it |
| Make the SIMULATION cheaper | [`plans/22`](plans/22-the-field-cache/README.md) — ⚠ the field, not the roster.  `flow_sweep` is **17.6% self and ~69% with its passability family** (re-profiled 2026-08-17, third reading in agreement), it is UNBOUNDED, and it is only read inside the 25-hex bubble, so ~60% of every sweep is never looked at.  ⚠ The field is a pure function of `(pal, pw, hl, climb, core)` and its invalidation surface is **two functions** (`height_raise` / `height_clear`) plus `paint` — so caching is exact, and `11_f8::test_the_field_a_tick_uses_equals_a_fresh_build` is the gate, written in advance and currently vacuous |
| Judge a simulation-LOD idea (coarser away from the player) | ⚠ **Granularity must NOT follow the CAMERA** — if it does, where the player looks changes the outcome, which is unfalsifiable from inside.  The boundary is the interaction radii (tower range 15, bubble 25, nibble reach 1, salvage reach 1, blocker = same hex), which are stable under camera movement.  ⚠ And ticking distant things every N ticks with N× movement is the `n × TICK_SECONDS` defect again — bank progress, never multiply it.  [`plans/22`](plans/22-the-field-cache/README.md) § What this plan does NOT build carries the trigger |
| Find out what the SUITE spends its time on | `LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` — § Profiling the suite.  Read the op count, never the wall clock |
| Speed up frame measurement further | `src/measure.loft::classify_canvas` is already written for the pixel count — do not "tidy" it.  ⚠ And it is no longer where the time goes: `classify_canvas` + the `Canvas` primitives are **~5%** (2026-08-17), against ~69% for the distance field.  [`docs/PROFILING.md`](docs/PROFILING.md) |
| Find out what the SUITE spends its time on, or optimise anything | [`docs/PROFILING.md`](docs/PROFILING.md) — ⚠ re-profile first and quote the DATE; the reading in this file has inverted once already |
| Look up what a `src/` file owns before editing it | [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — the full listing, plus the key data structures.  ⚠ Each `.loft` file's own header is the source of truth |
| Add a script to the gate | drop a `.keys` in `tests/scripts/` — the sweep finds it.  ⚠ every file there must play GREEN; a run that must FAIL belongs in a test as an inline string |

## Branch policy

### Current phase — pre-game-shippable: commit + push directly to `main`

**Until a runnable game build exists, direct commits to `main`
are the normal flow.**  The repo is small, single-author, and
the cost of branching ceremony outweighs its benefit while the
foundation is being laid.  Commit locally, push when the user
asks — no automatic pushes.

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

- First dryopea-side P-issue gets numerous enough that prose
  references stop being practical (PROBLEMS.md currently has
  one `@D` row; trigger fires somewhere around ~20).
- Documentation count crosses ~25 (currently ~12).
- A specific drift incident makes the manual scan painful.

Until then: keep cross-references prose-form (§ section names)
+ explicit relative-path markdown links.  Run `scripts/test.sh`
before committing — it's the only doc-adjacent automation we
have today (validates tests via assert_golden + the loft test
runner).
