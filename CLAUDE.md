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
| **No wave-1 trigger, no ordering, no tower repair, no beacons and no scramble** | [16](plans/16-the-wave-system/README.md), W3 next |

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

**Suite: 924/924 green under `scripts/test.sh`** (~95 s measured
2026-08-14 — the `frame` measurements classify full 960x720 frames, the
cost gate ticks a radius-40 world twice, and since plan 13 a dozen tests
run whole scenarios to their fall.  ⚠ This line carried "~35 s" from
plan 12 until H2 re-measured it; the figure grew with the scenario
tests, not with any one phase).
**Gate: 25 scripts green under `scripts/validate.sh`** (~10 s, 465
measurements).

⚠ Do not run two `scripts/test.sh` at once — both pre-clean
`tests/actual/`, so they clobber each other and fail for no reason.

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
⚠ **The spread is by APPROACH, not by
sidestepping**: enemies from different directions meet the wall at
different hexes, but four down ONE corridor still queue, because the
desire gradient points AT the wall rather than along it.  Chewing one
face along its length would need an equal-distance sidestep, which is a
second steering rule and nobody has built it.

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
routes cross.  So B3's bracing rule is exact and its consequence is
latent; the missing half is F7's equal-distance sidestep, which is a
second steering rule and still nobody has built it.

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

⚠ **That third trigger is coming, and it is a design decision rather
than a regression.**  A tick is 667 ms only because it is *defined* as
the time an enemy takes to cross one hex, and the design intends to
break that: speed varies by role, by tier, and by CONDITION (a damaged
robot moves slower), so the tick becomes a simulation timestep chosen
on its own merits and every enemy banks movement progress instead.
Pick a shorter timestep for smooth varied speeds and the per-tick
budget shrinks in direct proportion — the rebuild that fits at 667 ms
does not fit at 100 ms.  See `spawn.loft` § What a tick is worth.

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
per-function + per-line + call-path report over every run in the suite.

- ⚠ **The report goes to STDERR.**  A plain `> out.txt` keeps the test
  results and silently drops the profile, which reads as "the profiler
  says there is nothing to see".
- ⚠ **Read the SAMPLE COUNT, not the seconds.**  It is an op counter, so
  it is *deterministic* — two runs of an unchanged suite agree exactly
  (1 421 358 twice, measured).  The wall clock has **~3.5 s of run-to-run
  variance on a ~33 s suite**, so it cannot see a 2.4 s improvement at
  all: `classify_canvas`'s 2.6x landed as 32.6 s → 32.8 s, i.e. inside
  the noise and pointing the wrong way.  Quote the op count.
- `LOFT_NO_NATIVE_LIBS=1` makes no difference here — both ways give
  identical counts, so loft's "a `use`d library is a cdylib the sampler
  cannot see into" inversion trap does not apply to `loft test`.
- ⚠ **`loft test --check` is not a compile-only measurement** — it falls
  through to rustc and took 72 s, twice the suite.  loft's own
  `doc/claude/PERFORMANCE.md` § Profiling a run warns about this.
- `ticks()` is in **microseconds** (`default/02_files.loft`), so a probe
  that prints it as ms overstates by 1000x.

**Where the time goes (profiled 2026-08-12, 13.7 s interpreted of ~33 s
wall — the rest is per-file process start + ~37 ms/file codegen):**

⚠ **58% is `graphics`'s `canvas()`**, and it is not dryopea's to fix.
`graphics.loft:45` is `[for _ in 0..cw * ch { fill_color }]` — 691 200
elements at 3 bytecodes each, ~231 ms per 960x720 canvas, ~35 canvases a
run.  loft's `PERFORMANCE.md` § O8 describes exactly this cost but **O8.5
would not fix it**: `cw * ch` is a runtime value, and 691 200 exceeds its
hard 10 000-element unroll limit.  The gap is a *runtime* bulk fill
(`zero_fill` / `copy_block` already exist in the store) and no design on
that page covers it.  Worth ~7.5 s, i.e. 21% of the suite — the largest
remaining win, and an upstream ask against `loft-libs-graphics`.

All of plan 11 — `flow_sweep`, `hex_neighbor`, `hex_ground`,
`hex_height` — is under 15% put together.

⚠ **`ticks()` is loft's clock builtin — never shadow it**, not even as
a parameter name.  A probe that took `ticks` as a parameter compiled
clean and reported a tick 4x cheaper than it was; the same trap `now`
sets, and a blind stopwatch is worse than none because it fails in the
reassuring direction.

⚠ **A struct RETURNED from a function is a COPY, so mutating it is a
silent no-op.**  `hurt(first(state), 10)` — where `first` returns the
roster's element — lands 0 damage; indexing the vector inline
(`state.enemies[0] ?? Enemy {}`) lands it, and so does a `for e in
state.enemies` loop variable.  Measured all three, plan 12 B4.  It
compiles, it type-checks, there is no warning, and the read-back looks
like the mutation never happened — which reads as a bug in the thing
being mutated rather than in the accessor.  A one-line "get me the
element" helper is fine to READ through and never to write through.
Filed as [loft#894](https://github.com/loft-lang/loft/issues/894); the
ask there is the missing `lost-write`, not the value semantics.

⚠ **A struct returned through TWO nested tail calls loses what its
loop wrote** — 1 cell interpreted, 0 native, silent on both
([loft#880](https://github.com/loft-lang/loft/issues/880)).  It bites
when an algorithm is factored out of a function into a shared helper,
because every CONSUMER's one-line wrapper then becomes the second tail
call — so the defect appears at call sites nobody edited.  Bind the
inner call to a local.

⚠ **Never index a call's result in TAIL position** — `steps(a, b)[0] ??
fallback` as a function's last expression reads the absent sentinel, so
it answers the fallback on the interpreter and PANICS on native
(loft#877).  Bind the call to a local, then index it.  It bites hardest
where the fallback is a sane default, because a function that returns
only its default still looks like a working function.

⚠ **Never interpolate a struct that has a `hash` field** — `"{f}"`
SIGSEGVs the interpreter (loft#873) and exits silently on native.
It bites hardest inside an assertion message, where it replaces the
diagnostic of a failing test with a crash three lines from the real
site.  Format the fields: `{flow_count(f)}`, never `{f}`.

Plan 06 (editor-to-stencil pipeline) is drafted and waits on the
shared substrate.  The full design lives in [`docs/DESIGN.md`](docs/DESIGN.md);
the fiction in [`docs/SETTING.md`](docs/SETTING.md); the full
feature roadmap in [`plans/ROADMAP.md`](plans/ROADMAP.md).

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

# Run the interactive editor (E1-live; opens a 960x720 GL window).
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

```
src/
  dryopea.loft     library aggregator — `use dryopea;` brings every
                   submodule into scope (tests use this entry)
  bindings.loft    the ONE key table (plan 09 I1) — EditorAction
                   {name, keys, ctrl rule, palette index} +
                   editor_actions() + editor_input_from(), the single
                   door from keys to the seam.  The GL loop polls it
                   and a `.keys` run FEEDS it, so `do undo` presses
                   the keys a player presses and a wrong binding
                   fails the gate.
                   ⚠ ELEVEN palette hotkeys over a TWELVE-entry
                   palette: `rubble` is deposited by the runtime and
                   painted by nobody, so plan 12 B1 deleted the `=`
                   binding it would otherwise have had.  An authored
                   rubble hex is a second representation of a pile that
                   `height_clear` could not take away.
                   ⚠ The ctrl rule is DATA, not resolver code:
                   `input::ActionBinding` has no modifier concept,
                   and a rule written once in the resolver and once
                   in the runner is a second table wearing a hat.
                   ⚠ EDGES are NOT here — plan 08 V0 put edge
                   detection in the seam and I1 kept it, so this
                   reads LEVEL state and `input`'s `is_action_just_*`
                   are deliberately unused.  Two edge detectors is
                   the drift this file exists to prevent
  main.loft        interactive editor entry point — `fn main()`,
                   NOT in the aggregator (runs via `loft src/main.loft`).
                   The GL shell only: open window, poll input,
                   call the seam, render.  Parse-check it by hand
                   after every edit — `scripts/test.sh` can't see it
  editor_step.loft the input seam (plan 08 V0) — EditorState (all
                   session state) + EditorInput (one frame of intent)
                   + editor_step(s, input).  EVERY action runs through
                   it.  No GL and no clock, ever; disk only via the
                   save / reload actions, and only when a path is
                   attached (editor_state_attach).
                   ⚠ `s.prev` is READ-ONLY for the whole of a step and
                   written ONCE at the end.  It records a frame that
                   already happened, so a mid-step write does not
                   cancel an edge — it FORGES one for every branch
                   below it.  That was @D001: four writes clearing
                   `prev.in_mouse_left` to "drop a held button" made
                   Tab / Ctrl+R / Ctrl+N place a marker the player
                   never asked for.  An action that wants to end a
                   gesture sets the GESTURE's state (`s.painting`),
                   never the input history
  editor_view.loft render_editor_frame(s, w, h, ppm) -> Canvas —
                   what the player sees, composed ONCE: world, hover
                   preview, markers, ghost, picker, save indicator,
                   mode badge.  Both the GL loop and the script
                   runner's `snap` ask for it, so a shot is the
                   editor's frame and not a harness renderer's.
                   Also owns VIEW_W / VIEW_H / VIEW_PPM (the window
                   size IS the shot size).  Never mutates the state
  measure.loft     frame measurement (plan 08 V2) — classify_canvas
                   / classify_world -> FrameCounts.  Reads the WORLD
                   layer, never the composited shot (the HUD puts a
                   floor under every bucket — V2p).  Classification
                   is an EXACT lookup, not nearest-colour: the
                   rasteriser does not blend, so a pixel that is not
                   a palette colour lands in `unknown` and is a
                   FAULT.  The colour table comes from render.loft's
                   `palette_color` — the function that drew the
                   pixels — with palette.json drift caught by its
                   own test
  validate.loft    the gate (plan 08 V4) — validate_all(scripts_dir,
                   shots_dir, palette[, only]) -> ValidateReport.
                   Sweeps a directory of `.keys` scripts, plays each
                   in a session of its own, sums the measurements and
                   reports the FIRST failure with the number that
                   moved.  Refuses to be green over nothing: no
                   palette, no directory, no scripts, or no
                   measurements taken are each a named failure
  validate_main.loft  the gate's entry point — `fn main()`, NOT in the
                   aggregator (runs via `scripts/validate.sh`).  Six
                   lines, no decisions: a file carrying `#cwd` cannot
                   be `use`d as a library, so anything written here is
                   compiled by nothing.  Parse-check it by hand
  script.loft      the `.keys` script runner (plan 08 V1) —
                   script_run(s, source[, shots_dir]) /
                   script_run_file(s, path[, shots_dir]) -> ScriptRun.
                   Commands name ACTIONS, never keys
                   (`do toggle_mode`); ⚠ `do Tab` must keep FAILING —
                   a key name that starts working means a second
                   table was built.  Since plan 09 I1 the runner
                   TYPES on `bindings.loft`: an action name becomes
                   key codes, which go through `input`, which the
                   same resolver the GL loop uses turns back into an
                   EditorInput.  The round trip looks pointless
                   written down and is the whole point — before it, a
                   binding could be wrong in the editor with all 14
                   scripts green.  Reaches the editor ONLY
                   through editor_step — even `at` walks the camera
                   with pan frames.  An unknown command / action /
                   number / arity is an ERROR, never a skipped line.
                   `snap <name>` writes <shots_dir>/<name>.png
                   (default `shots/`, gitignored) and CHECKS what
                   save_png answers.  V2 added the measurements
                   (count / kind / marker / frame — each ASSERTS and
                   ends the run when out of band) plus `wave` /
                   `tick`; WaveState lives on ScriptRun, not on
                   EditorState — an edited session has no enemies.
                   V3 added `range <lo> <hi>` (how far the live
                   enemies are from the core — a SPAN, because a
                   walking wave is strung out) and the five scenario
                   scripts in `tests/scripts/`.  Plan 11 added `enemy
                   <i> <q> <r>` / `enemies passable` (F1) and `enemies
                   distinct` (F5c — no two live enemies on one hex,
                   RED until a freshly-spawned wave has walked apart).
                   F6 added `wave <n> [class]` (robot / insect — an
                   unknown name is an ERROR, because a script that
                   silently got robots would assert the opposite of
                   what it says) and `raise <q> <r> <metres>`, which
                   piles runtime height onto a hex the way a body does.
                   Plan 12 B1 gave `raise` an optional `[source]`
                   (wreckage / carapace / masonry — named, never
                   numbered, and a typo is an ERROR) and added
                   `clear <q> <r>`, which takes a pile away and is how
                   a run states the layer's defining property.
                   F7 added `target <i> <q> <r>` and `count targets
                   <lo> <hi>` — the SET of hexes under attack, which is
                   the only measurement that can tell a spread siege
                   from one collapsed onto a single chokepoint, and the
                   only enemy measurement that does not depend on
                   spawn order.
                   Plan 12 B2 added `damage <q> <r> <hp>` (which cannot
                   BREAK anything — only a tick does, so it stays one
                   code path) plus the `hp` and `pile` band
                   measurements; B4 added `hit <i> <hp>`, the same rule
                   for an ENEMY — a separate verb rather than an
                   overload, because `damage 4 10` reading as either a
                   hex or an index is a line whose meaning depends on
                   knowing which.  ⚠ `hp` over a hex with nothing
                   breakable on it is an ERROR, because "at 0 HP" and
                   "no wall here" are the two states a break moves
                   BETWEEN and one number for both is green before the
                   siege and after the wall is gone.
                   B6 added `wallet <lo> <hi>` — POINTS LEFT, so
                   `wallet 0 0` is how a run says "the base fell".
                   ⚠ It needs no core marker and no wave: the budget
                   belongs to the RUN, not to the battlefield, so 200
                   is the honest answer before a single enemy exists.
                   Plan 15 C2 added `take <who>` / `drop <who>` (`player`
                  or a crew index) plus the `cargo` and `roster` bands.
                  ⚠ **Two verbs for `DESIGN.md` § 11's ONE key** — the
                  same choice `park` / `drive` made: the key is
                  context-resolved on the carrier's state, and a script
                  SAYS what it means so a line asserting a pickup cannot
                  quietly have been a deposit.  What a script does NOT
                  get to decide is where the cargo ends up: `drop` at
                  the core retrieves where `drop` one hex out merely
                  puts down.
                  ⚠ `roster` counts crew STANDING, not enrolled —
                  `len(crew)` never falls (a wreck keeps its slot) and
                  `helper <i> <q> <r>` is true whether it is standing
                  there or lying there, so neither can see a loss.  A
                  crew member in RECOVERY is not standing either, which
                  is what makes the 60 s visible to a script.
                  B7 added `fall <max>` (tick until the wallet empties
                   — ⚠ still standing after `<max>` is an ERROR, or a
                   later `ticks` band would read a collapsed premise as
                   a measurement) and `ticks <lo> <hi>`, the run's
                   CLOCK, which `ScriptRun` now carries
                   ⚠ **A new coordinate-carrying verb needs a row in
                   `convert.loft::keys_schemas`**, or a future lattice
                   conversion leaves it in the old labels — silently,
                   because an unknown command passes through unchanged.
                   `tests/09_c5a_converter.loft` § The schema is
                   complete is the gate, and it only fires if its
                   vocabulary list is updated too
  lattice.loft     THE lattice (plan 09) — pointy-top odd-r offset, the
                   convention every hex_* library and moros speak.
                   Owns `Hex { q, r }` (q is a COLUMN, r a ROW),
                   HEX_DIAMETER = 1.5m, HEX_FLAT_TO_FLAT, and the
                   `lat_*` verbs: lat_neighbour(s), lat_direction,
                   lat_distance, lat_line, lat_disc, lat_to_metres /
                   lat_from_metres, lat_corner_*, lat_to/from_axial.
                   ⚠ `src/world.loft` and its axial arithmetic are
                   DELETED (C6).  The `lat_` prefix is a scar from the
                   period when both existed; it stays because every
                   call site reads it.
                   ⚠ It DELEGATES to `hex_grid` — never a second
                   implementation, which is what makes the lattice
                   right by construction rather than by two copies
                   agreeing.  It adds only what the library cannot
                   know: dryopea's `Hex` type and dryopea's METRES
                   (one hex_grid unit = 0.75 m = one circumradius).
                   ⚠ `hex_offset` has NO counterpart — in odd-r the
                   neighbour delta depends on row parity, so a constant
                   (dq, dr) table does not exist.  The operation is
                   deleted by the conversion, not translated.
                   ⚠ `hex_grid::hex_round` answers AXIAL, not offset —
                   `lat_from_axial` is what stops that shearing a cell
                   silently.
                   ⚠ The metre conversions NEGATE y, because dryopea
                   follows hex_grid's COMPASS and hex_grid's +y is
                   north while dryopea's is south.  So dir 5 really is
                   NE on screen — and existing maps will render
                   vertically mirrored, which is the accepted cost.
                   The metre round-trip cannot see this (a consistent
                   flip is invisible to it); the compass sign test is
                   what gates it
  relabel.loft     old label → new label (plan 09 C2) — the bijection
                   from every axial coordinate dryopea ever wrote to
                   disk to its odd-r offset name, plus the direction
                   permutation `new = (old + 5) % 6` (DERIVED from
                   geometry, uniform on both row parities).  C5 runs
                   it over the real files.
                   ⚠ The invariant is DISTANCE, not adjacency — a
                   relabel can keep neighbours neighbours and still
                   fold the plane, and two painted hexes landing on
                   one is silent.  Distance implies adjacency AND
                   injectivity, and is what keeps plan 11's flow-field
                   distances still.
                   ⚠ The picture moves by a mirror AND a 60° hex
                   rotation — flat-top → pointy-top is itself a
                   re-orientation.  Old dir 0 was due SOUTH; it
                   relabels to new dir 5, which renders NORTH-EAST.
                   A converted map does not look "upside down"
  camera.loft      ⚠ pan NORTH is `r += 1` since plan 09 C3 — north is
                   LARGER r in the new lattice, the opposite of axial.
                   `script_walk_camera`'s convergence test must agree,
                   or every `at` fails as "more than 4096 camera steps
                   away" rather than as anything naming the cause.
                   EditorCamera { pos: Hex, zoom: integer }
                   + InputState (moros-style: factories + pure tick
                   + struct of booleans)
                   + camera_update(c: &EditorCamera, input: InputState)
  painted.loft     PaintedHex { q, r, kind: u8 }
                   + PaintedWorld { painted: hash<PaintedHex[q, r]> }
                   + paint(), lookup_painted(), paint_line()
                   (sea-default sparse storage — un-painted hex is sea)
  palette.loft     GroundType { name, color, sub_palette, slope, drop,
                   drainage, walk_*, buildable }
                   + load_palette(path) via `text as vector<GroundType>`
                   + parse_hex_color() + GROUND_RUBBLE (11) — the one
                   palette index dryopea's code names, because the
                   RUNTIME produces it.  ⚠ APPENDED, so 0-10 are
                   unsheared: an index is an identity, it is what
                   `painted.loft` stores and `MapFile` round-trips.  `slope` / `drop` /
                   `height_override` are declared NULLABLE because
                   palette.json writes null in them — see the file's
                   own warning
  damage.loft      what a structure has TAKEN, and what happens when it
                   has taken enough (plan 12 B2) — DamageLayer +
                   damage_apply / damage_taken / damage_clear / count,
                   structure_max_hp / structure_breakable / structure_hp,
                   rubble_height_of, break_structure, damage_resolve,
                   plus B4's `enemy_max_hp` / `body_source` / the
                   `BODY_HEIGHT_METRES` a death drops and B5b's
                   `ENEMY_HEIGHT_METRES` / `enemy_height` (the class→
                   number tables live here; the per-enemy verbs live
                   in `spawn.loft`, where `Enemy` is).
                   ⚠ **A body is 0.5 m and a STANDING robot is 1.0 m**,
                   and both numbers are here because they are the same
                   robot in two states.  The body height is the unit
                   the ramp band is counted in, and at 1.0 m two and
                   four bodies land exactly on the band's endpoints
                   with no interior; the standing height is what a
                   tower aims at, and aiming at the FEET instead puts
                   the canonical shot exactly on the LOS boundary.
                   ⚠ It stores damage TAKEN, not HP remaining, for the
                   reason `height.loft` stores a RISE: a miss has to
                   mean something useful, and "HP remaining" reads as
                   ALREADY BROKEN on a sparse map.
                   ⚠ A break is TWO effects and `break_structure` is
                   the ONE site that does both: the wall is REMOVED
                   (repainted to `BROKEN_GROUND`, and that edits the
                   painted world — a broken wall really is gone) and a
                   heap of masonry is DEPOSITED (runtime, clearable).
                   Never ERASE the hex: the painted layer is
                   sea-default, so an erased breach is less passable
                   than the wall it replaced.
                   ⚠ Max HP is keyed on the palette NAME, not the
                   index — an index is storage, a name is what a
                   modder edits around.
                   B3 added `brace_of` / `brace_name` / `brace_factor`:
                   `structure_max_hp` is `structure_base_hp` times how
                   the structures AROUND a hex hold it up.  ⚠ It is
                   computed from the world and never stored, which is
                   what makes a perimeter UNZIP from a breach.
                   ⚠ `structure_breakable` asks the BASE figure and has
                   to — `brace_of` asks it of all six neighbours, so
                   routing it through `structure_max_hp` is an infinite
                   recursion.
                   ⚠ **Only a ROW is straight** — two neighbours brace
                   along one line when their direction indices differ
                   by exactly 3, and odd-r row parity means a
                   constant-`q` COLUMN zigzags and reads as braced

  tower.loft       what a TOWER is (plan 12 B5a + B5b) — the numbers
                   (range 15 hex, 1.0 s interval, 10 HP a shot, 6.0 m
                   tall, 30 shots), TowerState + tower_charge /
                   tower_bank / tower_hold / tower_shots /
                   tower_spend_shot / tower_black / tower_budget_left /
                   count, tower_in_range, and B5b's tower_eye /
                   tower_sees / tower_sight_fault.
                   ⚠ Range is `lat_distance` and
                   NOTHING else; a `+ 1` on a q or an r reaching for it
                   is moros#10 again.
                   ⚠ A tower BANKS charge rather than firing per tick,
                   because a 1.0 s interval is 1.5 ticks and B5b has to
                   COUNT shots against a 30-shot budget.  A shot
                   SUBTRACTS an interval, never resets — and the
                   comparison needs `TOWER_CHARGE_EPSILON`, because
                   `1/1.5` has no exact float form and a bare `>=`
                   silently drops every third shot.
                   ⚠ **LOS is ONE straight line and no table.**
                   `tower_sees` runs `lat_line` from the eye (the
                   tower's hex plus 6.0 m) to the target's top and
                   refuses any hex whose `hex_height` rises above the
                   ray.  Both ENDPOINTS are skipped — a tower on a wall
                   is not blinded by its own hex — and the comparison
                   needs `TOWER_SIGHT_EPSILON`, because the canonical
                   geometry lands exactly ON the boundary.
                   ⚠ **Do not add a "what blocks" lookup.**  A
                   `wall_high` beside the tower does NOT block and a
                   `wall` near the target DOES; the kind never decides
                   on its own.  `tests/12_b5b_los_budget.loft` § The
                   difference is the HEIGHT fails both ways round if
                   anyone tries.
                   ⚠ Shots FIRED, never shots remaining — zero is the
                   neutral value, so a tower nobody ticked is ready
                   rather than black (the same choice `damage.loft`
                   makes).
                   ⚠ `tower_pick` and `wave_fire` are in `spawn.loft`,
                   where `WaveState` is: this file must not depend on
                   the wave engine, because the tick calls INTO it

  vehicle.loft     the PLAYER (plan 13 V1) — VEHICLE_SPEED_HEX_PER_SECOND
                   (3.0, twice an enemy), Vehicle + vehicle_empty /
                   _place / _drive / _present / _arrived /
                   _hexes_per_tick / vehicle_tick.
                   ⚠ **Two hexes a tick, and it is a RATE** — the tick
                   is DEFINED by an enemy's speed, so "one hex per
                   tick" is what every other mover does and would
                   silently halve the player.
                   ⚠ **No passability code here.**  A hover unit's
                   climb is its clearance (`CLIMB_VEHICLE`, 0.4 m) and
                   the player is a third KIND in `passable.loft` —
                   `walk_vehicle` is true for all twelve palette
                   entries, so the height step is its whole
                   passability.
                   ⚠ **It DRIVES, never routes** — `lat_line` to where
                   it is pointed, stopping at the first refused step.
                   A flow field would be the machine choosing the way,
                   which `DESIGN.md` § 11 rejects.
                   ⚠ `vehicle_tick` takes the tick's DURATION as a
                   parameter: `TICK_SECONDS` is in `spawn.loft`, which
                   `use`s this file.
                   V2 added `vehicle_salvage` — one dead robot a second
                   (`VEHICLE_SALVAGE_METRES_PER_SECOND`, derived from
                   `BODY_HEIGHT_METRES`), no key pressed, because
                   clearing is a POSITION.
                   ⚠ **Reach is 1 and it is FORCED**: a hover unit
                   climbs 0.4 m and the ramp that beats a tower is
                   1.5 m, so it CANNOT stand on what it must clear.
                   ⚠ ONE heap a tick, the deepest in reach — clearing
                   is meant to take time you do not have.
                   ⚠ It takes a BITE via `height_raise`, never
                   `height_clear` (which still has no caller).
                   ⚠ Since plan 14 H2 the rule itself is `salvage_at`,
                   which takes a HEX — the second half of the shared
                   chassis, after H1's `drive_along` — so the player
                   and every helper clear one implementation.  There is
                   no helper salvage RATE: `numbers.json` § helper has
                   none, and a second constant would be a tunable the
                   plan invented.
                   V4 added boost: `vehicle_boost` / _boosting /
                   _boost_ready / _climb / _speed, four hexes a tick and
                   a 3.0 m climb for 2 s, then 5 s of cooldown armed as
                   the boost EXPIRES.
                   ⚠ **Boost is not a movement mode** — it is the same
                   height rule with a bigger number, and 3.0 m is
                   EXACTLY a `wall`, so a `wall_high` still refuses it.
                   ⚠ It calls `can_climb`, never `can_step`: the climb
                   is the vehicle's STATE, not its class.
                   ⚠ `VEHICLE_TIMER_EPSILON` — 2.0 s over a 1/1.5 s
                   tick is three ticks that sum to 1.9999999999999998,
                   so a bare `> 0.0` gives a FOURTH tick of boost
  helper.loft      the NPC crew (plan 14 H1) — HELPER_SPEED_HEX_PER_
                   SECOND (2.5), _HP, _ROSTER_START / _CAP, Helper +
                   helper_new / _drive / _arrived / _hp / _hurt /
                   _bank / helper_tick.
                   ⚠ **The first mover whose speed does not fit the
                   tick**: 2.5 hex/s is 1.667 hexes, so it BANKS
                   progress and steps the whole hexes out — the pattern
                   `tower.loft` and plan 13 V4 already use, arriving a
                   third time and LOCAL to the mover that needs it.
                   ⚠ **This is NOT "the tick becomes a timestep"** —
                   that warning is about a SHORTER tick, and F8's
                   budget trigger does not fire.
                   ⚠ `HELPER_PROGRESS_EPSILON` is worth 6.7% of the
                   speed, compounding: without it the carry sits on
                   0.99999999999999956 and a hex is deferred for ever.
                   The gate is the 1-2-2 step PATTERN, because both
                   wrong versions still arrive.
                   H2 added `helper_salvage` — the player's clearing
                   rule, on the player's chassis, done by an NPC — and
                   a crew turn in `wave_tick` that earns into the RUN's
                   wallet.  ⚠ It adds no mechanic at all, which is what
                   makes it a gate on the ROSTER rather than on a job.
                   ⚠ **The gate is a RATE**: a crew that is not in the
                   tick and a crew sharing ONE vehicle's bite both
                   empty the heap and both read exactly like one
                   helper, so "the rubble is gone" cannot see either.
                   H3 added `helper_wreck` / `helper_wrecked` — and
                   `alive` IS the wreck, because every verb already
                   asks it, so a downed crew member stops driving,
                   clearing and BLOCKING at once.
                   ⚠ **Nothing puts it back BY ITSELF**, and that is the
                   one rule where a helper is not the player's chassis
                   doing the player's job: `vehicle_respawn` is three
                   lines away in the tick and reads the opposite way.
                   The roster slot is KEPT (never compacted), which is
                   what retrieval needed.
                   Plan 15 C2 added the way back: HELPER_RECOVERY_
                   SECONDS (60.0), HELPER_TIMER_EPSILON, `helper_
                   recovering` / `_lost` / `_begin_recovery` / `_recover_
                   tick`.
                   ⚠ **60.0 s is EXACTLY 90 ticks and a bare `> 0.0`
                   gives 91** — the epsilon trap's FOURTH appearance and
                   its least visible.  The discriminator inverts the
                   intuition: the 5.0 s boost cooldown is 7.5 ticks and
                   is IMMUNE, so the trap fires only where the timer
                   divides the tick exactly — the case that looks
                   safest.  ⚠ And the two conditions over the timer must
                   AGREE: an epsilon in `helper_recovering` but not in
                   the exit test stalls the clock for ever.
                   ⚠ `helper_wrecked` is true during RECOVERY too (a
                   crew member at the core is not driving, clearing,
                   earning or blocking either); `helper_lost` is the one
                   that means *still out there and needs fetching*
  carry.loft       what a vehicle is HOLDING (plan 15 C1) — CARGO_WRECK
                   / CARGO_NONE / CARGO_GONE, CarryObject + CargoLayer +
                   cargo_empty / _spawn / _count / _slots / _held_by /
                   _carrying / _on_ground_near / _get / _owner / _take /
                   _put / _consume / _spill / _follow / _owned_by /
                   _slot_fault.
                   ⚠ **NOT a hash keyed by hex**, and it is the only
                   runtime layer that is not: two carry objects on one
                   hex is REACHABLE (a helper carrying a downed
                   colleague is itself destroyed), and a hash answers
                   with one of them while the other is a crew member
                   deleted with no fault raised.  A vector with stable
                   slots, never compacted — `WaveState.crew`'s shape.
                   ⚠ **Conservation is STRUCTURAL, not maintained**: ONE
                   record with an `owner` field, where "on the ground"
                   is a VALUE of that field rather than a different
                   place to be.  A pickup is a single assignment, so
                   duplication is unrepresentable — the move
                   `damage.loft` makes with *damage TAKEN* and
                   `wallet.loft` with *points SPENT*.  A slot on the
                   carrier PLUS a ground layer makes a pickup two
                   writes, and every path doing one of them duplicates
                   or destroys.
                   ⚠ Owner ids are `occupancy.loft`'s BLOCKER
                   vocabulary — `BLOCKER_NONE` (-1) IS the ground — and
                   never a second numbering, which is the door H3
                   deleted `vehicle_on` for.
                   ⚠ **A KIND is data, not a code path** (the enemy
                   rule): what varies per kind is only what a valid
                   destination is and what arriving there does.  A
                   tower-top or a beacon that needs new CARRYING code
                   has broken the contract in `plans/15` § C0.4.
                   ⚠ `cargo_consume` is the ONE way out of the world;
                   a carrier that DIES calls `cargo_spill` instead, or
                   dying becomes a free retrieval.
                   C2 added the destination half: CARGO_REACH_HEXES,
                   `cargo_destination_ok` (a wreck goes to the CORE and
                   nowhere else) and `cargo_deliver`.
                   ⚠ ONE reach for both halves, because § 11's key is
                   ONE key — two reaches would make it mean two
                   distances depending on what the vehicle happens to
                   hold.
                   ⚠ An unknown kind has NO destination rather than
                   every destination, or a kind added without a rule
                   would be depositable anywhere and consumed silently.
                   ⚠ `cargo_deliver` does NOT apply the effect: what
                   arriving DOES needs the roster, and a carry model
                   that knew about helpers could not serve tower-tops.
                   `spawn.loft::wave_arrived` is the other half
  wallet.loft      the run's budget, and the only END STATE dryopea
                   has (plan 12 B6) — WALLET_STARTING_POINTS (200),
                   NIBBLE_POINTS_PER_SECOND, NIBBLE_REACH_HEXES,
                   Wallet + wallet_new / wallet_left / wallet_spent /
                   wallet_drain / wallet_broke, and nibble_in_reach.
                   ⚠ **The core is invulnerable** (`numbers.json` §
                   core.hp is `null`), so "the heart is destroyed" is
                   spelled the WALLET reaches zero.
                   ⚠ **Reach is a straight-line `lat_distance` of 1**,
                   derived from `core.footprint_layout` — the core is a
                   radius-1 disc, so an enemy within one hex is
                   standing ON it.  Draining for every live enemy
                   passes every rate-and-floor assertion while making
                   walls and towers pointless; that is what
                   `tests/12_b6_wallet.loft`'s perimeter test refuses.
                   ⚠ Points SPENT, never points left — the same
                   zero-is-neutral rule `damage.loft` and `tower.loft`
                   keep, and here it stops a `Wallet {}` literal from
                   starting the run already over.
                   ⚠ The clamp is on the WRITE, not just the read: a
                   ledger allowed past the budget would swallow the
                   first loot credit whole.
                   ⚠ `wallet_earn` (plan 13 V3) is the ONLY income, and
                   "the wallet never refills unattended" still holds —
                   because its only caller needs a VEHICLE, not because
                   the verb is missing.
                   ⚠ `loot_rate` reads the rubble SOURCE: wreckage and
                   carapace pay, MASONRY pays nothing, or demolishing
                   your own wall would be an income stream.
                   V5 added blocker damage — `vehicle_hp` / _hurt /
                   _respawn plus `VEHICLE_HP_BLOCKER` — and
                   `spawn.loft::enemy_blocked_by` is the rule.
                   ⚠ `vehicle_on` is DELETED (plan 14 H3): "who is
                   standing on this hex" is the whole crew's question
                   now, and `occupancy.loft`'s `BlockerMap` is its one
                   door — a per-vehicle predicate beside it is the one
                   a future caller would reach for.
                   ⚠ **Blocking is a property of the MAP**: an enemy
                   with a sidestep goes round and nobody is hurt, so
                   the player is only a liability in a chokepoint.
                   ⚠ A COMPANION blocking the same hex is never
                   attacked — which is why this needed its own
                   predicate rather than reusing occupancy
  flow.loft        the distance field (plan 11 F2) — flow_build(pal,
                   pw, kind, core) -> FlowField, a BFS out from the
                   core over what that CLASS can occupy, plus
                   flow_distance / flow_reachable / flow_count, and
                   flow_step (F3): which neighbour is closest to the
                   core, COMPUTED from the distances and never stored
                   — F5c needs the ordering over all six neighbours
                   at move time, which a baked direction cannot give.
                   Ties break by lowest direction index, because a
                   scripted run has to be repeatable.
                   ⚠ no-route is FLOW_UNREACHABLE, a LARGE value:
                   0 means "at the core", and every "closest
                   neighbour" search must refuse a routeless cell
                   rather than prefer it.  Built from lattice.loft's
                   neighbour relation only, which is what kept it
                   independent of plan 09's conversion.
                   `flow_steps` (F5c) is the same answer as a LIST —
                   every strictly-closer neighbour, best first — which
                   is what the mover reads so it can skip an occupied
                   one.  In a BFS field every entry is at `d - 1`, so
                   the ordering is direction order alone.
                   `flow_desire` (F7) is the SAME sweep with the climb
                   lifted (`FLOW_CLIMB_ANY`) — where an enemy wants to
                   go when it has no route.  One field for every class,
                   because the class only ever contributed its climb
  height.loft      the RUBBLE layer — what runtime has piled on the map
                   (plan 11 F6, named by plan 12 B1).  HeightLayer +
                   height_raise (metres AND a source) / height_clear /
                   height_rise / height_piled / height_source / count,
                   plus RUBBLE_WRECKAGE / _CARAPACE / _MASONRY.
                   A sparse map of metres ADDED to what the palette
                   paints, so a pile on grass and a pile on a wall are
                   one arithmetic.  ⚠ It ACCUMULATES (bodies do) and a
                   negative rise floors at the ground.
                   ⚠ **An entry means a PILE**: shrinking one to nothing
                   REMOVES it, exactly as `painted.loft` removes a hex
                   painted back to sea.  A zeroed-but-present entry
                   would leave a hex standing on debris that is not
                   there — over water that is a hole in the sea.
                   ⚠ The source is one per hex and the NEWEST deposit
                   names the pile; a withdrawal leaves the name alone.
                   Nothing reads it back yet — B2 and B4 are its three
                   producers.  Runtime state: it rides on `WaveState`
                   and never reaches a save
  occupancy.loft   who is standing where, this tick (plan 11 F5c) —
                   Occupancy + enter / leave / taken / count / stacked.
                   A COUNT per hex, not a boolean set: a wave spawns
                   stacked, so one of a pair stepping off must not free
                   the hex.  ⚠ It is not passability (that is the
                   GROUND, per class) and never a target — a companion
                   blocks a step and is never attacked for it.
                   Plan 14 H3 added the OTHER map: BlockerMap +
                   blocker_empty / _set / _at / _taken / _count /
                   _crew_index, built by `spawn.loft::wave_blockers`.
                   ⚠ **A second map rather than a second count**, and
                   the asymmetry is why: an enemy steps BESIDE a
                   companion and ATTACKS a vehicle, so one structure
                   would be read with a "but which kind?" everywhere.
                   ⚠ It answers WHO (`BLOCKER_PLAYER` is 0, helper `i`
                   is `BLOCKER_CREW + i`, nobody is -1), because with a
                   roster the damage must land on the vehicle that is
                   actually in the way.
                   ⚠ A WRECK is not in it — a downed helper blocks
                   nothing, or the first crew member to die in a
                   corridor would be a free wall with no HP left
  passable.loft    may a class of enemy make this move? (plan 11 F1 +
                   F6) — the enemy KIND discriminants + climb_limit()
                   + hex_height() + can_stand() / can_step() /
                   can_occupy(), each with a `*_fault` twin that names
                   the numbers.  TWO questions: is the SURFACE one this
                   class stands on (`walk_ground`), and is the STEP
                   within its climb.
                   ⚠ Since plan 12 B1 every surface question takes the
                   HeightLayer, because a piled hex's surface is
                   `rubble` and not what the map paints.  `hex_height`
                   deliberately does NOT — it reads `painted_ground`
                   and adds the layer, which is what makes a pile on a
                   wall the wall PLUS the pile.
                   ⚠ `walk_ground` alone is the BUG — `wall` and
                   `wall_high` are walk_ground=true (a wall's walkable
                   part is its TOP), so the one-field predicate walks
                   robots through 3 m walls.
                   ⚠ `can_step` is the rule (an EDGE, asymmetric — a
                   drop is free); `can_occupy` is what a POSITION can
                   honestly say with no history — "some neighbour
                   offers a legal step in", i.e. height minus the
                   LOWEST standable neighbour.  It is the measurement's
                   rule and must never be the field's node filter
  picker.loft      Picker { palette, active }
                   + picker_default(), picker_set_active(),
                   render_picker(cv, p, x0, y0) — Canvas-painted UI
  render.loft      software rasterizer using graphics::Canvas
                   + render_to_canvas, render_with_hover, palette_color,
                   draw_hex, draw_hex_outline,
                   world_to_canvas, screen_to_world, screen_to_hex.
                   ⚠ Draws from `lattice.loft` since plan 09 C3 —
                   pointy-top, so a hex is TALLER than it is wide.
                   `draw_hex` reads `lat_corner_offset` rather than
                   carrying a vertex table, so the hexagon drawn IS
                   the lattice's; there is no y-flip here, because
                   the one sign inversion lives in the metre
                   conversion
  golden.loft      assert_golden(cv, name) — writes tests/actual/<n>.png,
                   asserts byte-equality against tests/golden/<n>.png;
                   FAILs via loft's now-working assert (@P367 fixed)
  map_file.loft    MapFile { version, name, cam_q, cam_r, cam_zoom,
                   ground: vector<GroundEntry> }
                   — 6 fields, flat, vector LAST — see § Known constraints
  save.loft        paint_to_mapfile, save_map_file, load_map_file,
                   mapfile_to_painted, palette_index_of,
                   save_world, load_map_or_empty (returns tuple)
```

Tests live in `tests/<plan>_<phase>_*.loft` (one file per phase).
Goldens live in `tests/golden/` (committed); actuals in
`tests/actual/` (gitignored).  `.keys` scripts live in
`tests/scripts/` (committed — they are source, not output);
scripted-run shots land in `shots/` (gitignored, written fresh
each run — a shot a doc cites is copied into `docs/`), and the
suite redirects its own shots into `tests/actual/`.

## Key data structures

| Type | File | Purpose |
|---|---|---|
| `Hex` | `lattice.loft` | `{ q, r }` pointy-top odd-r offset coord — `q` a COLUMN, `r` a ROW |
| `EditorState` | `editor_step.loft` | the whole editor session — layers, camera, picker, mode, history, chunk dirty set |
| `EditorInput` | `editor_step.loft` | one frame of player intent (hover hex + action flags) |
| `EditorCamera` | `camera.loft` | `{ pos: Hex, zoom: integer }` |
| `InputState` | `camera.loft` | per-frame camera flags (in_pan_*, in_zoom_*) — folds into `EditorInput` in plan 08 V0b |
| `PaintedHex` | `painted.loft` | `{ q, r, kind: u8 }` — one painted cell |
| `PaintedWorld` | `painted.loft` | wrapper holding `hash<PaintedHex[q, r]>` |
| `GroundType` | `palette.loft` | one row from `examples/palette.json` |
| `Picker` | `picker.loft` | palette UI state |
| `MapFile` | `map_file.loft` | save record (6 fields; see Known constraints) |
| `GroundEntry` | `map_file.loft` | one persisted hex with kind as text name |
| `ScriptRun` | `script.loft` | one `.keys` run — ok / failing line / message / counts, plus the pointer, the shots directory and the wave it is playing |
| `FrameCounts` | `measure.loft` | one classified frame — pixels per bucket, `unknown` (not a palette colour = a fault), `total` |
| `WaveState` | `spawn.loft` | the enemy roster + round-robin cursor + the runtime rubble layer + the structure damage ledger + every tower's banked charge + the run's wallet + the crew + the cargo — runtime, not editor state |
| `Vehicle` | `vehicle.loft` | the player: where it is, where it is pointed, and whether it is in the world at all — ⚠ `parked` is separate because (0, 0) is a real hex and is the core in every scenario |
| `Wallet` | `wallet.loft` | points SPENT out of the run's 200 — zero is a FULL wallet, and the ledger is clamped at the budget so a later credit is not swallowed |
| `TowerState` | `tower.loft` | per tower: the seconds banked toward its next shot and the shots it has FIRED out of its 30 — runtime, never saved |
| `Enemy` | `spawn.loft` | `{ q, r, kind, heading, alive, taken }` — `taken` is damage ABSORBED, so an `Enemy { … }` literal that omits it is HEALTHY |
| `CarryObject` | `carry.loft` | one carryable thing — ⚠ `owner` is the WHOLE state machine (ground / a carrier / spent), because two fields that can disagree about one fact is the defect the model exists to make unwritable |
| `CargoLayer` | `carry.loft` | every carryable thing in the run — ⚠ a VECTOR with stable slots, never a hash by hex: two objects share a hex and a hash deletes one |
| `HeightLayer` | `height.loft` | metres of rubble piled on the map at runtime, and what it is made of — never saved |
| `DamageLayer` | `damage.loft` | HP each structure has ABSORBED — runtime, never saved; a miss means undamaged |
| `FlowField` | `flow.loft` | one class's distance field: cells (distance + the height it was swept with), the core, and the CLIMB it was built for |
| `ValidateReport` | `validate.loft` | one `make validate` sweep — scripts / passed / failed / measurements / shots, and the FIRST failure with the number that moved |

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

The following are dryopea-side workarounds for known loft
behaviour.  Full reproducers + loft-side issue refs live in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md):

- **A local may shadow a builtin, and that is now BY DESIGN** —
  so a shadowing mistake is silent rather than caught.
  [loft#852](https://github.com/loft-lang/loft/issues/852) made a
  local carry a function's name in every binding form while the
  function stays reachable as a call, so `ticks = 4` and `ticks()`
  coexist in one scope and mean different things (measured
  2026-08-14: `4` and the clock).  ⚠ The old note here — that
  `now = ticks()` ends up holding a `fn() -> integer` — **no longer
  reproduces**; `src/main.loft`'s `tnow` rename is a scar, not a
  live workaround.  What survives is the § Profiling warning: a
  parameter named `ticks` beside a `ticks()` call compiles clean
  and measures the wrong thing, and nothing warns.
- **`graphics::KEY_*` need explicit qualification.** Bare-name
  UPPER_CASE constants without `pub` don't re-export across
  `use` chains.  `gl_key_pressed(graphics::KEY_W)` works;
  `gl_key_pressed(KEY_W)` doesn't.
- **JSON cast HANGS on ≥8 declared fields with a
  `vector<Struct>`.**  `text as MapFile` with 10 fields hangs
  forever; 7 fields work.  MapFile is constrained to 6 fields
  until the loft fix ships.
- **`:j` formatter omits empty fields** (empty strings, empty
  vectors, zero ints under some conditions).  Round-trip
  `save → load` of a struct with empty fields can produce JSON
  the cast can't reload.  We avoid empty fields in MapFile.
- **Empty `[]` after a text field in JSON corrupts the prior
  field on cast.**  `{"name":"b","items":[]}` reads back as
  `name=""`.  We keep vectors non-empty (or put them first).
- **Early `return (a, b)` of a tuple of two struct types fails
  type-check**, despite the if-else *expression* form of the
  same tuple working.  In `load_map_or_empty` we use the
  if-else expression form, not early return.
- **`text as Struct` cast IGNORES unknown JSON fields**
  (lenient — @P366 fixed).  We rely on this for forward-compat
  saves.
- **A missing `use` reports as `Expect token ;` on a tuple
  access.**  Calling a function from a module the file didn't
  import leaves its return untyped, so the *next* line's `.0`
  fails to parse — and the whole aggregator goes red with
  "parse errors" while the real mistake (the absent `use`) is
  never named.  `Expect token ;` on a `.0` / `.1` line means the
  tuple's producer didn't resolve; check the imports first.
- **A struct literal that omits a field takes that field's
  default silently** ([loft#914](https://github.com/loft-lang/loft/issues/914)
  — both backends, and `loft --check` says ok).  So in any struct
  that callers build field-by-field — `EditorInput` above all —
  the NEUTRAL value must be the ZERO value.  A "none" sentinel of
  `-1` becomes `0` in every partial literal, which for a palette
  index means "select sea", which erases.  Build from the
  `*_empty()` factory, not from a literal.
  ⚠ **loft HAS declared field defaults** — `palette_pick: integer
  = -1` is honoured by a literal (measured 2026-08-14) — so the
  rule above is a workaround for not knowing, and `EditorInput`'s
  `in_select_palette` / `in_palette_index` PAIR could be one
  field.  ⚠ Literal-only: a `text as Struct` cast IGNORES a
  declared default ([loft#876](https://github.com/loft-lang/loft/issues/876)),
  so nothing dryopea loads from JSON may lean on one.
- **Loop variable name reuse must keep consistent type per
  function-scope** ([loft#915](https://github.com/loft-lang/loft/issues/915))
  — different types in different loops fails ("loop variable 'i'
  has type text but was previously used as integer"), and the
  variable OUTLIVES its loop.  Prefix loop vars per function; 122
  of `src/`'s 131 loops do.
- **Two libraries may declare one struct name; qualify at the use
  site.** `camera::InputState` / `input::InputState` both work, and
  the bare name is a clean error naming its own fix.  The old
  `Double structure type …` panic is GONE — so plan 07 W1's stated
  blocker is stale, and no `Hex` → `Axial` rename is needed.
  ⚠ **But that error dumps a FALSE `warning[lost-write]` against
  `src/spawn.loft::move_order`** ([loft#883](https://github.com/loft-lang/loft/issues/883)).
  The write is fine — measured on both backends.  Qualify the type
  and the warning goes with the error; do **not** go "fix"
  `move_order`.  It bites because `lost-write` is the one warning
  class that catches loft's most expensive real bug (plan 11 F8),
  so it reads as urgent, and because a green suite never aborts —
  the warning is unreachable by the warning-clean gate.

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
  GROUND_TYPES.md       — 11-type palette (water + land + structure)
  NUMBERS.md            — tunable values
  PROXY_ART.md          — placeholder shapes for entities

PROBLEMS.md             — dryopea-internal bugs (@D-prefixed; none open — @D001 fixed)
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
| [docs/DESIGN.md](docs/DESIGN.md) | Master design — towers / walls / waves / scramble / camera / HUD / economy / run shape |
| [docs/SETTING.md](docs/SETTING.md) | Fiction — autonomous AIs (girl-hacker imprint), faction wars dormant, surface-vs-underground, future contact gates, crew-doesn't-walk justification, combat-bot escalation |
| [docs/DESIGN_HISTORY.md](docs/DESIGN_HISTORY.md) | 2023 prototype seeds |
| [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) | ⚠ DESIGN, not built — the six robot installations (mines, factories, transport routes, military stockpiles, repair points, carbon plants) whose traffic is what waves are made of; the replacement for plan 16's authored list.  ⚠ Also § Crystal (the boss supply, and the only input with one product) and § The vertical dimension (a withered TREE is the shaft that reaches it) |
| [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) | Enemy movement — two steering modes, passability as a height step, bodies as terrain, sealing punished not forbidden, structural wall HP, retaliation, the tick resolving once |
| [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) | Palette spec — 11 painted types plus `rubble`, which the runtime deposits and nobody paints |
| [docs/NUMBERS.md](docs/NUMBERS.md) | Guide to `examples/numbers.json` — what is in it, what reads it, and ⚠ that nothing LOADS it yet |
| [loft_repros/README.md](loft_repros/README.md) | Minimal reproducers for loft bugs — filed, and ready to file |
| [docs/PROXY_ART.md](docs/PROXY_ART.md) | Placeholder shapes |
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
| Design where WAVES eventually come from | [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) — six installation types and the routes between them.  ⚠ Its governing rule is the enemy rule again: ONE system, per-type DATA, so a new installation costs a row and no new behaviour |
| Find a mechanic that is designed but NOT built | [docs/DESIGN.md](docs/DESIGN.md) (the mechanics) and [plans/ROADMAP.md](plans/ROADMAP.md) (the index).  ⚠ `plans/12` § Design recorded during this plan POINTS at them rather than restating — a second copy is the one that drifts |
| Understand the fiction | [docs/SETTING.md](docs/SETTING.md) |
| Pick next work to do | [plans/ROADMAP.md](plans/ROADMAP.md) — 5-tier feature list |
| TUNE a number | `examples/numbers.json`, then run `scripts/test.sh`.  ⚠ `tests/numbers_design_targets.loft` gates five of `docs/NUMBERS.md` § Design targets against the running sim, so a tuned value fails there naming the promise it broke.  ⚠ And nothing LOADS numbers.json: every value is hand-copied into a `.loft` constant, so edit BOTH — that test pins them together |
| Continue plan 01 work | [plans/01-ground-editor/README.md](plans/01-ground-editor/README.md) § Implementation status |
| Add a regression test | `tests/01_*.loft` for patterns; `golden.loft::assert_golden` for image tests |
| Script a run of the editor | `tests/scripts/*.keys` for the vocabulary; `script.loft::script_run_file` to play one; `snap <name>` for a picture |
| Add a validation scenario | a new `tests/scripts/<name>.keys` + one test in `tests/08_v3_scenarios.loft` (pin its check count — a scenario with its measurements deleted still reports ok) |
| Change what a frame contains | `editor_view.loft::render_editor_frame` — the GL loop and `snap` both draw it, so edit it there, not in `main.loft` |
| Write/edit a `.loft` file | Loft language conventions: see § Important conventions above + loft's own `loft-write` skill |
| Run the editor | `loft src/main.loft` |
| Add a `.keys` verb that takes a hex | `src/script.loft`, AND a row in `src/convert.loft::keys_schemas` + the vocabulary list in `tests/09_c5a_converter.loft`.  A missing schema row is silent: the converter passes an unknown command through untouched |
| Place or restore a marker of any kind | `src/markers.loft::place_marker` (and `history.loft::place_marker_and_record`) — the ONE dispatch.  ⚠ Sidecar load, undo and redo each used to fall through to SPAWN, so a kind they had not learned about arrived as a wave source with a heading |
| Add a marker kind | append a constant in `markers.loft`, bump `MARKER_KIND_COUNT`, add a row to `place_marker` + `marker_kind_name`.  ⚠ The editor's place-kind CYCLE grows, so every `.keys` script that cycles back to spawn needs another press — B5a paid that for nine scenarios |
| Change what a key does | `src/bindings.loft::editor_actions` — the ONE table.  Both the GL loop and every `.keys` script read it, so a change is visible to the gate.  Never add a `gl_key_pressed` |
| File an outbound loft request | [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) |
| File a dryopea-internal bug | [PROBLEMS.md](PROBLEMS.md) (`@D<NNN>` convention) |
| Understand library extraction | The `hex_*` family is published — `loft api --registry` |
| Change how enemies move | [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) — the whole spec.  [plans/11](plans/11-flow-field/README.md) is what it costs to build |
| Step a hex coordinate | `lattice.loft::lat_neighbour`.  ⚠ Never a `+ 1` on a `q` or `r`, and never a constant `(dq, dr)` table either — odd-r deltas depend on row parity, so no such table exists |
| Tell GEOMETRY from LABEL SPACE (any coordinate change) | Ask what the site depends on. **Geometry** ("where on screen?") depends on the lattice alone. **Label space** ("which cell?") is only meaningful relative to how the DATA is labelled — `paint_line`, `enemy_tick`, the flow BFS and every `.keys` literal are label-space.  Plan 09 is the worked example: the two had to move in separate phases (C3, then C5), and converting one label-space site alone turns `scripts/validate.sh` red for a reason that is not a defect |
| Ask whether an enemy may MOVE somewhere | `src/passable.loft::can_step` — the rule, an edge.  Never `walk_ground` on its own, and never the destination's height on its own |
| Ask whether an enemy may BE somewhere | `src/passable.loft::can_occupy` — what a position can say with no history.  The measurement's rule; never the field's node filter |
| Raise a hex at runtime (bodies, broken walls) | `src/height.loft` — the rubble layer: a rise above what the palette paints, plus what it is made of.  Lives on `WaveState`, never saved.  ⚠ Shrinking a pile to nothing REMOVES its entry — a 0.0 m pile would still read as a rubble surface |
| Ask what a hex's SURFACE is (vs what is painted on it) | `src/passable.loft::hex_ground` — rubble where a pile stands, the painted kind otherwise.  `painted_ground` is the other half and is what `hex_height` adds the layer to |
| Ask whether a hex is free of enemies | `src/occupancy.loft` — a separate question from passability, and a count rather than a flag |
| Ask who on the PLAYER's side is standing on a hex | `src/occupancy.loft::blocker_at` over the map `spawn.loft::wave_blockers` builds each tick — it answers WHICH vehicle, because the blocker damage has to land on the one in the way.  ⚠ Never a per-vehicle predicate: `vehicle_on` was deleted for being the second door |
| Bring a lost crew member back | `src/spawn.loft::wave_drop` at the core — and NOTHING else does it (`DESIGN.md` § 9: *"retrieval is the only way back"*).  ⚠ The clock is exactly 90 ticks and the epsilon in `helper_recover_tick` is what keeps it 90 rather than 91 |
| Take a crew member out of the run | `src/helper.loft::helper_wreck` — and the tick is the only caller, at the end, beside the deaths and the breaks.  ⚠ It is TWO effects at one site since plan 15 C1: the helper goes down AND a carryable wreck appears where it stood.  ⚠ Nothing brings it back yet: retrieval is plan 15 C2 |
| Pick something up, carry it, put it down | `src/carry.loft` — one record per object with an `owner`, so conservation is structural.  ⚠ Never add a "carried" field to a vehicle beside it: a slot on the carrier and an owner on the object are two facts that can disagree |
| Add a new kind of carryable thing | a `CARGO_*` constant plus what a valid destination is and what arriving there does — and NOTHING in the carrying path.  ⚠ A kind that needs new carrying code has broken `plans/15` § C0.4 |
| Ask what a blocked enemy attacks | `src/spawn.loft::enemy_target` over `flow.loft::flow_desire` — per route, never a global "nearest wall" |
| Ask whether a tower can HIT something | `src/tower.loft::tower_sees` — one straight line from the eye over `hex_height`.  ⚠ Never a "which kinds block" table: a `wall_high` beside the tower does not block and a `wall` near the target does |
| Ask why a tower is not shooting | `src/tower.loft::tower_sight_fault` names the hex, the two heights and how far along the line it sits; `tower_black` is the other answer |
| Ask how much a wall has left | `src/damage.loft::structure_hp` — max minus taken.  ⚠ 0.0 answers BOTH "broken" and "never a structure"; ask `structure_breakable` first if you need to tell them apart |
| Ask how strong a wall hex is | `src/damage.loft::structure_max_hp` — the kind's figure scaled by `brace_of`.  ⚠ `numbers.json`'s wall_hp (100) is the BRACED number; a lone plug in a corridor is a STUB and gets 15 |
| Break a wall | `src/damage.loft::break_structure` — the one site, and it does both halves.  The tick calls `damage_resolve` AFTER every enemy has moved, so a breach belongs to the NEXT tick |
| Clear rubble / collect after a tower | `src/vehicle.loft::salvage_at` — the rule, taking a HEX, so the player and every helper read one implementation (`vehicle_salvage` / `helper_salvage` are the two doors).  The counter-play to `ENEMY_MOVEMENT.md` § Bodies are terrain.  ⚠ A crew inside a SEALED base can only reach the ramp by BOOSTING out (V4): the ramp forms outside the wall and an idle vehicle climbs 0.4 m — and no helper has a boost |
| Give a mover a climb that changes while it lives | `src/passable.loft::can_climb` — the rule with the climb passed rather than looked up.  ⚠ Never widen `climb_limit(kind)`: it is a CLASS lookup and a convenience for callers that have a kind.  `vehicle_climb` is the worked example |
| Ask why a fresh wave is not moving | `src/spawn.loft::enemy_standing` — the pre-walk window (plan 16 W2), 8 ticks at the marker.  ⚠ Spent ONCE per tick by `wave_stand`, at the END beside `helper_recover_tick`; the predicate only asks.  ⚠ A standing enemy does not move, attacks nothing and blocks nobody — but is NOT immune, which is what "stand visible" means |
| Ask whether the run is over | `src/wallet.loft::wallet_broke` — the wallet at zero, and the ONLY end state.  ⚠ Never `core.hp`: it is `null` by design |
| Judge whether a DEFENCE is worth building | [plans/12](plans/12-combat-resolution/README.md) § B7 — three scenarios that differ only in their defences, and the measured clock (69 / 112 / 128 since plan 16 W2).  ⚠ A sealed wall nearly doubles it; a wall with a GATE buys nothing at all; and a tower now ADDS 16 ticks where it used to cost 9 — because the pre-walk window moved its kills off the wall's foot, so the ramp that used to bury it no longer forms there |
| Judge whether fetching a lost crew member is worth it | [plans/15](plans/15-the-carry-model/README.md) § Status — three clocks on one base (93 / 87 / 87 since plan 16 W2).  ⚠ The trip costs six ticks and the CARRY costs nothing, and the middle run is the control that keeps those apart.  ⚠ It cannot pay until a base outlives the 90-tick recovery, which is the wave system's job |
| Judge what another CREW MEMBER is worth | [plans/14](plans/14-helpers/README.md) § Status — three scenarios that differ only in their crew lines, and the measured clock (⚠ **123 / 135 / 138** since plan 16 W2, from 77 / 214 / 242).  ⚠ A roster buys COVERAGE, not throughput — but the base can now barely express it: the pre-walk window means far fewer ramps form for a crew to clear, so the whole spread is 15 ticks where it was 165 |
| Hurt or kill an enemy | `src/spawn.loft::enemy_hurt` lands damage and never kills; `wave_deaths` (the tick's, after the move loop) is the ONE death path, so B5's tower and a script's `hit` cannot drift.  ⚠ A fatal hit is followed by one last STEP — the tick moves before it kills, so the body lands one hex down the route from where the shot landed |
| Validate the GAME (not a function) | `scripts/validate.sh` — then [plans/08-game-validation/README.md](plans/08-game-validation/README.md) |
| Check a change did not cost anything | `tests/11_f8_the_tick_budget.loft` — a RATIO gate, because a copy changes no behaviour and no other test can see it |
| Find out what the SUITE spends its time on | `LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` — § Profiling the suite.  Read the op count, never the wall clock |
| Speed up frame measurement further | `src/measure.loft::classify_canvas` is already written for the pixel count — do not "tidy" it.  The remaining 58% is `graphics`'s `canvas()`, upstream |
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
