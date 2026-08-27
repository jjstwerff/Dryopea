<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Hard-won rules

**Every rule here cost a real defect to learn, and most of them describe a
test that CANNOT see the thing it appears to test.**  They are grouped by
what they protect.

⚠ This file is the detail.  [`CLAUDE.md`](../CLAUDE.md) § Hard-won rules
carries the one-line headline of each, so the warning fires in context and
the evidence is one link away — **read the headline there, come here before
changing the thing it names.**

⚠ **A rule is not retired by disagreeing with it.**  Each one carries the
measurement that produced it and the `@M` / `@X` code that indexes it
([`docs/DECISIONS.md`](DECISIONS.md)); if a reading here is stale, re-measure
and rewrite the row with a new date rather than deleting it.

## Movement + passability

**How an enemy moves today.**  `wave_tick` rebuilds the distance field
ONCE per tick before anybody moves — one field per climb limit in the
roster — and `enemy_tick` steps down it.  Two steering modes hand off at
the **scrambler bubble**: inside 25 hexes the field steers, outside it the
spawn heading does.  An enemy with no route at all follows the DESIRE
field instead and besieges what it cannot climb.

⚠⚠ **The mover asks `enemy_engaged`, which is *cut off AND inside*,
never the geometry alone** (BACKLOG C3).  They were one question until
the core could be switched off, and `wave_cutoff`'s top-of-tick sweep is
what keeps them one while it is jamming.  Read the bubble directly in a
mover and a robot crossing a DARK bubble on its business turns round and
besieges — silently, because with the jammer on every gate still passes.

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

## Cost

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
fit at 100 ms — so [`plans/22`](../plans/22-the-field-cache/README.md) is
the prerequisite for the shorter tick, not a follow-up to it.
⚠ And the epsilon travels with it: a **tenth-length tick loses a whole
hex** without `ENEMY_PROGRESS_EPSILON` (`@M013`).  See `spawn.loft`
§ What a tick is worth.

## Testing something that moves

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

⚠⚠ **AND THE VACUITY CAN BE IN THE NUMBERS RATHER THAN IN THE
ASSERTION** (plan 26 L4, `@M034`) — the same rule at its hardest to
notice, because this one PASSES and looks like a result.  `plans/26`
§ L4 asked that *"a 1 Hz clock driven by a 30 Hz clock's ticks equals
one driven from the wall"*.  Measured over 600 frames the two agree
after every single one — **and they must**, for any implementation
that hands on whole steps, because 1 Hz is exactly thirty 30 Hz steps
so every slow boundary falls ON a fast one.  There was nothing to lag
by.  ⚠ The property the gate names is real and needs steps that do NOT
divide: 700 000 units driven by 300 000 is behind at **3 of 21**
frames, **0** ahead, never more than **1** behind, level again at the
common multiple.  ⚠ **So the generic control has a second form**: not
only *can this gate produce a non-trivial reading at all?* but *are the
NUMBERS I chose capable of disagreeing?*  A round ratio between two
quantities is exactly where they are not.

⚠⚠ **AND A NEGATIVE CONTROL THE WRONG IMPLEMENTATION ALSO SATISFIES IS
NOT ONE** (plan 26 L4, `@X083`).  The same row's control read *equal
tick counts would mean the cap did nothing* — true, and cheap.  The cap
a driver actually writes by mistake clamps the answer and leaves the
backlog BANKED, which answers **fewer** ticks on the stalled frame too
and then pays the stall off over the frames that follow, running the
simulation behind the wall for ever.  Over one stream with a 20 s
stall, the dropping cap plays **4** ticks and the deferring one **24**
and still owes 6.  ⚠ **The control has to be the plausible WRONG
version reproduced beside the shipped one**, not the null version — the
null version is the one nobody would have written.

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

⚠⚠ **A GATE AIMED AT THE MECHANISM YOU EXPECT TO BE THE HAZARD IS NOT ONE
AIMED AT THE HAZARD** (plan 25 M2, `@M025`) — the second time the repo has
paid for this, from the opposite direction.  M2's headline test was *one map
painted in two orders gives one mesh*, on the theory that a walk over
`pw.painted` would follow the paint order.  It could not fail: **loft's
keyed collections are an ORDERED INDEX, not a bucket hash.**  Measured over
six fixtures — reversed insertion, a scrambled spread, a key landing
mid-range, negative coordinates, grow-then-erase — `hash<PaintedHex[q, r]>`
comes back sorted lexicographically every time, and stays sorted when an
unrelated distant key is added.
⚠ **The control is what said so, not the code**: the test asserted the two
fixtures iterate differently, and they do not.  Without that assertion the
phase would have shipped a green test guarding nothing.
⚠ The mechanism was still worth keeping, for a reason the plan had not
connected to it — a coordinate walk buys COVERAGE (the drawn region is wider
than the painted set), never determinism.  So **the right code with the
wrong justification** is what to look for when a gate refuses to fail.
⚠ The other direction is plan 12 B3's fence tripwire: written to go red the
day a steering rule landed, still green when it did (`@M020`).

⚠⚠ **A COST GATE CAN BE A COUNT INSTEAD OF A CLOCK, AND USUALLY SHOULD
BE** (plan 25 M4, `@M029`).  M4 was specified as a timing RATIO — the
`11_f8` shape — and the clock could not carry it: two IDENTICAL
back-to-back calls differed **5.4x** (1.84 s against 0.34 s) when a
registry fetch stalled inside one of them.
⚠ **A ratio survives a uniformly slow machine; it does not survive a
1.5-second stall landing inside one of its two halves.**  `11_f8` never
meets that because both its halves are `wave_tick` and neither touches
the disk — so *"cost is a ratio"* is a rule about `wave_tick`, not about
cost.
⚠⚠ **What a mesher's cost regression IS, is geometry that should not be
there** — so count the FLOATS uploaded.  Deterministic, immune to a busy
box, and it priced M1's two invisible breaks for the first time: a
zero-area sliver at every hex boundary bakes **331 776 floats against
110 592**, exactly 3x, and draws not one pixel.
⚠ Ask what the change would actually DO before reaching for a stopwatch;
the artefact is often countable.

⚠⚠ **A COUNT IS PERMUTATION-INVARIANT, SO IT CANNOT SEE A MIRRORED
WORLD** (plan 25 M3, `@M027`).  Reflect the drawn world in y — tops and
sides together, winding reversed to match — and the GL gate reports
`other == 0`, `sea == 0`, and grass / wall / wall_high / rubble every one
**in band**.  Only a LANDMARK, compared against `camera_screen`'s own
prediction, sees it, and it sees it at **490.8 px**.
⚠ `render_camera.loft` calls a mirrored world the failure that *"reads as
a base that is its own reflection rather than as an error"*, so a gate
built out of counts is blind to the one defect that file is most afraid
of.  **Any gate that counts pixels needs one assertion about WHERE.**
⚠ **A landmark has to be FLAT**: a column that stands draws its sides in
its own colour, and those sides sit between the top face and the screen
centre (a camera above the middle of the frame sees the inward face of an
off-centre column) — measured at **29 px** for a 5 m `wall_high` against
**0.6 px** for a flat hex.  Loosening the tolerance would have hidden
that rather than measured it.
⚠⚠ **And a FALSIFICATION has to be clean or it proves the wrong claim.**
The first mirror attempt moved only the tops, so tops and sides disagreed
and opened background — the gate fired on `other` and the landmark check
was never reached.  A gate that reports its FIRST failure ranks its
assertions exactly as M1's bundled test function did.

⚠⚠ **AN EMPTY ARTEFACT SATISFIES EVERY EQUALITY** (plan 25 M2).
`mesh_crc` of an empty mesh is **0** — the fold starts at all-ones and ends
by inverting — so a mesher stubbed to emit nothing makes every *"one build
equals the other"* assertion pass.  ⚠ It is `tests/21_r1`'s finding in a
third place (a gate that reads PERFECT is as suspect as one that reads
wrong) and the fix is the same two lines: every equality carries a
**non-zero floor**, and one test states out loud that zero is what empty
means, so `!= 0` reads as a requirement rather than as superstition.

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

⚠⚠ **A SMOOTHER MEASURED ON THE THING IT SMOOTHS CANNOT SEE WHAT IT
FAILS TO SMOOTH** (`@M035`, plan 26 L5).  `@M023` proved the camera's
ease works by reading `cam.target` — un-eased it moves on 12 frames of
240, eased on 221 — and every one of those numbers is true.  Measured in
PIXELS, on the same drive, the eased camera leaves **96 px** of jump on
the vehicle where the un-eased one leaves **0**, and the un-eased one
throws 14.9 px at the ground where the eased leaves 1.3.  **The ease does
not remove a discrete mover's jump; it moves it off the world and onto
the mover, and it grows six times doing so** — because the one object a
follow camera cannot smooth is the object it is chasing, and the target
is exactly what `@M023` chose to read.
⚠ The generic form is worth more than the camera: **a gate pointed at the
mechanism reports the mechanism, so point it at the ARTEFACT** — here the
pixels a player would see, which is `25_m3`'s *never gate on counts
alone, ask WHERE* arriving from the other direction.
⚠ And the second half is that a fix applied to half the composition
leaves a fraction behind rather than nothing: drawing the mover at alpha
under a camera still easing toward its HEX takes 96 px to **14**, and
only a camera following the DRAWN point takes it to 0.  *Two things
composed are one decision.*

⚠ **A REFERENCE ROUNDED TO SEVEN DIGITS CANNOT CERTIFY A POLICY THAT IS
EXACT** (plan 26 L5).  The same sweep priced a drawn position against
`VEHICLE_SPEED × t × HEX_FLAT_TO_FLAT`, and charged the one exact policy
with **1.3 µm** of error over 15.6 m — which was the constant's own
truncation of `√3 × 0.75`, not the policy's.  `lat_to_metres` is exact,
so the fix was to read the lattice's own spacing instead of a rounded
copy of it, and the residual fell to 9.5e-16 m.  ⚠ It only shows up
against a candidate good enough to be measured at that scale, which is
why it is worth writing down: the instrument was fine for three of the
four policies and wrong for the one that mattered.

## Profiling the suite — and why the wall clock cannot do it

`LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` gives one merged
per-function + per-line + call-path report over the whole suite.  The
method, the numbers of record and the compilation half of the wall
clock live in [`docs/PROFILING.md`](PROFILING.md); these three are
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
  reading agrees and [`plans/22`](../plans/22-the-field-cache/README.md) is
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
  divides a uniform increase out.  [`docs/PROFILING.md`](PROFILING.md)
  has the per-file table.

## Timers and epsilons

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

⚠⚠ **AND THE FOURTH MEMBER IS THE SITE THAT NEVER GOT A GUARD AT ALL**
(plan 26 L0, `@M030`, `@D003`).  dryopea has **seven** independent
implementations of *do not lose a fraction* and one of them omits it:
`vehicle_hexes_this_tick` TRUNCATES `speed × tick_seconds` with no
carry, and `Vehicle` has no `progress` field to put one in.  Measured
over one simulated minute at **667 / 500 / 333 / 200 / 100 / 50 / 33
ms**: miner 1.0, robot 1.5, scout 2.5 and helper 2.5 hex/s are exact at
all seven, and the player reads **180 / 120 / 180 / 0 / 0 / 0 / 0**
against a true 180.  End to end it **never leaves its hex** at 200 ms
across a whole minute of corridor while a robot beside it arrives.
⚠ **The failure is not "a bit slow"** — at 200 ms a BOOSTING player
covers 300 hexes and a cruising one none, so a truncation does not
scale a rate down, it **reorders which rates exist**.
⚠⚠ **What makes it the family's worst member is why no gate saw it —
three accidents, and the third is a new shape.**  Both shipped vehicle
speeds are exact at the shipped tick (`3.0 × (1/1.5)` is 2.0 to the
bit), the half-tick `23_k2a` already sweeps is exact too, and the ONE
shortened timestep in the repo that WOULD have caught it — `23_k2a`'s
tenth-tick, where `3.0 × 0.0667` truncates to zero — **banks an
ENEMY**.  That is `11_f8`'s markerless-world trap with the axis and the
subject swapped: *the right sweep over a roster with none of the broken
thing in it*, and it is harder to see because the sweep looks thorough.
⚠ So the rule is a CROSS-PRODUCT: **sweep the neighbours of the value
you shipped, and sweep them over every mover, not the one you were
thinking about.**  `@M013` varies the SPEED at one tick length through
movers that carry, and cannot reach this from any direction.
⚠ **FIXED in plan 26 L2** (2026-08-17) — the vehicle carries a `Bank`
and the sweep reads 180 at all seven.  ⚠ A vehicle that ROUNDS instead
of truncating fires all four records while **overshooting** (240 hexes
at 500 ms) and still zeroes at the short end, which is what said the fix
had to be a bank and not a nudge.

⚠⚠ **AND THE FIFTH IS THE ACCUMULATOR ITSELF, NOW GONE** (plan 26 L1,
`@X079`, `@M031`).  Simulation time is an integer count of a chosen
step, so `advance(n × step) == step(n)` holds for all of 1..100000
against the float path's 602-of-1000 failures.  ⚠⚠ **The step's BASE
UNIT is the decision, and microseconds are the wrong one**: 2/3 of a
second is not a whole number of µs, and the 666 667 the plan itself
recommended as *"moving nothing"* moves **seventeen tests** while the
654 gate measurements cannot see it.  The answer is 1/3 µs — the
coarsest unit in which the tick is whole — and *a measurement's
resolution is not its authority*.

⚠⚠ **AND THEN ALL SEVEN SITES COLLAPSED INTO ONE** (plan 26 L2,
`@X080`).  `src/tick_bank.loft` is now the only *do-not-lose-a-fraction*
a mover has; `ENEMY_PROGRESS_EPSILON` and `HELPER_PROGRESS_EPSILON` are
**deleted rather than zeroed**, which is the strongest form of `plans/17`
T1's rule — a guard you can delete is one the arithmetic no longer
needs, where a guard whose removal leaves the suite green is one that
could never fire.  The three numbers that bought the epsilon are kept
unchanged in `tests/23_k2a` (1.0 hex/s over 60 ticks is 40 hexes, 2.0 is
80, 1.5 is 60): *the same answers, with no constant producing them.*

⚠ **Two design constraints came out of it and both are counter-intuitive.**
First, a `Bank` holds the CARRY and nothing else: the rate arrives per
call (`@X061` — *a damaged robot moves slower* makes a rate a property
of a CONDITION), and `whole` is a **parameter** rather than a field,
because loft defaults an omitted struct field silently ([loft#914]) and
dryopea builds `Enemy` from partial literals in a dozen places — a
`Bank` carrying its own scale would default to zero in every one of them
and freeze that mover, where a defaulted CARRY of zero is a fresh bank.
⚠⚠ Second, **the reciprocal form was refused on arithmetic**: storing
*base units per whole unit* needs no rate scale and would make `Bank`
into `clock_advance` with a variable step, but `3 000 000 / 2.25` is
1 333 333.33 and `@M013` already sweeps 2.25 hex/s.  *A rate scaled UP
is exact for every authored number; a rate inverted is not* — which is
why the two types stayed apart.

⚠⚠ **AND THE SIXTH MEMBER IS THE ONE THE FAMILY WAS NAMED AFTER, WHERE
THE GUARDS TURNED OUT TO BE THE HEALTHY SITES** (plan 26 L3, `@M033`,
`@D004`).  Boost, cooldown, recovery, repair and the lull were the
one-shot half, and `plans/26` counted their three 1e-9 nudges as the
brittleness.  Swept at the same seven tick lengths, through the shipped
code, before anything was converted:

| timer | guard | 667 | 500 | 333 | 200 | 100 | 50 | 33 ms |
|---|---|---|---|---|---|---|---|---|
| helper recovery 60 s | epsilon |  90 | 120 | 180 |  300 |  600 | 1200 | 1800 |
| tower rebuild 20 s   | epsilon |  30 |  40 |  60 |  100 |  200 |  400 |  600 |
| boost 2 s            | epsilon |   3 |   4 |   6 |   10 |   20 |   40 |   60 |
| **wave lull 15 s**   | **none** |  23 |  30 |  45 | **76** | **151** |  300 | **451** |
| **pre-walk 5 s**     | **none** |   8 |  10 | **16** |  25 | **51** | **101** | **151** |

⚠ **Every guarded timer is exact at every tick length; both unguarded
ones run a tick long** — and both are right at the shipped 667 ms, which
is why nothing had seen them.  That is the FOURTH member's rule again —
*the site that never got a guard at all* — and two of the three healthy
timers count DOWN exactly as the broken pair does, **so the DIRECTION is
not the discriminator.  A guard is.**

⚠⚠ **AND THE MOST REUSABLE THING L3 FOUND IS A BLIND GATE, NOT A
DEFECT.**  `plans/26` § L3's own expected result was *"a `Timer`
counting UP to 20.0 s and one counting DOWN from it both fire on the
same tick"*.  Measured at the shipped tick over the six exact-multiple
durations, float UP / float DOWN / true: **7/7/6, 16/16/15, 30/31/30,
45/45/45, 61/61/60, 91/91/90**.  The two directions disagree at exactly
ONE of six and **agree while both being a tick long at FOUR** — so the
gate as worded would have read agreement at four times as many cases as
it caught.  ⚠⚠ ***Two agreeing instruments are not a control; the TRUE
count is.***  This is `@M022`'s question one turn further on: not *can
this gate produce a non-trivial reading at all* but *is the thing it
compares against independent of the thing it measures*.

⚠⚠ **AND THEN THE ONE-SHOTS COLLAPSED INTO ONE TOO** (plan 26 L3,
`@X082`).  `src/tick_timer.loft` holds `{spent, total}` in integer base
units and `timer_left` is `total − spent`, so **there are no longer two
directions to disagree** — the up-count and the down-count are one
number read two ways.  All three epsilons are **deleted**.
⚠ **A `Timer` MAY hold its `total` where a `Bank` may not hold its
`whole`, and it is the same [loft#914] rule reaching the opposite
conclusion**: a defaulted `whole` of 0 is a mover that never moves, a
defaulted `total` of 0 is an UNARMED timer — which is exactly what every
`0.0` seconds field it replaced already meant.  ⚠ The family boundary
was ATTACKED and held: a one-shot built on `bank_gain` fires a SECOND
time with nobody re-arming it, and its residue leaks into the next
arming — a 5.0 s cooldown costs 8 ticks the first time and **7** the
second.  Two small exact types, never one clever one.

⚠ **And the seam did not close, it changed hands.**  `tick_clock.loft`
predicted L3 would delete `clock_seconds_from_units`.  What is left
after it is not simulation at all: `.keys` **authoring** (a person
writes seconds, exactly where `bank_fraction` sits) and the camera's
**ease**, which the invariant already puts outside.

⚠⚠ **AND THE COUNT WAS SEVEN AND THERE ARE EIGHT.**  The tower's
CHARGE accumulates float seconds and `wave_fire` subtracts one whole
fire interval per shot, carrying the remainder for ever — `bank_gain`
written by hand, still holding `TOWER_CHARGE_EPSILON`.  ⚠ It is on the
BANK side of the boundary and is NOT converted: a tower may only
release a shot it is ALLOWED to fire, so the held count and the carry
have to come apart first.  Pinned by
`tests/26_l3::test_the_tower_charge_is_still_a_hand_rolled_bank`.
⚠ **A census taken from the source is itself a measurement and can be
short** — this one was, by one, in a document that counted carefully.
