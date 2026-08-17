<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 12 — Combat resolution

**Value:** `G` · **Effort:** `MH`

## Status

**COMPLETE — B0-B7 shipped** (2026-08-13). The exchange resolves and a
run can end.

⚠⚠ **B7 half-falsified this plan's own Goal, and that is its most
valuable output.** The goal says a base *"falls on a measurable clock
that gets markedly longer when the defences are present"*. Measured,
with the same ground, core, spawn and wave list, and only the defences
changing:

| base | clock | ending |
|---|---|---|
| no defences | **61** ticks (41 s) | they walked in |
| sealed wall | **104** ticks (69 s) | **the wall broke at its 30 HP END**, and the perimeter unzipped |
| sealed wall **+ a tower** | **95** ticks (63 s) | **the pile went over** — the wall never lost a tenth of its HP |

⚠⚠ **Plan 16 W2 INVERTED the tower row of this table** (2026-08-14).
The pre-walk window holds a wave stacked on its spawn marker for 8
ticks inside a 15-hex tower range, so the tower's first kills pile up
out THERE instead of at the foot of the wall — no ramp forms, and the
clocks become **69 / 112 / 128**.  A tower is now worth **+16 ticks
where it used to cost 9**, and this plan's Goal holds for the tower as
well as the wall.  The mechanic below is not deleted, only conditional
on where the kills land: `a-base-on-two-fronts` still falls to a ramp,
because 1.0 m of bodies is exactly a 3.0 m wall less a 2.0 m climb.
See [`plans/16`](../16-the-wave-system/README.md) § W2.

So the goal holds for the WALL (+70%) and is still **inverted by the
TOWER**, which gives back 9 of the 43 ticks the wall bought. Five
surviving robots take a base defended by a gun and a sealed wall,
because eight of their friends died at its foot: three bodies on one
hex is 1.5 m, B0's ramp band onto a 3.0 m wall is [1.0, 2.0], and the
survivors climb their own dead. That is `ENEMY_MOVEMENT.md` § Bodies
are terrain firing exactly as written in a base with nobody to clear
up — which § What this plan does NOT build predicted in as many words.
See § B7 below.

⚠ **These are the plan 11 F7b numbers, and the rebaseline is itself
evidence.** B7 first measured 161 / 311 / 180 on the queueing mover it
was built against; its own findings then motivated F7b, which changed
how every enemy walks. Every clock moved and **all three of the
phase's conclusions survived** — a seal buys time, a gate buys none, a
tower gives some back. A scenario whose verdict is stable across a
change to the mover underneath it is measuring the defences rather
than an artefact of how enemies happen to walk.

B0 was a probe and changed no mechanic: it wrote down what the
simulation does in the two places this plan leans on, and it
**falsified half of its own recommendation** — see § The blocker B0
answered. The climb number it handed B1 is **2.0 m**, not the ~1.0 m
the plan first proposed.

**B1 applied it and built the rubble layer.** A robot climbs 2.0 m, so
rubble up to 2.0 m is a ramp and 2.1 m is not; `rubble` is palette
entry 11, reached from the runtime layer and painted by nobody;
`height.loft` gained a **source** and a **clear**, and clearing a pile
restores the authored hex exactly. What it cost, measured rather than
estimated: **25 tests red at once**, all of them named in advance —
B0's three inverted assertions, F6's nine and F8's three 1.5 m pile
fixtures, and ten in the palette / bindings tables that a twelfth entry
moved. The suite is **607 green** (from 585) and the gate still 233
measurements over 14 scripts.

⚠ **The under-gated constant is gated now.** B0's note said a change
from 0.0 to 1.0 moved exactly one assertion in 585 tests, and that
assertion was a string check on a fault message. F6 and F8 carry a
named `PILE_OVER_A_ROBOT` with an assertion tying it to
`CLIMB_REGULAR`, so the next move of that number fails once, in a test
that names itself, instead of scattering across nine.

**B2 made the exchange resolve.** `src/damage.loft` gives a structure
HP, the tick spends each besieging enemy's 1 HP/s against the hex
`enemy_target` names, and a wall that runs out comes down into a heap
of masonry. `tests/scripts/a-wall-breaks.keys` is the artefact: a
sealed corridor, a robot that stops at the seal, a wall that falls, and
**a robot that ends up on the core**. Suite **630 green**; the gate is
15 scripts and **260 measurements**.

⚠ **Two things B2 settled that the plan had left loose.**
*Where the break lands in the tick*: damage is spent off the PRE-move
targets and structures are resolved AFTER every enemy has moved, so
the breach belongs to tick N+1 and the roster order cannot change the
outcome — the same rule bodies already followed, now with a ledger
that a wrong answer would show up in. *What the ledger holds*: damage
TAKEN rather than HP remaining, so an untouched base needs no entries
and a miss reads as "undamaged" instead of "already broken".

**B4 gave enemies HP and death.** An `Enemy` carries damage TAKEN —
zero is neutral, so every roster literal that predates the phase stays
healthy — `hit <i> <hp>` lands damage from a script and cannot kill,
and `wave_deaths` at the end of a tick is the ONE death path B5's tower
will share. A death frees its hex AND drops 0.5 m of wreckage on it, so
the kill zone `ENEMY_MOVEMENT.md` § Bodies are terrain describes now
actually closes: five dead robots in a one-hex corridor make a 2.5 m
heap and the survivors cannot get over their own dead. Suite **648
green**.

⚠ **A fatal hit is followed by one last STEP**, because the tick moves
before it resolves deaths — so a body lands one hex down the route from
where the damage landed. It is the same "consequences land at the end
of a tick" rule a broken wall follows, and it is B5's input: a tower
shot kills where the enemy WAS and the body falls where it was going.

⚠ **A loft trap this phase paid for.** A struct returned from a
function is a COPY, so `enemy_hurt(first(state), 10)` lands nothing —
silently, with no warning and no type error, and it reads as a bug in
the thing being mutated. Indexing the vector inline or using a loop
variable both write through. Six tests failed on it before the probe
found it; recorded in `CLAUDE.md` § Loft language gotchas.

**B3 made a wall's HP structural.** `brace_of` classifies a hex by its
structure neighbours — braced / straight run / end / stub — and
`numbers.json` § wall.brace_factor_* scales the kind's figure by
1.0 / 0.6 / 0.3 / 0.15. Equal damage over two six-hex walls: the
fence loses both ends and the ring loses nothing, which is the
invariant gate verbatim. Suite **667 green**.

⚠⚠ **B3 FALSIFIED the spec's own claim about why it matters, and the
correction is the phase's most valuable output.**
`ENEMY_MOVEMENT.md` said the two rules combine — *"enemies spread along
the perimeter and chew everywhere at once, so they do not need to FIND
the weak hex"*. **Measured: they do not.** Six robots at a six-wide
fence spend twelve ticks chewing the braced MIDDLE and land **nothing
at all** on either end. The cause is plan 11 F7's own caveat: the
spread is by APPROACH and never by sidestepping, so enemies converge
onto the hexes their routes cross — which, with the core behind the
middle of a fence, is its strongest part.

So the bracing rule is exact and its consequence is **latent**: a
player's loose end is only the breach if a route happens to meet it.
Closing the gap needs the equal-distance sidestep F7 explicitly did not
build — a second steering rule, not a fix, and not this plan's. The
test asserts TODAY's behaviour so that building it turns the gate red
and points at the paragraph to rewrite.

⚠⚠ **Built 2026-08-13 as [plan 11 F7b](../11-flow-field/README.md), and
the spec's claim is now TRUE — but B3's own test stayed GREEN, which is
the interesting part.** A queued wave now arrives as a FRONT and a wall
it spans breaks at its 30 HP END rather than its braced middle
(`tests/11_f7b_the_sidestep.loft` measures exactly that, and B7's
scenario shows it end to end). B3's six robots do not queue — they come
from six different directions, so no companion ever blocks one and no
sidestep fires — so its measurement stands verbatim and means something
narrower than it first read: **the spread is by approach AND by
occupancy, and six separate approaches were never the case that needed
fixing.** Two further conditions, both measured: the fan has a WIDTH,
so a wall longer than it still hides its ends; and a wave spread thin
enough never to block itself still chews where its routes cross.

⚠⚠ **And in 2026-08-17 the gap really was closed — by
[plan 24](../24-the-siege-front/README.md) W1 — and B3's test stayed
GREEN A SECOND TIME.** The paragraph above promised the gate would go
red *"and point at the paragraph to rewrite"*. It did not, for the same
reason as in F7b: each of B3's six robots already touches the fence
where its own route meets it, so a precedence about *attacking what you
are touching* changes nothing for them.

⚠ **The lesson is about the tripwire, not the rule.** B3 aimed its
tripwire at the RULE it expected somebody to build — *the equal-distance
sidestep* — and that rule was never the fix (plan 24 `@M019`: dryopea had
one already, and it steps off a wall face as readily as along it). What
DID fire is `11_f7b`'s bracing test, which was written about the
BEHAVIOUR: *the wall a front spans breaks at its weak end.* ⚠ **Aim a
tripwire at the behaviour you want, never at the mechanism you predict**
— of the two written here, only the behavioural one ever fired, and it
fired for both landings.

⚠ And the first condition above is now the OLD mechanism: a longer wall
no longer HIDES its ends (the front grows with the wall), it DILUTES the
wave across more hexes. Length still pays, continuously rather than off
a cliff.

⚠ **Only a ROW is straight on this lattice**, and a fixture on the
wrong axis would have tested the opposite of what it said. Odd-r row
parity flips which delta a direction index carries, so a constant-`q`
column zigzags and every hex in it reads as BRACED — a "vertical" wall
a player drags is a crinkle-crankle wall and is stronger for it.
Measured against `lat_neighbour` before the fixtures were written.

⚠ **A perimeter UNZIPS, for free.** Bracing is computed from the world
rather than stored, so the hex beside a fresh breach becomes the new
end and loses more than half its allowance. The cascade takes one TICK
per link, because `damage_resolve` collects everything at or past its
limit before it breaks any of it.

⚠ **What it cost:** five fixtures in B2's file plus
`a-wall-breaks.keys`, all the same cause — a lone wall plugging a
one-hex corridor is a STUB and worth 15 HP, not 100. `numbers.json`'s
100 is the BRACED figure and almost nothing on a real map gets it. The
scenario reads better for it.

**B5a made the tower fire.** Towers are a third marker kind, range is
`lat_distance` and nothing else, and a tower **banks charge** rather
than firing per tick — because B0's awkward number says a 1.0 s fire
interval is 1.5 ticks, and a smooth rate would throw away the discrete
SHOT that B5b has to count against the 30-shot budget. Suite **691
green**.

⚠ **The float that ate a third of the tower's damage.** `1 / 1.5` has
no exact float form, so three ticks sum to 1.9999999999999998 and a
bare `>= 1.0` refused the second shot — a tower quietly at two-thirds
of its documented DPS. **No assertion about "it killed the thing"
could have seen it**; the cadence test caught it on its first run.
`TOWER_CHARGE_EPSILON` is the fix, and because a shot SUBTRACTS an
interval rather than resetting the charge, the debt stays within an
ulp of zero over 300 ticks — which is its own gate.

⚠ **The 7-hex footprint is not built:** a tower stands on one hex,
because the footprint decides what it BLOCKS rather than what it can
reach, and blocking is a passability question for plan 06 S1's
structure layer.

**B5b gave it eyes and a magazine.** A shot travels in a straight line
from the tower's eye (its hex plus 6.0 m) to the target's body (its hex
plus the 1.0 m robot on it), and anything the line does not clear stops
it — one rule, read off `hex_height`, with **no table of what blocks**.
Thirty shots and the tower goes black. Suite **715 green**.

⚠⚠ **It falsified BOTH halves of what `DESIGN.md` § 7 said LOS was**,
and that is this phase's most valuable output — the same shape B3 hit.
The doc said *"blocked by `wall_high` + `steep_rock`, not by `wall`"*:

- **A `wall` blocks** once it stands past ~3/5 of the way to the
  target, because the shot has descended below 3 m by then. The rule a
  player learns is **a tower must overlook the wall it covers**: one
  hex behind it the besiegers are targets, two hexes back they are in
  dead ground. It is "sealing is punished rather than forbidden"
  arriving from a direction nobody planned.
- **`steep_rock` blocks nothing**, because dryopea has no terrain
  elevation at all — `height_override` is null for every terrain kind.
  Nothing has to change when plan 02's slope solver lands; the test
  asserting today's answer is what goes red that day.

⚠ **And a pile of bodies blinds the tower that made it.** Rubble is a
height, so the heap that ramps a kill zone shut also puts it out of
sight — ten dead robots are 5 m of wreckage. Nobody designed that; it
falls out of reading a height instead of a material.

⚠ **One rule replaced two, and it turned two B5a assertions red.**
B5a burnt the charge whether or not there was anything to shoot, so an
idle tower could not bank a burst. B5b's rule is *a shot is spent only
when it is FIRED, and it is fired only at something the tower can see*,
which covers an empty field and a blocked line together — and keeps the
anti-burst property with a CAP: a tower holds exactly one interval,
because a capacitor holds one shot and not a magazine. Without the cap
a tower blinded for a hundred ticks empties two thirds of its budget
into the first thing it sees.

⚠ **The cost gate could not see this phase, and now it can.**
`wave_fire` does nothing in a world with no markers, so
`tests/11_f8_the_tick_budget.loft` was measuring an undefended tick and
would have stayed green through any sight-line cost whatever. It now
ticks six firing towers — 173 ms against 167 ms undefended, a quarter
of the 667 ms budget. But the honest reading is that **the budget still
cannot see LOS**, at 3% of a tick with 3.8x headroom above it, so a
second test prices the ALTERNATIVE: twelve shots the shipped way cost
**51%** of ONE roster-wide `tower_sees` pass, where tracing per enemy
would cost twelve passes. 2x margin below it, 24x above.

**B6 gave the run an ending.** `src/wallet.loft` holds 200 points, an
enemy within one hex of the core drains 1 pt/s off them, and zero is
the only end state dryopea has — the core is invulnerable and stays
so. `wallet <lo> <hi>` is the measurement, and `wallet 0 0` is how a
`.keys` scenario says *the base fell*. Suite **734 green**.

⚠ **The half that is not arithmetic is WHO counts as a nibbler**, and
it is where the phase could have been silently wrong. Reach is a
straight-line `lat_distance` of 1, read off `numbers.json` §
core.footprint_layout — the core is a radius-1 disc, so an enemy
within one hex is standing ON it rather than near it. The tempting
alternative, *drain for every live enemy*, passes every assertion
about rates and floors in this phase's file while **making walls and
towers pointless**: a base under siege would bleed at exactly the rate
of a base that had been overrun. `test_a_perimeter_that_holds_costs_
the_player_nothing` is the one that refuses it — six robots grinding a
wall ring for twenty ticks, and the budget does not move.

⚠ **A nibbler is a POSITION, not a target.** `enemy_target` answers an
arrived enemy's OWN hex, because it names what is in the way and
nothing is — so there was no existing answer to reuse and no field to
walk. One `lat_distance` per live enemy, which is strictly less work
than the `enemy_in_bubble` the mover already does.

⚠ **The ledger is clamped where it is WRITTEN, and that is not a
duplicate of the read.** Flooring only at the read lets `spent` run to
10 000 against a 200-point budget: `wallet_left` still answers 0 and
everything looks right, until the first thing that ever CREDITS the
wallet buys nothing for the next 9 800 points. Loot is 10 points a
kill and the carryover between bases is 1:1, so that thing is designed
and coming.

⚠ **The clock is 301 ticks for one nibbler, not the 300 the arithmetic
says.** `TICK_SECONDS` is `1 / 1.5`, which has no exact float form, so
three hundred of them sum a hair under 200 s. Same fact
`TOWER_CHARGE_EPSILON` exists for, with a consequence a hundred times
smaller — there it dropped every third shot, here it is 0.3% of a
200-second clock. So there is deliberately **no epsilon**: adding one
would make `wallet_broke` answer true while `wallet_left` still
answered a positive number, and two public verbs disagreeing about the
floor is worse than a clock that runs two thirds of a second long.
`numbers.json`'s own rule of thumb — *5 enemies, 200 pts, 40 s* — is
exact at 60 ticks, because five thirds of a point per tick divides in.

⚠ **A probe before a representation, and it paid.** All three existing
runtime layers on `WaveState` are hash-backed, so nothing in the
codebase said whether a write to a SCALAR field of a nested struct
survives — which is loft#894's exact shape. Measured on both backends
before building on it: a nested struct reached by field access is a
reference and writes through; only a struct RETURNED from a function
is a copy.

**B7 built the clock and its control.** Three `.keys` scenarios that
differ only in their defences, a `fall <max>` verb that plays until
the wallet empties, a `ticks <lo> <hi>` measurement, and
`tests/12_b7_the_clock.loft` — where the three clocks are in scope at
once, which is the only place the comparison can be made. Suite **739
green**; the gate is **18 scripts and 303 measurements**.

⚠⚠ **The finding under every number was that the drain did NOT scale
with the wave — and it is what got the mover fixed.** Thirteen robots
arrived at the undefended core and exactly TWO ever nibbled it: they
came down one axis from one spawn, and on a hex AXIS the distance
field offers exactly ONE closer neighbour, so a blocked enemy waited
where an off-axis one would have had a second choice. Priced as an
invariant rather than inferred — a column of four and a column of
twelve drained at exactly the same rate — with two corollaries measured
while the scenario's world was chosen: the WIDTH was scenery (161
ticks over five rows, 161 over thirteen) and so was the ROSTER.

**[Plan 11 F7b](../11-flow-field/README.md) built the missing rule on
the strength of this**, and the numbers above are the rebaseline. The
drain now scales and saturates at the core's seven-hex footprint.

⚠ **A GATE is not a defence, and the measurement survived the mover
changing.** The same five-hex wall with its middle hex left open falls
in **62 ticks** against the undefended base's 61, where a SEAL buys
43. (It was 161 against 161 before plan 11 F7b — exact, and now one
tick for the step it costs a robot to find the doorway.) Walking
through an entrance costs an attacker essentially nothing, so a wall
buys time only where it has to be CHEWED. It is a rule a player can learn: an entrance is a decision
about their own convenience.

⚠ **Everything a tower does to the clock, it does through BODIES.**
The tower is not weak — it kills eight of thirteen, and its magazine is
gone before the wave list is. What undoes
it is that a kill is a permanent terrain change nobody can reverse,
and two of them do three things at once: they ramp over the wall
(B0's band), they blind the tower that made them (B5b), and they push
the queue off-axis so MORE of the survivors reach the core. The design
already has the answer and plan 12 does not build it — salvage decays,
so bodies must be collected at the worst possible moment, and the crew
that does it arrives with the vehicle.

⚠ **B7 asserts today's behaviour on purpose.** Building F7's sidestep,
or a body-clearing crew, turns `tests/12_b7_the_clock.loft` red — and
red is the correct answer that day, pointing at the paragraph to
rewrite. Same discipline B3 used for the bracing consequence it
falsified.

⚠ **Quote the measured work, never a suite wall clock.** Timed
directly, the three replays cost 668 + 1703 + 717 ms. The suite's own
before-and-after swung by twenty seconds across runs of the SAME tree
and would have justified almost any conclusion about them —
`CLAUDE.md` § Profiling the suite's warning arriving in the one place
it is easiest to ignore, judging the cost of your own change.

⚠ **Cost, honestly.** B2 adds one `enemy_target` per live enemy per
tick — it reuses the fields the tick already built, so no new sweep —
and `tests/11_f8_the_tick_budget.loft` stays green. A standalone
stopwatch on the same load was NOT trustworthy (leaked stores, 4x
swings between runs of an unchanged probe), so the in-suite gate is
the number of record; do not quote a figure taken any other way.

Plan 11 gave an enemy a complete journey that ends in nothing. It spawns,
routes round walls by its climb limit, spreads rather than stacks, and when
the perimeter is sealed it follows the desire field and `enemy_target` names
the exact wall hex it attacks. Then the simulation holds forever: no wall
loses HP, no enemy loses HP, nothing dies, nothing breaks. **Every damage
number in `examples/numbers.json` is read by nobody.**

This plan makes the exchange resolve, and its headline artefact is one
unattended scenario: **a base with walls that funnel and towers that fire,
nobody defending it, and a wave list that eventually takes it — slowly,
because of the defences.** The clock is the deliverable. A base that falls
in 40 s and a base that falls in 400 s are the difference between "damage
exists" and "defences work", and only the second is worth building.

## Goal

An authored base defends itself unattended — walls funnel, towers kill,
bodies pile, walls break into rubble — and falls on a measurable clock that
gets markedly longer when the defences are present.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 5 (wall topology, entrances),
  § 7 (towers: range / interval / damage / shot budget / LOS; enemy
  targeting + nibble), § 4 (core invulnerability → wallet drain).
- [`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) § Bodies are
  terrain, § A wall's HP is structural, § The tick resolves once.
- [`examples/numbers.json`](../../examples/numbers.json) — every value this
  plan consumes already exists there. **This plan adds no new tunable
  without a row in that file.**

Source files it touches: `src/passable.loft` (the climb number and
`hex_ground`'s fall-through), `src/height.loft` (the additive runtime layer
— already built, gains a **source** and a **clear** and becomes the rubble
layer), `examples/palette.json` + `docs/GROUND_TYPES.md` (the rubble entry),
`src/spawn.loft` (the tick), `src/script.loft` (the new measurements), and a
new `src/damage.loft` / `src/tower.loft`.

## The two things settled before phase 1

**1. The core stays invulnerable; the wallet is the clock.**
`DESIGN.md` § Invulnerability + nibble → points and `numbers.json`
(`core.hp: null`) both commit to it, and the doc says so explicitly —
it *"retires the @PLAN46-original 'core destroyed = run ends' framing"*.
So "the heart is destroyed" is spelled **the wallet reaches zero**: 200
starting points, 1 pt/s per nibbling enemy. Unattended, nothing refills it,
so the base falls on schedule with no design reversal. The measurement is
the same number either way.

**2. Rubble is its OWN layer, above the ground — and the rubble is a hill.**
A broken wall becomes a pile of rubble; a killed enemy becomes one too.
Higher than the ground it sits on, lower than the wall was, climbable from
every side, and **clearable by the player** — which is precisely why it is a
layer and not a repaint. The ground underneath is never overwritten, so
clearing a pile restores exactly what was authored, with nothing to
reconstruct.

⚠ **That is what dissolves the sea trap**, and the trap is worth recording
because it would have passed a naive test. Had rubble been a repaint, a
broken wall would have had to erase the painted `wall` — and the painted
layer is sparse and **sea-default**, so an erased hex reads as `sea`, which
is `walk_ground: false`. The breach would have been *more* impassable than
the wall it replaced, while "the wall broke" asserted true.

**Extend `src/height.loft` rather than adding a third sparse map.** It is
already *"a sparse map of metres ADDED to what the palette paints"*, already
ACCUMULATES, already runtime-only and out of every save, and
`ENEMY_MOVEMENT.md` § Bodies are terrain already specifies the mechanic —
its one real semantic today *is* a body pile, which is machine rubble under
another name. What B1 adds to it is a **source** and a **clear**. A pile on
a hill then sits higher than the same pile on grass for free, because the
arithmetic is already additive.

⚠ **The rubble is the hill; what is IN it is a different layer** — and the
line between them is not "rubble is one thing". **Ask whether two values
can be true of one hex at once.**

- **Source material** — machine wreckage, insect carapace, broken masonry —
  is a **closed set, one per hex**, exactly like `sand` / `grass` / `rock`.
  That is a legitimate ground-type axis, and eventually there are three
  rubble entries rather than one.
- **Salvage contents** — several types in one pile, mashed, each visible to
  the player — is **open and multiple per hex**. A palette kind cannot hold
  it, so it lands on the **stacked-layer / additional-mesh layer**, the same
  one [plan 06 S1](../06-editor-stencil-pipeline/README.md) builds for
  content inside houses.

Neither is built here. **B1 ships ONE rubble kind** — three entries whose
only difference is a colour would be two rows nothing can fail on — but the
producer takes its **source** as an argument, so the eventual split is a
palette row and an enum value rather than a hunt through every site that
makes rubble. B2 and B4 are already three distinct producers (a wall
breaking, a robot dying, an insect dying), so the argument has real callers
from the day it exists.

⚠ **"A natural ramp on all sides" is not a new rule — it is a height under
the climb limit.** The passability rule is already
`height(to) - height(from) <= climb(class)`, and a rise a class can clear is
exactly a ramp to it. If rubble needs a special case to be climbable, the
number is wrong, not the rule. That is what B0 exists to settle.

## The blocker B0 answered

**`CLIMB_REGULAR` was 0.0** — B1 raised it to 2.0, and everything in this
section is what B0 measured while it still was. A robot could not climb
*any* rise whatsoever: a drop was free, a rise of 0.01 m was refused. Three
consequences, and all three were load-bearing for this plan:

- **Rubble cannot be climbable by a robot at any positive height.** The
  request is unbuildable as the number stands. *Confirmed:* 0 of the 40
  heights from 0.1 m to 4.0 m are climbable.
- **The design's own body-ramp mechanic is dead for robots.**
  `ENEMY_MOVEMENT.md` § Bodies are terrain says *"enemies climb their own
  dead onto the wall"*; at climb 0.0 only insects (3.0) ever can, so the
  clause is false for the class it was written about.
- **It has never been visible**, because nothing has ever raised a hex
  under a robot in anger. F6 built the layer and gated it with insects.

### ⚠ The recommendation of ~1.0 m was wrong, and here is the rule

The plan proposed *"a small positive value (~1.0 m)"* on the strength of
three claims. B0 measured all three. The first and third hold; **the
second — "revives the body ramp for robots" — is false**, and the reason
generalises:

> **A single-hex body ramp onto a structure `H` high needs a climb of
> `H / 2`.** The pile has to be low enough to step ONTO *and* high enough
> to leave less than a climb above it, and one number does both jobs. So
> the workable pile heights are a **band**, `[H - c, c]`, and it is EMPTY
> whenever `c < H / 2` — however many bodies fall.

A `wall` is 3.0 m, so the robot ramp opens at **1.5 m and not before**.
One metre buys the rubble ramp and leaves the body ramp exactly as dead
as it found it. `tests/12_b0_probe.loft` sweeps the rule rather than
restating it, and checks it against the one ramp the game already walks
(insect 3.0 m onto `wall_high` 5.0 m — needs 2.5, has 3.0).

### The number B0 hands B1: `CLIMB_REGULAR = 2.0`

Four constraints, and 2.0 is the interior of what they leave:

| Constraint | Bound | Why |
|---|---|---|
| rubble climbable at all | `c > 0` | B1's whole request |
| body ramp onto a `wall` | `c >= 1.5` | `H / 2` — and **B7 needs it**, since "the pile went over it" is one of the two endings B7 must be able to report |
| `wall_high` stays the harder barrier | `c < 2.5` | else a robot single-hex-ramps a 5 m wall too, and the two wall types stop differing for robots |
| a bare `wall` still stops a robot | `c < 3.0` | plan 11 F1b |

At 2.0 the band onto a wall is `[1.0, 2.0]` — **one to two robot bodies**
at `numbers.json` § enemy_regular.height — which reads as "a couple of
dead robots get the next one over", the mechanic as written.

⚠ **The band has a CEILING as well as a floor**, which is worth knowing
before B4 drops bodies in anger: three bodies on one hex is a 3.0 m step
and a 2.0 m climber cannot get onto its own ramp any more. A pile can
grow past being a ramp.

~~⚠ **Input for B4's body height.**~~ — **DECIDED in B4: 0.5 m**, as
recommended, and added to `numbers.json` as
`enemy_regular.body_height` rather than reusing `enemy_regular.height`
(1.0 m), which is a STANDING robot. At 0.5 m the band `[1.0, 2.0]` is
**two, three or four dead robots** — three strictly inside it — and
**five is a heap the next one cannot climb**, which is the ceiling B0
warned about arriving as a real mechanic rather than an edge case.
`tests/12_b4_death.loft` sweeps the band in bodies rather than
restating the arithmetic.

### What B1 actually costs — measured, not estimated

| `CLIMB_REGULAR` | `scripts/test.sh` | `scripts/validate.sh` |
|---|---|---|
| 0.0 (today) | 585 pass | 233 measurements, green |
| 1.0 | 1 fail | green |
| **2.0** | **12 fail** | **1 scenario fails** |
| 2.9 | 12 fail (the same 12) | — |
| 3.0 | 51 fail across 6 files | — |

⚠ **The binding constraint is a TEST FIXTURE, not a design number.** All
12 failures are `tests/11_f6_height_step.loft` (9) and
`tests/11_f8_the_tick_budget.loft` (3), plus
`tests/scripts/two-classes-two-routes.keys` — and every one of them
traces to the same choice: a **1.5 m pile** used to mean *"past a robot's
climb"* back when any positive number meant that. Once the climb is
positive those fixtures have to name a height above it (2.5 m does).
That is B1's mechanical cost and it is why B1 is no longer free.

⚠ **The 3.0 row is the negative control, and it is why the 1.0 row means
anything.** A constant whose change breaks nothing might simply be read
by nobody; 51 failures across 6 files prove the gates see this one, so
"1.0 costs one string assertion" is a measurement rather than a silence.

## Invariant gate

Exact invariants per phase — the concrete expected result, the invariant it
pins, and the input that must be **refused**.

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **B0** ✓ | a robot refuses a 0.1 m rise today; an erased `wall` hex is impassable today | the probe records what IS, before anything changes it | — (B0 asserts the present, so its own gate is that B1 turns it red) |
| **B1** ✓ | robot steps onto rubble at `climb`; refuses at `climb + ε`; **clearing a pile leaves the hex identical to before it was piled** | a ramp is a height under the climb, not a flag — and rubble is a layer, so clear is an identity | rubble one notch above the climb **must** be refused, else the height rule is decorative; a cleared hex that differs from the authored one means the ground was overwritten after all |
| **B2** ✓ | wall at 1 HP does not break; at 0 HP the hex carries rubble and is standable | breaking OPENS a route (the sea trap is closed) | a broken hex that reads as `sea` — impassable — is the bug this phase exists to avoid |
| **B3** ✓ | a straight fence breaches at an **end**; a closed curved ring of equal length and equal attackers does not breach at that tick | HP is structural, from bracing, not a constant | the ring breaking on the same tick means bracing was never read |
| **B4** ✓ | 30 HP enemy survives 2 shots' worth, dies on the 3rd; death hex gains one body of height | death frees occupancy and raises terrain, both | two deaths on one hex must stack — a body pile that overwrites is not a pile |
| **B5a** ✓ | enemy at 15 hex is hit; at 16 it is not | range is a lattice distance, `lat_distance` and nothing else | a `+1` on q/r reaching for range is moros#10 again |
| **B5b** ✓ | tower kills through a `wall` three hexes out; does **not** kill through a `wall_high` there; stops firing after 30 shots; **a blocked shot is not FIRED** — neither the charge nor the budget is spent | LOS reads the height, and decay is per-shot not per-time | a tower that fires shot 31 has no budget; one that shoots through `wall_high` has no LOS; one whose budget falls while every line is blocked is spending shots it never took |
| **B6** ✓ | N nibblers drain exactly N pt/s × tick seconds; the wallet floors at 0 | the wallet never goes negative and never refills unattended | a negative wallet means the run has no end state |
| **B7** ⚠ | **half met.** A sealed wall: 311 ticks against the bare base's 161. A wall **plus a tower**: 180 — the defences make it *shorter* | the wall costs the attacker time; a tower's BODIES give it back | equal times = the scenario measures nothing, whatever it draws — refused, and the gated-wall variant is exactly that failure at 161 = 161 |

⚠ **B5b's row was rewritten by building it**, and the change is the
finding rather than a slackening. It said *"does not kill through
`wall_high` or `steep_rock`"*, which named two materials; what the
phase built names a HEIGHT and a PLACE, so the row now fixes the
geometry (*a `wall_high` three hexes out*) and drops `steep_rock`
entirely — it is 0.0 m and blocks nothing, because dryopea has no
terrain elevation yet. The original wording would have been satisfiable
only by the materials table the phase exists to avoid.

## Phases

Each phase must be able to go red on its own. Where a phase would otherwise
build something no caller reaches, the **script runner** is the caller —
the same instrument-first move as plan 08 V2 and plan 11 F1.

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **B0** — the climb number, and what a tick is worth | XS | `tests/12_b0_probe.loft` — asserts TODAY's refusals so B1 must turn them red | **Done** |
| **B1** — rubble is a clearable layer, and a robot can climb it | S→M | `tests/12_b1_rubble.loft` — robot crosses rubble; refused at climb+ε; clear round-trips to the authored hex; F1b's wall stays green. Plus: B0's three ⚠ B1 tests turn red, and F6/F8's 1.5 m pile fixtures move above the new climb | **Done** |
| **B2** — a wall breaks into rubble | S | `tests/scripts/a-wall-breaks.keys` — sealed base, siege, breach, and an enemy ends up INSIDE | **Done** |
| **B3** — structural HP by bracing | M | `tests/12_b3_bracing.loft` — straight fence vs closed ring, equal hexes and attackers | **Done** |
| **B4** — enemies have HP, die, and leave rubble | S | `tests/12_b4_death.loft` + `count alive` falling under a scripted `damage` | **Done** |
| **B5a** — the tower fires | M | `tests/12_b5a_tower.loft` — killed at 15 hex, untouched at 16 | **Done** |
| **B5b** — line of sight and the shot budget | M | `tests/12_b5b_los_budget.loft` — `wall` vs `wall_high`; shot 31 never fires; a blocked tower spends nothing | **Done** |
| **B6** — nibble drains the wallet, zero ends the run | S | `tests/12_b6_wallet.loft` — the rate, the floor, the reach's negative control, and `wallet <lo> <hi>` through the seam | **Done** |
| **B7** — the scenario, and its control | S | `tests/12_b7_the_clock.loft` over three `.keys` scenarios — the clock separates, and a tower inverts it | **Done** |

### Why the order is this order

B1 before B2 because a wall that breaks into a terrain nothing can stand on
is worse than a wall that never breaks — the sea trap makes a breach *more*
impassable than the wall it replaced, and it would pass a naive "the wall
broke" assertion. B4 before B5 because death and the tower are separate
claims: the script kills the first enemy so that when a tower kills the
next one, the only new thing under test is the aiming. B7 last because it
is the only phase whose gate is a comparison between two runs, and it can
only be built once both runs are possible.

## What this plan does NOT build

No player, no vehicle, no helpers, no runtime wall construction, no repair,
no boost, no beacon ferry, and **no item layer** — the recoverable salvage
that will later sit on a rubble pile is plan 06 S1's stacked-layer work, and
B1 only has to avoid foreclosing it. Clearing rubble exists as an
**operation** in B1, because a layer that cannot be cleared is a repaint
wearing a layer's name and its round-trip is what gates it; what does not
exist is a **player to trigger it**, which arrives with the vehicle. The
base is
**authored** — walls
and towers are painted or placed before the run and nobody touches them
after. That is what makes the clock a clean measurement: with a player in
it, time-to-zero measures the player.

⚠ Consequence worth stating: bodies pile and **nobody collects them**, which
`ENEMY_MOVEMENT.md` § Bodies are terrain says is the losing line — the kill
zone ramps itself shut and then over the wall. That is not a defect of the
scenario, it is the scenario's most interesting outcome, and B7 should
report which of the two ends the base: the wall broke, or the pile went
over it.

### ⚠ Design recorded during this plan, and NOT built by it

A run of owner design input landed while B1-B5a were being built
(2026-08-13). **The mechanics live in
[`docs/DESIGN.md`](../../docs/DESIGN.md)** — § 7 § Future tower types
for the damage-type matrix, aiming time and retaliation, § 10 § Small
robots for the four roles and the speed rule, and
[`ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) § Bodies are
terrain / § Retaliation for the movement halves. They are not restated
here; a second copy is the one that gets read and the one that drifts.

In one paragraph, so this plan reads on its own: a wreck is not rubble
yet — it carries harvestable salvage that decays into rubbish, and a
**big** body seals its hex until it settles, which makes a plugged
chokepoint push the wave onto the wall instead. Towers get damage
TYPES that trade kill speed against salvage against how long the corpse
plugs the gap, plus a traverse time that makes switching targets
expensive. Enemies retaliate against towers that hurt them, by an
information rule the scrambler gates — but never against routing, so a
tower behind a closed perimeter is simply unreachable.

**What bears on the phases still open here**, and only that:

- ✓ **B5b: line of sight is a HEIGHT question, not a table of
  materials.** Built, and the height turned out to decide *less* than
  the note assumed: what blocks depends on where an obstacle stands as
  well as how tall it is, so the *`wall_high` blocks / `wall` does not*
  shorthand is wrong in both directions. See § B5b above and
  `DESIGN.md` § LOS is a HEIGHT question, which the phase rewrote.
- ✓ **B5b: a shot that has become impossible is NOT fired** — built,
  and it absorbed the empty-field case with it: one rule, *a shot is
  spent only when it is fired*, plus a cap so holding fire is a
  capacitor rather than a stockpile.
- ⚠ **B5a's `tower_pick` is a placeholder.** It re-chooses the nearest
  enemy every shot with no cost to switching, which is exactly what
  traverse time replaces. Whatever replaces it will want hysteresis
  and will still have to be deterministic, because plan 08 gates
  dryopea by replaying written-down runs.

#### What it needs that does not exist — the implementation ledger

This table is the one thing here that is NOT in the design docs, and it
is why the section stays: it maps each mechanic onto the file that
would hold it, so the eventual plan starts from a survey rather than a
reading.

| Needs | Where it would go |
|---|---|
| a decay clock per pile | `height.loft`, beside the source — the layer is already runtime and already per hex |
| a per-class body height and decay rate | `numbers.json` rows beside B4's `enemy_regular.body_height` |
| tower damage TYPES (laser / artillery / explosive / EMP) | `numbers.json` § tower has `damage_per_shot` and no type; B5 should avoid foreclosing one |
| ⚠ speed DECOUPLED from the tick | owner instruction: speed varies by role, by tier and by CONDITION (a damaged robot moves slower), so *do not link speed too closely to a tick*.  The tick becomes a timestep and every enemy banks movement progress the way `tower.loft` banks a fire interval — reuse that, epsilon included.  ⚠ It turns B0 § 4's one-hex assertions into an inverted gate, and it makes "the tick got shorter" a third trigger for plan 11's incremental rebuild, because the per-tick budget shrinks in proportion |
| a builder that can REPAIR a boss | an option on the role, gated by near / room to reach / unharassed — two of which are ARCHITECTURAL, decided by where the player put their walls |
| a per-ROLE damage-to-wall rate | `spawn.loft::ENEMY_DAMAGE_TO_WALL_PER_SECOND` is one constant that already carries the note saying it becomes a lookup.  ⚠ The four small-robot roles — scout, harvester, builder, miner — differ in THIS AND NOTHING ELSE, so they cost a row each in `numbers.json` and one branch, with no new behaviour |
| enemy ARMOUR and SIZE, and damage scaled by type against them | `numbers.json` rows per class beside `hp`; the scaling belongs beside `enemy_max_hp` in `damage.loft`.  ⚠ SIZE is needed by the blocking rule anyway, so the flame thrower costs no new property |
| tower HP, and a tower that can be destroyed | `numbers.json` § tower has a shot budget and repair times but no HP — retaliation needs one |
| retaliation memory: which tower hurt whom | per-enemy while scrambled, shared while not; the scrambler state itself is a global nothing yet models |
| a BOSS that orders the squad onto its attacker | ⚠ **as DATA, not as a second AI.** *Bosses are not special in their AI; their size and options are different, and that is what makes them special events* (owner). It is a per-class option beside armour and size — the same discipline `climb_limit` keeps when it calls itself a class's WHOLE contribution to passability |
| routing for a unit wider than one hex | ⚠ the field is built for a ONE-hex unit; a boss needs a sweep with a clearance requirement, a second key beside the climb limit `wave_fields` already groups on |
| LOS from a HEIGHT rather than a materials table | B5b, and it is the one item here that phase should not defer — a materials lookup is a second source of truth for numbers `passable.loft` already owns |
| a per-TYPE range PROFILE, not one `TOWER_RANGE_HEXES` | `tower.loft` — `tower_in_range`'s single `<=` becomes a curve: a sniper is bad below a minimum, a flame thrower nothing beyond a short one.  ⚠ `lat_distance` stays the only thing that measures; it is the comparison that grows |
| a tower FACING, and traverse time between targets | `TowerState` — it already carries the per-tower charge, and B5a wrote it as a struct so a second field costs a line |
| projectile travel time, so a fast enemy can be missed | a shot in flight is state nothing has; it is the one item here that needs a new record rather than a new field |
| splash radius, and shots that damage structures | `damage.loft::damage_apply` already takes a hex — this is a caller, not a mechanism |
| the salvage CONTENTS a wreck carries | plan 06 S1's stacked layer (§ The rubble is the hill: open and multiple per hex, so not a ground type) |
| a player to aim, and presence to gate it | the vehicle — not in this plan at all |

**Not this plan's.** Plan 12 ends at the exchange resolving; nothing here has
a player, an item layer, a decay clock or a tower damage type. It wants its
own plan once plan 06 S1 exists to hold the contents — and B5 only has to
avoid foreclosing a damage type, which one `numbers.json` field and one
parameter would later add.

## Open questions

1. ~~**`CLIMB_REGULAR`**~~ — **DECIDED in B0: 2.0 m**, not the ~1.0 m first
   proposed, because a body ramp needs half the structure it climbs and
   half a `wall` is 1.5. The rule, the four constraints and the measured
   cost are in § The blocker B0 answered. B1 applies it.
2. ~~**What is a tick worth in seconds?**~~ — **DECIDED in B0: the tick
   carries a duration, and the rates stay per-second.**
   `spawn.loft::TICK_SECONDS` is `1 / 1.5 ≈ 667 ms`, **derived** rather
   than chosen: `enemy_tick` advances exactly one hex and the design says
   1.5 hex/s, so there is no second opinion to have. B0's probe measures
   the one-hex half against the running mover, which is what makes it a
   derivation and not two numbers that happen to agree.

   The fork was settled by a measurement rather than a preference: **a
   tower's 1.0 s fire interval is 1.5 ticks** — not a count of anything —
   so per-tick integers cannot express the design's own numbers and
   rounding either way moves the tower's DPS by 33%. (The 5.0 s scramble
   window is 7.5 ticks and the 15.0 s inter-wave delay 22.5.) So
   `numbers.json` goes on saying `HP/s`, which is also what keeps it
   moddable, and a rate reaches the sim through **`spawn.loft::per_tick`**
   at the point it is applied. B4 and B6 are its first two callers.
3. ~~**Where do towers come from?**~~ — **DECIDED in B5a: a third
   marker kind**, `MARKER_KIND_TOWER`, exactly as this entry proposed.
   The marker layer's own header says what it is for — *placeholder
   content the runtime reads but the painted palette does not
   represent* — and towers are authorable in the editor that already
   exists, measurable by a `.keys` script that already speaks
   `marker <q> <r> tower`.

   ⚠ **It was NOT free, and the cost was the place-kind cycle.** Two
   presses of `K` used to return to spawn and now land on `tower`, so
   nine `.keys` scenarios and five inline fixtures each needed a third
   press. That is what a player does too, which is the point of the
   scripts going through the same seam — but it is the widest fixture
   churn any phase of this plan has caused.

   ⚠ **And it found a latent bug in three places.** `save.loft`'s
   sidecar load, `history.loft`'s undo and its redo each tested for
   TARGET and fell through to spawn, so any kind they had not learned
   about would have been silently placed as a **spawn marker with a
   heading** — a wave source where the player had put a gun. All three
   go through one `markers.loft::place_marker` now, which skips a kind
   it does not know rather than guessing.
4. ~~**Rubble is a palette entry reached from the rubble layer, not from
   the painted one.**~~ — **DECIDED and BUILT in B1**, as recommended,
   with one addition the phase found: the entry is reached for the
   SURFACE only. `hex_height` reads the AUTHORED entry and adds the
   layer's metres, because answering the height off the surface would
   let rubble's null `height_override` swallow the 3.0 m wall it is
   sitting on — piling debris onto a wall would LOWER it. So
   `passable.loft` has two lookups, `painted_ground` for the height and
   `hex_ground` for the surface, and `hex_walkable` / `stand_fault` /
   `can_stand` / `hex_ground_name` took the layer as a parameter.
   The hotkey question resolved the other way from "not a blocker":
   `bindings.loft` HAD a twelfth palette hotkey (`=`, inert over an
   eleven-entry palette), and B1 **deleted** it. An authored rubble hex
   would be a second representation of a pile that `height_clear` could
   not take away, so no key and no `.keys` verb reaches entry 11. The
   picker still draws twelve swatches; that is the UI question, still
   open and still not a blocker. The original text follows.

   **Rubble is a palette entry reached from the rubble layer, not from the
   painted one.** Keeping it a `GroundType` is what lets `can_stand` /
   `can_step` read `walk_ground` with no branch for rubble — the passability
   rule stays one rule. So `hex_ground` prefers the rubble layer when a hex
   carries a pile and falls through to the painted kind otherwise, and the
   entry appends at index 11 (leaving 0-10 unsheared). It is the first
   ground type the **player never paints**: `numbers.json` § input says
   *"11 entries match the 11 ground types"* and lists exactly 11 hotkeys, so
   the 12th simply has no binding. Whether the picker should show an
   unpaintable entry at all is a UI question, not a blocker. *Decided in B1.*
5. ~~**What is the ground under a destroyed wall?**~~ — **DECIDED in
   B2: a default ground, `grass`, named once as
   `damage.loft::BROKEN_GROUND`** — the cheapest of the three answers
   below, exactly as this entry recommended, with the site routed
   through one constant so the third answer stays a change of one body.

   Two things the phase found while building it:

   ⚠ **"Remove the wall" really does edit the painted world.**  This
   entry called the removal *persistent* and B2 took that literally:
   `break_structure` repaints the hex rather than shadowing it behind
   a runtime overlay.  The alternative — a "removed structures" set
   consulted by `passable.loft` — would have threaded a fourth
   parameter through `hex_walkable`, `can_step`, `can_climb`,
   `flow_build`, `enemy_tick` and `enemy_target`, and the NEXT runtime
   layer would have threaded a fifth.  The consequence to know: a
   session that plays a wave and then saves persists the breach, which
   is right for a run and would be wrong for an editor that ran one by
   accident.

   ⚠ **The default ground is the guard that LASTS.**  The heap alone
   makes a breach standable, so the repaint looks redundant — until a
   player clears the rubble, which B1 built.  Sweep a breach that had
   been *erased* rather than repainted and the painted layer's sea
   default seals it again, so tidying up would undo the breach.
   `tests/12_b2_break.loft::test_and_it_survives_the_player_sweeping_it`
   is the one that would catch it.  The original text follows.

   **What is the ground under a destroyed wall?** The one question the
   separate rubble layer does *not* answer. A broken wall is two effects,
   not one: the wall is **removed** (authored content gone, persistent) and
   rubble is **deposited** (runtime, clearable). Clearing the rubble must
   therefore reveal ground — and today nothing knows what that ground was,
   because painting `wall` in the editor overwrote it and `MapFile` cannot
   grow a second kind per hex (the loft JSON-cast hang caps it at 6 fields).
   Three answers, cheapest first: revert to a **default ground** (`grass`,
   or a per-map authored default) — simple, slightly lossy; **remember the
   overwritten kind** in the painted layer — blocked by the field cap today;
   or move **walls out of the ground palette into their own layer**, so the
   ground beneath was never overwritten at all. The third is the honest
   answer and it is also plan 06 S1's direction (walls are built structures,
   not terrain), which makes it too big for here. *Decided in B2* —
   recommendation: default ground now, with the site routed through one
   function so the third answer stays a change of one body.

## Found while planning, not this plan's to fix

~~⚠ **`examples/numbers.json` § world is stale on the lattice.**~~
**Fixed before B0 started** — it reads `offset_pointy_top_odd_r` as of
`b45b68f`, with the neighbour-parity rule and the `lat_neighbour`
pointer spelled out. Nothing left to do.

⚠ **The climb limit is under-gated, and B0 only half-fixed that.** A
change from 0.0 to 1.0 — a core movement rule — moved exactly one
assertion in 585 tests, and that assertion was a `contains("0.0")`
string check on a fault message rather than a behaviour. What gates the
constant today is F6/F8's incidental 1.5 m pile, not any statement about
what a robot is meant to climb. B0 adds the missing statement for the
values it cares about (§ 2 of the probe measures the ramp band directly),
but a class whose climb has no dedicated gate is a class whose limit can
drift; worth a look when B4 gives bodies a real height.

## See also

- [`plans/11-flow-field`](../11-flow-field/README.md) — the journey this
  plan terminates. F7 already names the hex under attack; B2 is its first
  consumer.
- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  instrument every phase here asserts through, and the `.keys` vocabulary
  B2/B6/B7 extend.
- [`plans/05-validation-scenario`](../05-validation-scenario/README.md) —
  the consumer. Its M-S5 ("wave 1 triggers + plays out") needs the tower
  engine this plan builds.
