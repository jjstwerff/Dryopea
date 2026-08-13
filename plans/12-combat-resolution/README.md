<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 12 — Combat resolution

**Value:** `G` · **Effort:** `MH`

## Status

**B0 and B1 shipped** (2026-08-13). B2 and B4 are next, both unblocked.

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

⚠ **Input for B4's body height.** At exactly 1.0 m per body, one body and
two bodies each land exactly ON a band endpoint (float equality, exact
here but with no margin). A collapsed wreck is flatter than a standing
robot; a body height of ~0.5 m puts 2–4 bodies strictly inside the band
and is the healthier number. B4 decides it.

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
| **B2** | wall at 1 HP does not break; at 0 HP the hex carries rubble and is standable | breaking OPENS a route (the sea trap is closed) | a broken hex that reads as `sea` — impassable — is the bug this phase exists to avoid |
| **B3** | a straight fence breaches at an **end**; a closed curved ring of equal length and equal attackers does not breach at that tick | HP is structural, from bracing, not a constant | the ring breaking on the same tick means bracing was never read |
| **B4** | 30 HP enemy survives 2 shots' worth, dies on the 3rd; death hex gains one body of height | death frees occupancy and raises terrain, both | two deaths on one hex must stack — a body pile that overwrites is not a pile |
| **B5a** | enemy at 15 hex is hit; at 16 it is not | range is a lattice distance, `lat_distance` and nothing else | a `+1` on q/r reaching for range is moros#10 again |
| **B5b** | tower kills through `wall`; does **not** kill through `wall_high` or `steep_rock`; stops firing after 30 shots | LOS reads the height, and decay is per-shot not per-time | a tower that fires shot 31 has no budget; one that shoots through `wall_high` has no LOS |
| **B6** | N nibblers drain exactly N pt/s × tick seconds; the wallet floors at 0 | the wallet never goes negative and never refills unattended | a negative wallet means the run has no end state |
| **B7** | the defended base's time-to-zero is markedly longer than the same base stripped of walls and towers | the defences are what cost the attacker time | equal times = the scenario measures nothing, whatever it draws |

## Phases

Each phase must be able to go red on its own. Where a phase would otherwise
build something no caller reaches, the **script runner** is the caller —
the same instrument-first move as plan 08 V2 and plan 11 F1.

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **B0** — the climb number, and what a tick is worth | XS | `tests/12_b0_probe.loft` — asserts TODAY's refusals so B1 must turn them red | **Done** |
| **B1** — rubble is a clearable layer, and a robot can climb it | S→M | `tests/12_b1_rubble.loft` — robot crosses rubble; refused at climb+ε; clear round-trips to the authored hex; F1b's wall stays green. Plus: B0's three ⚠ B1 tests turn red, and F6/F8's 1.5 m pile fixtures move above the new climb | **Done** |
| **B2** — a wall breaks into rubble | S | `tests/scripts/a-wall-breaks.keys` — sealed base, siege, breach, and an enemy ends up INSIDE | Open |
| **B3** — structural HP by bracing | M | `tests/12_b3_bracing.loft` — straight fence vs closed ring, equal hexes and attackers | Blocked on B2 |
| **B4** — enemies have HP, die, and leave rubble | S | `tests/12_b4_death.loft` + `count alive` falling under a scripted `damage` | Open |
| **B5a** — the tower fires | M | `tests/12_b5a_tower.loft` — killed at 15 hex, untouched at 16 | Blocked on B4 |
| **B5b** — line of sight and the shot budget | M | `tests/12_b5b_los_budget.loft` — `wall` vs `wall_high`; shot 31 never fires | Blocked on B5a |
| **B6** — nibble drains the wallet, zero ends the run | S | `wallet <lo> <hi>` in `script.loft`; drain rate and the floor at 0 | Blocked on B4 |
| **B7** — the scenario, and its control | S | `tests/scripts/an-undefended-base.keys` + the stripped control — the clock separates | Blocked on B3, B5b, B6 |

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
3. **Where do towers come from?** `src/markers.loft` already has a kind
   discriminant (`MARKER_KIND_SPAWN` 0, `MARKER_KIND_TARGET` 1) and the
   comment says to add one per kind. A third kind is the cheap answer, and
   it makes towers authorable in the existing editor. *Decided in B5a.*
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
5. **What is the ground under a destroyed wall?** The one question the
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
