<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 12 — Combat resolution

**Value:** `G` · **Effort:** `MH`

## Status

**Open.** No phase shipped.

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

## The blocker B0 exists to answer

**`CLIMB_REGULAR = 0.0`** (`src/passable.loft:102`). A robot cannot climb
*any* rise whatsoever — a drop is free, a rise of 0.01 m is refused. Three
consequences, and all three are load-bearing for this plan:

- **Rubble cannot be climbable by a robot at any positive height.** The
  request is unbuildable as the number stands.
- **The design's own body-ramp mechanic is dead for robots.**
  `ENEMY_MOVEMENT.md` § Bodies are terrain says *"enemies climb their own
  dead onto the wall"*; at climb 0.0 only insects (3.0) ever can, so the
  clause is false for the class it was written about.
- **It has never been visible**, because nothing has ever raised a hex
  under a robot in anger. F6 built the layer and gated it with insects.

**Recommendation, to be falsified in B0:** raise `CLIMB_REGULAR` to a small
positive value (~1.0 m). It buys the rubble ramp, revives the body ramp for
robots, and costs nothing already gated — a `wall` at 3.0 m still stops a
robot, so F1b's wall still works, and an insect at 3.0 still clears exactly
a wall and not a `wall_high`. B0 measures that claim rather than assuming
it.

## Invariant gate

Exact invariants per phase — the concrete expected result, the invariant it
pins, and the input that must be **refused**.

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **B0** | a robot refuses a 0.1 m rise today; an erased `wall` hex is impassable today | the probe records what IS, before anything changes it | — (B0 asserts the present, so its own gate is that B1 turns it red) |
| **B1** | robot steps onto rubble at `climb`; refuses at `climb + ε`; **clearing a pile leaves the hex identical to before it was piled** | a ramp is a height under the climb, not a flag — and rubble is a layer, so clear is an identity | rubble one notch above the climb **must** be refused, else the height rule is decorative; a cleared hex that differs from the authored one means the ground was overwritten after all |
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
| **B0** — the climb number, and what a tick is worth | XS | `tests/12_b0_probe.loft` — asserts TODAY's refusals so B1 must turn them red | Open |
| **B1** — rubble is a clearable layer, and a robot can climb it | S | `tests/12_b1_rubble.loft` — robot crosses rubble; refused at climb+ε; clear round-trips to the authored hex; F1b's wall stays green | Blocked on B0 |
| **B2** — a wall breaks into rubble | S | `tests/scripts/a-wall-breaks.keys` — sealed base, siege, breach, and an enemy ends up INSIDE | Blocked on B1 |
| **B3** — structural HP by bracing | M | `tests/12_b3_bracing.loft` — straight fence vs closed ring, equal hexes and attackers | Blocked on B2 |
| **B4** — enemies have HP, die, and leave rubble | S | `tests/12_b4_death.loft` + `count alive` falling under a scripted `damage` | Blocked on B1 |
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

1. **`CLIMB_REGULAR`** — 0.0 today, which makes every ramp in this plan and
   in `ENEMY_MOVEMENT.md` unbuildable. *Decided in B0.* Recommendation above.
2. **What is a tick worth in seconds?** Every damage number in
   `numbers.json` is per-second (1 pt/s, 1 HP/s, 1 shot/s) and the tick is
   a hex step at 1.5 hex/s ≈ 667 ms (plan 11 F8 measured against exactly
   this). Whether rates convert at the numbers boundary or the tick carries
   a duration is a real fork. *Decided in B0.*
3. **Where do towers come from?** `src/markers.loft` already has a kind
   discriminant (`MARKER_KIND_SPAWN` 0, `MARKER_KIND_TARGET` 1) and the
   comment says to add one per kind. A third kind is the cheap answer, and
   it makes towers authorable in the existing editor. *Decided in B5a.*
4. **Rubble is a palette entry reached from the rubble layer, not from the
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

⚠ **`examples/numbers.json` § world is stale on the lattice.** It reads
`"hex_layout": "axial_flat_top"` and warns that switching *"would invalidate
all neighbour-direction code"* — which is precisely what
[plan 09](../09-lattice-conversion/README.md) did, and dryopea has been
pointy-top odd-r since. The file is loaded by nothing today, so it misleads
a reader rather than breaking a run. One-line correction, separate commit.

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
