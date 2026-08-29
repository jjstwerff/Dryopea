<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `32` — The landing: a base placed on a map from a pick

**Value:** `G` · **Effort:** `MH`

## Status

**COMPLETE 2026-08-29.  A pick becomes a landed base — core moved, starter
tower down, crew out — a refusal changes nothing, and `land <q> <r> [seed]`
is a `.keys` verb so a scenario can author one.**

⚠⚠ **AND THE PLAN CLOSES ON A PROBLEM RATHER THAN A NUMBER** (`@M091`, L4).
The pick is worth **58 ticks** across the band it can be made in — and
holding it while sweeping only the SEED is worth **59**.  **The dice are
worth as much as the decision**, so `@X317`'s *land in the OVERLAP, because
the overlap is what makes a choice exist* cannot be felt through this
landing.  ⚠ The cause is `DESIGN.md` § 15 step 6's **random direction** for
the free starter tower, not the touchdown: `core_landing_area_radius` moves
a base 3 hexes, a hashed bearing puts its only defence anywhere on a ring
twenty hexes across.  ⚠⚠ **Pointing it at the nearest LIVE SPAWN is a
one-line change and contradicts § 15 as written**, so it is left OPEN as
§ Open questions 3 — the owner's ruling, not this plan's.

⚠⚠ **L0's numbers, and they are not what the plan guessed.**  Only **14-30 %
of an authored map takes a landing at all**, and the dominant constraint is
the CLEARANCE rather than the spawn rule:

| map | painted | takes a landing | of those, leave a live spawn |
|---|---|---|---|
| `starter_01` | 460 | 162 (35 %) | **80** (17 % of the map) |
| `crossroads_02` | 539 | 159 (30 %) | **159** (30 %) |
| `the_gap_03` | 510 | 169 (33 %) | **71** (14 %) |

⚠ `crossroads_02` loses NOTHING to the spawn rule because its two markers sit
at opposite ends (±22); the other two carry one marker and lose half.  **So
the content rule is *spawns at opposite ends*,** which answers § Open
questions 2 — author more markers rather than shrink the radius.

⚠⚠ **THE PROBE WAS WRONG FIRST, AND THE BUILD FOUND IT.**  L0's first version
had its own `footprint_stands` checking a disc of ONE while `landing_ground_ok`
requires a disc of THREE — footprint plus `obstruction_clearance_buffer` — so
it reported 210 / 395 / 196 where the truth is 80 / 159 / 71.  It was caught
by a landing on a real map failing at a pick the probe had called landable.
⚠ *Two implementations of one rule is a defect with a delay*: the probe now
calls the real doors and restates nothing.

⚠⚠ **AND L1's SEARCH BOUND WAS WRONG BY THE SAME KIND OF SLIP.**  It was
derived cleanly — the pick sits `LANDING_PICK_EDGE_BUFFER` (5) from the edge,
so bound the search by 5 and it cannot leave the map — but bounded from the
HASHED START, which is itself up to `CORE_LANDING_AREA_RADIUS` (3) out: 3 + 5
= 8, past the very buffer the derivation rested on.  `tests/32_l1` § A pick
over water caught it at seven hexes.  The bound is now measured from the PICK.

[`ROADMAP.md`](../ROADMAP.md) § Then the run becomes a RUN item **6**, and the
step [`ROADMAP.md`](../ROADMAP.md) § THE SESSION IS THE GAP NOW puts **first**:
[`plans/31`](../31-carryover/README.md) made a run possible and **nothing in
the game can reach it** — there is no way to play base 2.  A sortie today
begins by opening a `.keys` file or a `MAP=`, with the core already on it.

⚠⚠ **What this plan changes, in one line: where the base goes stops being
authored and becomes a DECISION.**

## Goal

A pick plus a map yields a landed base — core, starter tower, crew — and
`land <q> <r>` is a `.keys` verb, so a scenario can author one and the gates
can measure what the pick was worth.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 15 Landing flow (the eight
  steps), § 4 The core, § 9 § Roster.
- `examples/numbers.json` § `core` — `landing_pick_edge_buffer` 5,
  `core_landing_area_radius` 3, `obstruction_clearance_buffer` 2,
  `close_spawn_disable_radius` 10, `starter_tower_landing_offset` [5, 10];
  § `helper.roster_start` 2.
- `src/maps.loft::map_fault`, `src/play.loft::play_core`,
  `src/passable.loft::can_stand`, `src/markers.loft::place_marker`,
  `src/spawn.loft::active_spawn_markers`, `src/wallet.loft::wallet_carrying`.

## ⚠⚠ Three of the eight steps need NO code, and saying so is half the plan

| § 15 step | state |
|---|---|
| 1 map selection, the planet view | ⚠ **NOT this plan** — [`plans/04`](../04-map-library/README.md).  It is UI over a list, and everything below is testable headlessly without it |
| 2 landing-spot pick | this plan, L1 |
| 3 rocket descent, auto-steering off invalid hexes | this plan, L1 — the whole of it |
| 4 **random rotation of the core's six faces** | ⚠⚠ **NOTHING TO BUILD**: the faces do not exist.  `plans/28` S0 found it — *"§ 4's six faces and interior do not exist, so beside the core you TRADE, on the core you LEAVE"* (`@X294`).  A rotation of nothing is nothing |
| 5 close-spawn auto-disable | ⚠⚠ **ALREADY BUILT** — `spawn.loft::active_spawn_markers(mw, core_q, core_r, disable_radius)` has silenced markers within `SPAWN_DISABLE_RADIUS` of the core since plan 16, and it reads the core's CURRENT hex, so it follows a landing for free |
| 6 starter tower lands 5-10 hex out | this plan, L2 |
| 7 helpers emerge | this plan, L2 |
| 8 player gains control, wallet = baseline + carryover | ⚠⚠ **ALREADY BUILT** — `wallet_carrying` (`plans/31` N1, `@X347`) |

⚠ So the plan is **steps 2, 3, 6 and 7**, and the two that already work do so
because they read the core's position rather than a landing's.

## ⚠⚠ The core is AUTHORED, so a landing is a TRANSFORM and not a precondition

`maps.loft::map_fault` refuses *"a map needs exactly ONE target marker for the
core"* and then validates the map **against that core** — every spawn must be
pokeable, the ground under the core must not be sea, a regular robot must be
able to reach it.  All three authored maps carry a core at `(0, 0)`.

⚠⚠ **So a landing MOVES the core rather than creating one**, and that is the
load-bearing choice:

- a map stays **valid as authored** — `make maps` keeps refusing a map nobody
  could play, and `@M045`'s three teaching maps keep teaching;
- the authored core becomes **where the rocket lands if the player does not
  choose**, which is exactly what every `.keys` fixture in the corpus already
  means;
- and `map_fault`'s reachability guarantee stays a statement about the map
  rather than about one landing, which is the only form it can take —
  ⚠ **a map cannot be checked against a pick nobody has made yet.**

## The invariant

```
   (L-Landing-Is-A-Function)
                     a landing is a pure function of the world, the pick
                     and the seed.  Same three in, same base out, every
                     time — so a scenario can author one, `emit` can
                     write it down, and a gate can measure what a pick
                     was worth.

   (L-Landing-Is-Total)
                     a landing either yields a base that can be played
                     or is REFUSED by name.  There is no half-landed
                     base: a core on water, a footprint over a cliff, or
                     a base every spawn is silenced around are all the
                     same defect — a sortie that cannot happen — and
                     each is named before anything is placed.
```

⚠⚠ **`@FR-W-Position-Hash` is what makes the first one reachable.**  § 15 says
*random* four times — a hex within 3, a rotation, a direction, an offset
5-10 — and `docs/WORLDGEN.md` already ruled how this project spells random:
**a hash of POSITION, never a draw from a stream**, because *"a world that
cannot be reproduced cannot be gated"*.  ⚠ dryopea has **no RNG at all**
today, and this plan must not introduce one.

## Failure paths — written before the code, because they are the design

| the pick | what must happen |
|---|---|
| within `landing_pick_edge_buffer` (5) of the painted area's edge | ⚠ REFUSED at the door — § 15 step 2 |
| on sea, `steep_rock`, or any hex `can_stand` refuses | the descent searches OUTWARD (step 3) — *"visually diegetic; the rocket appears to choose safe ground"* |
| valid, but the 7-hex footprint + 2-hex clearance is obstructed | keep searching |
| no valid landing anywhere in the search | ⚠⚠ REFUSED, and this is the one a naive descent gets wrong: an unbounded outward search always "succeeds", on the far side of the map |
| valid, but **every spawn marker is within 10 hex** | ⚠⚠ REFUSED — the base can never be attacked, and `@M058` measured what that produces: **a base standing at 378 ticks with zero targets**.  § 15 says map authors guarantee enough markers survive; ⚠ **all three authored maps carry ONE or TWO spawns**, so this refusal will fire on a real map and is not hypothetical |
| valid, but the starter tower has nowhere to stand 5-10 hex out | ⚠ the tower is DROPPED, not the landing — a base with no free tower is poorer, not impossible |
| valid, but a helper's emergence hex is taken | step aside; the core's own footprint is seven hexes and a crew of two |

## Invariant gate

| phase | concrete expected result | invariant | negative control |
|---|---|---|---|
| **L0** | on `starter_01`, the fraction of picks that land, and where | — probe, no claim | ⚠ a probe that answers *every pick lands* has not found the water |
| **L1** | `landing_of(pal, pw, mw, pick, seed)` twice on the same input gives the same hex; a pick on water lands on the nearest standing ground | `@FR-L-Landing-Is-A-Function` | ⚠ a landing that differs between two calls, and a search that "succeeds" off the map |
| **L2** | a landed base has a core, ≥1 live spawn, a tower and two crew | `@FR-L-Landing-Is-Total` | ⚠⚠ a base with every spawn silenced must be REFUSED, not landed — `@M058`'s 378 |
| **L3** | `land 4 -2` → emit → re-read → `state_diff` empty | the `.keys` pair round-trips | ⚠ the writer and reader are a PAIR (`@D007`, `@D009`) |
| **L4** | two landings on one map, one in reach of the work and one not | the pick is worth a number | ⚠ **price the supply against the capacity first** (`@M085`, `@M090`) |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **L0** — probe the three maps: where can a base actually land? | XS | `tests/32_l0_where_a_base_can_land.loft` (2) | ✅ **Done** |
| **L1** — the DESCENT: pick → core hex, hashed, searched, refused | M | `tests/32_l1_the_descent.loft` (9) | ✅ **Done** |
| **L2** — the BASE: core moved, starter tower, two crew, refusals | M | `tests/32_l2_the_base.loft` (7) | ✅ **Done** |
| **L3** — `land <q> <r> [seed]` + emit + `state_diff` | S | `tests/32_l3_the_land_verb.loft` (8) + `a-base-that-landed.keys`.  ⚠⚠ **It needed NO new emit verb and no `state_diff` row** — a landing's whole output is a POSITION, and markers and crew already round-trip | ✅ **Done** |
| **L4** — what a pick is WORTH | M | `a-base-landed-near-the-road.keys` + `a-base-landed-far-from-the-road.keys` — **174 against 221**, and a SWEEP showing the 47 ticks are not the walk (`@M091`) | ✅ **Done** |

## What this plan does NOT build

⚠ **The planet view and map selection** ([`plans/04`](../04-map-library/README.md)) —
§ 15 step 1.  It is UI over a list of maps and every step below it is
testable without one.

⚠ **The core's six faces and the rotation** — they do not exist (`@X294`).

⚠ **A second base's WORLD state.**  Carryover is the player's half
(`plans/31`); [`BACKLOG.md`](../BACKLOG.md) F7's world half — denied
throughput per crossing, the state each POI was left in — is not this plan.

## Open questions

1. ⚠ **Does the player pick, or does the game?**  § 15 has the player click a
   hex; nothing in dryopea has a pick UI, and `screen_to_hex` already
   un-projects a pointer (BACKLOG C7).  L3's `.keys` verb makes the mechanism
   testable either way, and the key binding is a separate decision — the key
   table is a BUDGET (`@X139`).
3. ⚠⚠ **THE DICE ARE WORTH AS MUCH AS THE DECISION, and this is the
   plan's real finding** (`@M091`).  Sweeping the PICK across thirty hexes
   moves the clock 174-232; holding the pick and sweeping only the SEED
   moves it 171-230.  **Same spread.**  So a player choosing where to land
   buys exactly what the landing's own randomness hands out for free, and
   `@X317`'s *the overlap is what makes a choice exist* cannot be felt
   through it.  ⚠ The cause is § 15 step 6's **random direction** for the
   free starter tower — a defence that lands anywhere on a 5-10 hex ring
   swamps everything else.  ⚠⚠ **Pointing it at the nearest LIVE SPAWN
   would make the pick legible and is a one-line change**, but it
   contradicts § 15 as written, so it is the owner's ruling and not
   this plan's.
2. ⚠⚠ **What does a map with one spawn do?**  All three authored maps have one
   or two, and § 15 assumes four to six.  The refusal above makes it loud
   rather than silent; whether the answer is *author more markers* or *shrink
   the disable radius on small maps* is a content decision L0 should inform.
