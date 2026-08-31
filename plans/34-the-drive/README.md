<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `34` — The drive: momentum, and the shapes it draws

**Value:** `G` · **Effort:** `MH`

## Status

**OPEN — D0 SHIPPED 2026-08-31 (`@X362`, `@M098`).  D1 next; D3 is
independent and can go any time.**

⚠⚠ **D0 is a map and it needed no code**: `maps/the_flats_04` — an open floor
**61 hexes square** inside two rings of `steep_rock`, a core dead centre, a
spawn at each end of the middle row and **nothing else**.  It is bigger than
every base in **both** dimensions, which is the assertion and not the
description: `crossroads_02` is 49 columns wide and would pass a width test on
its own while being thirteen rows tall.

⚠⚠ **THE SIZE IS THE DESIGN'S OWN BOUND**, `numbers.json`
§ `atmosphere_haze_radius`: **4225 hexes against the 4921 of the radius-40
disc** `tests/11_f8_the_tick_budget.loft` measures the tick over — fewer
hexes, so the cost comparison holds.  ⚠⚠ **But it is a SQUARE and the disc is
not**: measured with `lat_distance`, the edge of it is **32** hexes from the
core and the four corners are **48**, past the haze's 40.  That is the right
way round for a sandbox — every row and column through the core is visible
end to end, and the corners are over the horizon.

⚠⚠ **AND THE MEASUREMENT SAYS THAT IS ABOUT THE CEILING** (`@M098`): a
fighting tick costs **183 ms of a 667 ms budget** on it against **12 ms** on
`starter_01` — nine times the hexes for fifteen times the tick.  ⚠ **But an
IDLE tick is FREE**: 81 µs on the flats against 97 µs on `starter_01`, because
with no wave running there is no field to build.  ***The size of a map costs
nothing until something is walking on it***, which is why D0 is drivable at
all, and it is [`plans/22`](../22-the-field-cache/README.md)'s subject exactly.

⚠⚠ **The third number is the one that shaped the gate**: replaying the source
through the seam costs **~12 s** where `starter_01` costs 0.3 s, because a
`.keys` map is painted a hex at a time through the editor's own door.  So the
flats is in `a2_shipped` and **not** in the new `a2_bases`, and the drift check
it would have cost twelve seconds for is asked of it directly instead — the
source's own `count painted` and `kind` measurements, re-asked of the pair that
shipped (`tests/a2_the_maps.loft` § The flats is ROOM).  ⚠ **A drift check is a
claim about the pair, not a method.**

⚠ **And a fourth number is the one a player will FEEL**: the CPU half of a cold
ground bake costs **1362 ms** on the flats against **197 ms** on `starter_01`,
and it is paid on **every press of P**, because `play_view_sync` resets its
watch whenever play mode is off.  So entering play mode here is a **~1.4 s
hitch**, and incremental tiles for ever after.

⚠ **What D0 has NOT answered is its own first question** — *does it feel
good?* — because `@X358`'s feel target can only be answered by a person with
their hands on it.  `make play MAP=the_flats_04`.

⚠⚠ **This plan exists to test [`docs/PUZZLES.md`](../../docs/PUZZLES.md), not
to build it.**  That document is eleven decisions deep (`@X351`-`@X361`) and
**ten of them rest on momentum, which has never existed**.  The job here is to
find out — cheaply, and with the owner's hands on it — whether the foundation
holds before anything is built on top.

⚠ **The owner wants to drive it.**  So the phases are ordered by *how soon
somebody can feel it* rather than by dependency, and D0 needs no code at all.

## Goal

Answer three questions, in this order:

1. **Does it feel good?**  `@X358`'s *"a reasonable quick speed to get
   somewhere quick but also less precise"* is a FEEL target and only a person
   can answer it.
2. **Does a fast drive actually draw a sweeping curve?**  ⚠⚠ This is the
   FALSIFIER.  `@X359` and `@X361` — the keystone and the race — both assume
   that momentum plus build-by-driving produces rail-legal geometry.  **If a
   fast drive draws something jagged, both collapse**, and it is far better to
   learn that from a throwaway than after a rail system is built on it.
3. **Is a wall worth anything when it LEADS rather than BLOCKS?**  `@M094`
   measured a wall at **−80 / −117 ticks** in front of a tower, and `@X355`
   says no fixture has ever built a funnel.

## ⚠⚠ Cap warning — this would be a FIFTH active plan

[`plans/README.md`](../README.md) caps active plans at **2-3** and
[`01`](../01-ground-editor/README.md), [`19`](../19-the-interactive-loop/README.md),
[`22`](../22-the-field-cache/README.md) and [`33`](../33-exploration-finds/README.md)
are all open.  ⚠ Three of those look **stalled rather than active** (19 has P5
left, 22 has never started S0, 01 has been open since the first week) and
`plans/33` has only E4 outstanding.  **Closing or deferring one before starting
this is the convention; it is the owner's call and it is recorded here rather
than quietly ignored.**

⚠ **D0 was taken with that question still open, and deliberately**: it is XS,
it needed no code, it touches nothing any other plan owns, and what it produces
— ROOM — is [`plans/04`](../04-map-library/README.md)'s debt rather than this
plan's (`docs/PUZZLES.md` § What CANNOT be measured yet).  ⚠⚠ **D1 is the first
phase that writes simulation code, and it is where the cap actually bites.**

## Anchors

- [`docs/PUZZLES.md`](../../docs/PUZZLES.md) — ⚠ **the whole design; this plan
  restates none of it.**  §§ The vehicle model (`@X357`), the curve (`@X356`),
  the keystone (`@X359`), the race (`@X361`).
- `src/vehicle.loft` — `VEHICLE_SPEED_HEX_PER_SECOND` is a flat **3.0** and
  there is **no heading-change cost anywhere**.
- `fixstep` — ⚠ `Bank` is the shape a carried speed wants, and
  [`plans/26`](../26-the-fixed-step/README.md) is why nothing here may be a
  hand-rolled float.
- `src/build.loft` + [`plans/27`](../27-building/README.md) — build-by-driving,
  which is what makes `@X359` true.
- `@M094` — the wall's measured negative, and the factorial this plan adds one
  cell to.

## ⚠⚠ What this plan does NOT build

**No rails, no trains, no sidings, no curved-wall strength.**  Every one of
those is downstream of the falsifier in D2, and building any of them first is
how the eleven decisions become eleven sunk costs.  ⚠ `@X353` and `@X361`
stay design until D2 answers.

## Invariant gate

| phase | concrete expected result | invariant | negative control |
|---|---|---|---|
| **D1** | held input reaches top speed in N ticks and NOT in one; released input decays over M ticks; reverse decays faster | **speed is a STATE, never an input** (`@X357`) | ⚠ a tick length that halves must not change the distance covered — `tests/26_l0`'s cross-product, and the axis `@M030` got wrong |
| **D2** | a full-speed drawing drive changes direction at most once every K hexes | the drawn shape is a function of the DRIVE, not of the input | ⚠⚠ **a CRAWLING drive over the same route must be able to turn every hex** — otherwise the sweep is a lattice artefact and not momentum |
| **D3** | a funnel and a barrier of the SAME wall budget, on one base | the wall's sign is a question of SHAPE | ⚠ `@M094`'s four existing cells re-run unchanged, or the new cell is not comparable |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **D0** — ⚠⚠ **ROOM to drive in**: one big open map in `maps/`, no enemies, no pressure | XS | ⚠ **`make play MAP=the_flats_04` and drive it** — no code.  Two cheap gates in `tests/a2_the_maps.loft` pin the SIZE and the RIM | ⚠ **Built 2026-08-31**; the FEEL half is the owner's to answer |
| **D1** — the THROTTLE: accelerate held, coast released, brake reversed, turning free | M | `tests/34_d1` + the tick-length sweep | **Next** |
| **D2** — ⚠⚠ **the FALSIFIER**: does a fast drawing drive make a rail-legal sweep? | S | `tests/34_d2` over a captured wall shape | Blocked on D1 |
| **D3** — the FUNNEL: `@M094`'s factorial plus one cell | S | four `.keys` + `scripts/validate.sh` | ⚠ **independent — needs no code and can run any time** |
| **D4** — the BOOST, re-decided against `@X357` | S | `tests/34_d4` | Blocked on D1 |

### ⚠ What D0 shipped, and the two things its gate pins

⚠⚠ **A sandbox map is a MAP but it is not a BASE, and the test file now says
so**: `a2_shipped` is every map the repo ships and `a2_bases` is the three that
teach a shape.  *Poking a spawn sends a wave at a defence* is a claim the flats
does not make and its three siblings already carry three times over.

⚠⚠ **What CAN silently take ROOM away is the SIZE and the RIM**, so those are
what is gated.  A map that quietly lost half its floor would still load, still
draw and still play, and nothing else in the tree looks at how BIG a map is.
⚠ And the rim's closure rests on a **palette column** rather than on the map —
two rings of `steep_rock` shut the vehicle in only while `walk_vehicle` is
false for it, and it was TRUE until BACKLOG C10 turned it over — so the column
is asserted beside the ring it holds shut.

⚠ **The floor asserts a NULL, and that is the map's one claim**: no hex of it
refuses the vehicle, refuses a building, or is a height anybody has to climb.
The speckle of `sand` / `hill` / `rock` is a **MOTION CUE** — ⚠⚠ *you cannot
feel speed over one flat colour*, because `ground_gl.loft` draws one flat
unlit colour per palette kind — and those three differ from `grass` only in
colour and in FOOTING (`@X284`).

⚠ **And the sweep says how much it read before it says what it found.**  Three
counters that are all zero is also what a loop that never ran answers.

### ⚠ D0 — why a map and not a `.keys` fixture

⚠⚠ **A sandbox does not belong in `tests/scripts/`.**  Three corpus-wide tests
sweep every file there — the round trip (`18_s2`), the converter (`09_c5a`) and
the footing sweep (`c6`) — so a 2000-hex field would tax all three for a
fixture that gates nothing.  ⚠ `maps/` is the right home: it is CONTENT, it has
a `.keys` source beside the built `.json` (`scripts/build_maps.sh`), and
`make play MAP=…` opens it.

⚠ **The authored maps are all strips** — `starter_01` **34 x 15**,
`crossroads_02` **49 x 13**, `the_gap_03` **30 x 17** — and at 60° a hex, a
curve of any useful radius eats most of thirteen rows.  ***D0 exists because a
player cannot build a round base on a map fifteen rows tall*** (`@X360`).

### ⚠ D1 — the one thing to get right first

⚠⚠ **The speed belongs to the MOVER and the acceleration is per-class DATA.**
`drive_along` is one implementation with two doors (`DESIGN.md` § 9 — *same
chassis as the player*), so a speed state that lived only on `Vehicle` would
fork the crew off it.  ⚠ Give every mover the state and give the CREW an
acceleration that reproduces today's behaviour; then `@X357`'s *"a turn cost
re-prices every servicing trip"* becomes a decision with a number rather than a
side effect.

⚠ **It must be `fixstep`, not a float.**  `plans/26` retired seven hand-rolled
*do-not-lose-a-fraction* sites and `@M030` measured the vehicle reading
**180 / 120 / 180 / 0 / 0 / 0 / 0** hexes a minute across seven tick lengths
when it had no bank.  A momentum model is a second such site by construction.

### ⚠⚠ D2 — the phase this plan is really for

⚠ It is cheap, it comes second, and it can kill `@X359` and `@X361` outright.
**Drive at full speed with build ordered, capture the wall, and read its
direction changes.**  ⚠⚠ The negative control is what makes it mean anything:
*a crawling drive over the same route must be able to turn every hex.*  If both
speeds draw the same shape, the sweep is a property of the lattice and momentum
bought nothing.

## Open questions

1. **Does the crew get momentum too?**  ⚠ `@X357` says a turn cost re-prices
   every servicing trip in the game.  *Recommendation: give them the state and
   an acceleration that reproduces today, so the change is a NUMBER and D1 does
   not silently move `plans/17` T3's measurement.*
2. **What happens to BOOST?**  ⚠⚠ `VEHICLE_BOOST_HEX_PER_SECOND` is 6.0
   against a base 3.0 — an instant doubling, which is exactly what `@X357`
   refuses.  Ceiling, acceleration, or both.  *Resolution: D4, and not before
   D1 has a feel.*
3. **Is D3 worth running BEFORE D1?**  ⚠ It needs no code, it is four `.keys`
   files, and it answers `@M094`'s ungated sight hypothesis independently of
   everything else here.  *Recommendation: yes, whenever there is an idle
   gate slot — it is the cheapest open question in the repo.*
