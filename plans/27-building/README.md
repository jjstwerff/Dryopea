<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `27` — Building

**Value:** `G` · **Effort:** `H`

## Status

**COMPLETE — C0-C5 shipped 2026-08-27.**

⚠⚠ **The player can build.**  `make play SCRIPT=…`, press **P**, press
**Q**, and every hex you drive over is ordered as a wall your crew then
raise.  Measured worth: **+44 ticks on a base that otherwise falls at
130** (`@M050`).  ⚠⚠ **And the wallet buys something for the first time since it
existed**: press **E** at the core and 100 of the opening 200 points
becomes a tower beacon to carry out and plant (`@M051`).

Walls and towers are placed in the **editor**.  The player cannot make a
base, and the wallet buys nothing.  This plan builds the missing verb:
[`BACKLOG.md`](../BACKLOG.md) § B2, and item **3** of
[`ROADMAP.md`](../ROADMAP.md) § The critical path — *the biggest missing
mechanic, and it gates three finished designs* (`@X022`, `@X024`,
`@X019`).

⚠⚠ **Almost none of this plan invents a number.**  Every constant it
needs is already written down in `examples/numbers.json` and consumed by
nothing — `wall.build_time_wall` (10 s), `wall.build_time_wall_high`
(20 s), `wall.build_cost_points` (0),
`helper.construction_tick_hp_per_second` (10 HP/s),
`tower.order_cost_points` (100), `tower.construction_time_helper`
(30 s), `input.wall_paint_toggle.key` (Q).  ⚠ The palette has carried a
**`buildable`** flag since plan 01 that no simulation code reads.  So
this is a plan about **wiring designed numbers to a verb**, not about
choosing them.

## Goal

The player lays wall outlines by driving, ferries a beacon from the core
for a tower, and the crew turn both into structures that the simulation
and the window agree exist.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 11 § Wall paint (trail
  outline + erasable), § New towers via beacon ferry, § 13 § Currency
  (walls free, helper-seconds the bottleneck), § 9 (who builds).
- [`docs/NUMBERS.md`](../../docs/NUMBERS.md) + `examples/numbers.json`
  §§ `wall`, `helper`, `tower`, `economy`, `input`.
- `@X252` — helpers are **semi-automatic by default**, so a build order
  is picked up rather than assigned.  `@X197`/`@X198` — the assignment
  pillar this must not delete.
- `@X139`/`@X140` — the key table is a **budget**, and **Q is the
  sharpest discoverability hazard in the design**.
- Source: `src/painted.loft`, `src/damage.loft` (the layer shape),
  `src/helper.loft`, `src/wallet.loft`, `src/carry.loft`,
  `src/spawn.loft` (the tick), `src/play.loft` (the seam),
  `src/bindings.loft` (the one key table), `src/play_view.loft`
  (`@X095`).

## The invariant

> **A build order is ONE record — a site, what goes there, and the work
> still owed — and it is the only way a structure comes into existence
> during a run.  Everything that orders creates one; everything that
> builds spends helper-seconds against one; an order that reaches zero
> paints its hex and disappears.**

⚠ **Re-assertion sites: two creators (wall paint, beacon drop), one
worker, one completion.**  Omitting the order at a new creator is
**loud** — nothing appears — which is why the count is acceptable
without a type to enforce it.

⚠⚠ **What this invariant deliberately does NOT absorb.**  A build order
and a `StructureHit` are the same *layer shape* (a sparse hash keyed by
hex, one door, zero removes the entry) and they are **not the same
rule**: damage is harm to a structure that exists, an order is intent
for one that does not.  `damage.loft`'s pattern is copied; its record is
not extended.  *Equality of shape is evidence; sameness of rule is the
claim.*

⚠⚠ **And the second tempting absorption is REFUSED on arithmetic.**
The work a wall owes is **100**, which is `wall.wall_hp` — and
`wall_high` owes **200**, which is `wall_high_hp`, both exactly
(`build_time × construction_tick_hp_per_second`).  It is therefore
tempting to define the work owed as `structure_max_hp`, which would
*also* handle bracing for free.  **That is wrong**: `structure_max_hp`
scales by `brace_of`, so an isolated stub would owe **15** and build in
1.5 s, where `DESIGN.md` says *one helper, one wall hex, 10 s* flatly.
The work owed is the **kind's unbraced figure**.  A wall is cheaper to
break at an end; it is not cheaper to build there.

## Invariant gate

| phase | concrete expected result | invariant pinned | negative control |
|---|---|---|---|
| **C1** | an order authored at a `grass` hex reads back `kind = wall, work = 100`; the same order at `steep_rock` is **refused by name** | an order exists only where the palette says `buildable`, and never where a structure already stands | `steep_rock`, `water`, an occupied `wall` hex, and a **second order on the same hex** |
| **C2** ✅ | one helper finishes a wall in **15 ticks** (10.0 s); two in **8**, four in **4**; `wall_high` in **30**; on completion `lookup_painted` answers `wall` and `can_step` **refuses** the hex | work is helper-seconds and nothing else — N helpers are N times as fast, exactly | a helper out of reach adds **zero**; a wrecked helper adds **zero**; a run with **no crew** builds nothing |
| **C3** ✅ | driving Q-on across the field orders **every hex entered** (12 of 12, one gap — the hex it started on); driving back rubs out the middle and leaves **the two ends**; a hex a helper has **started** survives | the trail toggles on ENTRY, along the path actually taken | the endpoint-only version reads **6 laid, 7 gaps** — the wall with holes |
| **C4** ✅ | beacon pickup at the core debits **100** of 200; a drop on a legal site leaves a tower order owing **30 s**, raised in **45 ticks** | points are spent at PICKUP and a failed deposit does not refund | a pickup one point short is refused **and does not drain**; a drop on a cliff or a marked hex keeps the beacon; **a tower order refuses erasure** |
| **C5** ✅ | **130 ticks against 174** — the pair is ONE token apart (`paint off` / `paint on`) | `@X022`'s *pre-wave window is a budget* is a measurable claim | the wave must CHEW THROUGH the wall (1 of 5 hexes left standing), or the gain is a detour rather than a defence |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **C0** — the probes: what the design assumes, measured | S | `tests/27_c0_probe.loft` | **Shipped** |
| **C1** — the order record + its layer + `.keys` authoring + `order_fault` | M | `tests/27_c1_the_order.loft`; `state_diff` + emit round-trip | **Shipped** |
| **C2** — helpers build it, and the structure appears | M | `tests/27_c2_construction.loft`; N-helpers-N-times; `can_step` flips | **Shipped** |
| **C3** — wall paint (Q): the trail lays orders, re-driving erases | M | `tests/27_c3_wall_paint.loft` | **Shipped** |
| **C4** — the beacon ferry: 100 points, carry, drop, tower | M | `tests/27_c4_the_beacon.loft` | **Shipped** |
| **C5** — the measurement: does building buy a base anything? | S | `tests/27_c5_the_wall_is_worth_it.loft` + the scenario pair | **Shipped** |

## What this plan does NOT build

- ⚠ **Helper orders** (`helper.order_cost_points`, the 6-cap).  It is the
  economy's other 100-point sink and `BACKLOG` § B2 does not name it.
  Ordering a *person* is a different verb from ordering a *structure*,
  and folding it in here is what the invariant above would have to
  stretch to absorb.
- ⚠ **The 8-walls wave-1 trigger** — see § Open questions 1.
- ⚠ **Bridges between walls** (`DESIGN.md` § 13 calls them phase 2).
- ⚠ **Directed helpers** (`@X252`'s *later, per person* half).  This plan
  ships the **semi-automatic default** only, which is what `@X252` says
  a new player meets.

## Open questions

1. ⚠⚠ **Does building start wave 1?**  `numbers.json`
   § `wave_system.wave_1_wall_trigger` = **8 walls built**, documented as
   *"one of two wave-1 triggers (whichever fires first)"* — and the other
   one is built (`wave_provoke_step`).  But `@X022` reads the recon
   window as **unlimited**, ended *"deliberately by poking a marker"*.
   Both triggers are deliberate player acts, so they may not actually
   conflict — but the number has never been implemented and `@X022` is
   the shipped reading.  **Not built here**; it wants the owner.
2. ~~**Does a wall under construction block anything?**~~  **DECIDED in
   C2: no.**  A site is an intent — no height, no passability, and an
   enemy walks over it.  The alternative (a half-height obstacle) is a
   mechanic rather than a detail, and nothing in the design asks for it.
4. ~~**`BuildOrder.kind` is a PALETTE INDEX, and a tower is not a
   palette kind.**~~  **DECIDED in C4: the discriminant** (`@X273`).
   ⚠⚠ **And the estimate was wrong in the useful direction** — it said
   three branches and there are **four**, because probing found one the
   design does not list: a tower order must refuse ERASURE, or the wall
   trail destroys a paid-for beacon (`@X274`).  *A difference a probe
   finds is worth more than three the design already lists.*
3. **What happens to an order the wave overruns?**  It is a record on a
   hex enemies are standing on.  C2's answer is that it simply waits;
   whether an enemy should *destroy* one is `ROBOT_ECONOMY.md`'s builder
   class, not this plan.

## Design recorded during this plan

*(filled as phases land)*

- **C0** — `@M048`: **the renderer cannot see a structure that appears**,
  and `@X095` named the case in advance (*"a repaint with no rubble"*).
  0 dirty hexes against a tile checksum that moved.  ⚠ The RULING — whether
  the watch learns the painted layer or a finished order moves the height
  layer — is **C2's**, because it is a fix and this phase measured.
  ⚠ Three further claims probed TRUE (`buildable` exists and is unread,
  the work owed is already a number, a wall closes a step) and a fourth is
  the § The invariant refusal, measured: a lone stub is **15**, not 100.
- **C4** — `@X273`, `@X274`, `@M051`.  ⚠⚠ **The wallet bought something
  for the first time since it existed.**  One queue with a `what`
  discriminant; the marker check lives in `wave_drop` because a build
  layer has no marker world, and `CARGO_BEACON` is the first carry
  consumer to add NO row to `cargo_destination_ok` at all.  ⚠⚠ **The
  fourth branch was found by probing, not by reading the design**, and
  it is the one that would have shipped a defect.
- **C5** — `@M050`, `@X272`.  ⚠⚠ **+44 ticks, a third of the base's
  life**, from a pair one token apart.  ⚠⚠ **And the measurement caught a
  defect in the mechanic before it shipped**: a five-row band could not
  be SEALED, because the trail never orders the hex the player is
  standing on — and `plans/12` § B7 already priced a wall with one gate
  at **+1 tick**.  `play_set_painting` seeds it, at ONE door, because the
  key and the script both set the mode.
- **C3** — `@X270`, `@X271`.  ⚠⚠ **The trail walks `lat_line` and skips
  the hex it started on**, which fixes two things at once: a vehicle
  crosses two hexes a tick so the endpoint alone leaves gaps, and the
  previous tick already ordered the start so re-laying it rubs the trail
  out.  ⚠ Paint mode is on `PlayState`, so `WaveState` gains nothing.
  ⚠ **Both design-budget gates fired as designed** — `09_i1`'s row count
  and `a4_the_controls`' documentation check — which is `@X139` working.
- **C2** — `@M049`, `@X269`.  ⚠⚠ **The first build of the work was a
  FLOAT rate and it came up short**: `wall_high` took **31 ticks against
  a true 30**, which is `@D003`/`@D004`'s family in a new mechanic.  The
  order's work is now an integer count of base units and its total is a
  DURATION, which is what `numbers.json` said in the first place.
  ⚠⚠ **`fixstep::Timer` was refused on semantics**: it fires once and
  DISARMS, and a build order must stay readable after it completes.
  ⚠⚠ **`@X095`'s rule is retired** — the watch now notices any change to
  the ground it draws, which needs no claim about what the simulation may
  do.  ⚠ Who builds: the CREW and not the player, because § 13 prices
  walls at zero points *because helper-seconds is the bottleneck*.
- **C1** — `src/build.loft`.  Work is stored as **DONE, never LEFT**, and
  [loft#914] is why: a partial literal omitting `done` reads as *nobody has
  started*, where storing REMAINING would make it read as **already
  finished** and paint a wall the moment anything looked at it.  Same rule
  as `damage.loft` § DAMAGE TAKEN and `wallet.loft` § points SPENT, and the
  same fail-closed argument `font.loft` makes for `loaded`.
  ⚠ `order_place` asks `order_fault` itself, so an illegal order cannot be
  placed by forgetting to check — `maps.loft::map_fault`'s shape.
  ⚠ `structure_base_hp` now delegates to a new `structure_kind_hp`, because
  an order's hex is still grass and there is nothing painted there to read.
  ⚠⚠ **Adding a field to `WaveState` costs FOUR sites and three of them
  fail silently** — `compare.loft` (two runs differing only in orders would
  read as identical), `emit.loft` (a capture that loses the field),
  `convert.loft`'s schema and `tests/09_c5a`'s vocabulary list.  The last
  one is the only one that went red on its own.
