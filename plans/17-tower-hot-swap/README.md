<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 17 — Tower hot-swap: the upkeep loop, so a base can RECOVER

**Value:** `G` · **Effort:** `M`

## Status

**T0 done** (2026-08-15).  T1 is next.  No `src/` change yet; suite 943
green, gate 27 scripts / 498 measurements.

**T0 tried to falsify the premise and could not.**  The magazine binds,
and harder than W4 could show: *without repair, building more towers
buys nothing at all* — 3, 5 and 7 towers give 321, 319 and 317 ticks and
four of seven waves each.  With repair, seven towers **clear the whole
authored list at tick 332**.  § T0 has the tables.

⚠ **This plan exists because [plan 16](../16-the-wave-system/README.md)
W4 named it, twice over.**  W4 measured the game at its real length and
found one constraint behind two separate failures:

| what W4 measured | the constraint |
|---|---|
| the authored seven-wave list plays **FOUR** and falls at 321 | 205 robots is 6150 HP; a tower is **300 HP of ammunition for the whole run** |
| a retrieval is worth **one tick**, even where the crew member does come back at 187 | the JOB is gone by the time they return — a base spends its magazines and its wall and has **no way back up** |

Both are the same sentence: **nothing lets a base recover between
waves**, so the lull is a pause rather than a repair window, and every
mechanic priced across waves has nowhere to land.

## Goal

A spent tower can be brought back — by a crew member in the lull, or by
transplanting a top from a tower that can spare one — so the lull is a
**repair window** and a base's firepower is a thing the player maintains
rather than a thing it spends once.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § Towers — pulsed laser,
  attack-count decay (⚠ the **repair rule**: a *firing* tower cannot be
  repaired), § Tower-top salvage — the scramble mechanic lived
  tactically, § Tower overload + hot-swap (the deferred tail).
- [`examples/numbers.json`](../../examples/numbers.json) § tower —
  `repair_time_helper_rebuild` (20.0 s), `repair_time_top_transplant`
  (0.0, instant).  **This plan adds no new tunable without a row there.**
- [`plans/15`](../15-the-carry-model/README.md) § C0.4 — the contract a
  new carry kind arrives under: a kind row, a valid-destination rule,
  and what arriving does.  **No new carrying code.**

Source files it touches: `src/tower.loft` (where the budget lives),
`src/carry.loft` (one kind row), `src/spawn.loft` (the tick's repair
turn), `src/helper.loft` and `src/vehicle.loft` (who repairs), and
`src/script.loft` (the verbs).

### ⚠ Two decisions taken at the outset, both recorded rather than assumed

1. **BOTH the player and helpers repair** (project owner, 2026-08-15).
   `DESIGN.md`'s prose says *"the player walks up to a black-in-place
   tower"* while `numbers.json`'s only repair rate is
   `repair_time_**helper**_rebuild` — a real conflict in the sources.
   Resolved by honouring both: repair stays **presence-locked** (somebody
   has to stand there, which is the § What kind of game this is test),
   and the crew gets a job that exists **between** waves — which is
   precisely what W4 found missing when a retrieved crew member came back
   to nothing.  ⚠ It is also what gives losing a crew member a cost in
   FIREPOWER rather than only in labour.
2. **Upkeep only.**  Strain, overload, tactical type-swap, ammo
   bookkeeping and swap-pit authoring are out — `DESIGN.md` § Tower
   overload + hot-swap marks them *"Validation tier: deferred"* and
   several need tower variants that do not exist.  See § What this plan
   does NOT build.

### ⚠ Why repair comes BEFORE the swap

The plan is named for the swap and builds the repair first, and the
design's own rule is why: **a firing tower cannot be repaired.**  So the
swap is not an alternative to repair — it is the thing that *creates the
grounded state repair needs*.  Repair is also the smaller half: it needs
no carry model, it is the half W4 actually named, and it can go red on
its own.

## T0 — the probe (2026-08-15)

No code.  W4 asserted the magazine binds by **exhausting** it; T0 asks
the same question from the other direction — hand-refill the towers on a
cadence and look.  The refill is idealised (every tower at once, no
travel, no presence, no cost), because an idealised upper bound is what
a falsifier needs.

Cadences are chosen to mean something: `numbers.json`
§ tower.repair_time_helper_rebuild is 20.0 s = **30 ticks per tower per
helper**, so on the seven-tower band, *every 45 ticks* is about two
helpers working the round.

### ⚠⚠ 1. Without repair, tower COUNT buys nothing

W4's band, the authored seven-wave list, no refill:

| towers | fell | waves |
|---|---|---|
| 3 | 321 | 4/7 |
| 5 | 319 | 4/7 |
| 7 | **317** | 4/7 |

**Tripling the towers moves the clock by four ticks, in the wrong
direction.**  That is the finding T0 exists to produce: a base's
firepower is capped by AMMUNITION, not by how much of it you built, so
the one lever a player has over their own defence is inert.  W4 measured
the ceiling; this says the ceiling does not care what you build under it.

### ⚠⚠ 2. With repair, the seven-wave base EXISTS

Free refill, same band and list:

| towers | fell | waves | wall |
|---|---|---|---|
| 3 | 454 | 6/7 | broken |
| 5 | 651 | 7/7 | 100 |
| 7 | **never — CLEARED at 331** | 7/7 | 100 (never touched) |

So the list `numbers.json` authors is playable, and W4's *"not
survivable"* was a statement about upkeep rather than about the list.

### 3. It takes about TWO helpers, and the third buys nothing

Seven towers, cadence swept:

| cadence | ≈ crew | result |
|---|---|---|
| never | — | 317, 4/7 |
| 180 | ½ helper | 327, 5/7 |
| 90 | 1 helper | 278, 6/7 |
| **45** | **2 helpers** | **CLEARED 332** |
| 30 | 3 helpers | CLEARED 331 |

⚠ The knee is at two, and `numbers.json` § helper.roster_start is **2** —
so the design's own starting crew is exactly the labour its own tower
numbers need.  That is a target T1 can be built against rather than a
coincidence to tune towards.  ⚠ Five towers never clear at any cadence
(637 / 651), so labour does not substitute for firepower.

### ⚠ 4. Fall tick is NOT a progress metric any more

Seven towers at cadence 90 falls at **278** having played SIX waves;
with no refill at all it falls at **317** having played four.  A base
that kills faster **meets waves faster**, because the schedule advances
on a clear (plan 16 W1).  ⚠ So T3 measures WAVES REACHED with the clock
as a secondary reading, and W4's *"fell at 321"* was taken on a base
whose schedule had stalled.

### ⚠ 5. Two probe artefacts that are T1 design notes

**A repair resets the MAGAZINE and not the CHARGE.**  Rebuilding the
whole `TowerState` zeroes the banked charge too, and a tower needs 1.5
ticks of charge to reach one firing interval — so a refill every tick
left every tower permanently a hair short and it **never fired at all**:
233 ticks, which is *exactly* the undefended band.  ⚠ `charge` and
`shots` are two clocks on one struct and repair touches one of them;
233 is the signature to recognise if T1 gets it wrong.

**The ramp is unmanaged on a sealed base.**  Piles reach 28-36, and no
helper can reach them — the ramp forms OUTSIDE the wall and helpers have
no boost (plan 13 V4).  It did not decide anything at seven towers
because nothing survived to climb it, and it will decide something on a
base that is not overwhelming.

## ⚠⚠ The finding that shapes the plan: the budget is keyed by the wrong thing

Today a tower's magazine lives in `TowerState`:

```loft
pub struct TowerCharge { q, r, charge, shots }
pub struct TowerState  { cells: hash<TowerCharge[q, r]> }
```

— **keyed by HEX**.  That is correct for a tower whose top never moves,
and it is wrong the moment one can be carried: a top that is detached and
re-mounted somewhere else must take its spent shots WITH it, or
detach-and-reinstall is a free repair and the whole upkeep loop is
bypassed by two keypresses.

⚠ **So T2's substantive work is an ownership move**, not a feature: the
budget stops being a property of the HEX and becomes a property of the
TOP.  Its sharpest negative control follows directly — *detach a top and
put it straight back; the shots fired must be unchanged.*

⚠ **And the zero-value trap is live here** ([loft#914](https://github.com/loft-lang/loft/issues/914),
`CLAUDE.md` § Loft language gotchas).  "Does this tower have a top?" must
be stored as `top_removed: boolean` and never `has_top`, because a
`TowerCharge { }` literal takes the field's ZERO — and `has_top: false`
silently disarms every tower in a suite that has never heard of the
field.  It is the same rule `Enemy.stand` and `damage taken` already
keep, and the same one that made B4's `taken` a count of damage ABSORBED.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **T0** ✓ | 317 / 319 / 321 unrefilled at 7 / 5 / 3 towers; CLEARED at 332 with two helpers' worth of repair | the magazine is the binding constraint — W4 asserted it, this removes it and looks | ✓ the plan's own falsifier was live: a base with free refills that still played four waves would have stopped the plan.  It played seven |
| **T1** | a black tower fires again after 20 s of somebody standing at it | repair is **presence-locked and time-priced**; the ledger is clamped on the WRITE, so no tower ever holds more than `TOWER_SHOT_BUDGET` | a repair attempt on a **firing** tower is REFUSED (`DESIGN.md`'s rule); an unattended black tower never recovers a single shot; ⚠ **repair must not touch the CHARGE** — T0 § 5, and the signature of getting it wrong is a base that reads exactly like an undefended one (233 ticks) |
| **T2** | a detached top does not fire; installed on an empty tower it fires instantly | **conservation is structural** (plan 15 C1): a top is mounted, carried, on the ground, or spent — exactly one, and never two | ⚠ **detach and re-mount must not refill** — the shots travel with the top; installing onto a tower that already has one is REFUSED, not silently overwritten |
| **T3** | the seven-wave list REACHED, and the three retrieval clocks re-measured | a base that can recover is a base that can be played | a reading taken with repair switched off must reproduce W4's 321 / 247 / 248 / 248 exactly, or the harness moved and not the mechanic.  ⚠ Measure WAVES, not the fall tick — T0 § 4 |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **T0** — the probe: is the magazine really what binds? | XS | the tables in § T0, measured against the shipped sim | **Done** |
| **T1** — the rebuild: a black tower comes back | M | `tests/17_t1_the_rebuild.loft` — 20 s of a crew member standing at a black tower and it fires again; nobody there and it never does; a FIRING tower refuses repair; the ledger never exceeds the budget; the CHARGE is untouched | Open |
| **T2** — the top is a carry kind | M | `tests/17_t2_the_top.loft` — detach / install / deposit at the core, the budget travels WITH the top, and conservation asserted over the whole layer after every mutation (plan 15 C1's `assert_sound` shape).  ⚠ The diff must touch **no function in `carry.loft`'s carrying path** — that is plan 15 § C0.4's contract and is itself a gate | Blocked on T1 |
| **T3** — the loop, measured | S | `tests/17_t3_what_upkeep_is_worth.loft` + a `.keys` scenario — W4's two measurements re-run with upkeep in the game.  ⚠ T0 predicts a two-helper seven-tower base clears the list; a reading that does not is the phase's finding | Blocked on T2 |

### Why the order is this order

**T0 first** because it is the cheapest thing that could kill the plan.
W4 asserted the magazine is the constraint by *exhausting* it; T0 tests
the same claim by *removing* it, which is the other direction and the one
that can surprise.  ⚠ It is the move `plans/12` B5b and `plans/14` H2 both
name — price the SUPPLY against the CAPACITY before believing a reading.

**T1 before T2** per § Why repair comes before the swap.  ⚠ T1's verb is
deliberately written to take the tower's HEX and repair *whatever top is
mounted there*, so that T2's ownership move changes what is inside it and
not one call site.

**T3 last** because it is the measurement the plan exists to make, and
because it is the only phase that can say whether any of this paid.

## What this plan does NOT build

**No strain, no overload, no type-swap, no ammo** — `DESIGN.md` § Tower
overload + hot-swap is explicit: *"Validation tier: deferred … overload +
strain + spare-top swap + type-swap + ammo bookkeeping + swap-pit
authoring arrive in a later phase, once the base tower model is stable"*.
This plan is that stabilisation.

**No BOOST.**  `numbers.json` has every boost number and `DESIGN.md` says
validation ships it with strain disabled — but boost is about OUTPUT and
this plan is about RECOVERY, and W4's finding does not name it.

**No beacon ferry**, so **spares stay zero-sum** and that is the design's
own framing rather than a shortfall: *"A spare top sitting in a swap pit
is a top that is NOT firing on a different tower"* (§ The opportunity-cost
layer).  A base with N towers runs N-1 and rotates the spare through
repair, and buying an N+1th top is the beacon ferry's job.

**No swap-pit authoring.**  ⚠ And there is a mechanical reason to leave
it: a carry object on the ground **blocks nothing** today, so a pit's
whole purpose — keeping the spare from blocking the swap corridor — has
no force yet.  Building pits before that would author terrain against a
rule the game does not have (§ Open questions 3).

## Open questions

1. **Is an interrupted repair WASTED, or banked?**  A crew member driven
   off a black tower at 19 of 20 seconds either loses the lot or keeps the
   progress.  Wasted is more in keeping with § What kind of game this is;
   banked matches `helper_recover_tick`, which is the codebase's only
   existing timer of this shape.  *Recommendation: bank it on the TOWER
   (so the progress belongs to the thing being repaired, not to whoever
   happened to be standing there) and let a second crew member finish what
   a lost one started — decided in T1.*
2. **Does repair cost POINTS?**  `numbers.json` lists a repair TIME and no
   repair cost, while a new tower is 100.  *Recommendation: time and
   position only, so the wallet stays the run's clock and repair stays a
   labour decision — decided in T1, and it needs a `numbers.json` row if
   the answer changes.*
3. **Is a top on the ground vulnerable, and does it block?**  Cargo is
   inert today: it is not a blocker, nothing attacks it, and it does not
   decay.  A top parked in a kill zone being destroyed is exactly this
   game's kind of cost.  *Recommendation: leave it inert in this plan and
   record the gap — it belongs with wreck decay, which
   `plans/ROADMAP.md` already carries as its own designed-not-built
   entry.*
4. **Can a helper be ORDERED to repair, or does it repair what it is
   standing next to?**  Every helper job so far is positional —
   `salvage_at` clears what is in reach with no key pressed.  *Recommendation:
   positional, for consistency and because `DESIGN.md` § 11 keeps
   instructions to a destination — decided in T1.*

## See also

- [`plans/16-the-wave-system`](../16-the-wave-system/README.md) § W4 — the
  measurement that named this plan, and the numbers T3 re-runs.
- [`plans/15-the-carry-model`](../15-the-carry-model/README.md) § C0.4 —
  the contract T2's kind arrives under; and § Status, whose retrieval
  clocks T3 re-measures for the second time.
- [`plans/12-combat-resolution`](../12-combat-resolution/README.md) § B5a
  / B5b — what a tower is and what it can see.
