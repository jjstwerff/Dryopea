<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `28` — The scramble

**Value:** `G` · **Effort:** `MH`

## Status

**S0-S5 SHIPPED 2026-08-28.  The scramble is built and it is the run's
ending.**

⚠⚠ **Measured: a sortie cut short carries 200.0 points and one played out
carries 225.3** (`@M068`) — **the first measurement in dryopea of what a
SORTIE was worth.**  Every other clock in the corpus counts down to a base
FALLING; this one counts what the player carried out, which is
`DESIGN.md` § 2's *hold longer for more haul; launch now to keep what you
have* as a number.

The scramble is [`ROADMAP.md`](../ROADMAP.md) § The critical path item **4**,
the last of its four gaps, and the mechanic the game is named after
([`docs/DESIGN.md`](../../docs/DESIGN.md) § 2: *scramble-and-salvage*).  Its
ingredients all shipped — the carry model ([15](../15-the-carry-model/README.md)),
detachable tower tops ([17](../17-tower-hot-swap/README.md) T2), the wallet,
the core marker, and `fixstep::Timer`.

⚠⚠ **What this plan changes, in one line: the scramble becomes how a base
ENDS, and `wallet_broke` stops being an ending at all** (owner, 2026-08-28,
`@X292`).  `DESIGN.md` § 14 is categorical — *dryopea does not have a fail
screen; every base ends with the player launching the rocket* — and § 4 makes
the core invulnerable precisely so *the player is never forced out*.  **The
wallet reaching zero is POVERTY, not an ending.**

⚠⚠ **AND THE BASE NEVER ENDS ITSELF** (`@X293`, owner 2026-08-28): *"the
player can have their own reason to wait out their time.  They might be a
spectator to something else that is going on.  **It is always their decision to
stop.**"*  So this plan's job is not to add an ending beside the others — it is
to make the player's launch **the only one there is**, and to refuse every
candidate by name: the wallet at zero plays on, the wave list running out is a
PHASE, an expired permit costs the cargo, and a destroyed vehicle arms a
countdown **that exiting cancels**.

⚠ It also sets a standing refusal for this plan and everything after it:
**nothing may hurry the player along** — no auto-launch, no hard timer, no
*"nothing left to do"* prompt.  Each is the game making the decision, however
gently.

⚠⚠ **`wallet_broke` as an ending is a STEPSTONE, and this plan is where it
gets written out.**  It has been called *"the run's ONLY end state"* since plan
12 B6 — in `CLAUDE.md`, in `wallet.loft`'s own header, in four `@M` rows and in
the prose of forty scenarios that say *the base FELL*.  ⚠ Per `@X292` the
current implementation may stand for the time being; **what may not stand is
writing it down as a rule**, and S2 is where the framing changes.

## Goal

The player drives onto the core, a countdown runs, and the run ends with a
manifest — so that **what a sortie was worth** becomes a number the gates can
read.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 4 (the core, the opening face, the
  countdown), § 14 (run structure, the cargo manifest), § 15 (landing flow).
- [`docs/SETTING.md`](../../docs/SETTING.md) § The quarantine — the scramble is
  the only confirmed exit through the cordon.
- `src/spawn.loft` (`wave_take` / `wave_drop`, the core), `src/play.loft` (the
  seam and the clock), `src/wallet.loft` (`wallet_broke`, the current end
  state), `src/carry.loft`, `src/vehicle.loft` (`vehicle_respawn`),
  `src/helper.loft`, `fixstep` (`Timer`).
- `examples/numbers.json` § core.launch_countdown_duration (6.0 s),
  § economy.carryover_points_ratio (1.0).

## Invariant gate

⚠ **The countdown is a `Timer`, and plan 26's family rules apply in full.**
`plans/26` § THE POLICIES and `@D004` cost two one-shot timers a tick each;
the concrete expected result is that **6.0 s is 9 ticks at every tick length
the sweep uses**, and the negative control is the one `tests/26_l3` already
names — a guard that cannot fire is not a guard, so the cancel branch is
exercised directly.

⚠ **The manifest is a CONSERVATION**, the same shape `carry.loft` gives cargo:
what is aboard plus what is left behind equals what existed.  The negative
control is a manifest taken twice — a second liftoff must not double it.

⚠ **No other phase has an exact-invariant surface**: the countdown's *feel*
and what a sortie is worth are measurements, not invariants.

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **S0** — probe: what the design assumes vs what exists | XS | readings below | ✅ **done 2026-08-28** |
| **S1** — inside the core: the position, the countdown, the cancel | S | `tests/28_s1_the_countdown.loft` (8) | ✅ **done** |
| **S2** — liftoff: the ending, and the wallet stops being one | S | the `launch` / `launched` verbs; 852 UNMOVED | ✅ **done** |
| **S3** — the manifest: what goes aboard, and what is left | M | `tests/28_s3_the_manifest.loft` (6) | ✅ **done** |
| **S4** — what a sortie is WORTH | S | `a-sortie-cut-short` / `a-sortie-played-out` — **200.0 vs 225.3** | ✅ **done** |
| **S5** — nothing ends the base but the player | S | `tests/28_s5_nothing_ends_it_but_you.loft` (5) | ✅ **done** |

## ⚠⚠ S0 — what the probe found

Four readings, taken before a line was designed.

1. ⚠⚠ **THE PLAYER DRIVES STRAIGHT OVER THE CORE.**  `DESIGN.md` § 4 describes
   a 7-hex prism with six faces and an *opening* you drive **through** into an
   interior — **none of it exists.**  The core is ONE marker on ONE hex of
   ordinary ground, and a drive from (−3, 0) to (3, 0) across it ends at (3, 0)
   without noticing: **a marker is not terrain.**
   ⚠ So *"inside the core"* has no meaning yet and S1 has to define it.
2. **`wallet_broke` is the only end state**, confirmed from a fresh
   `WaveState`: false, and true once drained.  Every clock in the corpus
   counts down to a base falling.
3. **Nothing boards and there is no manifest** — a fresh state has 0 crew and
   0 cargo slots, and no structure anywhere records what left with the player.
4. ⚠ **The countdown's raw material is already right**: `fixstep::Timer` armed
   with `clock_units_from_seconds(6.0)` holds 18 000 000 units against a
   `TICK_STEP_UNITS` of 2 000 000 — **exactly 9 ticks**, no remainder, which is
   what `plans/26` § A COUNT asked for in SECONDS comes back SHORT exists to
   guarantee.

### ⚠⚠ What S0 settles about the design

**Beside the core you TRADE; on the core you LEAVE.**

⚠ Every existing core interaction uses *within `CARGO_REACH_HEXES`* — that is
where `beacon_buy` debits the wallet and where `wave_drop` delivers a top.  The
scramble is the one that should be tighter, because the fiction is *drive
through the opening*: standing **on the core's own hex** is the position, and
it is distinguishable from beside it with no new geometry, no faces and no
interior.

⚠⚠ **And `vehicle_respawn` already puts the player there**, which means
`DESIGN.md` § 4's *"vehicle death starts the launch countdown automatically —
a free 'ready to leave?' prompt"* **falls out with no code at all.**  That is
the strongest evidence the position is the right one.

## What this plan does NOT build

⚠ Named so a later reader does not think they were forgotten.

- **The core's six FACES and its interior.**  S0 measured that none exists; the
  core is a marker.  A face is art plus a geometry rule and belongs with the
  entity work, not with the run's ending.
- **The bottom pulse** (`DESIGN.md` § Two surface signals) — a render signal,
  and `docs/PARTS.md` owns it.
- **Helpers boarding on their own** when their work list is empty.  S3 takes
  *who is at the core at liftoff*; a helper walking home by itself is
  `@X252`'s directed helpers, deliberately parked.
- **Landers, the landing flow, the starter tower** (`DESIGN.md` § 15) — D2
  measured that none of it exists, and it is the run's BEGINNING.
- **The next base.**  Carryover is produced and measured; nothing consumes it,
  because a run is a sequence of bases and there is one base.
- ⚠⚠ **The permit clock** — BACKLOG D1, which is blocked on *this* plan:
  an expired permit costs the CARGO, and until S3 there is no cargo to cost.

## Open questions

1. ⚠ **Does the countdown pause the wave?**  It must not — `DESIGN.md` § 4
   calls it *a hazard window*: enemies keep nibbling while the player sits
   inside.  Recorded as read; S1 asserts it.
2. ⚠ **What does a scenario ASSERT at a scramble?**  `wallet 0 0` is the
   corpus's *the base fell*, which is the stepstone's framing (`@X292`).  A
   scramble is the ending, so the `fall` verb gains a sibling and its own prose
   changes meaning.  S2 owns it, and it is a vocabulary decision rather than a
   design one.
3. ✅ **ANSWERED by `@X292`** — *is a scramble with an empty manifest
   distinguishable from a fall?*  The question dissolves: **there is no fall.**
   A base ends one way, and *"meagre carryover"* is the whole of what a bad
   sortie is.  ⚠ The wallet emptying stays a real, measurable EVENT — the
   moment the player stops earning — and it stops being an ending.


## ⚠⚠ What shipped, and the two things the build changed

**`src/scramble.loft`** — `Scramble` on `WaveState`, `vehicle_in_core`,
`scramble_step`, and `manifest_of`.  `wave_tick` steps it right after the
player has moved, so it reads the hex they ENDED on.  `script.loft` gains
`launch <max>` and `launched <yes|no>`.

⚠ **Arriving and arming are the same tick**, which the S1 gate states as a
number: four hexes at 3 hex/s is two ticks, and the second of them arms — so
a drive-in-and-leave is **eleven** ticks, not twelve.

⚠⚠ **`manifest_of` takes the PIECES and not the `WaveState`, and that is
structural**: `spawn.loft` uses `scramble.loft`, so this file cannot see
`WaveState` — a `use` imports one way only.  `moat.loft::moat_depth` was
shaped by the same constraint, and the same benefit follows: a manifest can
be asked about a roster and a wallet that are not in a run at all, which is
what makes it testable.

⚠ **Two of the manifest's four columns have nothing to read yet** and the
code says so rather than pretending: there is no *deposit a top at the core*
anywhere in the tree, and delivered loot enters the wallet the moment it is
cleared, so it is already in the points.

## What is now unblocked

⚠ **BACKLOG D1 — the permit clock.**  It was blocked on this plan: an
expired permit costs the CARGO, and until S3 there was no cargo to cost.
⚠⚠ It stays blocked on the RULING (`@X287` — the battleships cannot be the
clock in the view the game ships), not on the mechanism.

⚠ **The next base.**  Carryover is produced and measured; nothing consumes
it, because a run is a sequence of bases and there is one base.  That is the
next thing this plan makes possible and deliberately does not do.

⚠⚠ **AND THE CONSUMER IS NAMED, 2026-08-28** (`@X306`,
[`docs/ERRANDS.md`](../../docs/ERRANDS.md) § The compact RESULT): **the
SERVER**.  The owner ruled that a finished scenario hands back a compact
result *"to use as a basis for future missions"*, and `manifest_of` is
its PLAYER half already built — the other half is the WORLD's, the same
snapshot rows changed.  ⚠ So this plan's `Manifest` is not waiting on a
second base; it is waiting on the store that reads it.
