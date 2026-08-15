<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 18 — Scenario capture: turn a situation into a test

**Value:** `S` · **Effort:** `MH`

## Status

**Planned.**  Nothing built.  Suite 983 green, gate 28 scripts / 520
measurements at the point this opens.

⚠ **Two facts measured while opening it**, both of which move the risk:

1. **Floats round-trip through text exactly.**  `1.0 / 1.5` prints as
   `0.6666666666666666` and re-parses identical, and so does the awkward
   `20.0 - (1/1.5) * 29` residue.  So a text format can carry a banked
   timer without losing it, and the round-trip identity this plan is
   built on is reachable rather than approximate.
2. **The live editor holds no game.**  `WaveState` rides on `ScriptRun`
   and `editor_step.loft` does not mention it — *"a session that is
   being edited has no enemies in it"*.  So the play-testing half of
   this idea waits on an interactive loop that does not exist; capture
   works on SCRIPTED runs today.  § What this plan does NOT build.

## Goal

Play until something interesting happens, then emit the smallest
`.keys` scenario that reproduces it — so a state you can reach but could
not have thought of becomes a test.

## Why this is `S` rather than a convenience

[Plan 15](../15-the-carry-model/README.md) C1 recorded the problem in
one sentence:

> ⚠ **The two-objects-on-one-hex case is now reachable in a test, and it
> was reachable in the shipped game before this plan existed.**  A
> helper carrying a downed colleague, destroyed while blocking, leaves
> its load AND its own wreck on one hex.  Under the hash-keyed-by-hex
> shape every other runtime layer uses, one of them is a crew member
> deleted from the run **with no fault raised anywhere**.

That state was reachable in play for as long as helpers existed, and
nothing could see it until somebody happened to imagine it.  That is a
silent-failure class, not a missing convenience — and the same shape
recurs: plan 11 F7b's sidestep sat latent for three phases, plan 14 H2's
saturated gate read flat for a reason nobody suspected.  ⚠ **A tool that
turns a reached state into a fixture is how you stop needing to guess
first.**

## Anchors

Implements, and does not restate:

- [`plans/08`](../08-game-validation/README.md) — the `.keys` runner,
  the seam, and § The instrument comes first, which is why S0 is a
  comparison rather than a feature.
- `src/script.loft` (the vocabulary), `src/convert.loft::keys_schemas`
  (every coordinate verb needs a row), `src/spawn.loft` (`WaveState`).

### ⚠⚠ Decision: it emits `.keys`, never a state blob

A saved `WaveState` would be a **golden of the simulation**, and it
inherits the trap `CLAUDE.md` § Test discipline already records — *a
golden AGREES WITH A SHEAR*.  Worse than an image: a rebaselined PNG at
least gets looked at, and a state dump does not.

A `.keys` file is a different thing entirely.  It is an authored
STARTING POSITION — the same kind every scenario in `tests/scripts/`
already is — so there is nothing derived in it to go stale, a reviewer
can read it, and it replays through the one seam everything else does.

⚠ **And it dodges the blocker.**  `WaveState` has never been
serialisable: `MapFile` is capped at 6 fields by a loft JSON hang, and
`text as vector<Struct>` miscompiles to zero entries on the native
backend.  A line-oriented text format goes through a parser that already
exists and touches none of that.

### ⚠⚠ So the real work is making `.keys` TOTAL over the state

Checked field by field against `WaveState`:

| field | authorable today |
|---|---|
| `heights` | ✓ `raise <q> <r> <m> [source]` |
| `damage` | ✓ `damage <q> <r> <hp>` |
| `player` | ~ `park <q> <r>` — position only, not hull, boost or cooldown |
| `crew` | ~ `crew <q> <r>` — not HP, not wrecked, not recovery |
| `schedule` | ~ `schedule <counts…>` — arms a list, cannot start mid-list |
| `enemies` | ✗ `wave <n>` spawns stacked at markers; `enemy <i> <q> <r>` is an **assertion** |
| `towers` | ✗ `shots` / `top` are measurements, not setters |
| `wallet` | ✗ |
| `cargo` | ✗ |

⚠ Once every field has a setter, the emitter is nearly trivial and the
gate falls out: **capture → emit → replay, and the two states must be
identical**.  That is an exact invariant with a real oracle, which is
what makes the tool trustworthy rather than plausible.

## ⚠ The crop has a MINIMUM RADIUS set by the mechanics

A geometric crop around the interesting hex will silently change
behaviour:

- **The core** is what every enemy routes to.  Crop it out and the flow
  field is empty, so every enemy becomes routeless and switches to the
  DESIRE field — a different steering mode (`ENEMY_MOVEMENT.md` § Two
  modes), and the fixture reproduces nothing.
- **The scrambler bubble** is a straight-line 25 hexes, and it decides
  whether an enemy steers by the field or by its spawn heading.
- **Spawn markers** under `SPAWN_DISABLE_RADIUS` are silenced and ones
  at 12+ can be provoked, so cropping moves a marker's MEANING.
- **A tower's range is 15** and its sight line runs over `hex_height`,
  so terrain fifteen hexes away can be load-bearing for whether a shot
  lands.

⚠ So a crop is bounded by the rules rather than by the picture, and a
fixture that reproduces under the full map and not under the crop is a
**cropping bug that reads exactly like a flaky test**.  S3 owns this,
and its negative control is that a crop dropping the core must be
REFUSED rather than quietly emitted.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **S0** | two states built the same way compare EQUAL | a state comparison that can see **every** field | ⚠ the load-bearing half: two states differing in ONE field must compare UNEQUAL, field by field — a digest that skips a field passes the equality test and makes every later gate vacuous |
| **S1a** | an enemy placed by a `.keys` line matches one built in loft | the script can express an `Enemy`'s whole state | `stand` and `taken` are zero-neutral (plan 12 B4, plan 16 W2), so a line that omits them must give a HEALTHY robot that walks, not a corpse that has not arrived |
| **S1b** | ditto for towers, wallet, cargo, and the condition fields | the vocabulary is TOTAL over `WaveState` | a field with no setter must fail S2's round trip loudly rather than be dropped silently |
| **S2** | capture → emit → replay → identical | **round-trip = identity**, the exact invariant this plan is built on | a state carrying a field the emitter forgot must go RED; S0's per-field comparison is what makes that possible |
| **S3** | a cropped fixture reproduces the property | a crop is bounded by the RULES, not the picture | a crop that drops the core, or lands inside the bubble radius, is REFUSED — not emitted and left to read as a flaky test |
| **S4** | the reduced fixture still shows the property | minimality is checkable: removing any one more line breaks it | a reducer with no predicate reduces to nothing and calls it minimal |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **S0** — the instrument: comparing two states | XS | `tests/18_s0_the_comparison.loft` — equal states compare equal, and a state differing in exactly one field compares unequal, **once per field**.  ⚠ There is no `WaveState` equality today, so every later gate has no oracle until this exists | Open |
| **S1a** — an enemy becomes authorable | S | `tests/18_s1a_placing_an_enemy.loft` — `enemy` gains a setter form (or a new verb); a placed enemy equals one built in loft, including heading, damage taken and the pre-walk window | Blocked on S0 |
| **S1b** — the rest of the state becomes authorable | M | `tests/18_s1b_the_vocabulary_is_total.loft` — setters for towers, wallet and cargo plus the condition fields, and a test that walks every `WaveState` field asserting each has one | Blocked on S0 |
| **S2** — the emitter, and the round trip | M | `tests/18_s2_the_round_trip.loft` — capture a state from each `tests/scripts/*.keys` run, emit, replay, and assert S0-identical.  ⚠ Sweeping the REAL scenarios rather than hand-built states, the shape `09_c5a` uses | Blocked on S1a + S1b |
| **S3** — the crop | S | `tests/18_s3_the_crop.loft` — a cropped fixture reproduces its property; a crop that would drop the core or cut inside the bubble is refused | Blocked on S2 |
| **S4** — the reduce | M | `tests/18_s4_the_reduce.loft` — delta-debug against a supplied predicate; the output still shows the property and is minimal | Blocked on S3 |

### Why the order is this order

**S0 first because nothing else has a gate without it.**  Plan 08's own
rule — *the instrument comes first* — and here it is literal: the round
trip's whole claim is "the two states are identical", and dryopea cannot
currently say whether two states are identical at all.

**S1 before S2** because an emitter can only emit what the language can
say.  ⚠ And the emitter is what makes S1 honest: a field with no setter
is invisible while you are writing setters and obvious the moment a real
state fails to round-trip.

**S3 and S4 last** because both are transformations of a fixture that
must already be known-good.  Cropping something that never round-tripped
would be minimising a lie.

## What this plan does NOT build

**No binary save format.**  The `.keys` text IS the format — see
§ Decision.  A future path-backed `Store` (`CLAUDE.md` § Save path)
would be a different feature with a different purpose.

**No interactive play loop**, and this is the honest limit on the
play-testing half.  `editor_step.loft` has no `WaveState` in it, so
there is nothing to capture from a live session yet — capture runs
against `.keys` scenarios.  ⚠ The moment an interactive loop exists this
plan's emitter is what makes it a save button, and that is worth
knowing when the loop is designed.

**No automatic "interesting" detector.**  What counts as interesting is
a PREDICATE the caller supplies.  A tool that guessed would produce
fixtures nobody asked for and could not explain.

**No suite speed-up**, and it is worth saying plainly because it is what
prompted the idea.  The two slowest files are trajectory measurements —
plan 17 T3's finding is literally *"537 ticks later it cleared"* — so
there is no interesting spot to crop to and the whole run is the
finding.  ⚠ Suite time is a separate question and it wants a profile
first.

## Open questions

1. **Does `pick_cursor` need to round-trip?**  It is the round-robin
   index into the active markers, so it decides which marker the NEXT
   wave uses.  A fixture that dropped it would replay identically until
   a wave spawned and then diverge.  *Recommendation: carry it — the
   cost is one verb and the failure mode is a fixture that works until
   it does not, which is the worst kind.  S1b decides.*
2. **What does the emitter do about the PAINTED world?**  A scenario
   authors terrain with `palette` + `drag` + `click`, which is a
   sequence of editor actions rather than a description of the result.
   Emitting one `click` per painted hex is correct and unreadable for a
   120-hex band.  *Recommendation: emit runs as `drag` where the hexes
   are collinear and fall back to `click`, and gate it on the round trip
   rather than on how it reads.  S2 decides.*
3. **Should a captured fixture keep its ORIGIN?**  A comment naming the
   scenario and tick it came from makes a fixture explicable a year
   later, and is the difference between a test somebody trusts and one
   they delete.  *Recommendation: yes, as a `#` header the runner
   ignores — S2, and it costs nothing.*
4. **Can the reducer use a test's own assertions as its predicate?**
   That would make S4 nearly free at the call site: point it at a
   failing measurement and let it shrink.  Unknown whether the runner
   can be driven that way.  *S4 decides, and it is the phase most
   likely to be deferred if the answer is no.*

## See also

- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  `.keys` runner and the seam this plan writes for, and the source of
  § The instrument comes first.
- [`plans/15-the-carry-model`](../15-the-carry-model/README.md) § C1 —
  the reachable-but-unauthorable state that makes this plan `S`.
- [`plans/17-tower-hot-swap`](../17-tower-hot-swap/README.md) § T3 —
  where the idea came from, and the measurement that shows it will not
  speed the suite up.
