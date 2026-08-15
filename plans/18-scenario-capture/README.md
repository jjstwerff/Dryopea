<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 18 — Scenario capture: turn a situation into a test

**Value:** `S` · **Effort:** `MH`

## Status

**S0 + S1a + S1b shipped** (2026-08-15).  S2 is next.  Suite **1028
green**, gate **28 scripts / 520 measurements**.

**S0 built the oracle every later phase needs.**  `src/compare.loft`
answers *how* two `WaveState`s differ — the first difference, named —
and `tests/18_s0_the_comparison.loft` discriminates **every field of
every struct in the state**, one case each.  § S0.

**S1a made the roster authorable.**  `place <q> <r> <class> [heading]`
puts ONE enemy down, and `stand` / `dead` reach the two fields a
placement leaves neutral — so all seven fields of an `Enemy` can now be
said in a `.keys` file.  § S1a.

**S1b made the vocabulary TOTAL.**  Seven more verbs — `tower`,
`object`, `spent`, `player`, `member`, `pending`, `cursor` — and every
field of a `WaveState` can now be authored from a script.  The gate is
one state with every field non-neutral, built both ways and compared.
§ S1b.

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

| field | authorable | by |
|---|---|---|
| `heights` | ✓ | `raise <q> <r> <m> [source]` |
| `damage` | ✓ | `damage <q> <r> <hp>` |
| `enemies` | ✓ **S1a** | `place` + `hit` / `stand` / `dead` |
| `towers` | ✓ **S1b** | `tower <q> <r> <shots> <charge> <repair> <on\|off>` |
| `wallet` | ✓ **S1b** | `spent <points>` |
| `cargo` | ✓ **S1b** | `object <q> <r> <kind> <subj> <owner>` |
| `player` | ✓ **S1b** | `park` / `drive` + `player <on\|off> <boost> <cool> <taken>` |
| `crew` | ✓ **S1b** | `crew` / `send` + `member <i> <on\|off> <progress> <taken> <recover>` |
| `schedule` | ✓ **S1b** | `schedule <counts…>` + `pending <sent> <lull> <on\|off>` |
| `pick_cursor` | ✓ **S1b** | `cursor <n>` |

⚠ Every field has a setter, so the emitter is nearly trivial and the
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
| **S0** ✓ | equal states compare equal; a fork compares equal; one tick apart they do not | a comparison that sees **every** field, and is INDEPENDENT of the emitter | ✓ 35 per-field discriminations, verified by three blindness mutations rather than by reading; ✓ hash FILL ORDER is not a difference; ✓ one ulp IS one, so nobody adds an epsilon for tidiness |
| **S1a** ✓ | a placed enemy is S0-identical to one built in loft, all seven fields | the script can express an `Enemy`'s whole state, and a bare placement is the NEUTRAL one | ✓ both zero-neutral fields verified by mutation — a placement that starts mid-window stops walking, one that starts pre-damaged reads as a corpse, and each turns three assertions red; ✓ `dead` deposits NO body, so `wave_deaths` stays the one death path |
| **S1b** ✓ | one state with every field non-neutral, authored from `.keys` and S0-identical to the same state built in loft | the vocabulary is TOTAL over `WaveState` | ✓ three setters mutated to drop a field: each fails the TOTALITY test and `state_diff` names the field — *'towers (4, 0).repair: 0 vs 6'*.  ⚠ Seven separate 'this verb works' tests would stay green while the one state a capture needs is unreachable |
| **S2** | capture → emit → replay → identical | **round-trip = identity**, the exact invariant this plan is built on | a state carrying a field the emitter forgot must go RED; S0's per-field comparison is what makes that possible |
| **S3** | a cropped fixture reproduces the property | a crop is bounded by the RULES, not the picture | a crop that drops the core, or lands inside the bubble radius, is REFUSED — not emitted and left to read as a flaky test |
| **S4** | the reduced fixture still shows the property | minimality is checkable: removing any one more line breaks it | a reducer with no predicate reduces to nothing and calls it minimal |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **S0** — the instrument: comparing two states | XS | `tests/18_s0_the_comparison.loft` — 15 tests; 35 per-field discriminations, and three blindness mutations verified (drop `Enemy.stand`, drop `CarryObject.owner`, compare a layer by count alone) each turn exactly one assertion red | **Done** |
| **S1a** — an enemy becomes authorable | S | `tests/18_s1a_placing_an_enemy.loft` — 15 tests; a placed enemy is S0-identical to one built in loft, all seven fields; three mutations verified (a placement mid-window, a placement pre-damaged, and `dead` depositing a body) | **Done** |
| **S1b** — the rest of the state becomes authorable | M | `tests/18_s1b_the_vocabulary_is_total.loft` — 15 tests, centred on ONE state with every field non-neutral built both ways; three dropped-setter mutations verified (`tower`'s repair, `cursor`, `member`'s recovery) | **Done** |
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

## S0 — the comparison (2026-08-15)

`src/compare.loft::state_diff(a, b)` answers `""` when two states are
identical and otherwise names the **first** difference and both values
— *"cargo[2].owner: 3 vs -1"*.  A gate that fails with *"the states
differ"* sends you to a debugger; this sends you to the line, which is
the choice `validate.loft` makes in reporting the number that moved.
`states_equal` is defined in terms of it, so the boolean cannot
disagree with the message.

### ⚠⚠ It is NOT built on the emitter, and that is the phase

The shortcut is to define equal as *emit both and compare the text*.
That makes S2's round-trip gate **circular**: an emitter that silently
dropped a field produces identical text for two states that differ in
it, and the gate is green precisely where the tool is broken.  So this
reads the state field by field and knows nothing about how a state is
written down.

### ⚠⚠ The gate is the per-field discrimination, not the equality

*"Two states built the same way compare equal"* is true of
`fn states_equal(a, b) { true }`.  So the file spends 35 assertions
building two states that differ in **exactly one field** and checking
the diff sees it — every field of `WaveState` and of `Enemy`, `Helper`,
`Vehicle`, `HexRise`, `StructureHit`, `TowerCharge`, `Wallet`,
`CarryObject` and `WaveSchedule`.

⚠ Verified by MUTATION rather than by reading: made the diff blind to
`Enemy.stand`, then to `CarryObject.owner`, then compared a whole layer
by count alone.  Each turns exactly one assertion red.

### ⚠ Three things the phase settled

- **Hash fill order is not a difference.**  Layers are compared by KEY
  — walk A, look each entry up in B, then check the counts — because
  iteration order is *"stable within a single program run but not
  across runs"*.  Walking both in step would make identical states
  differ on some runs and not others.
- **Floats are compared EXACTLY.**  An epsilon would hide the ulp of
  drift S2 exists to catch, which inverts the rule `helper.loft` and
  `tower.loft` follow for *deciding* a timer — and `one ulp IS a
  difference` is asserted so nobody adds one for tidiness.
- **A withdrawal does not rename a pile.**  The first draft asserted
  that raising a pile by 0.0 m with a different source is visible; it
  is not, because `height_raise` renames only on a deposit
  (`if metres > 0.0`).  ⚠ The test was wrong and the comparison was
  right — the direction that costs a debugging session if you assume
  the other one.

### ⚠ And the file order is a scar

Every helper is declared **before** its caller and the one public door
is at the bottom, which reads backwards.  It is
[loft#918](https://github.com/loft-lang/loft/issues/918) — a local
bound to a call whose callee sits lower in the file panics the parser.
dryopea filed it from a plan 17 probe and this file hit it on the first
compile.

## S1a — placing one enemy (2026-08-15)

`place <q> <r> <class> [heading]` puts one enemy on a hex.  `stand <i>
<seconds>` sets the pre-walk window and `dead <i>` marks one down;
`hit <i> <hp>` already existed and is what reaches damage taken, reused
rather than duplicated.

⚠ **A separate verb rather than an overload of `enemy`**, which is an
ASSERTION.  Plan 12 B4 set the rule when it refused to overload
`damage` for enemies: *a line whose meaning depends on knowing which
reading applies* cannot be checked by looking at it.

### ⚠⚠ The gate is the zero-neutral half

`Enemy` has two fields whose neutral value is not obvious, and both
were chosen so an omitted field means the useful thing:

| field | 0 means | set by |
|---|---|---|
| `taken` — damage ABSORBED (plan 12 B4) | a HEALTHY robot | `hit` |
| `stand` — seconds LEFT at the marker (plan 16 W2) | one free to WALK | `stand` |

So a bare `place` must give a robot that is whole and moving.  ⚠ Get
either backwards and the fixture spawns a corpse that has not finished
arriving, while every assertion about *"the wave is there"* stays green
and nothing moves.  Verified by mutation: a placement that starts
mid-window turns three assertions red, and one that starts pre-damaged
turns three others.

### ⚠ It AUTHORS and never simulates

`dead <i>` marks an enemy down and deposits **no body**.  That is
`damage <q> <r> <hp>`'s rule — it fills the ledger and *"cannot BREAK
anything, only a tick does"* — and it is what keeps `wave_deaths` the
engine's one death path.  A captured state carries the body as its own
`raise` line, so replay reproduces both without a second path to drift.
⚠ Mutated to deposit one and three assertions go red.

### ⚠ S0 paid for itself immediately

Every equality here is `state_diff`, and when a mutation broke a
placement the failure read **`enemies[0].stand: 5 vs 0`** — the field
and both values, not "the states differ".  That is the phase-ordering
argument made concrete: S1a's gate would have been a hand-written pile
of per-field assertions without it.

## S1b — the vocabulary becomes total (2026-08-15)

Seven verbs: `tower <q> <r> <shots> <charge> <repair> <on|off>`,
`object <q> <r> <kind> <subj> <owner>`, `spent <points>`,
`player <on|off> <boost> <cool> <taken>`,
`member <i> <on|off> <progress> <taken> <recover>`,
`pending <sent> <lull> <on|off>` and `cursor <n>`.

### ⚠⚠ The gate is the TOTALITY test, not seven verb tests

One state in which **every field of every struct holds a non-neutral
value**, authored entirely from a `.keys` source and asserted
S0-identical to the same state built in loft.  A field with no setter
cannot pass it.

⚠ That is strictly stronger than seven *"this verb works"* tests, which
stay green while the ONE state a capture actually needs is unreachable.
Verified by mutation: `tower` dropping its repair field, `cursor` made a
no-op, `member` dropping the recovery clock — each fails the totality
test, and `state_diff` names it (*"towers (4, 0).repair: 0 vs 6"*).

### ⚠ Seven command words, and the CONVERTER is why

The tidy design is one `set` verb with a subject — `set tower 4 0 …`,
`set wallet 30`.  It is wrong here: `convert.loft::keys_schemas` keys a
coordinate's POSITION on the first token, so one `set` row would need
the pair at index 2 for a tower and nowhere for a wallet.  The mismatch
is **silent** — a lattice conversion would rewrite `set member 0 on 0.5`
as though `0 on` were a hex, which is exactly what
`09_c5a_converter.loft` § What must NOT be touched exists to refuse.

⚠ `object` therefore takes its coordinates FIRST, before the kind, so
its pair sits where every other coordinate verb's does.

### ⚠ They author and never simulate

`tower` does not fire, `object` does not check reach, `player` does not
respawn.  `damage <q> <r> <hp>`'s rule for the third time: a setter
fills the ledger and triggers no consequence, so the engine keeps one
code path for each thing that actually happens.  ⚠ Asserted — a tower
authored black shoots nothing and an object authored into the player's
hands needs no player within fifteen hexes of it.

### ⚠ And they live in their own function

`script_author`, not seven more branches of `script_command` — whose
control-flow complexity the advice lint already flags at 214.

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
