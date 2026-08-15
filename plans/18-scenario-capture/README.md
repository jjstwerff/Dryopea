<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 18 — Scenario capture: turn a situation into a test

**Value:** `S` · **Effort:** `MH`

## Status

**COMPLETE** (2026-08-15) — S0 + S1a + S1b + S2 + S3 + S4.  Suite
**1051 green**, gate **28 scripts / 520 measurements**.

**A situation can now be captured, written down, cut to size and cut to
the bone.**  The tool exists end to end; what it is still waiting for is
an interactive loop to capture FROM — § What this plan does NOT build.

**S2 closed the round trip.**  `src/emit.loft::emit_keys` writes the
`.keys` file that reproduces a situation, and **all 28 real scenarios**
survive capture → emit → replay with both the state and the world
identical.  § S2.

**S3 built the crop, and measured its limit.**  A crop keeps a disc and
refuses the ones that certainly break — but ⚠ **a LEGAL crop can still
change the answer**, and the phase measures one doing it.  § S3.

**S4 answered S3 with a predicate**, and the predicate turned out to be
a `.keys` fragment — the measurement vocabulary the suite already
speaks.  ⚠ Open question 4 is closed: a test's own assertions ARE the
predicate, and it needed no machinery.  § S4.

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
| **S2** ✓ | 28 of 28 scenarios round-trip, state and world identical | **round-trip = identity**, over the REAL corpus rather than a fixture | ✓ three dropped fields each go red and NAME themselves (*'towers (4, 0).charge: 0.4 vs 0'*); ✓ a hand-built state holds the fields no shipped scenario exercises; ✓ a state one TICK on survives, so the emitter handles what the SIMULATION produces and not only what a script authors |
| **S3** ✓ | a radius-25 crop reproduces the run exactly 30 ticks on; a radius-15 crop of the same base silently stops the waves | a crop is bounded by the RULES, not the picture — and the rules are NECESSARY, not sufficient | ✓ dropping the core and cutting under a tower's reach are both refused, and a refused crop writes NOTHING; ✓ the tight crop is the control that stops this reading as 'cropping works'; ✓ three mutations (skip the core check, split a tower from its cell, ignore the radius) each go red |
| **S4** ✓ | the fixture shrinks and what is left still shows the property | **1-minimal**: removing any ONE remaining line breaks the predicate, checked line by line | ✓ a predicate holding over an EMPTY fixture is REFUSED — the whole difference between a reducer and a delete button; ✓ a predicate that does not hold over the fixture is refused too; ✓ the origin comment is pinned, so a reduced fixture stays explicable |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **S0** — the instrument: comparing two states | XS | `tests/18_s0_the_comparison.loft` — 15 tests; 35 per-field discriminations, and three blindness mutations verified (drop `Enemy.stand`, drop `CarryObject.owner`, compare a layer by count alone) each turn exactly one assertion red | **Done** |
| **S1a** — an enemy becomes authorable | S | `tests/18_s1a_placing_an_enemy.loft` — 15 tests; a placed enemy is S0-identical to one built in loft, all seven fields; three mutations verified (a placement mid-window, a placement pre-damaged, and `dead` depositing a body) | **Done** |
| **S1b** — the rest of the state becomes authorable | M | `tests/18_s1b_the_vocabulary_is_total.loft` — 15 tests, centred on ONE state with every field non-neutral built both ways; three dropped-setter mutations verified (`tower`'s repair, `cursor`, `member`'s recovery) | **Done** |
| **S2** — the emitter, and the round trip | M | `tests/18_s2_the_round_trip.loft` — all 28 real scenarios survive capture → emit → replay, state AND world; three dropped-field mutations verified (a tower's charge, a pile's source, the terrain) | **Done** |
| **S3** — the crop | S | `tests/18_s3_the_crop.loft` — 8 tests: a lossless crop reproduces the run tick for tick, a legal-but-tight one demonstrably does NOT, and the two certainly-broken crops are refused.  Three mutations verified | **Done** |
| **S4** — the reduce | M | `tests/18_s4_the_reduce.loft` — 9 tests: greedy line removal against a `.keys` predicate, 1-minimality checked by removing every surviving line, and a predicate that holds over an EMPTY fixture refused | **Done** |

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

## S2 — the emitter, and the round trip (2026-08-15)

`src/emit.loft::emit_keys(pal, pw, mw, state, origin)` writes the
`.keys` file that reproduces a situation.  The gate plays every file in
`tests/scripts/`, captures the state it reached, emits it, replays it
into a session that has never seen the map, and asserts both the state
and the world are identical.  **28 of 28.**

⚠ **The corpus, not a fixture** — `09_c5a_converter.loft`'s shape and
its reason: a hand-built state exercises what its author remembered,
the corpus exercises what the game produces.  Backed by two controls a
sweep cannot give: a hand-built state holding the fields no shipped
scenario exercises (a boost running, a recovery clock, a spent cargo
slot, a retuned lull), and a state taken one TICK past a scenario's end,
so the emitter is shown to handle what the SIMULATION produces.

### ⚠⚠ The world is compared too

Terrain and markers are not in `WaveState`, so a round trip comparing
only the state would be green for an emitter that lost half the map.
`compare.loft` grew `world_diff`, and mutating the emitter to drop the
ground turns the sweep red — as a REPLAY failure, because the fixture
then tries to damage a wall that is now sea.

### ⚠ Order is load-bearing, and that is a feature

The verbs validate each other's subjects — `flag` before `tower`,
`crew` before an `object` owned by a crew index, `place` before `hit` /
`stand` / `dead`, `schedule` before `pending`.  Get one wrong and the
emitted file fails to REPLAY, loudly, naming the line.

### ⚠ A direct marker verb, because the editor's dance is not emittable

Every shipped scenario places a marker by toggling to MARKER mode,
pressing `cycle_kind` the right number of times and clicking — and the
right number depends on `MARKER_KIND_COUNT`, which grows.  A generated
fixture written that way rots the day a kind is added, **silently
placing the wrong one**.  So S2 added `flag <q> <r> <kind> [dir]`, and
`marker` stays the assertion — the same split `place` / `enemy` keeps.

### ⚠⚠ And S1b's totality test had a hole

`WaveSchedule.delay` had **no setter at all** and the totality test
passed anyway, because both the script and the hand-built state used
`INTER_WAVE_DELAY_SECONDS`.  ⚠ **A field left at its DEFAULT on both
sides of a comparison compares equal and proves nothing** — so a
totality test has to hold every field at a value the default would not
produce.  Fixed in `pending`, and S1b's fixture now uses a delay of 9.0.

⚠ Note what would NOT have caught it: the round trip.  No shipped
scenario has a non-default delay, so emit-and-replay would have agreed
with itself for ever.  It took writing the emitter and asking *"which
verb writes this field?"* to find it.

## S3 — the crop, and what it cannot promise (2026-08-15)

`crop_keys(…, centre, radius, …)` keeps a disc and drops the rest.
`crop_fault` refuses two crops outright:

| refused | because |
|---|---|
| the core outside the disc | every enemy routes to it; without it the flow field is empty and every enemy falls back to the DESIRE field |
| radius < `CROP_MIN_RADIUS_HEXES` (= `TOWER_RANGE_HEXES`, 15) | `tower_sees` traces `hex_height` up to 15 hexes, so terrain that decided whether a shot landed becomes sea |

⚠ A refused crop **writes nothing** rather than emitting a smaller file:
a crop that broke the situation must not leave a fixture that looks fine
and reproduces something else.

### ⚠⚠ THE FINDING: a legal crop can still change the answer

The same band, cropped at radius 25 and at radius 15, both centred on
the core and both passing every rule `crop_fault` knows:

| crop | after 30 ticks |
|---|---|
| radius 25 | **identical** to the uncropped run, state for state |
| radius 15 | the base is **never attacked at all** |

The spawn marker sits eighteen hexes out, so the tight crop drops it —
and the wave list still runs, still counts a wave as sent, and puts
nobody on the ground.  ⚠ `SPAWN_DISABLE_RADIUS` and
`WAVE_1_PROVOCATION_HEXES` are distances from the core, so **cropping
moves what a marker MEANS** while nothing about the crop looks wrong.

⚠⚠ So `crop_fault` is documented as **necessary and not sufficient**,
and whether a particular crop preserved a particular behaviour is a
question about that behaviour — answerable only by running both and
comparing.  That is S4's predicate, and this phase is the argument for
why it is needed rather than optional.  ⚠ A test that only showed crops
working would have told the opposite of the truth.

### ⚠ What travels together

- **A tower's cell goes with its marker**, by the same predicate.  Kept
  apart, a marker without its cell replays as a FRESH tower — full
  magazine, top on — which is a different base that reads as the crop
  working.
- **The crew, the cargo and the player are never cropped.**  An index is
  an identity and `cargo.subj` points into the crew roster, so dropping
  a crew member leaves a wreck pointing at nobody.  They are few, and
  they are the interesting part of a situation anyway.

## S4 — the reduce (2026-08-15)

`reduce_keys(palette, fixture, predicate, shots)` removes every line the
predicate does not need, and `reduce_fault` says why it will not try.

### ⚠⚠ The predicate is a `.keys` fragment — open question 4, closed

The plan left it open whether a test's own assertions could BE the
predicate, *"unknown whether the runner can be driven that way"*.  They
can, and it needed **no machinery at all**: a predicate is script text
appended to the fixture, and it holds when the whole thing runs green.

```
tick 15
hp 6 0 5.0 6.5
```

So the measurement vocabulary the suite already speaks is the predicate
language, an assertion that fails is a predicate that does not hold, and
there is nothing new to keep in step.

### ⚠⚠ A predicate that cannot FAIL is a delete button

The reducer drops a line whenever the predicate still holds without it.
Hand it one that holds over an **empty** fixture and every line is
removable — it returns nothing and calls it minimal.  So `reduce_fault`
runs the predicate against an empty fixture first and refuses if it
passes.  That is the negative control this phase exists to have.

⚠ **And a WEAK predicate is not a vacuous one.**  The first draft used
`tick 0` as its example of a predicate that cannot fail — and the guard
correctly did **not** fire, because `tick` demands exactly one target
marker and so fails over nothing.  `tick 0` is legal and nearly
worthless: reducing against it honestly strips the fixture back to the
core marker.  The real example is `wallet 0.0 200.0`, which needs no
core, no wave and no map, because the budget belongs to the RUN.

### ⚠ 1-minimal, which is a guarantee rather than a hope

When the reducer returns, removing any ONE remaining line breaks the
predicate — checked by doing exactly that to every surviving line.  It
is **not** the smallest fixture that could exist: a pair of lines that
only matter together survives.  `ddmin`'s subset search buys more and
costs a run per subset rather than a run per line.

⚠ **The multi-pass loop is not exercised by any fixture in the suite** —
cutting `reduce_keys` to a single pass leaves the file green.  Recorded
rather than removed, because the gate is the 1-minimality test, which
checks the OUTPUT and does not care how many passes produced it.

### ⚠⚠ And it answers S3

S3 measured a legal crop silently stopping every wave and could not
tell, because a crop knows only geometry.  Reduce the same too-tight
crop against the property it is meant to preserve and the reduction is
**refused** — the property no longer holds.  That is the phase's closing
argument: geometry proposes, and a predicate disposes.

### ⚠ Two fixture lessons, both caught by the gate rather than by reading

- **The towers shot the robot before it reached the wall.**  Three
  towers at (3, 0) are well within range 15, so the property never held
  over the fixture and `reduce_fault` refused it.  A fixture must
  exhibit its property before anything can be reduced against it.
- **The HP band was guessed and the wall had already broken.**  (6, 0)
  is a lone wall hex, so plan 12 B3 makes it a **15 HP stub** rather
  than 100, and one robot chews it to nothing by tick ~25.  Asking about
  it at tick 40 made `hp` refuse the hex — "at 0 HP" and "nothing here
  to break" are different states.  Measured: 12.3 / 9.0 / 5.67 / 2.3 at
  ticks 5 / 10 / 15 / 20.

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
   ⚠ **ANSWERED — yes, and it needed no machinery.**  A predicate is
   `.keys` text appended to the fixture, and it holds when the whole
   thing runs green.  The measurement vocabulary is the predicate
   language.  See § S4.

## See also

- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  `.keys` runner and the seam this plan writes for, and the source of
  § The instrument comes first.
- [`plans/15-the-carry-model`](../15-the-carry-model/README.md) § C1 —
  the reachable-but-unauthorable state that makes this plan `S`.
- [`plans/17-tower-hot-swap`](../17-tower-hot-swap/README.md) § T3 —
  where the idea came from, and the measurement that shows it will not
  speed the suite up.
