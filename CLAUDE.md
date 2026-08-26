<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Claude Code Instructions for the dryopea Project

## What dryopea is

**dryopea** is a sci-fi free-build / tower-defence game built on
[loft](https://github.com/jjstwerff/loft).  The defining mechanic
is **scramble-and-salvage**: when a base is about to be overrun,
the player fires a rocket out of the core building and evacuates
key components — each carried-out component disables the tower
it came from, so grabbing salvage *hastens* the overrun.
Evacuated components give an advantage at the next base.  A run
is a sequence of bases, chained by what you carry out.

⚠ **It is a strategy game built out of tower-defence mechanisms,
and the player cannot lean back** (project owner, 2026-08-13;
`docs/DESIGN.md` § What kind of game this is).  That is a design
TEST, not flavour, and it is worth knowing before adding a
mechanic: *does this put something in the player's hands at a
moment when using it costs them something?*  A tower's budget
decays per shot and only a player standing at it can refill it;
bodies ramp a kill zone shut until someone drives in and clears
them; salvage decays, so it must be collected at the worst
moment.  A mechanic that hands out a permanent advantage for a
one-time placement decision is what this design avoids — and the
few things that DO let the player rest (an idle tower never
decays, a tower that never fired is never retaliated against)
are load-bearing pressure valves rather than slack.

The full design lives in [`docs/DESIGN.md`](docs/DESIGN.md); the
fiction in [`docs/SETTING.md`](docs/SETTING.md); the feature
roadmap in [`plans/ROADMAP.md`](plans/ROADMAP.md).

## Status

**Active implementation.**  ⚠ **Each plan's own `## Status` is the source of
truth** and [`plans/README.md`](plans/README.md) indexes them.

- **What exists today, one line per shipped phase** —
  [`docs/STATUS.md`](docs/STATUS.md).  Read it to find out whether a thing is
  built before designing around it; ~45 rows from the hex editor through the
  terrain mesh.
- **How the toolchain fails, and how to tell that from a real defect** —
  [`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md).

**Where the game is right now:** the simulation is complete enough to play a
seven-wave base to its end, and **the game is PLAYABLE AND VISIBLE** — `make
play`, press **P**, and the map editor becomes the game: the ground as
triangles, every entity as a part-tree, through an eased camera that follows
the vehicle, with the WALLET in the corner, ramping amber to red as it drains.  Press P again and the editor
comes back.  ⚠ The HUD is one number and `docs/DESIGN.md` § HUD says it should
be — no wave counter, no health bar, no minimap; everything else is diegetic.
⚠⚠ **The player still cannot BUILD** ([`plans/ROADMAP.md`](plans/ROADMAP.md)
§ The critical path, item 3 — the biggest missing mechanic, and it gates three
finished designs).

### The three gates, and their numbers

| gate | command | today |
|---|---|---|
| tests | `scripts/test.sh` | **1407 green**, ~180 s, 101 files |
| scenarios | `scripts/validate.sh` | **33 scripts, 654 measurements**, ~14 s |
| drawn pixels | `scripts/validate_gl.sh` | **3 fixtures, 55 measurements** (needs xvfb) |

⚠ `scripts/test.sh` is the canonical runner — **never `loft test` directly**
(§ Key commands says what it does that you would otherwise skip).

⚠⚠ **`loft test` HARD-KILLS AT 300 s BY DEFAULT and the suite is close
enough that a busy box kills the run** — the message names a PARSE phase in
an unrelated file and reads exactly like the cdylib fault.
**`LOFT_TIMEOUT=1500 scripts/test.sh` is the way through it.**  ⚠ It is also
a real budget constraint on new tests: one phase's first version cost 63 s
alone and pushed the run over the cliff.

⚠ **Do not run two `scripts/test.sh` at once** — both pre-clean
`tests/actual/`, so they clobber each other and fail for no reason.

⚠ **Both gates run INTERPRETED, and that is not a preference** — on the
native backend `load_palette` answers 0 entries
([`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)), which no test could see
because `loft test` runs the interpreter only.

⚠⚠ **Before believing any timing figure here, look at what else is running.**
Three sessions have now measured the suite against a busy box and two
declined to rewrite the number; both were right to.
[`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § The wall clock is not yours alone.

⚠⚠ **The `graphics` cdylib can take out BOTH gates, and it is a toolchain
fault every time** — *"native function not loaded"*, a 300 s hard-kill in an
unrelated PARSE phase, a `SIGABRT` at the end of a green run, or
`rust-lld: unable to find library -lloft_graphics_native`.  Not reproducing
as of 2026-08-17.  ⚠ Two tidy explanations have been FALSIFIED, so do not
re-derive them: [`docs/TOOLCHAIN.md`](docs/TOOLCHAIN.md) § The `graphics`
cdylib fault has the symptoms and the hand rebuild that works.


## Hard-won rules — [`docs/HARD_WON_RULES.md`](docs/HARD_WON_RULES.md)

**Every rule there cost a real defect to learn, and most of them describe a
test that CANNOT see the thing it appears to test.**  ⚠ The headlines are
below so the warning fires without a lookup; **the evidence, the numbers and
the dates are in the file** — go and read the section before changing the
thing it names.

### Movement + passability — [detail](docs/HARD_WON_RULES.md#movement--passability)

- **How an enemy moves today**: `wave_tick` rebuilds the distance field ONCE
  per tick, one field per climb limit; `enemy_tick` steps down it.  Two
  steering modes hand off at the **scrambler bubble** (a straight-line 25
  hexes, never a route length); an enemy with no route follows the DESIRE
  field and besieges what it cannot climb.
- ⚠ **A robot climbs 2.0 m** (`CLIMB_REGULAR`), and the number is DERIVED:
  a single-hex body ramp onto a structure `H` high needs `H / 2`, so 2.0 is
  the interior of four constraints — `src/passable.loft` § Why a robot climbs
  2.0 m.  It was 0.0 until plan 12 B1.
- ⚠ **Rubble is a LAYER, never a repaint** (`src/height.loft`) — a pile makes
  the SURFACE `rubble` while the authored ground underneath is untouched, so
  clearing restores exactly what was authored.  That is what dissolves the sea
  trap: a breach that ERASED its hex would be *less* passable than the wall it
  replaced, while "the wall broke" asserted true.
- ⚠ **The SURFACE is not the painted kind** — a hex under a pile stands on
  `rubble`.  `painted_ground` answers the HEIGHT, `hex_ground` the SURFACE;
  swap them and piling debris onto a wall LOWERS it.
- ⚠ **Passability is TWO questions**: the field filters NODES by `can_stand`
  and EDGES by `can_step`.  Filtering nodes by `can_occupy` is **vacuous** —
  it compiles, reads well, and deletes the height rule with no test moving.
- ⚠ **A drop is free and a climb is not**, which is why `flow_build` asks
  `can_step(n, a)`: the sweep runs outward and the enemy walks inward.
- ⚠ **You attack what you could STAND on and cannot climb** — an enemy at the
  water's edge besieges nothing.
- ⚠ **ONE AI, per-class DATA** — a design rule, not an accident.  A class that
  needs its own mover has broken it; the four small robots cost one row each.
- ⚠ **Blocked by a COMPANION → step beside; blocked by the GROUND → stand and
  attack** (F7b).  The condition is the whole rule, and it was missing for
  three phases and was the whole BALANCE (161/311/180 → 61/104/95).
- ⚠⚠ **THE SIEGE FRONT IS THE WALL'S WIDTH** (`@M020`) — 4 hexes on a
  five-row wall, 6 on a seven-row one.  A wave is worth its front class PLUS
  what the front cannot COVER; **the screen is arithmetic, bodies against
  face width**.  ⚠ It was THREE for any length until plan 24, and five
  documents named the wrong fix.
- ⚠ **The siege chews where the ROUTE meets the wall**, never where it is
  weakest — and plan 24 did NOT change that, which is the surprise.

### Cost — [detail](docs/HARD_WON_RULES.md#cost)

- **The tick's budget is ~667 ms**, derived from the design's own numbers.
- ⚠ **Do not reach for a standalone stopwatch** — an unchanged probe answers
  173 / 737 / 754 ms on three runs, because discarded structs are not freed.
  `tests/11_f8_the_tick_budget.loft` is the number of record.
- ⚠ **NEVER bind a `FlowField` to a local in a per-enemy path** — a
  whole-value bind COPIES the heap value: 2250x, unseen since F5.
- ⚠ **A copy changes no behaviour, only cost**, so 490 green tests sat over a
  tick 25% past budget for four phases.  The gate is a RATIO, not a stopwatch.
- ⚠ **The incremental field rebuild is deliberately NOT built** — and its
  third trigger (**the TICK getting shorter**) is now ARMED, which is why
  [`plans/22`](plans/22-the-field-cache/README.md) is the prerequisite for a
  shorter tick rather than a follow-up to it.

### Testing something that moves — [detail](docs/HARD_WON_RULES.md#testing-something-that-moves)

- ⚠ **A 1-hex corridor cannot tell a flow field from a fixed heading** — every
  enemy test dryopea had was blind to the field when it landed.
- ⚠ **A corridor cannot see F5c either**: on a hex AXIS the field offers ONE
  closer neighbour, off it TWO — so a blocked enemy has no *beside*.
- ⚠ **A wave spawns STACKED** — `range` over a walking wave is a SPAN.
- ⚠ **Route every step through `lat_neighbour`** — a `+ 1` on a `q` anywhere
  else is the bug, and it is how moros#10 sheared every reach computation.
- ⚠ **A walking test must paint the ground it walks on** — an unpainted hex
  IS sea.
- ⚠ **A world where every source hex is at 0 m cannot tell a RISE from a
  destination height** — the whole height rule can change, suite green.
- ⚠ **"N enemies attack N hexes" does NOT gate the desire field** — measured;
  their spawn headings already spread them.
- ⚠ **A MIRRORED base is not a symmetric one** — 112 vs 211 ticks on a map
  that looks mirror-symmetric, and none of it was the crew's.  `q -> -q` is
  not a symmetry of this lattice; control for BRACING first.
- ⚠⚠ **A gate that reads PERFECT is as suspect as one that reads wrong** —
  0.0 rad of disagreement, twice, for two unrelated reasons.  **The missing
  control is generic and costs two lines: can this gate produce a non-trivial
  reading at all?**
- ⚠⚠ **AND THE VACUITY CAN BE IN THE NUMBERS RATHER THAN IN THE ASSERTION**
  (`@M034`) — *a 1 Hz clock driven by a 30 Hz clock* cannot disagree for ANY
  implementation, because 1 Hz is exactly thirty 30 Hz steps.  **So the second
  form of the control is: are the numbers I chose capable of disagreeing?**
  ⚠ And a **negative control the plausible WRONG version also satisfies is not
  one** — a DEFERRING cap answers fewer ticks too (4 vs 24), so the control has
  to be that version reproduced, not the null one.
- ⚠⚠ **SEVERAL COUNTS IN ONE TEST FUNCTION ARE RANKED, NOT INDEPENDENT** —
  loft abandons at the first failed assertion, so three of four counts are
  unmaintained decoration.  ⚠ Where the rows are ONE claim about one subject,
  the fix is not four functions but one assertion whose MESSAGE carries every
  reading (`tests/26_l0`).
- ⚠⚠ **A GATE AIMED AT THE MECHANISM YOU EXPECT TO BE THE HAZARD IS NOT ONE
  AIMED AT THE HAZARD** (`@M025`) — and its own control is what said so.
  *The right code with the wrong justification* is what to look for when a
  gate refuses to fail.
- ⚠⚠ **A COST GATE CAN BE A COUNT INSTEAD OF A CLOCK, AND USUALLY SHOULD BE**
  (`@M029`) — two identical calls differed **5.4x**.  Ask what the change
  would actually DO before reaching for a stopwatch.
- ⚠⚠ **A COUNT IS PERMUTATION-INVARIANT, SO IT CANNOT SEE A MIRRORED WORLD**
  (`@M027`) — every band green at 490.8 px of error.  **Any gate that counts
  pixels needs one assertion about WHERE.**
- ⚠⚠ **AN EMPTY ARTEFACT SATISFIES EVERY EQUALITY** — `mesh_crc` of an empty
  mesh is 0, so every equality carries a non-zero floor.
- ⚠⚠ **A SMOOTHER MEASURED ON THE THING IT SMOOTHS CANNOT SEE WHAT IT FAILS
  TO SMOOTH** (`@M035`) — `@M023` proved the camera's ease by reading
  `cam.target`, and every number in it is true.  In PIXELS the eased camera
  leaves **96 px** of jump on the vehicle where the un-eased one leaves **0**:
  **the ease moves the jump off the world and onto the mover**, and the one
  thing a follow camera cannot smooth is what it is chasing.  ⚠ Point the gate
  at the ARTEFACT, not at the mechanism.  ⚠ And a fix applied to half a
  composition leaves a FRACTION behind — alpha under a hex-following camera
  takes 96 px to **14**, not to 0.
- ⚠ **A gate whose reading is already saturated cannot see what you built** —
  price the SUPPLY against the CAPACITY before believing a flat reading.
- ⚠ **A cost gate over a world with none of the thing you changed is not a
  gate** — and even a healthy one cannot see a 20x regression in a 3% share;
  price the ALTERNATIVE and compare.

### Profiling the suite — and why the wall clock cannot do it — [detail](docs/HARD_WON_RULES.md#profiling-the-suite--and-why-the-wall-clock-cannot-do-it)

`LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1`.  Method, numbers of
record and dates: [`docs/PROFILING.md`](docs/PROFILING.md).

- ⚠ **The report goes to STDERR** — a plain `> out.txt` silently drops it.
- ⚠ **Read the SAMPLE COUNT, never the seconds** — the wall clock has pointed
  the wrong way twice on real improvements.
- ⚠⚠ **A profile AGES, and the stale one gets quoted** — *"58% is `canvas()`"*
  was cited three plans after it stopped being true.  Re-profile, quote the
  date.  Three readings now agree: the field family is **~69%**.
- ⚠ **A test that RE-DERIVES an expensive value a sibling already computed is
  the cheapest thing to find** — −10.1% of the suite, assert counts identical.
  ⚠ And the refactor that LOOKS identical measured as **free**.

### Timers and epsilons — [detail](docs/HARD_WON_RULES.md#timers-and-epsilons)

- ⚠⚠ **A COUNT asked for in SECONDS comes back SHORT** — `n * TICK_SECONDS`
  through an accumulator answers `n - 1` for **602 of the first 1000 `n`**,
  and neither exactness nor an epsilon saves it.  That is why `play_ticks`
  and `play_advance` are two verbs.
- ⚠ **A banked timer's DIRECTION is half the epsilon rule** — counting UP and
  counting DOWN do not accumulate the same way, and **neither is safe**.
- ⚠ **An epsilon whose removal leaves the suite green is a guard that cannot
  fire** — exercise the branch directly.
- ⚠⚠ **And the branch test is NECESSARY, not SUFFICIENT: the third member is
  a guard invisible at the value you shipped** — at 1.5 hex/s the carry is
  0.0 for ever (`@M014`), so **sweep the NEIGHBOURS of the value you
  shipped** (`@M013`).
- ⚠⚠ **And then SHIP a value that can see it** — 2.5 hex/s was chosen partly
  on testability (`@X063`, `@M017`).
- ⚠⚠ **THE FOURTH MEMBER IS THE SITE THAT NEVER GOT A GUARD AT ALL**
  (`@M030`, `@D003`) — `vehicle_hexes_this_tick` TRUNCATES with no carry, so
  the player reads **180 / 120 / 180 / 0 / 0 / 0 / 0** hexes a minute against
  a true 180 and **stops moving entirely under a 250 ms tick**, while every
  banked mover is exact at all seven.  ⚠ Three accidents hid it, and the
  third is a new shape: the ONE tick-length gate in the repo banks an ENEMY.
  **So the instrument is a CROSS-PRODUCT — sweep the tick length AND every
  mover.**
- ⚠⚠ **AND THE SIXTH IS THE FAMILY THE RULE WAS NAMED AFTER, WHERE THE
  GUARDS WERE THE HEALTHY SITES** (`@M033`, `@D004`) — swept at seven tick
  lengths, every ONE-SHOT TIMER WITH an epsilon is exact at all seven and
  the two WITHOUT one run a tick long (the lull **76 / 151 / 451** against
  75 / 150 / 450, the pre-walk window **16 / 51 / 101 / 151** against 15 /
  50 / 100 / 150).  ⚠ Two of the three healthy ones count DOWN exactly as
  the broken pair does, **so the direction is not the discriminator — a
  guard is**.
- ⚠⚠ **AND THE MOST REUSABLE FINDING IS A BLIND GATE**: `plans/26` asked
  for *"UP and DOWN fire on the same tick"*, and over six exact-multiple
  durations the two float directions disagree at **ONE** of six and
  **agree while both being a tick long at FOUR**.  ***Two agreeing
  instruments are not a control; the TRUE count is.***
- ⚠⚠ **AND THEN THE ONE-SHOTS COLLAPSED TOO** (plan 26 L3, `@X082`) —
  `src/tick_timer.loft` is `{spent, total}` in integer base units and
  `timer_left` is `total − spent`, so **there are no longer two directions
  to be unsafe in**; all three epsilons are **deleted**.  ⚠ A `Timer` MAY
  hold its `total` where a `Bank` may not hold its `whole` — same
  [loft#914] rule, opposite conclusion.  ⚠ The `Timer`-as-`Bank`
  refutation was RUN and the boundary held.  ⚠⚠ **And the census was
  SEVEN and there are EIGHT** — the tower's CHARGE is a hand-rolled
  `bank_gain`, still float, pinned rather than converted.
- ⚠⚠ **AND THEN ALL SEVEN COLLAPSED INTO ONE** (plan 26 L2, `@X080`) —
  `src/tick_bank.loft` is the only *do-not-lose-a-fraction* left for a
  mover, both `*_PROGRESS_EPSILON`s are **deleted** rather than zeroed, and
  `@D003` is closed: the player reads **180 at all seven tick lengths**.
  ⚠ A `Bank` holds the CARRY and nothing else — the rate arrives per call
  (`@X061`) and `whole` is a PARAMETER, because a nested struct's silent
  zero-default ([loft#914]) would freeze every mover built from a partial
  literal.  ⚠⚠ **The reciprocal form was refused on arithmetic**: *units
  per hex* needs no scale and would make `Bank` into `TickClock`, but
  `3 000 000 / 2.25` is not an integer and `@M013` sweeps 2.25 hex/s.
  ⚠ **The one-shot TIMERS went the same way one phase later** — see the
  sixth member above.
- ⚠⚠ **AND THE FIFTH IS THE ACCUMULATOR ITSELF, NOW GONE** (plan 26 L1) —
  simulation time is an integer count of a chosen step, so `advance(n × step)
  == step(n)` for all of 1..100000.  ⚠ **The step's BASE UNIT is the decision
  and µs is the wrong one**: 2/3 of a second is not a whole number of
  microseconds, and the 666 667 the plan recommended moves **17 tests** while
  the 654 gate measurements cannot see it (`@M031`, `@X079`).  *A measurement's
  resolution is not its authority.*

## Relationship to loft

loft is the language + runtime; dryopea is a consumer project.
Dryopea is also the **second partner** for loft's universal
hex-world editor (loft `lib_plan 24`) — moros is the first;
dryopea drives the bug-hunt phase that hardens the shared
libraries.

When dryopea surfaces a need from loft — a language feature, a
stdlib gap, a runtime bug — **file it as a GitHub issue on
`loft-lang/loft`** (`gh issue create --repo loft-lang/loft`;
`jjstwerff/loft` redirects there).  A write-up that stays in this
repo is not filed: `QUESTIONS_FOR_LOFT.md` is dryopea's outbound
queue, not loft's inbox.

The flow, in order:

1. Cut the minimal reproducer into
   [`loft_repros/`](loft_repros/README.md) and check it fails
   standalone on the backends you claim.
2. `gh issue create` with the repro **inline** in the body —
   dryopea is a separate repo, so a link into `loft_repros/` is
   not self-contained.  Label it: `bug` / `enhancement`, plus
   `sev:*`, `area:*`, `wa:*` and `hit-by:dryopea`
   (`gh label list --repo loft-lang/loft` for the set).  Search
   open AND closed issues for the shape first.
3. Record it in [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)
   under **Submitted** with the issue link, per that file's own
   Open → Submitted → Resolved convention.

Do **not** fix it locally by patching loft from this repo; loft
has its own contribution flow.  Internal-to-dryopea bugs go in
[`PROBLEMS.md`](PROBLEMS.md) with `@D<NNN>` IDs.

## Key commands

dryopea uses the **installed** `loft` binary — **`loft` on PATH, which is
`~/.local/bin/loft`**.  ⚠⚠ This said `/usr/local/bin/loft` until 2026-08-18 and
that path is a DIFFERENT, OLDER binary on this box: both report
`loft 2026.8.0`, so **the version string cannot tell them apart and only the
mtime can**.  ⚠ It matters — plan 20 A2 spent a session blocked on a heap
corruption that the PATH binary had while `/usr/local/bin`'s predecessor also
had it, and a rebuild during the session fixed it silently
([loft#969](https://github.com/loft-lang/loft/issues/969)).  **Check
`which loft` and its mtime before believing any toolchain symptom.**  There is no local loft build step: the
libraries it depends on resolve from the loft package registry
via `loft.toml` + `loft.lock`, so no `--lib` path is passed
anywhere.

```bash
# Run dryopea's test suite (canonical entry — DO NOT run `loft test` directly)
scripts/test.sh

# Play every tests/scripts/*.keys and gate on what they measure —
# the SECOND gate (plan 08 V4).  Prints each measurement beside its
# band, writes a PNG per `snap` into shots/, exits non-zero on a
# reading out of band.  `make validate` is the same thing.
scripts/validate.sh                  # all of them (~11 s)
scripts/validate.sh paint-a-base     # just one, while iterating

# Draw every tests/gl/*.keys through REAL GL and classify the frame —
# the THIRD gate (plan 25 M3).  Needs xvfb; `validate.sh` deliberately
# does not, so a machine without it still runs the 654.  Captures land
# in shots/gl-*.png.  `make validate-gl` is the same thing.
scripts/validate_gl.sh               # both fixtures
scripts/validate_gl.sh the-ground    # just one

# Run the game / editor (opens a 960x720 GL window; P toggles play).
# Use `make play` — it passes --interpret, and the NATIVE backend is
# broken for dryopea today: it panics on the marker load, and where it
# does not panic it silently loads an EMPTY palette (both filed in
# QUESTIONS_FOR_LOFT.md).  `loft src/main.loft` is `make play-native`,
# kept for testing the eventual fix.
make play
make play MAP=starter_01

# Parse-check a single .loft file without running it.
# ⚠⚠ NOT the aggregator — `loft --native-emit … src/dryopea.loft`
# PANICS today (loft#962: a const initialised from a sibling module's
# const), naming `spawn.loft::per_tick`, which is not the cause.  Every
# real entry and all 91 test files compile clean; check one of those.
loft --native-emit /tmp/check.rs src/<file>.loft
loft --native-emit /tmp/check.rs src/main.loft      # the entry check

# Inspect a dependency's public API (never guess a signature)
loft api                 # every reachable library + its path
loft api graphics        # one library's full public surface
```

`scripts/test.sh` is the canonical test runner.  It:
- Creates `tests/actual/` — it is gitignored, so a fresh
  checkout does not have it, and neither `save_png` nor the file
  writer creates parent directories.  Without it every write
  silently goes nowhere and the golden tests fail as a
  "mismatch" against a file that was never written.
- Pre-cleans `tests/actual/*.png` and `tests/actual/*.json`
  between runs so stale artefacts can't masquerade as current.
  **Running `loft test` directly skips this** and leaks a save
  file into the next run's cold-start assertions.
- Invokes `loft test` against the dryopea `tests/` directory,
  with warnings VISIBLE (the suite is kept warning-clean).
- Exit code 0 = all green; non-zero = failures (the loft test
  runner surfaces assertion failures as FAIL since `@P367`
  shipped on the loft side).

### Relative paths resolve against the PROGRAM's directory

A relative path in a `.loft` file resolves against
`source_dir()` — the directory of the program entry, not the
process cwd, and not the directory of the file containing the
`file()` call.  Under `loft test` the entry is the test file, so
`source_dir()` is `tests/`.

dryopea's paths (`examples/palette.json`, `tests/golden/…`,
`maps/…`) are all repo-root-relative, so every entry point
declares the **`#cwd`** directive at the top of the file, before
the first declaration.  That restores cwd-relative resolution,
and both `scripts/test.sh` and the `Makefile` run from the repo
root.  A new test file needs `#cwd` or its palette load and
golden compare will silently miss.

⚠ **`#cwd` is legal only in a program ENTRY.**  A file carrying it
cannot be `use`d as a library — the import fails to parse with
`Syntax error: unexpected '#' at <file>:1:2`, and the aggregator
goes red naming the importer rather than the directive.  So an
entry point cannot also be an aggregator member, which means it is
compiled by nothing and every entry must stay a shell with no
decisions in it: `main.loft` over `editor_step.loft`,
`validate_main.loft` over `validate.loft`.

## Architecture — src/ layout

⚠ **The full listing — what each file owns, and the trap in it — is
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)**, together with the
key data structures.  Read it before editing any file below; each
`.loft` file's own header is the source of truth and the listing is a
navigational summary of it.

| File | Owns |
|---|---|
| `dryopea.loft` | the library aggregator — `use dryopea;` brings every submodule into scope |
| `main.loft` | the interactive entry — the GL shell (NOT in the aggregator; parse-check it by hand).  ⚠ Since plan 19 P3 it runs the GAME and owns the CLOCK: it measures the frame and hands the seconds over, and decides nothing else |
| `editor_step.loft` | **the editor's input seam** — `EditorState` + `EditorInput` + `editor_step`.  Every editor action runs through it |
| `play.loft` | **the game's seam** (plan 19 P1) — `PlayState` + `play_ticks` / `play_advance` / `play_step`, and the ONE call to `wave_tick`.  ⚠ Also the MODE (P3): `play_mode` / `play_set_mode` / `play_begin` / `play_frame_seconds`, and since plan 21 R2 the game's CAMERA, stepped LAST and on every frame  ⚠ Since plan 26 L1 the bank is a `TickClock` and the doors come in pairs — `play_advance_units` / `play_step_units` are the exact ones, the float ones round at the boundary.  ⚠ And since L5 `play_alpha` reads where the frame falls between two ticks; nothing draws with it yet, and `@M035` says what it will cost when something does |
| **`fixstep`** (library) | ⚠⚠ **NOT in `src/` since plan 26 L6 — it is a PACKAGE**, in `loft-libs-game` beside `input`, because *"a fixed timestep is needed by every game that is built"* (project owner, 2026-08-17) and that chunk is the ecosystem's *runtime game services* layer.  It owns `TickClock` (`clock_advance` / `clock_step` / `clock_alpha` / the L4 policies), `Bank` (`bank_gain`) and `Timer` (`timer_arm` / `timer_spend`), plus the `approach` ease.  ⚠ `loft api fixstep` is the surface and its own README is the guide — **do not re-document it here**, a second copy is the one that drifts.  ⚠⚠ The base unit is the CONSUMER's: dryopea's `TICK_STEP_UNITS` (2 000 000 thirds of a µs) lives in `spawn.loft`, and `@X079` is why it is not µs.  ⚠ Every dryopea file that needs a clock, a bank or a timer says `use fixstep;` — never a submodule name |
| `play_view.loft` | **what a live session LOOKS LIKE** (plan 19 P6) — the composition `main.loft` calls in play mode, `render_editor_frame`'s sibling one mode over.  ⚠⚠ **The renderer works out for itself what the terrain did** (`@X095`): a live session moves the ground three ways that never go near `paint`, so `MeshWatch` keeps a SNAPSHOT of the height layer and diffs it — no dirty list on a simulation struct to leak into `state_diff` or grow for ever in 1397 headless tests.  ⚠ Exact only because **every terrain change a TICK can make moves the HEIGHT LAYER**, which `tests/19_p6` asserts against a played base rather than quoting.  ⚠⚠ **`play_view_draw` leaves the GL state as it found it**, and that is load-bearing: without the two `gl_disable` lines the editor's frame after a play frame is **691 200 black pixels of 691 200** (`@M041`).  ⚠ The roster uploads on a TICK boundary (10 ms) and the frame draws every time (5 ms), because the camera eases whether or not anything moved.  ⚠ No HUD, no interpolation, no editor in 3-D |
| `bindings.loft` | **the ONE key table** — keys → actions → `EditorInput`.  Never add a `gl_key_pressed`.  ⚠ Since plan 19 P2 it carries the PLAY actions too, and `editor_input_from`'s `playing` argument decides whether WASD pans or drives.  ⚠ And since P3 one SHELL action (`toggle_play`, P), filled in BOTH branches — fill it in one and there is no way out of play mode |
| `script.loft` | the `.keys` script runner and its whole vocabulary — commands name ACTIONS, never keys |
| `validate.loft` / `validate_main.loft` | the second gate: sweep `tests/scripts/`, sum the measurements, report the FIRST failure |
| `editor_view.loft` | `render_editor_frame` — what the player sees, composed ONCE for both the GL loop and `snap` |
| `measure.loft` | frame measurement — `classify_canvas` / `classify_world` → `FrameCounts` |
| `golden.loft` | `assert_golden` — write `tests/actual/`, compare bytes to `tests/golden/` |
| `compare.loft` | `state_diff` — are two runs in the same state? (plan 18 S0) |
| `emit.loft` | write a situation down as `.keys`, plus `crop_keys` (plan 18 S2/S3) |
| `reduce.loft` | cut a fixture to what a behaviour needs, against a predicate (plan 18 S4) |
| `lattice.loft` | **THE lattice** — pointy-top odd-r offset, `Hex`, and every `lat_*` verb.  Delegates to `hex_grid` |
| `relabel.loft` / `convert.loft` | plan 09's old-label → new-label bijection, and the `.keys` converter |
| `camera.loft` | `EditorCamera` + `camera_update`.  ⚠ pan NORTH is `r += 1` |
| `render_camera.loft` | **the GAME's camera** (plan 21 R1) — `RenderCamera`, the two presets, and `lat_to_world`; and since R2 the EASE — `CameraRig`, `camera_rig_step`, `camera_boom_free`.  ⚠⚠ Its world is `+y` **NORTH** with `+z` up, which is NOT dryopea's `+y`-south canvas frame: that one is left-handed once z points up, and `mat4_look_at` MIRRORS it.  ⚠ Assert on `camera_eye_of_view`, never on the struct.  ⚠⚠ The approach is `1 − e^(−k·dt)`, never `k·dt` |
| `ground_mesh.loft` | **the GROUND, as triangles** (plan 25 M0-M2) — `ground_top_face`, a six-triangle fan per hex in the CAMERA's world; `ground_side_faces`, one vertical quad per faced edge; and `ground_chunk_mesh` / `ground_chunk_kinds`, one TILE for one palette kind.  ⚠⚠ There is no blend and that is measured, not lazy (`@X072`): the corner mean is a no-op at every hex in both directions.  ⚠ HEIGHT off `hex_height`, COLOUR off `hex_surface_index` — two lookups, and swapping them makes debris LOWER a wall.  ⚠ Colour is a UNIFORM, so it emits one mesh per palette kind (`@X074`); putting it on the vertex throws away the exact classification.  ⚠⚠ A side face is emitted ONCE, by the column that STANDS (`if hh <= nh { continue; }`) — and **both halves of that guard fail invisibly**, so they are gated as COUNTS.  ⚠⚠ A tile is walked by COORDINATE, and the reason is COVERAGE rather than determinism (`@M025`) |
| `mesh_chunks.loft` | **the mesher's DOMAIN** (plan 25 M2) — which hexes are drawn, which 32×32 tile each lands in, and which tiles an edit invalidates.  ⚠⚠ The drawn region is the painted set **plus a one-hex ring** (`@X075`), because sea is stored as absence — and its limit (a gap wider than two hexes still holes) is asserted so closing it is deliberate.  ⚠⚠ **TWO reaches share the value 1 and not a constant**: `MESH_HALO_K` is how far an edit REACHES, the ring is how far past the paint anything is DRAWN.  ⚠⚠ No `ChunkField` — `collect_dirty_inputs` skips a dirty chunk owning no cells, and `mark_borders` steps chunk coords rectangularly.  ⚠ `mesh_chunks_touched` is ONE mechanism: the dirty rule read as an edit, the tile list summed over the paint |
| `ground_gl.loft` | **the ground, DRAWN** (plan 25 M3) — one flat-unlit shader, one `graphics::GroupVboSet` per palette kind keyed by chunk, the kind's colour as a UNIFORM.  ⚠⚠ Flat unlit is a GATE requirement (`@X074`): the frame can only contain palette colours, so `classify_canvas`'s exact lookup survives GL — measured at zero drift (`@M026`).  ⚠⚠ It turns `GL_CULL_FACE` **on** itself, because M0's and M1's winding arguments depend on it and a reversed winding draws NOTHING with every other valve healthy.  ⚠ `_upload_chunks` CLEARS a kind a tile no longer holds, or the GPU goes on drawing an erased wall |
| `gl_gate.loft` / `gl_gate_main.loft` | **the THIRD gate** (plan 25 M3) — sweep `tests/gl/*.keys`, draw each through real GL, capture, decode with `imaging`, count with `classify_canvas` ITSELF.  ⚠⚠ A per-kind COUNT cannot see a MIRRORED world (`@X078`), so it also asks WHERE two uniquely-coloured hexes landed vs `camera_screen`.  ⚠ Expectations live HERE, never in the `.keys` file — a fixture with no case is REFUSED by name.  ⚠ Every branch a TEST can reach comes before `gl_create_window` |
| `mesh_crc.loft` | **do two mesh builds agree?** (plan 25 M2) — the geometry folded to one integer, because a count cannot see a mesh with the right vertices in the wrong places and a golden agrees with a shear.  ⚠⚠ It folds the **TRIANGLES** too, where moros's port folds vertices only: a top face SHARES its rim vertices, so M0's reversed fan moves no vertex at all.  ⚠ It ROUNDS where moros's truncates, and that guard was unreachable until a branch test reached it.  ⚠⚠ An **empty mesh folds to 0**, so every equality needs a non-zero floor.  ⚠ It belongs in `mesh3d` |
| `painted.loft` | `PaintedHex` / `PaintedWorld` — sparse, sea-default ground |
| `palette.loft` | `GroundType` + `load_palette` + `GROUND_RUBBLE` |
| `markers.loft` / `marker_file.loft` / `marker_render.loft` | the marker layer, its save format and its drawing.  `place_marker` is the ONE dispatch |
| `map_file.loft` / `save.loft` | the save record (6 fields — see § Known constraints) and the save/load path |
| `render.loft` | the software rasteriser over `graphics::Canvas` |
| `picker.loft` / `hud.loft` / `editor_mode.loft` / `chunks.loft` / `history.loft` | palette UI, HUD, the mode flag, the dirty-chunk set, undo/redo.  ⚠ Since plan 19 P7 `hud.loft` also owns **the ONE number the game shows** — `digit_segments` / `render_digit` / `render_number` / `hud_wallet_points`.  ⚠⚠ **The digits are RECTANGLES and that is forced** (`@X097`): `graphics::draw_text` rasterises through `#native` and needs a font file, so a text HUD is one no test and no `snap` could see.  ⚠ A wrong row in the segment table draws a good-looking digit that reads the wrong number, so it is gated by an INDEPENDENT ORACLE — the lit-segment counts 6 2 5 5 4 5 6 3 7 6.  ⚠⚠ Since `@X098` it also owns the wallet's amber-to-red RAMP — a function of the INTEGER the digits show, which is what makes its reachable colours finite (201) and lets a test sweep them all |
| `spawn.loft` | **the tick** — `WaveState`, `wave_tick`, enemy movement, targeting, deaths, the schedule, `TICK_SECONDS`, and since plan 23 K2a the banked `enemy_bank` / `enemy_step` pair the mover is built on.  ⚠ Since plan 26 L2 `enemy_bank` takes INTEGER base units and its epsilon is gone |
| `waves.loft` | the authored wave list, its lull, and what a wave is MADE OF — `WavePart` / `wave_schedule_compose`.  ⚠ A wave's size is SUMMED from its parts, never stored |
| `flow.loft` | the distance field — `flow_build` / `flow_step` / `flow_steps` / `flow_desire` |
| `passable.loft` | may a class MOVE here? — `can_stand` / `can_step` / `can_occupy`, and `hex_height`.  ⚠ Since plan 21 R2 also the SIGHT line: `sight_first_block`, the ONE walker, shared by `tower_sees` and the camera's boom |
| `occupancy.loft` | who is standing where this tick — enemy counts, and the separate `BlockerMap` |
| `height.loft` | the RUBBLE layer — metres piled at runtime, and what they are made of |
| `damage.loft` | what a structure has TAKEN, bracing, and `break_structure` |
| `tower.loft` | towers — range, the banked charge, the 30-shot magazine, LOS, repair, the detachable top |
| `wallet.loft` | the run's budget and the ONLY end state (`wallet_broke`) |
| `vehicle.loft` | the PLAYER — drive, boost, salvage.  `salvage_at` is the shared chassis.  ⚠ Since plan 26 L2 it carries a `Bank`, which is what closed `@D003`; `vehicle_bank` releases and `vehicle_hexes_per_tick` is the CEILING that spends nothing (`@X081`) |
| `helper.loft` | the NPC crew — banked movement, wrecking, and the 60 s recovery |
| `carry.loft` | one record per carryable thing, with an `owner` — conservation is STRUCTURAL |
| `part.loft` | **what an entity IS** (plan 20 A1) — the `Socket` a part offers and the `Binding` that fills it, over `hex_body::Rig`.  ⚠⚠ It holds **no rig, joint, limit, pose or hitbox**: every one of those is the published library, and `part_fault` asks `rig_admissible` first.  ⚠ **A socket's position is COMPUTED and stored nowhere** — `socket_world` is the ONE site, and a `Binding` carries neither a coordinate nor a POSE (which pose a tower draws in is `TowerState`'s answer).  ⚠ The cycle check walks a PATH, never a visited set || `catalogue.loft` | **what each entity is MADE of** (plan 20 A3) — the hover unit, the robot, the tower base + top, as `Limb` tables over a rig.  ⚠⚠ **It declares no footprint**: `part_size` DERIVES the extent from the limbs, which is the only reason § D6's check against `VEHICLE_WIDTH_M` is not a tautology.  ⚠ The helper is the hover unit and never a fifth entry (§ D8); a class is a row of DATA, so no colour lives here.  ⚠ § D7's four BOOMS are absent — they run diagonally and nothing carries a rest orientation, which is A2's to solve || `part_mesh.loft` | **a part, as TRIANGLES** (plan 20 A2) — a box is 12 triangles with per-face corners, a disc and a cone are `4 x PART_ROUND_SEGMENTS`.  ⚠⚠ **It contains no forward kinematics**: every vertex is placed by `hex_body::frame_point` over one `rig_world_frame3` per BONE, held across that bone's limbs.  ⚠ A NORMAL is a direction, not a point — posing one as a point still normalises to something plausible and is wrong everywhere the bone is not at the origin.  ⚠ No colour (that is a UNIFORM, `@X074`) and no world placement (that is A5) |
| `pose.loft` | **the pose comes from the SIMULATION** (plan 20 A4) — `pose_vehicle` / `pose_hover_unit` / `emit_tower`, and the rotor and canopy numbers.  ⚠⚠ **Three claims, three owners, and none of them is new state**: the tower's top is `tower_has_top`, the canopy is `cargo_carrying` (open exactly while laden, `@X090`), the rotors' rate is `vehicle_speed`'s own ratio (`@X091`).  A field on `Vehicle` saying *canopy open* is the defect § D3 is written against.  ⚠ The sim answers a TARGET and the SWING is presentation, which is why the canopy is a float.  ⚠⚠ `emit_tower` indexes `ps.ps_parts[...]` rather than calling `partset_get`, and that is a loft WORKAROUND ([loft#974]) rather than a style |
| `entity_view.loft` | **the ROSTER, as triangles** (plan 20 A5) — which entities exist, where they stand, which way they face and what colour each is, walked straight off a `WaveState` and a `MarkerWorld`.  ⚠⚠ **Nothing here is STATE**: no drawable list, no per-entity render record, no spawn hook — which is what makes *"a thing not drawn reads as ZERO"* an assertion that can fail.  ⚠⚠ **A drawn class IS the simulation's mover kind** (`@X093`) — `entity_colour` is indexed by `passable.loft`'s discriminants, unmapped, with crew / tower base / tower top appended after the last one, so a new robot class costs a colour and there is no second enumeration to drift.  ⚠⚠ **The colours are OUTSIDE the palette on purpose** (`@X092`): an entity in a palette colour would be counted as GROUND.  ⚠ A hover unit floats at `vehicle_climb` (`@X094`), so boost is visible for nothing.  ⚠ ONE emitter for the player and the crew — § D8's *"same chassis"* made structural |
| `entity_gl.loft` | **the entities, DRAWN** (plan 20 A5) — one `graphics::GroupVboSet` per drawn class, re-upserted WHOLE every frame over `ground_gl.loft`'s own shader SOURCE.  ⚠ **Every class, every frame, and an absent class is upserted EMPTY** — the roster moves on every tick so there is no clean dirty subset, and a skipped class leaves the GPU drawing last frame's robots after they died.  ⚠⚠ It turns `GL_CULL_FACE` on, and here that is load-bearing rather than defensive: `@D005` found half of every box in the catalogue wound INWARDS, invisible to counts, vertices, normals and `mesh_crc` alike |

## Important conventions

### Hex convention

**Pointy-top, odd-r offset** — `hex_grid`'s convention, which every
`hex_*` library and moros already speak.  `src/lattice.loft` is the
layer and it DELEGATES to `hex_grid`, so dryopea cannot drift from the
ecosystem: there is no second implementation to drift with.  Plan 09
converted everything and **C6 deleted the axial layer entirely** —
`src/world.loft` is gone, and `grep` finding `hex_offset`,
`cube_round_axial`, `hex_to_world`, `world_to_hex` or `visible_hexes`
anywhere means someone resurrected it.

`q` is a COLUMN and `r` is a ROW.  Odd rows sit half a hex EAST of
even ones, so a neighbour's delta depends on `r & 1` — which is why
⚠ **nothing may step a coordinate except `lat_neighbour`**, and why
there is deliberately no constant `(dq, dr)` table to reach for.

HEX_DIAMETER = 1.5 m vertex-to-vertex; one `hex_grid` unit is one
dryopea circumradius (0.75 m), so centre-to-centre is
HEX_FLAT_TO_FLAT = 1.299038.

World +y grows **south** (same direction as canvas +y); there is no
y-flip in the render path.  ⚠ `hex_grid`'s frame has +y NORTH, so
`lat_to_metres` / `lat_from_metres` / `lat_corner_*` NEGATE y — that
is what makes its compass true on dryopea's screen (direction 0 = E,
1 = SE, 2 = SW, 3 = W, 4 = NW, 5 = NE).  The metre round-trip cannot
see a consistent flip; `tests/09_c3_geometry.loft`'s sign check is
what gates it.

⚠ **The axial arithmetic survives in exactly one place** —
`tests/09_c2_relabel.loft`'s oracle — because `relabel_hex`'s DOMAIN
is axial (every coordinate dryopea ever wrote to disk) and a
bijection cannot be proved from one side.  Take an axial reference
from there if you ever need one; do not recreate a module.

⚠ **dryopea follows `hex_grid`'s COMPASS** (project owner,
2026-08-13).  The library documents "r increases upward" and names
direction 5 `NE` while placing row `r+1` at larger y; dryopea's +y is
south.  So `lat_to_metres` / `lat_from_metres` / `lat_corner_metres`
**negate y**, and direction 5 really is north-east on screen.  The
negation lives in the lattice→metres conversion, beside the metre
scale — the two things `hex_grid` cannot know.
⚠ The cost is accepted, not overlooked: **maps authored before C3/C5
render vertically MIRRORED**, and C5 added no compensating flip — a
map that came back looking the same would have meant the compass never
moved.  ⚠ Corner WINDING reversed with it (counter-clockwise in
`hex_grid`'s frame, clockwise in dryopea's); consecutive corners are
still adjacent and one side apart, which is all a convex fill needs.

### Naming

- Functions, variables: `lower_case`
- Types, structs, enums: `CamelCase`
- Constants (file-scope): `UPPER_CASE`
- Loop variables prefixed per function (`tslr_w`, `tpi_pal`)
  to dodge the flat-namespace gotcha
- `dryopea_*` save path is local-cwd-relative + gitignored

### Test discipline (moros-style)

- Factories for state construction (`camera_default()`,
  `painted_empty()`, `picker_default(path)`).
- Pure tick functions: `camera_update(c: &EditorCamera, input: InputState)`.
- `InputState` is a struct of named boolean fields, not a flag
  bitmask.  Tests construct it directly + assert on field changes.
- Golden-image tests via `assert_golden(cv, name)` — render to
  Canvas, write to `tests/actual/<n>.png`, compare bytes to
  `tests/golden/<n>.png`.  Bootstrapping a new golden: run, FAIL,
  review `tests/actual/<n>.png`, copy to `tests/golden/<n>.png`.
  ⚠ **A golden depends on BOTH the geometry and the coordinate
  labels**, so plan 09 could not move them once: C3 changed the
  geometry and C5 the labels, and in between a ring rendered as a
  lopsided blob.  They were rebaselined ONCE, in C5c, on a
  self-consistent system — see
  [`tests/golden/README.md`](tests/golden/README.md).  A golden is
  a review aid, not the drawing's gate: the exact ones live in
  `tests/09_c3_geometry.loft`.
  ⚠ **A golden AGREES WITH A SHEAR.**  Rebaseline it and it certifies
  whatever the code now draws, so it cannot gate a coordinate or
  geometry change at all — that needs an independent ORACLE (plan 09
  used `hex_grid` itself).  A golden that was rebaselined during the
  change under test has verified nothing; it is how moros#10 survived.

### Loft language gotchas we hit

Dryopea-side workarounds for known loft behaviour.  ⚠ **Almost every
one compiles clean and fails SILENTLY**, so a green suite cannot see
them — which is why they are written down rather than linted.  The full
list, with the shape of each defect and its issue link, is
[`docs/LOFT_GOTCHAS.md`](docs/LOFT_GOTCHAS.md); reproducers live in
[`loft_repros/`](loft_repros/README.md) and the outbound queue in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md).

By name, so you know when to go and read it:

- A struct **RETURNED from a function is a COPY** — mutating it is a
  silent no-op ([loft#894](https://github.com/loft-lang/loft/issues/894)).
- A struct **stored in a FIELD** of another struct is a copy too; loft
  says so as `advice[avoidable-copy]`.  A struct passed as a PARAMETER,
  or read out of a field into one, does alias.
- A struct returned through **TWO nested tail calls** loses what its
  loop wrote ([loft#880](https://github.com/loft-lang/loft/issues/880));
  bind the inner call to a local.
- **Never index a call's result in TAIL position** (loft#877) — but
  binding a call whose callee is declared LOWER in the file **panics the
  parser** ([loft#918](https://github.com/loft-lang/loft/issues/918)),
  so the fix for the two above triggers a third.
- **Never interpolate a struct with a `hash` field** — SIGSEGV
  ([loft#873](https://github.com/loft-lang/loft/issues/873)).
- A struct literal that **omits a field takes that field's default
  silently** ([loft#914](https://github.com/loft-lang/loft/issues/914))
  — build from `*_empty()`, never a partial literal.
- ⚠⚠ A **`const` initialised from a SIBLING MODULE's const, imported
  through `use dryopea;`, PANICS the compiler**
  ([loft#962](https://github.com/loft-lang/loft/issues/962), filed
  2026-08-17, both backends) — `index out of bounds: the len is N but the
  index is 65535`, blaming an unrelated function's RETURN TYPE.  ⚠⚠
  **`loft --native-emit src/dryopea.loft` is CLEAN**, so the library looks
  healthy and `loft test` panics on the first test file.  ⚠ Three
  conditions and each alone defuses it: the aggregator import, the read
  being in a const INITIALISER rather than a function body, and the
  program being a consumer entry.  ⚠⚠ **The fix MOVES it rather than
  removing it**: `use tick_clock;` keeps every real entry and all 91 test
  files compiling and makes the AGGREGATOR panic instead, naming
  `spawn.loft::per_tick` — a function below a *different* cross-module
  const.  There is no import style that compiles both, and the tree takes
  the side the gates are on.
- ⚠⚠ A file-scope **`const vector` holding a NEGATIVE number is EMPTY**
  ([loft#955](https://github.com/loft-lang/loft/issues/955), filed
  2026-08-17, both backends) — `len()` 0, every index `null(oob)`, and
  no diagnostic anywhere.  The SIGN is the whole trigger: `[10, 9, 5,
  0]` is fine, `[10, -5, 9]` and `[-1, 2, 3]` and `[1.0, -2.0]` are
  empty.  ⚠ A **local** with the same literal is correct, so bind it
  inside the function.  ⚠⚠ **A loop over an empty vector runs zero
  times, so every assertion inside it holds VACUOUSLY** — it made plan
  21 R1's camera gate report perfect agreement while iterating over
  nothing.
- **Loop variable names** must keep one type per function scope and
  OUTLIVE their loop ([loft#915](https://github.com/loft-lang/loft/issues/915))
  — prefix them per function.
- A **missing `use`** reports as `Expect token ;` on a later `.0`, and
  the whole aggregator goes red naming the importer.
- The JSON cast **HANGS** on ≥8 declared fields with a `vector<Struct>`
  — `MapFile` is capped at 6 — and **ignores declared defaults**
  ([loft#876](https://github.com/loft-lang/loft/issues/876)).
- `graphics::KEY_*` need **explicit qualification**.
- ⚠ **A zero-argument function in a TEST FILE is COLLECTED AS A TEST** —
  a one-line `fn m2_edge() -> integer` helper made `tests/25_m2` report 17
  tests where it has 16, and the extra one asserted nothing.  ⚠ A green
  suite counts it, and the count is what every plan's Status quotes; it was
  caught only because `1392 + 5` did not reconcile with 1398.  ⚠ Not every
  helper (`fn m2_band() -> PaintedWorld` is not collected) — the practical
  rule is *no zero-argument helper in a test file*.
- ⚠ `ticks()` is loft's clock builtin — **never shadow it**, not even
  as a parameter name.  A probe that did compiled clean and reported a
  tick 4x cheaper than it was.
- ⚠⚠ **Never add a `vector<Struct>` local to `script_command`**
  ([loft#935](https://github.com/loft-lang/loft/issues/935)) — give it a
  helper function, as `compose_parts` does.  A ~700-line function with
  one in it corrupts the interpreter heap **at compile time**, and the
  abort (`realloc(): invalid next size`) lands in an unrelated test file
  that never reaches the branch.  ⚠ Bisected at full-suite scale in plan
  23 K1: the nested data structure is innocent and the function's SIZE
  is the ingredient, so the same local is fine anywhere smaller.  ⚠ And
  a green suite cannot see the violation — the damage is latent until
  the allocator trips over it, so unrelated code can wake it up.
  ⚠ **FIXED and CLOSED upstream 2026-08-16**, so this is now a
  historical note rather than a live rule.  The split into
  `compose_fault` / `compose_parts` / `script_compose` **stays** — it
  reads better than the inline version and `script_command` is already
  at complexity 255 — but it is no longer load-bearing, and a future
  `vector<Struct>` local in a big function is not by itself a defect.
  ⚠ Retiring the split to re-test the fix is open work nobody needs.

### Save path

The interactive editor saves to `dryopea_save.json` in the
cwd.  Tests write to `tests/actual/*.json` (also gitignored).
Both paths are blown away between runs by `scripts/test.sh`.

**Eventual destination:** path-backed mmap'd `Store` (the hash
IS the file — no save loop).  Rust side ships; loft `.loft`
language surface for binding user-data Stores to a path is
missing.  Filed in [`QUESTIONS_FOR_LOFT.md` § Path-backed
user-data Store binding](QUESTIONS_FOR_LOFT.md); strategy in
[`plans/ROADMAP.md` § Persistence destination](plans/ROADMAP.md).
**Don't take the manual binary `file()` + `#read` detour** —
it's strictly worse than the JSON we have today.

### Plan structure

dryopea follows **moros's plan conventions** — see
[`plans/README.md`](plans/README.md) for the binding, and
[`plans/_TEMPLATE.md`](plans/_TEMPLATE.md) to start one.  The
essentials:

- **Layout is FLAT**: `plans/<NN>-<slug>/`.  There is no
  `future/` · `finished/` · `deferred/` — lifecycle is a field
  in the plan's own `## Status` section, so a plan that ships
  does not move on disk and invalidate every link to it.
- **Never renumber existing plans.**  New plans take the next
  unused integer.  Numbering carries no priority —
  `plans/ROADMAP.md` carries the ordering.
- **Most work is not a plan.**  A plan earns its directory only
  when the work is genuinely multi-phase; cap active plans at
  2–3.  See § Pick the lightest workflow that fits.
- Every phase names a **gate** — how you *see* it works.
  "It compiles" is not a gate.
- Value tags `S/R/G/F/U/C/Q/N` and effort letters
  `XS/S/M/MH/H/VH`, the same letters as moros and loft.

## Plans, ROADMAP, docs

```
plans/          one directory per multi-phase plan, flat: `<NN>-<slug>/`.
                README.md carries the conventions + the index (value,
                effort, lifecycle, one line each); _TEMPLATE.md starts a
                new one; ROADMAP.md carries the feature ordering across
                5 tiers; DEFERRED.md parks them.
                ⚠ Each plan's own `## Status` is the SOURCE OF TRUTH.
                Never keep a second copy of per-phase state here or in
                the index — it drifts, and the copy is what gets read.

docs/           ⚠ **listed once, in § Documentation index below** — a
                second copy of this listing is the one that drifts, and this
                one had grown three EXPLORATION.md rows saying three things.

PROBLEMS.md             — dryopea-internal bugs (@D-prefixed; ⚠ @D002 open — `cam.zoom`
                          changes no pixel; @D001, @D003, @D004 and @D005 fixed — the
                          player truncated its movement and froze under a 250 ms tick
                          until plan 26 L2 gave it a bank, and ⚠⚠ HALF OF EVERY BOX in
                          the catalogue was wound INWARDS until plan 20 A5, which draws
                          nothing under GL_CULL_FACE and changed no count, no vertex, no
                          normal and no mesh_crc)
QUESTIONS_FOR_LOFT.md   — outbound queue to loft (Open / Submitted / Resolved)
README.md               — public project intro
loft.toml               — package manifest (depends on graphics)
```

## Loft consumer relationship + library dependency

**Reuse is the rule.**  Do not write a dryopea-local version of
a routine a library already provides, and do not work around a
library bug with a private copy — fix it upstream (or file it)
and consume the release.  Libraries are owned by their
first-class projects; dryopea may ADD to them under their
existing contract, which is the right move when dryopea needs
something adjacent to what a library already does.

**Always check the real surface before writing against a
library** — `loft api <name>` prints its full public API, and
`.loft/api/<name>.api` holds the generated stubs.  Never guess a
signature.

- **Today:** `graphics`, `gridmesh` and `input` resolve from the
  loft package registry (`loft.toml` + `loft.lock`); the first two
  migrated out of loft's monorepo to `loft-libs-graphics`.
  `moros_map` is a path-dep into the moros checkout
  (`../moros/lib/moros_map`) — it is not published, and is declared
  but not yet consumed.
  ⚠ **`input` ships a PARKED banner that is STALE.**  Its header
  says it is blocked on loft `@P391` (`input_new`'s state in
  CONST_STORE under a cross-package call, so writes through
  `&InputState` panic).  It is not: dryopea consumes it from plan 09
  I1, and `input_new` / `input_tick_from_state` /
  `input_set_bindings` all work interpreted.  Probe it again before
  believing either the banner or this note.
- **The shared hex substrate now EXISTS as published libraries.**
  What the docs still call `lib_plan 24` shipped as the `hex_*`
  family in the registry: `hex_field` (exact-integer hex cell
  sets + outlines — the base), `hex_grid` (geometry: axial/pixel,
  neighbours, distance, corners), `hex_shape` (line / box / arc),
  `hex_form`, `hex_place`, `hex_draw`, `hex_edge`, `hex_way`,
  `hex_roof`, `hex_fit`, `hex_recover`, `hex_world` (sparse
  32×32-chunk world model with binary save/load), `hex_terrain`,
  `hex_body`.  moros additionally carries `moros_map` /
  `moros_render` / `moros_sim` / `hex_editor` / `hex_mesh` in
  `../moros/lib/`.
- **Convention mismatch — SETTLED 2026-08-12: dryopea adopts the
  libraries' convention.**  The question was which lattice is
  authoritative.  Answer, from the source: the entire `hex_*`
  family and `moros_map` are **pointy-top, odd-r offset** —
  `hex_grid` calls it "THE CONVENTION (shared with moros — the
  single executable source of it)", `hex_field`'s neighbour table
  is "odd-r offset, same SET as `hex_grid::hex_neighbor`", and
  `moros_map` carries a fixed bug from applying axial cube
  distance to odd-r coords (moros#10).  Plan 07's note that
  moros_map is axial was the stale one, and dryopea's
  **axial flat-top** was the odd one out.
  **The decision (project owner, 2026-08-12): dryopea converts**
  — one lattice across the ecosystem, and it is not the libraries
  that move.  **[Plan 09](plans/09-lattice-conversion/README.md)
  executed it and is complete** (2026-08-13), so § Hex convention
  describes what the code does today; the ask for a second
  `gridmesh` layout was withdrawn on the strength of it
  ([loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24)).
- **Plans 06 and 07 should be re-read against this.**  Both were
  written waiting on an extraction that has since happened, so
  their "blocked on lib_plan 24" framing is stale.

## Documentation index

| File | Topic |
|---|---|
| [README.md](README.md) | Public-facing project intro |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | ⚠ **The full `src/` layout** — what each file owns, the trap in it, and the key data structures.  `CLAUDE.md` § Architecture is a one-line index of this |
| [docs/HARD_WON_RULES.md](docs/HARD_WON_RULES.md) | ⚠⚠ **Every rule that cost a real defect to learn, with the measurement that produced it** — movement + passability, cost, testing something that moves, profiling, timers and epsilons.  `CLAUDE.md` § Hard-won rules carries the HEADLINES so the warning fires in context; this is the evidence.  ⚠ Most of them describe a test that CANNOT see the thing it appears to test |
| [docs/STATUS.md](docs/STATUS.md) | What exists today, one line per shipped phase, ~45 rows.  ⚠ Orientation only — **each plan's own `## Status` is the source of truth**, and `plans/README.md` indexes them |
| [docs/TOOLCHAIN.md](docs/TOOLCHAIN.md) | ⚠⚠ **How the gates go red for reasons that are not defects** — the 300 s hard-kill, two suites clobbering each other, a wall clock that is not yours alone, and the `graphics` cdylib fault (with two tidy explanations already FALSIFIED, so nobody re-derives them).  ⚠ Two of these read exactly like each other |
| [docs/EXAMPLES.md](docs/EXAMPLES.md) | ⚠ **The worked-example convention** — a public function is documented by the TESTS that show how to use it, pointed at by an index tag `@XXX-###` (an `@`, a THREE-LETTER acronym, a hyphen, three digits) in loft's own family (`@P367` / `@X072`), so ONE indexer carries them all; the hyphen is what keeps the families apart.  ⚠⚠ **The abbreviation namespace is the ECOSYSTEM's** — the indexer covers the registered libraries too, so `@XXX-001` must mean one test everywhere.  ⚠⚠ **A tag is not only an API example**: a first-class program tags a test because the ALGORITHM is worth reading, so a citation is any reference — a `// Example:` line OR prose in a doc.  ⚠ **NEW work only** (project owner, 2026-08-17): no retroactive sweep of the 387 existing public functions, and a file opts in with `// #examples`.  ⚠⚠ The gate carries an eight-control `--self-test`, and it earned its keep at once — `grep -r --exclude-dir='.*'` applies to the command-line directory too, so **any checkout under a hidden path scanned zero files and reported `ok`**, and every registered library lives under `~/.loft/` |
| [docs/PROFILING.md](docs/PROFILING.md) | How to profile the suite, the numbers of record and their date, and why the wall clock cannot see a real improvement |
| [docs/LOFT_GOTCHAS.md](docs/LOFT_GOTCHAS.md) | Every loft behaviour dryopea works around — ⚠ almost all of them compile clean and fail silently |
| [docs/DESIGN.md](docs/DESIGN.md) | Master design — towers / walls / waves / scramble / camera / HUD / economy / run shape |
| [docs/SETTING.md](docs/SETTING.md) | Fiction — autonomous AIs (girl-hacker imprint), faction wars dormant, surface-vs-underground, future contact gates, crew-doesn't-walk justification, combat-bot escalation |
| [docs/DESIGN_HISTORY.md](docs/DESIGN_HISTORY.md) | 2023 prototype seeds |
| [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) | ⚠ DESIGN, not built — the six robot installations (mines, factories, transport routes, military stockpiles, repair points, carbon plants) whose traffic is what waves are made of; the replacement for plan 16's authored list.  ⚠ Also § Crystal (the boss supply, and the only input with one product) and § The vertical dimension (a withered TREE is the shaft that reaches it) |
| [docs/MATERIALS.md](docs/MATERIALS.md) | ⚠ DESIGN, not built — **what things are MADE of**: the nine materials, the parts they make, what each robot class is rich in, and the catalogues of weapons, machines and defensive structures.  The tree behind `DESIGN.md` § 13's flat `points` scalar.  ⚠⚠ Its governing rule is what keeps it in-genre — **materials have WEIGHT and are SOMEWHERE ELSE**, so getting one is a trip; a tree that resolves into a menu is the failure mode (`@X106`).  ⚠⚠ § Power REFUSES a power grid for the player's base on measured grounds (`@X104`: a wire replaces a drive, and `plans/17` T3 measured the drive as the game), and § The 2023 catalogue flags the one upgrade that would delete the premise |
| [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) | Enemy movement — two steering modes, passability as a height step, bodies as terrain, sealing punished not forbidden, structural wall HP, retaliation, the tick resolving once |
| [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) | Palette spec — 11 painted types plus `rubble`, which the runtime deposits and nobody paints |
| [docs/NUMBERS.md](docs/NUMBERS.md) | Guide to `examples/numbers.json` — what is in it, what reads it, and ⚠ that nothing LOADS it yet |
| [loft_repros/README.md](loft_repros/README.md) | Minimal reproducers for loft bugs — filed, and ready to file |
| [docs/DECISIONS.md](docs/DECISIONS.md) | ⚠ **The greppable INDEX** — `@X###` design decisions, `@M###` measurements of record, each one line pointing at the doc that owns it.  ⚠⚠ **A bare plan phase is NOT unique** (`S0` is plans 18 AND 22, `C2` is 09 AND 15, `R0` is 20 AND 21) — write a code as `<plan>-<phase>`: `19-P3`, `22-S0`, `12-B7`.  ⚠ Every `@M` carries a DATE, because a measurement ages and the stale one gets quoted |
| [docs/PROGRESSION.md](docs/PROGRESSION.md) | ⚠ **The player gets better, the vehicle does not** (`@X016`-`@X019`).  Skill, not stats — which passes the genre test in its purest form.  The landscape is the school, the base is the exam, and there is a racing line because the measured-best defence is one only a good pilot can live in |
| [docs/PARTS.md](docs/PARTS.md) | ⚠ **Entity art — every entity is a PART-TREE and its GEOMETRY is derived** (plan 20).  The moros model (limbs on joints, three limb kinds, scale derived, hitbox a subset of the skin) and where dryopea deviates.  ⚠ Decisions D1-D8; moros's own `doc/claude/PARTS.md` § P9.0 is the authority on the model.  ⚠ § D4 replaced a SPRITE design — read it before quoting anything about pixels |
| [docs/EXPLORATION.md](docs/EXPLORATION.md) | ⚠⚠ **Exploration IS scouting** — `DESIGN.md` § 13 already ranks it *the* progression activity, so this doc ASSEMBLES rather than adding a pillar.  ⚠⚠ **§ X0 POINTS AT [docs/PROGRESSION.md](docs/PROGRESSION.md), which was rewritten 2026-08-26** — the old *"progression is SKILL, not stats"* reading is superseded.  ⚠ Two of the new rulings are about exploration: **the RULES are learnable and the INSTANCE is not** (`@X122`), which is what stops a knowledge-dominant progression from solving the game, and **scouting converts rules-knowledge into instance-knowledge every run** (`@X123`) — so the veteran scouts faster and better, never less.  ⚠ The measurements are unchanged (a sealed wall doubles the clock, a gate buys nothing, boost is the only way out of a sealed base).  ⚠⚠ **§ X2b: the game already WAITS** — `wave_provoke_step` means an unlimited free recon phase the player ends deliberately.  ⚠⚠ **§ X2c: a find accelerates BUILDING, so its value collapses once you are busy** — measured twice already (plan 16 W4: one tick; plan 17 T3: +76 points).  ⚠⚠ The run ALREADY opens with a sortie (`wave_provoke_step` needs a vehicle 12+ hexes out), so *explore earlier* is content on a trip the player already takes, not a new phase.  ⚠ A find is ONE marker row + ONE cargo row; **the first scouting scenario needs no code at all** |
| [docs/RENDERER.md](docs/RENDERER.md) | ⚠ **The camera and the pipeline** (plan 21) — moros's `RenderCamera`, FOLLOW behind the facing, and ⚠⚠ **`camera_overview` at 89° IS the editor's view**, so it is one camera with two presets.  ⚠ § R0 MEASURED that a GL frame survives `xvfb` → `gl_screenshot` → `imaging::png` → exact classification with **zero** colour drift — which is what makes going 3-D affordable at all.  ⚠ Retires `DESIGN.md` § 12 |
| [docs/PROXY_ART.md](docs/PROXY_ART.md) | Placeholder shapes.  ⚠ Its SIZES stay and become a gate (`PARTS.md` § D6); its SHAPES retire entry by entry as plan 20's catalogue covers them |
| [plans/README.md](plans/README.md) | Plan conventions (moros-style) + index |
| [plans/_TEMPLATE.md](plans/_TEMPLATE.md) | Template for a new plan |
| [plans/ROADMAP.md](plans/ROADMAP.md) | Comprehensive feature roadmap (5 tiers) |
| [PROBLEMS.md](PROBLEMS.md) | Dryopea-internal bugs (`@D<NNN>`) |
| [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) | Outbound queue to loft |

## Reading by goal

| Goal | Start here |
|---|---|
| Understand the game | [README.md](README.md) → [docs/DESIGN.md](docs/DESIGN.md) § What kind of game this is, then § 2 The pitch |
| Design a base site that is not flat ground | [docs/DESIGN.md](docs/DESIGN.md) § Trees as terrain — a 10-hex tree stem is a plateau, so the perimeter IS the terrain and no wall is needed.  ⚠ The catch is the whole design: it is impregnable until you start killing, because bodies ramp and your own kills build the only staircase up |
| Understand the END GAME | [docs/DESIGN.md](docs/DESIGN.md) § The end game, and why it is still this game — enemies change, robots and insects become co-belligerents, and the player still BUILDS BASES because humans cannot attack an old one at all.  ⚠ The scrambler changes JOB rather than switching off: it is a LURE (robots converge on it), so where you put one decides where the swarm fights — but it ATTRACTS and DEGRADES in one act, so you can have them coordinated or where you want them, never both.  ⚠ And every ROBOT_ECONOMY lever inverts its sign: feed the factories, keep crystal flowing, wake the stockpile you spent the mid-game avoiding |
| Judge whether DEEP-LORE content belongs | [docs/DESIGN.md](docs/DESIGN.md) § And the DEEP layers are what keep it a tower defence — the second test: *does it resolve into a statement about position, terrain or timing?*  If it resolves into the player's STATS or ABILITIES it is off-genre, however good the story is.  ⚠ The deep layers are load-bearing for the GENRE, not decoration on it |
| Judge whether a new MECHANIC belongs | [docs/DESIGN.md](docs/DESIGN.md) § What kind of game this is — the test is *does this put something in the player's hands at a moment when using it costs them something?*  ⚠ Second test since 2026-08-14: [docs/SETTING.md](docs/SETTING.md) § Nobody is attacking anybody — yet.  Both non-human tiers OPEN as maintenance (robots think they are repairing, insects guard a wound), so a mechanic that opens with hostility is off-fiction; aggression has to be EARNED by the player's accumulated pressure |
| Design EXPLORATION, or judge a scouting idea | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) — ⚠ it is not a new pillar: `DESIGN.md` § 13 § Scouting already ranks it *the* progression activity, § X2 shows the run already opens with a sortie, and § X2b that the game WAITS until you poke a marker.  ⚠ **The cost of leaving is already MEASURED** — plan 17 T3 priced parked-vs-shuttling helpers at two waves of the authored list — so exploration needs no new cost mechanic.  ⚠ The first scenario is a `.keys` file, not a feature |
| Ask why a find has to be found EARLY | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2c — a find is a BUILD ACCELERANT, and what decays is **the opportunity to use it**, not the thing itself.  ⚠ Already measured twice by accident: the same retrieval is worth **one tick** when the job is gone (plan 16 W4) and **+76 points** when it is not (plan 17 T3) |
| Author what a WAVE IS MADE OF | `schedule 4 12` arms the list, `compose 1 4 miner 8 scout` says what one wave of it is made of ([plan 23](plans/23-the-small-robots/README.md) K1, `@X056`).  ⚠ **`compose` REPLACES a wave and a later `schedule` line WIPES it**, so the order `emit.loft` writes is a requirement, not a style.  ⚠ A wave's SIZE is SUMMED from its parts and never stored (`@X055`), so `schedule 12` + `compose 0 3 miner 2 scout` is a wave of **five** — there is no total to disagree with.  ⚠⚠ **The ORDER you write is worth NOTHING** (plan 23 K3, `@M018`) — it sets the departure order, and since K2b the faster class overtakes, so four scouts first, four scouts LAST and four scouts alternated all land on the same tick.  K0's *"order is worth 20x"* was measured on enemies PLACED at different distances, before classes had speeds.  ⚠⚠ **What a mix IS worth is its FASTEST member and nothing else** — four harvesters in front of eight miners behaves like twelve harvesters, not like anything in between — so write compositions expecting the quickest class to decide the outcome.  ⚠ `examples/waves.json` is NOT the place — `WaveFile` deliberately has no composition (`@X057`) |
| Ask what CLOCKS a run, or why the player must be efficient | [`docs/EXPLORATION.md`](docs/EXPLORATION.md) § X2d — the **permit**.  `DESIGN.md` § 2 hires the player on a *"permit-bound sortie"*, `SETTING.md` § History calls them *"limited-time sorties"*, and § The quarantine puts the teeth at the exit: *"orbital exit is the chokepoint … permit on file = pass; permit missing = destroyed"*.  ⚠ Expiry must cost the CARGO, never the run — § 14 has no fail screen, and a bad run is one with *"meagre carryover"*.  ⚠ It also turns `NUMBERS.md`'s ungateable *"15-25 minutes"* into a tunable — but today's longest base falls at **321 ticks (~3.5 min)**, so the window is derived from content, not chosen |
| Ask what a wreck is MADE of, or add a material / weapon / machine | [`docs/MATERIALS.md`](docs/MATERIALS.md) — ⚠ DESIGN, not built.  ⚠⚠ Read § The governing rule first: a material earns its place only because **getting it is a trip**.  ⚠ Ship **three** materials, not the 2023 catalogue's 34 (`@X106`) — and check § The 2023 catalogue is much bigger before adding a row, because that catalogue was already written out in full once |
| Judge a POWER, AUTOMATION or TRANSPORT idea (cables, rails, conveyors, autopilot) | ⚠⚠ **It is almost certainly refused, and the refusal is MEASURED** — [`docs/MATERIALS.md`](docs/MATERIALS.md) § Power (`@X104`, `@X105`).  A wire or a rail replaces a DRIVE, and [`plans/17`](plans/17-tower-hot-swap/README.md) § T3 measured the drive as the game: two *shuttling* helpers clear all 205 robots where the same two *parked* reach 5/7 and the base falls.  ⚠ The ENEMY may have the full grid — `ROBOT_ECONOMY.md` § 3 already makes a route a thing to cut, which adds a decision instead of deleting one |
| Ask why the player must be physically present, or why there is no remote control | [`docs/SETTING.md`](docs/SETTING.md) § The recruitment (`@X099`) — ⚠⚠ **the charter of the whole game, and it is one sentence of dialogue**: the multi-frequency jammer that makes the robots beatable is what makes communication with the surface impossible, *"so there has to be personnel below to service the different systems"*.  ⚠ Every servicing loop the project has measured exists because of it, which is why a comms upgrade that works under the scrambler is refused |
| Ask why machines DECAY, or why the map is littered with wrecks nobody killed | [`docs/SETTING.md`](docs/SETTING.md) § The pollen (`@X100`) — aggressive to humans **and machines**, so repair is a *condition* rather than an event.  ⚠ It supplies the cause for four systems designed separately: the robots' repair points, why an approaching robot reads as helpful, free salvage with no combat attached, and base upkeep.  ⚠ It must stay a CONSTANT — a pollen *storm* the player waits out is a lean-back mechanic |
| Judge a KNOWLEDGE, lore-unlock or discovery idea | [`docs/SETTING.md`](docs/SETTING.md) § The knowledge tree (`@X102`) — ~55 facts in per-faction arcs, recovered from the 2023 game data.  ⚠⚠ **The boundary is one line**: knowledge changes what the player can **ATTEMPT** and what the map will SHOW them, never how well they do it.  *Cave entrances* makes a scan exist; it does not make scanning better.  ⚠ A knowledge item granting *+10% hacking* is a stat wearing a story's clothes — it belongs on [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2's crew axis instead.  ⚠⚠ And knowledge IS the answer, so it must be **found and never sold** (`@X118`) |
| Judge a PROGRESSION idea (upgrades, unlocks, XP, stats) | ⚠⚠ **[`docs/PROGRESSION.md`](docs/PROGRESSION.md) was REWRITTEN 2026-08-26 and the old answer is inverted** — it used to say *the progression is the player's skill* and refuse stats; the owner ruled otherwise (`@X016`/`@X017` superseded, `@X103` reversed).  ⚠ The model is **Blue Prince** (`@X116`): what the player KNOWS and DECIDES dominates (`@X117`), **and there is a lot of room for friction-reducers** — build speed, repair speed, detection radius.  ⚠⚠ The test is `@X118`: **does this reduce the cost of acting on what the player knows, or does it supply what they do not know?**  A shortcut saves you steps; it never tells you the combination |
| Ask why a stat or an upgrade is ALLOWED now | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P6 — the fence moved and narrowed.  ⚠ It forbids **doing the player's thinking** (a recommended layout, a readout that NAMES the incoming composition, salvage without the trip) and **a route to winning by personal power** (`@X119`, owner 2026-08-14).  ⚠ It does **not** forbid a stat, and `DESIGN.md` § 8's noncombatant player is the part that did not move: the crew get better at WORKING, nobody gets better at FIGHTING |
| Ask HOW a crew member improves, or add a skill to the lattice | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2a (`@X124`) — ⚠⚠ **it is a LOOP**: practice raises the skill fast and its **two** statistics slowly, and each statistic lifts **four** skills, so practising `repair` quietly improves build, operate, mine, boost, hack and scrounge.  ⚠ Measured off `archive/gameplay.data`: 12 skills, 6 statistics, 24 edges, **diameter 2** — no crew member ever dead-ends.  ⚠⚠ A new skill must take one of the **four free statistic pairs**, never duplicate an occupied one: `combat` and `scout` are already twins (both observe+stamina) and that is why they permeate to 5 where everything else reaches 6 (`@X128`) |
| Add a RELATIONSHIP, loyalty or affinity mechanic | ⚠⚠ **Not as a bar** — [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2e (`@X131`).  A relationship here is **a LEDGER of what happened**, and it is the same mechanism as the practice loop: the crew get better at what they DID and closer through what they WENT THROUGH.  ⚠ The player never raises it — there is nothing to farm and no bar to draw; their only influence is **who they send together and who they come back for**.  ⚠ The history already exists (`@X132`): who retrieved whom, who was left behind at force-launch, who boarded.  ⚠ A mechanical effect must be COVERAGE (a faster handoff), never a threshold, an unlock or a gate (`@X133`).  ⚠ Counts + landmarks, never a log (`@X134`) |
| Ask about a TUTORIAL, onboarding, tooltips or a hint system | ⚠⚠ **There is no tutorial** (`@X137`, [`docs/DESIGN.md`](docs/DESIGN.md) § There is NO TUTORIAL) — no tutorial mission, no first-run overlay, no tooltip layer.  ⚠ It is affordable because **most of the game has no key to learn**: § Position triggers means driving over loot picks it up and standing at a tower repairs it, so play mode is five keys plus the mouse.  ⚠⚠ Three of a tutorial's four jobs were already answered, and the **recon window is the sandbox** (`@X138`, `@X022`) — nobody built it for that.  ⚠⚠ The fourth job, *what to do first*, belongs to the crew's remarks, which makes them **the onboarding** rather than a convenience |
| Add a KEY BINDING, or a mechanic that needs explaining | ⚠⚠ **The key table is a design BUDGET** (`@X139`) — every key is a thing the player must discover unaided, so a new binding needs an argument, not a row; `tests/09_i1_bindings.loft` pins the row count and goes red by design.  ⚠⚠ **And the test for any new mechanic is: can a player find this by playing around?**  One that needs explaining needs redesigning, because there is nowhere left to explain it — design it in spatial terms first (§ Position triggers).  ⚠ Two hazards are already named (`@X140`): **Q** (wall-paint) is the sharpest, since a player who never presses it never builds, and **Shift** (boost) is the only way out of a sealed base |
| Ask whether a crew member may be WRONG | ⚠⚠ **Two different things, and only one is allowed** (`@X147`, `@X148`).  **Arbitrary error** — wrong by dice, some fraction of the time — is refused: it is being lied to by your own people, it is unlearnable, and it is frustrating.  **Systematic bias from perspective** is adopted: the crew are **unreliable narrators**, coloured by where they stood, what they know, what they had at stake and what frightened them.  ⚠⚠ Three rules keep it usable: **facts are reliable and causes/blame are not**, **bias misleads in DEGREE never in DIRECTION** (or players tune the crew out and the onboarding role collapses), and **bias is consistent per character**.  ⚠ It is learnable, so calibrating your own crew accumulates on the dominant axis |
| Add anything to the POST-MISSION screen, or ask what a debrief may say | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2f (`@X144`-`@X146`) — the player may **optionally** ask the crew what they thought, which is PULL where remarks are PUSH.  ⚠⚠ **The fence RELAXES here**: the base is finished, so there is no live question to short-circuit and the crew may CONCLUDE freely — and what the player carries away is **rules-knowledge**, the axis that is supposed to accumulate.  ⚠⚠ **But never a SCORING SCREEN** — `DESIGN.md` § 14 has no fail screen and says a bad run is *felt across the sequence, not announced*.  Opinions, not a grade; and no dialogue tree, skippable in one action.  ⚠ Note the **empty chair**: a helper left behind is not there to be debriefed |
| Ask WHO speaks, or when a crew remark fires | ⚠⚠ **The helper with NO TASK assigned** (`@X142`) — the player gives the orders, so an idle worker reporting in is what a foreman hears.  ⚠ It needs no stall detector: it reads a task list that already exists and **cannot misfire on a parked player who is repairing a tower**.  ⚠⚠ **It self-calibrates** — assign everyone and there is silence; on landing nobody has orders, so the onboarding remark falls out of the rule.  ⚠ **Idleness picks WHO and WHEN, skills pick WHAT**: the ex-miner says *"there's ore on that ridge"*, never *"I am idle"*.  ⚠ **One speaks, never six** (`@X143`), and assigning work is the in-fiction off switch |
| Ask what the CREW say, or add a line of dialogue | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2c (`@X129`) — ⚠⚠ **a remark POINTS, it never CONCLUDES**: *"soft ground on that ridge"* passes, *"build here"* and *"wave 3: eight miners"* are `@X118`'s failing row.  ⚠⚠ **The crew are PARTIAL SENSORS** — each right about their own domain and blind to the rest, so following one exclusively is how a base goes wrong and the synthesis stays with the player.  ⚠ A remark must CREATE a trip rather than save one, and a player who ignores every remark must lose nothing they could not get themselves.  ⚠⚠ **They fire only in OFF-TIME** (`@X135`) — gated on the player's own state, so a veteran who never stalls never hears a hint and `@X120`'s no-tutorial-wall comes free.  ⚠⚠ **SILENCE is the defence, not modesty** (`@X136`): if a stalled player is always answered, stalling becomes the way to get answers.  ⚠ Trigger on the absence of PROGRESS, never on idleness — *standing still is work here*, a parked player is repairing a tower.  ⚠⚠ **BLOCKED on text** (`@X130`, `@X097`) — the first designed feature that needs `draw_text`, which needs a font this repo does not have |
| Ask how a player GETS a good crew member | ⚠⚠ **Templates, never rerolls** — [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2b (`@X125`).  The player picks a background with known contents and then **optimises it by giving the NPC the work**; a reroll loop is `@X118`'s failure mode wearing a dice cup, because the player pays time for the game to hand them a stat block.  ⚠ **And never randomise which templates are offered** — that re-invents the reroll one level up, and `@X122` does not license it: a hiring pool is not a place you go and look at |
| Add a crew SKILL, or ask what one is allowed to change | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P2 (`@X111`, `@X112`) — twelve skills over six statistics, per character.  ⚠⚠ **A skill SCALES A NUMBER THAT ALREADY EXISTS** and introduces no mechanism: `build` scales helper-seconds, `repair` the 20 s standing clock, `scout` the detection radius, `scrounge` `loot_rate`.  ⚠ So the layer lands one skill at a time against constants that are already gated — and a proposed skill with no existing number to multiply is the one to push back on |
| Ask whether the game may GATE content, or balance the first mission | ⚠⚠ **No** — [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P0c (`@X120`): a returning player may skip the ramp and **nothing may stop them**, because their knowledge came back with them.  ⚠ So the first base may not be balanced for a naive player, and no content may be gated on progress already earned in the player's head.  ⚠⚠ The corollary is a rule for every unlock: **prefer FOUND over AWARDED** — a found unlock is permeable to knowledge (a veteran knows where to look), *"clear three missions"* is not |
| Design a RANDOM element, or ask why exploration does not go stale | [`docs/PROGRESSION.md`](docs/PROGRESSION.md) § P1d (`@X122`, `@X123`) — ⚠⚠ **vary the INSTANCE, never the RULES**: where the ridge is, which classes are in the wave, which tree withered — never what a sealed wall is worth or how fast a miner chews (which is also why `numbers.json` is global rather than per-map).  ⚠ Too little variation and the game is solved; too much and *roughly knowing what to expect* stops paying, which destroys the dominant axis from the other side.  ⚠ And every random element must be something you go and LOOK at — randomness the game announces is a dice roll with a caption |
| Find where an OLD idea of the owner's went | [`docs/DESIGN_HISTORY.md`](docs/DESIGN_HISTORY.md) §§ 4-5 — the routing tables: what was adopted and into which document, what was already canon, what CONTRADICTS canon and what was refused with the reason.  ⚠ The raw sources are [`archive/seed-notes.md`](archive/seed-notes.md) and [`archive/gameplay.data`](archive/gameplay.data), and they are **never edited** |
| Design where WAVES eventually come from | [docs/ROBOT_ECONOMY.md](docs/ROBOT_ECONOMY.md) — six installation types and the routes between them.  ⚠ Its governing rule is the enemy rule again: ONE system, per-type DATA, so a new installation costs a row and no new behaviour |
| Cite a design decision, or find where one was made | [`docs/DECISIONS.md`](docs/DECISIONS.md) — `grep -rn '@X025' .` finds every mention of a decision, `@M001` every quote of a number.  ⚠ **Never cite a bare plan phase in a code** — `S0` is two plans and `C2` is two more; write `22-S0`.  ⚠ A code is permanent even after its decision is reversed (it gains a `SUPERSEDED by` line, like `@D001`) |
| Find a mechanic that is designed but NOT built | [docs/DESIGN.md](docs/DESIGN.md) (the mechanics) and [plans/ROADMAP.md](plans/ROADMAP.md) (the index).  ⚠ `plans/12` § Design recorded during this plan POINTS at them rather than restating — a second copy is the one that drifts |
| Understand the fiction | [docs/SETTING.md](docs/SETTING.md) |
| Pick next work to do | [plans/ROADMAP.md](plans/ROADMAP.md) § **The critical path** — the natural order by DEPENDENCY (the 5 tiers below it are ordered by impact-per-line instead, and say so).  ⚠ Its organising principle: **every step must be measurable when it lands**, and the questions now open are FEEL questions, which is what moves drawing up the list |
| Ask what the BIGGEST missing mechanic is | ⚠⚠ **BUILDING.**  Walls and towers are placed in the EDITOR; the player cannot make a base, and the wallet buys nothing.  Three finished designs are inert without it — `@X022` (the pre-wave window is a budget), `@X024` (a find accelerates building), `@X019` (the layout is the exam).  ⚠ Its pieces are all designed and named: wall paint, the beacon ferry, helper construction time — and the carry model already moves a beacon exactly as it moves a tower-top |
| TUNE a number | `examples/numbers.json`, then run `scripts/test.sh`.  ⚠ `tests/numbers_design_targets.loft` gates five of `docs/NUMBERS.md` § Design targets against the running sim, so a tuned value fails there naming the promise it broke.  ⚠ And nothing LOADS numbers.json: every value is hand-copied into a `.loft` constant, so edit BOTH — that test pins them together |
| Continue plan 01 work | [plans/01-ground-editor/README.md](plans/01-ground-editor/README.md) § Implementation status |
| Document a new public function, or point at a test as its EXAMPLE | [`docs/EXAMPLES.md`](docs/EXAMPLES.md) — put an index tag in a comment above the test (`// @XXX-001 — a frame loop spends its backlog`) and cite it from the function (`// Example: @XXX-001`).  ⚠ A tag is an INDEX TAG in a comment, never part of an identifier, and it binds to the `fn` that FOLLOWS it.  ⚠⚠ **Anchoring to the comment is the better contract**: deleting the test makes the citation DANGLE (real breakage), while renaming it for clarity leaves the example valid.  ⚠ A snippet in prose ROTS (nothing compiles it) and an unlinked test is INVISIBLE — the pair is the deliverable.  ⚠ Write a NEW test if no existing one is clear: a test that proves a function works is not automatically one that shows how to use it |
| Add a regression test | `tests/01_*.loft` for patterns; `golden.loft::assert_golden` for image tests |
| Script a run of the editor | `tests/scripts/*.keys` for the vocabulary; `script.loft::script_run_file` to play one; `snap <name>` for a picture |
| Add a validation scenario | a new `tests/scripts/<name>.keys` + one test in `tests/08_v3_scenarios.loft` (pin its check count — a scenario with its measurements deleted still reports ok) |
| Ask whether two runs are in the same STATE | `src/compare.loft::state_diff` — the first difference, NAMED, or `""` (plan 18 S0).  ⚠ It reads the state field by field and knows nothing about the emitter: define equality as "emit both and compare the text" and S2's round-trip gate is CIRCULAR — green precisely where the tool is broken.  ⚠ Layers are compared by KEY, because hash iteration order is not part of the state.  ⚠ Floats EXACTLY — an epsilon would hide the ulp of drift the gate exists to catch.  ⚠ Its field list is hand-maintained: a new field on a state struct needs a row in `tests/18_s0_the_comparison.loft` or nothing covers it |
| Write a situation down as a `.keys` file | `src/emit.loft::emit_keys` (plan 18 S2) — the ground, the markers and the whole runtime state, as an authored STARTING POSITION with no `tick` in it.  ⚠ Order is load-bearing: `flag` before `tower`, `crew` before an `object` it owns, `place` before `hit`/`stand`/`dead`, `schedule` before `pending`.  ⚠ Gated by capture → emit → replay over all 28 real scenarios, comparing the WORLD as well as the state — terrain is not in `WaveState`, so a state-only comparison is green for an emitter that lost the map |
| Cut a captured situation down to the interesting part | `src/emit.loft::crop_keys` + `crop_fault` (plan 18 S3).  ⚠ **The refusals are NECESSARY and not SUFFICIENT** — dropping the core and cutting under a tower's 15-hex reach are refused, but a LEGAL crop can still change the answer: measured, a radius-15 crop of a band whose spawn marker sits at 18 silently stops every wave, because `SPAWN_DISABLE_RADIUS` and `WAVE_1_PROVOCATION_HEXES` are distances from the CORE.  ⚠ Only running both and comparing certifies a particular crop |
| Cut a fixture down to what a behaviour needs | `src/reduce.loft::reduce_keys` (plan 18 S4) — greedy line removal against a PREDICATE, which is just `.keys` text appended to the fixture: the measurement vocabulary is the predicate language.  ⚠ **A predicate that holds over an EMPTY fixture is refused** — otherwise every line is removable and the reducer is a delete button.  ⚠ The result is 1-MINIMAL (removing any one line breaks it), not minimal: a pair of lines that only matter together survives |
| Play the game in a window | `make play`, pan to the base, press **P** (plan 19 P3).  The crew lands at the core, WASD drives it, and waves arrive on the wall clock.  ⚠ **Nothing of the game is drawn yet** — P4 — so the console echo (a line per tick) is the whole of what you can see, and the camera does not follow the vehicle |
| Turn a state you REACHED into a test | [plans/18](plans/18-scenario-capture/README.md) — the tool is BUILT (S0-S4); the loop to capture FROM now exists (plan 19 P3), and wiring a key to it is plan 19 P5.  ⚠ Emits `.keys` and never a state blob: a saved `WaveState` is a golden of the simulation and *a golden agrees with a shear*.  ⚠ The work is making the vocabulary TOTAL over `WaveState` — enemies, towers, wallet and cargo have no setters today.  ⚠ A crop has a MINIMUM radius set by the mechanics (the core, the 25-hex bubble, a tower's range 15), so a naive one silently changes enemy steering |
| Change what a frame contains | `editor_view.loft::render_editor_frame` — the GL loop and `snap` both draw it, so edit it there, not in `main.loft` |
| Ask what an entity IS, or add a socket to one | `src/part.loft` (BUILT, plan 20 A1) — `Socket` / `Binding` / `Part` / `PartSet` over `hex_body::Rig`, and [`docs/PARTS.md`](docs/PARTS.md) § D3 for why the tower proves the model.  ⚠⚠ **Where a socket IS, is computed by `socket_world` and stored nowhere** — a `Binding` names a socket and a part and carries no coordinate, so moving a part moves everything in its sockets; adding a position field ends the design without failing a test.  ⚠ It carries no POSE either — plan 20 A4 asks `TowerState`.  ⚠ The doorstep is `part_fault`, which asks `hex_body::rig_admissible` before anything of dryopea's.  ⚠⚠ **The reuse gate is the `use` line, never the manifest**: dropping `hex_body` from `loft.toml` AND `loft.lock` leaves all 18 tests green ([loft#968](https://github.com/loft-lang/loft/issues/968)) |
| Draw an ENTITY, or change what one looks like | [`docs/PARTS.md`](docs/PARTS.md) — a part-tree, and the GEOMETRY is derived from it (plan 20).  ⚠ **Never a shape drawn inline in `editor_view.loft`**: that is the *"second renderer that happens to live in the test harness"* its own header refuses, one layer down.  ⚠ The SIZE is the durable artefact and § D6 gates it against the simulation's constant.  ⚠⚠ **And WHERE it is drawn is already measured** — plan 26 L5, `@M035`: a lattice mover drawn on its hex jumps two hexes a tick, the camera's ease cannot smooth it (it is the thing the ease chases), and the fix is TWO changes that only work together — the mover drawn at `play_alpha`, and the camera following that point rather than the hex |
| Ask where the game's CAMERA lives, or why the editor's view is a mode of it | `src/render_camera.loft` (built, plan 21 R1) and [`docs/RENDERER.md`](docs/RENDERER.md) § R1 — moros's `RenderCamera`, ported.  ⚠ `camera_overview` at elevation 89° reproduces the editor's top-down view **to 0.08° of bearing and 0.56% of scale** (`@M022`), so there is ONE camera with two presets.  ⚠ The game's camera belongs on `PlayState`, never on `EditorState.cam` (that is `EditorCamera`, and its zoom is `@D002`) — ⚠ **not built yet**: `@X014` stands and lands in R2, where an eased boom gives the session something to remember |
| Put a hex into the CAMERA's world, or ask which way is up in 3-D | `src/render_camera.loft::lat_to_world` — and it is the ONE place that may negate y.  ⚠⚠ **The camera's world is `+y` NORTH**, where every other metre in dryopea is `+y` SOUTH: that is a CANVAS convention, it is left-handed once `+z` is up, and `mat4_look_at` builds a right-handed basis — so carrying it into 3-D **mirrors the world** and no azimuth undoes it (`@M021`: one of eight azimuths works in the north frame, none in the south).  ⚠ The negation cancels `lat_to_metres`', so the camera's frame is `hex_grid`'s own — a library frame is a WORLD frame and dryopea's is a SCREEN frame |
| Ask what a frame draws BETWEEN two ticks, or why there is no interpolation | `src/tick_clock.loft::clock_alpha` and `src/play.loft::play_alpha` (plan 26 L5) — the number ships and **no policy does** (`@X085`).  ⚠⚠ **The ease does not cover it and never did** (`@M035`): an eased follow camera moves a lattice mover's jump off the world and ONTO the mover, and it grows six times doing so — **14.9 px** of ground becomes **96.1 px** of mover, where `@M023` read the camera's own target and saw none of it.  ⚠ Drawing the mover at alpha takes 96.1 to **14.1 px** and no further; only a camera following the DRAWN point takes it to 0, so the two are ONE decision.  ⚠ Priced with no camera at all: interpolate is **2.598 m behind for ever** (one whole step), extrapolate is exact to **9.5e-16 m** and jumps a whole step at every change of velocity |
| Ask why the camera eases, or add a valve to it | `src/render_camera.loft` § The ease (plan 21 R2) and [`docs/RENDERER.md`](docs/RENDERER.md) § R2b.  ⚠⚠ **The approach is `1 − e^(−k·dt)` and moros's `f = k·dt` is REFUSED** — the linear form is frame-rate dependent and `play.loft` is built on the opposite property (`19-P0`), so a linear camera would put a frame-rate dependence into the artefact a gate photographs.  ⚠⚠ **THREE valves ease, not the boom alone**: the vehicle is a lattice position and jumps 1.299 m on the tick it steps, so the target and the azimuth are what make the picture move at all (`@M023`: 12 of 240 frames un-eased, 221 eased).  ⚠ The azimuth eases the SHORT way — A then A+S is a real **−300°** swing otherwise (`@M024`).  ⚠ Rest SNAPS: an asymptote stopped by a tolerance rests wherever the frames fell |
| Ask what shortens the camera's boom, or add an occluder | `src/render_camera.loft::camera_boom_free` over `passable.loft::sight_first_block` — **the same walker `tower_sees` asks** (`@X071`).  ⚠ It answers WHERE rather than whether, because a boom needs a distance and a shot needs a yes/no.  ⚠ The camera reads a HEIGHT and never a kind: a `wall` at the far cell lends the whole boom and a `wall_high` there does not, while ONE HEX OUT both stop it because the ray is only 1.6 m up (`@M024`).  ⚠ The free length is quantised to hex steps and smoothed in TIME; the trigger for a sub-hex march is terrain elevation (plan 02) |
| Ask where the game's camera is REMEMBERED between frames | `PlayState.cam` — a `CameraRig`, which is the live `RenderCamera` plus the boom the PLAYER asked for (`@X014`, `@X070`).  ⚠ **Two booms are two facts**: occlusion lends the eye less, it never rewrites the ask, or a wall the vehicle drove past would shorten the camera for the rest of the run.  ⚠ `play_step` steps it LAST and on EVERY frame — inside `play_advance`'s tick loop it would run on one frame in forty at 60 fps and stutter with the right average |
| Point the camera at the vehicle, or ask which way it is facing | `src/render_camera.loft::camera_follow_vehicle` over `vehicle_facing` — the bearing comes from the **VELOCITY** (`metres(to) − metres(here)`), because a hover unit has no stored facing (`@X067`).  ⚠ It answers a PAIR: plan 19 P2 spells *stop* as `vehicle_drive(v, v.q, v.r)`, so a parked vehicle's velocity is zero and `atan2(0, 0)` would swing the camera east on every key release.  ⚠⚠ **Never paste moros's `azimuth = 270° − facing_deg`** — correct in moros's frame, and in dryopea's it puts the eye exactly ABEAM at all four cardinal headings, where it still tracks and still eases and still looks like a working camera |
| Draw the GROUND, or ask why the terrain mesh does not blend | [`plans/25`](plans/25-the-terrain-mesh/README.md) § What was measured first — ⚠⚠ **the corner-height MEAN is a no-op in dryopea** (`@X072`): `height_override` is non-null on two of twelve palette kinds, so the ground is a flat plane with pillars and the mean changes nothing across ground *or* across a structure's edge.  ⚠ It is honest rather than cheap — the sim asks `can_step`, a height DIFFERENCE, so a sloped mesh would draw a ramp the vehicle cannot climb.  ⚠ The corner↔direction relation is `lattice.loft::lat_edge_corners` over `hex_grid`, delegated and never tabulated (`@X073`) — and it takes no `Hex`, because unlike the neighbour LABEL delta the corner relation is parity-independent.  ⚠ The corner ring winds **counter-clockwise** in the camera's world (two negations cancel), so `GL_CULL_FACE` needs no reversal — and M3 turns culling ON so a reversed winding fails loudly.  ⚠ The trigger to add the blend is [`plans/02`](plans/02-solver-validation-viewer/README.md), and M2's halo gate is the tripwire |
| Add a face to the mesh, or ask why a wall's side is drawn once | `src/ground_mesh.loft::ground_side_faces` and [`plans/25`](plans/25-the-terrain-mesh/README.md) § M1 — one quad per edge where a column stands above its neighbour, `if hh <= nh { continue; }` (`@X046`).  ⚠⚠ **Both halves of that guard fail INVISIBLY**: no guard draws every faced edge twice and the second copy is back-facing (pixel-identical, twice the mesh); `<` instead of `<=` grows a zero-area sliver at every hex boundary in the world (also pixel-identical).  So it is gated as four COUNTS — **6** for a lone wall, **10** for two adjacent, **0** for flat ground, **5 and 6** across a step — and the step fixture is the only one that can see the face drawn by the WRONG side.  ⚠ **Absent is zero**: a sparse sea-default world means a wall at the painted region's edge has a 0 m neighbour and gets its quad.  ⚠⚠ A quad's NORMAL comes from the two hex CENTRES and its WINDING from the corner RING — two facts, computed differently, and the test asserts they AGREE, because normals-out-triangles-in draws nothing under `GL_CULL_FACE` with every normal reading healthy |
| Mesh a TILE, or ask which tiles an edit invalidates | `src/ground_mesh.loft::ground_chunk_mesh` (one tile, one palette kind) over `src/mesh_chunks.loft` (the domain), and [`plans/25`](plans/25-the-terrain-mesh/README.md) § M2.  ⚠⚠ **`mesh_chunks_touched` is ONE mechanism read two ways** — as an edit it is the dirty rule, summed over the paint it is the tile list; derive them separately and they disagree about a tile edge.  ⚠⚠ **No `ChunkField`, and that is a change of plan** (`@X075`): `collect_dirty_inputs` SKIPS a dirty chunk owning no cells, which with a one-hex ring is a tile that still has sea to draw, and `mark_borders` steps CHUNK coords rectangularly where only `lat_neighbour` may step a coordinate.  ⚠ `gridmesh::chunk_of` still earns its keep — its `chunk_div` FLOORS, which a hand-rolled `>>` gets wrong left of the origin.  ⚠ The kind list is ASCENDING because it is an upload order |
| Ask what draws SEA, or why the mesh is wider than the paint | `src/mesh_chunks.loft::mesh_hex_drawn` and [`plans/25`](plans/25-the-terrain-mesh/README.md) § M2 — ⚠⚠ **the drawn region is the painted set PLUS A ONE-HEX RING** (`@X075`), because `painted.loft` ERASES a hex painted sea: mesh only what is stored and an erased region is a **hole in the ground at exactly the height of the land around it**, which no side quad covers (sea and grass are both 0 m).  ⚠ **Its limit is asserted, not assumed** — a gap wider than two hexes still holes, and `test_a_gap_wider_than_the_ring_still_holes` pins it so closing it is deliberate.  ⚠ The two rejected candidates each fail on a principle: a bounding box is unbounded cost on a sparse world, and the tile's full extent makes how far the ocean reaches a function of where the tile boundaries fell.  ⚠ The real answer is water's **DROP**, which the palette already carries and nothing reads — a SIMULATION decision, so [`plan 02`](plans/02-solver-validation-viewer/README.md)'s |
| Compare two builds of one mesh | `src/mesh_crc.loft::mesh_crc` — the geometry folded to one integer, because a COUNT cannot see a mesh with the right number of vertices in the wrong places and a golden AGREES WITH A SHEAR.  ⚠⚠ It folds the **TRIANGLES** as well as the vertices, where moros's port folds vertices only: `ground_top_face` SHARES its six rim vertices between the six fan triangles, so **M0's reversed fan moves no vertex at all**.  ⚠⚠ **An empty mesh folds to 0**, so every equality needs a non-zero floor.  ⚠ The SCALE is a tolerance and it ROUNDS rather than truncates — every palette height sits exactly on truncation's discontinuity — and reaching that branch needs its own test, because every comparison in a gate runs identical arithmetic on identical inputs.  ⚠ It belongs in `mesh3d`; the trigger to move it is a second non-test caller |
| Gate anything that is DRAWN by GL | `scripts/validate_gl.sh` over `src/gl_gate.loft` (BUILT, plan 25 M3) and [`docs/RENDERER.md`](docs/RENDERER.md) § R4 — `xvfb` → GL → `gl_screenshot` → `imaging::png` → **`classify_canvas` itself**, measured at **zero** colour drift for a blit (`@M002`) and for a SHADER (`@M026`).  ⚠ Render FLAT UNLIT: a shaded frame turns one palette colour into a range and `unknown` stops meaning "fault".  ⚠ Never loosen to nearest-colour — that discards the property R0 measured.  ⚠⚠ **And never gate on COUNTS alone**: a mirrored world passes every band (`@M027`), so add a LANDMARK against `camera_screen` |
| Ask what makes the GL gate's claim TOTAL rather than a share | `src/gl_gate.loft::gl_entity_pixels` + `gl_case_a_defended_base` (plan 20 A5) — an entity colour is deliberately not a palette colour (`@X092`), so it lands in `unknown` exactly as the background does and the gate sums the ten by NAME: `unknown - entity pixels == 0` over a frame-filling world means **every pixel is a palette colour, an entity colour or the clear colour, and nothing else**.  ⚠ The roster is drawn for ALL THREE fixtures, so `the-ground`'s and `an-island`'s `other == 0` also say *nothing was drawn for a base with no roster* |
| Add a GL fixture, or ask why `other == 0` is a legal thing to ask | `tests/gl/*.keys` + a case in `src/gl_gate.loft` — ⚠ a fixture with no case there is REFUSED by name, because `.keys` has no GL verb and must not grow one (`@X076`).  ⚠⚠ **`other == 0` is only legal for a fixture that FILLS the frame** (`@X077`): the clear colour is magenta and deliberately outside the palette, so a hole and a horizon both read as faults — a fixture that cannot fill the frame asserts `an-island`'s shape instead (`other` large, and every one of those pixels EXACTLY the clear colour).  ⚠ A LANDMARK must be a FLAT hex in flat surroundings: a column draws its sides in its own colour and they sit between the top face and the screen centre, 29 px off for a 5 m wall against 0.6 px flat |
| Pick a colour for anything DRAWN OVER the world, or add a colour RAMP | ⚠⚠ **A ramp is not a colour — it is one per value it can reach**, and every one of them has to be nobody else's (`@X098`, `@M043`).  The wallet's amber-to-red ramp passes through `#ff8000` at exactly 134 points, which WAS the scout's, so the HUD would have been painted in a robot's colour at one value of the wallet and the GL gate would have counted it as a robot.  ⚠ Make the ramp a function of an INTEGER the UI already shows and the reachable set is finite, so the test can sweep it exhaustively instead of sampling — over a float it could only spot-check, *and the spots would have been the two clean endpoints* |
| Add something to the HUD | ⚠⚠ **Read [`docs/DESIGN.md`](docs/DESIGN.md) § HUD first — it almost certainly says no.**  Its entire numeric HUD is *one corner number, the wallet*, and it names what it refuses: no wave-number, no inter-wave countdown, no minimap, no boost cooldown bar (`@X097`).  Everything else is DIEGETIC and already built — rotors show boost (`@X091`), the canopy shows cargo (`@X090`), a tower's top shows whether it has one.  ⚠ The build is `hud.loft` § The wallet + `play_view.loft` § The one number; a second number is a design change, not a feature |
| Draw TEXT anywhere in dryopea | you cannot, and the reason is worth knowing (`@X097`): `graphics::draw_text` rasterises through `#native rasterize_text_into`, which answers *"native function not loaded"* under `loft test`, and it needs a FONT FILE this repo does not have.  **A thing nothing headless can draw is a thing no test and no `snap` can see.**  ⚠ Numbers go through `hud.loft`'s seven-segment digits, which are `fill_rect`s every gate already reads; `picker.loft` reached the same conclusion in plan 01 |
| Draw the game in the WINDOW, or ask what a play frame costs | `src/play_view.loft` (plan 19 P6) — `play_view_sync` then `play_view_draw`, which is all `main.loft` does in play mode.  ⚠ Measured in a real window: an idle frame is **5 ms**, the roster's per-tick upload **10 ms**, entering play mode one **218 ms** cold bake, and a one-hex terrain change **7 ms** (`@M041`).  ⚠⚠ **A play frame is never cached** — the camera eases toward the vehicle whether or not the vehicle moved |
| Ask how the renderer knows the TERRAIN moved | `src/play_view.loft::mesh_watch_dirty` (`@X095`) — it DIFFS a snapshot of the height layer, because a live session breaks walls and drops bodies without the caller ever touching `paint`.  ⚠⚠ It is exact only because **every terrain change a tick can make moves the HEIGHT LAYER** (`break_structure` raises masonry BEFORE it repaints), and `tests/19_p6::test_the_maintained_ground_equals_a_cold_rebuild` asserts that against a played base.  ⚠ A dirty list on `HeightLayer` is the refused alternative: it is not state, and `state_diff` would have to pretend it is |
| Ask why the ground tile is 8x8 | `@X096` + `@M041` — plan 25 M4 measured that 8x8 buys 8.6x on an edit for 12x the draw calls and changed NOTHING, naming *the phase that wires GL into `play_mode`* as its inheritor.  Plan 19 P6 measured the other half in a real window: **96 tiles draw as fast as 8**, so the draw calls are free and the edit decides.  ⚠ At 32x32 a body falling cost 54-112 ms and grew with the map; at 8x8 it is a constant 7 ms.  ⚠ Both fixtures that encoded 32 are now derived from `mesh_chunk_span()` and pass at 8, 16 and 32 |
| Add a GL draw call, or ask why the window goes BLACK after pressing P twice | the GL STATE.  `ground_gl_draw` and `entity_gl_draw` each turn depth testing and culling ON and neither turns them off; the editor's picture is a full-screen TEXTURE BLIT and draws **nothing** under a depth test nobody cleared — 691 200 black pixels of 691 200, measured.  ⚠ `play_view_draw` restores it, and `gl_gate.loft` § The GL state a play frame leaves is the gate, which no fixture could carry (each draws one frame and exits) |
| Draw the ROSTER, or ask why an entity is not in the frame | `src/entity_view.loft` (BUILT, plan 20 A5) — `entity_bake(m, ps, pal, pw, mw, ws, units, cls)`, one drawn class per call.  ⚠⚠ **Nothing here is state**: a robot is in the frame because `ws.enemies` has a live one, so a thing not drawn reads as ZERO and that is an assertion rather than a description.  ⚠ A new drawn thing is a row in `entity_colour` and a branch in `entity_bake` — never a list beside the roster.  ⚠ Gated pure in `tests/20_a5_the_frame.loft` and in pixels by `tests/gl/a-defended-base.keys` |
| Pick a colour for anything that is DRAWN | `src/entity_view.loft::entity_colour` — and run `entity_colours_distinct` / `entity_min_distance2` before believing it (`@X092`, `@M040`).  ⚠⚠ **An entity colour must be none of the twelve palette colours**, or `classify_canvas` counts it as GROUND and every band absorbs it silently.  ⚠ And *distinct* is not *distinguishable*: `PROXY_ART.md`'s player white measured **224** squared-RGB from `waterfall` and its crew silver **768** from `rock`, which is the same colour on a screen.  The floor is 3264 and a test pins it |
| Turn an entity to face somewhere | `src/part_mesh.loft::part_emit_facing` over `entity_view.loft::facing_turns` — a part is authored facing NORTH and a bearing of 0 is EAST, so the conversion is a QUARTER TURN and not a scale.  ⚠⚠ `frame_place` rotates the BASIS as well as the origin, and at rest every bone's basis is the identity — so only a joint off zero can see a wrong one, which is why `tests/20_a5` turns a vehicle with its canopy OPEN.  ⚠ The bearing itself is `lattice.loft::lat_bearing`, which `vehicle_facing` also delegates to |
| Ask why an entity draws NOTHING while every count looks right | the WINDING.  `ground_gl.loft` and `entity_gl.loft` both turn `GL_CULL_FACE` on, so a triangle wound the wrong way is invisible — and `@D005` is the worked example: half of every box in the catalogue wound inwards, past 42 tests, changing no count, no vertex, no normal and no `mesh_crc`.  ⚠ The instrument is a cross product against the stored normal, and `tests/20_a5::test_every_triangle_winds_with_its_normal` sweeps every catalogue entry |
| Ask what a tower's top is, in the art | `docs/PARTS.md` § D3 — it is a SOCKET, and the simulation has had one since plan 17 T2 (`tower_detach_top` / `tower_mount_top`, which refuses an occupied tower).  ⚠ Which pose a tower draws in is ASKED of `TowerState`, never a second flag beside it |
| Ask which POSE an entity is drawn in, or add a joint the simulation drives | `src/pose.loft` (built, plan 20 A4) — ⚠⚠ **read the sim, never a second flag** (`PARTS.md` § D3): `tower_has_top` for the tower's top, `cargo_carrying` for the canopy, `vehicle_speed` for the rotors.  ⚠ A new joint needs a bone in `catalogue.loft`, a number read out of state that already exists, and a row in `pose_hover_unit` — never a field on the entity.  ⚠⚠ **A gate on a pose emits BOTH states and asserts they DIFFER**; *it rendered* cannot see a pose looked up under the wrong key.  ⚠ **And a body of revolution cannot show that it is turning** under the flat-unlit colour (`@M039`), which is why a rotor is two crossed blades and why the silhouette is the instrument rather than `mesh_crc` |
| Write/edit a `.loft` file | Loft language conventions: see § Important conventions above + loft's own `loft-write` skill |
| Run the editor | `loft src/main.loft` |
| Author any part of a `WaveState` in a `.keys` file | `src/script.loft::script_author` (plan 18 S1b) — `tower` / `object` / `spent` / `player` / `member` / `pending` / `cursor` cover the layers and the condition fields no play verb reaches, so the vocabulary is TOTAL over the state.  ⚠ They AUTHOR and never simulate: a tower authored black fires nothing and an object authored into the player's hands checks no reach.  ⚠ SEVEN command words rather than one `set` with a subject, because `keys_schemas` keys a coordinate's position on the FIRST token — one `set` row would silently rewrite `set member 0 on 0.5` as if `0 on` were a hex |
| Author ONE enemy in a `.keys` file | `place <q> <r> <class> [heading]` (plan 18 S1a), plus `stand <i> <secs>`, `banked <i> <hexes>` and `dead <i>` for the three fields a placement leaves neutral; `hit <i> <hp>` is the fourth and already existed.  ⚠ A bare `place` is HEALTHY, WALKING and carrying NOTHING — `taken`, `stand` and `progress` are all zero-neutral, and getting any of them backwards spawns a corpse that has not finished arriving while every "the wave is there" assertion stays green.  ⚠ `dead` deposits NO body: it authors the ledger and never the consequence, so `wave_deaths` stays the one death path.  ⚠ `banked` arrived in plan 23 K2b, because `18_s2`'s round trip went red the moment a class walked at a speed whose carry is not zero — until then the field had no setter and nothing in the repo could tell |
| Add a `.keys` verb that takes a hex | `src/script.loft`, AND a row in `src/convert.loft::keys_schemas` + the vocabulary list in `tests/09_c5a_converter.loft`.  A missing schema row is silent: the converter passes an unknown command through untouched |
| Place or restore a marker of any kind | `src/markers.loft::place_marker` (and `history.loft::place_marker_and_record`) — the ONE dispatch.  ⚠ Sidecar load, undo and redo each used to fall through to SPAWN, so a kind they had not learned about arrived as a wave source with a heading |
| Add a marker kind | append a constant in `markers.loft`, bump `MARKER_KIND_COUNT`, add a row to `place_marker` + `marker_kind_name`.  ⚠ The editor's place-kind CYCLE grows, so every `.keys` script that cycles back to spawn needs another press — B5a paid that for nine scenarios |
| Change what a key does | `src/bindings.loft::editor_actions` — the ONE table.  Both the GL loop and every `.keys` script read it, so a change is visible to the gate.  Never add a `gl_key_pressed` |
| Add a PLAY action (a key that drives the game) | a row in `src/bindings.loft::editor_actions`, a field on `EditorInput`, a line in `editor_input_from`'s `playing` branch, and the effect in `src/play.loft::play_actions`.  ⚠ **Never in `editor_step`** — the editor's seam has no roster, and `tests/19_p2_the_keys.loft` § The editor seam is blind to the play fields is what keeps it that way.  ⚠ `tests/09_i1_bindings.loft` pins the table's ROW COUNT, so a new row goes red there by design |
| Ask why WASD does two different things | `src/bindings.loft::editor_input_from` — `playing` fills the pan set OR the drive set (plan 19 P2).  ⚠ `DESIGN.md` § 11 gives movement to WASD and § 12 has the play camera locked, so the two never coexist.  ⚠ Merge them and every `at` in the gate drives whatever vehicle its scenario parked.  ⚠ Who ANSWERS it is `play_mode(ps)` in the window (P3) and the ACTION NAME in a script — a `.keys` file has no mode, which is why `do toggle_play` is refused |
| File an outbound loft request | [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) |
| File a dryopea-internal bug | [PROBLEMS.md](PROBLEMS.md) (`@D<NNN>` convention) |
| Understand library extraction | The `hex_*` family is published — `loft api --registry` |
| Change how enemies move | [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) — the whole spec.  [plans/11](plans/11-flow-field/README.md) is what it costs to build |
| Step a hex coordinate | `lattice.loft::lat_neighbour`.  ⚠ Never a `+ 1` on a `q` or `r`, and never a constant `(dq, dr)` table either — odd-r deltas depend on row parity, so no such table exists |
| Tell GEOMETRY from LABEL SPACE (any coordinate change) | Ask what the site depends on. **Geometry** ("where on screen?") depends on the lattice alone. **Label space** ("which cell?") is only meaningful relative to how the DATA is labelled — `paint_line`, `enemy_tick`, the flow BFS and every `.keys` literal are label-space.  Plan 09 is the worked example: the two had to move in separate phases (C3, then C5), and converting one label-space site alone turns `scripts/validate.sh` red for a reason that is not a defect |
| Ask whether an enemy may MOVE somewhere | `src/passable.loft::can_step` — the rule, an edge.  Never `walk_ground` on its own, and never the destination's height on its own |
| Ask whether an enemy may BE somewhere | `src/passable.loft::can_occupy` — what a position can say with no history.  The measurement's rule; never the field's node filter |
| Raise a hex at runtime (bodies, broken walls) | `src/height.loft` — the rubble layer: a rise above what the palette paints, plus what it is made of.  Lives on `WaveState`, never saved.  ⚠ Shrinking a pile to nothing REMOVES its entry — a 0.0 m pile would still read as a rubble surface |
| Ask what a hex's SURFACE is (vs what is painted on it) | `src/passable.loft::hex_ground` — rubble where a pile stands, the painted kind otherwise.  `painted_ground` is the other half and is what `hex_height` adds the layer to.  ⚠ Need it as a KEY rather than as an entry (grouping, a mesh per kind)?  `hex_surface_index` is the same question given back as the palette index (plan 25 M0) |
| Ask whether a hex is free of enemies | `src/occupancy.loft` — a separate question from passability, and a count rather than a flag |
| Ask who on the PLAYER's side is standing on a hex | `src/occupancy.loft::blocker_at` over the map `spawn.loft::wave_blockers` builds each tick — it answers WHICH vehicle, because the blocker damage has to land on the one in the way.  ⚠ Never a per-vehicle predicate: `vehicle_on` was deleted for being the second door |
| Bring a lost crew member back | `src/spawn.loft::wave_drop` at the core — and NOTHING else does it (`DESIGN.md` § 9: *"retrieval is the only way back"*).  ⚠ The clock is exactly 90 ticks and the epsilon in `helper_recover_tick` is what keeps it 90 rather than 91 |
| Take a crew member out of the run | `src/helper.loft::helper_wreck` — and the tick is the only caller, at the end, beside the deaths and the breaks.  ⚠ It is TWO effects at one site since plan 15 C1: the helper goes down AND a carryable wreck appears where it stood.  ⚠ Nothing brings it back yet: retrieval is plan 15 C2 |
| Pick something up, carry it, put it down | `src/carry.loft` — one record per object with an `owner`, so conservation is structural.  ⚠ Never add a "carried" field to a vehicle beside it: a slot on the carrier and an owner on the object are two facts that can disagree |
| Add a new kind of carryable thing | a `CARGO_*` constant plus what a valid destination is and what arriving there does — and NOTHING in the carrying path.  ⚠ A kind that needs new carrying code has broken `plans/15` § C0.4.  ⚠ **The second consumer showed the contract's edge** (plan 17 T2): a tower-top has TWO destinations and `cargo_destination_ok`'s `(kind, at, core)` can only state one, so the tower-mount half lives in `spawn.loft::wave_drop` where the markers are.  Look in both places |
| Take a tower's top off, move it, or evacuate it | `src/spawn.loft::wave_take` / `wave_drop` (plan 17 T2) — `tower.loft::tower_detach_top` / `tower_mount_top` are the primitives.  ⚠ The magazine is the TOP's, carried as `CarryObject.subj`, so a round trip is not a repair.  ⚠ `tower_mount_top` REFUSES an occupied tower; the hot-swap is COMPOSED out of a detach and a mount at the call site, which is what conserves the count.  ⚠ A loose object on the ground beats detaching, and mounting beats evacuating — both ambiguous presses decided in `plans/17` § T2 |
| Ask what a blocked enemy attacks | `src/spawn.loft::enemy_target` over `flow.loft::flow_desire` — per route, never a global "nearest wall" |
| Ask whether a tower can HIT something | `src/tower.loft::tower_sees` — one straight line from the eye over `hex_height`.  ⚠ Never a "which kinds block" table: a `wall_high` beside the tower does not block and a `wall` near the target does |
| Ask why a tower is not shooting | `src/tower.loft::tower_sight_fault` names the hex, the two heights and how far along the line it sits; `tower_black` is the other answer |
| Bring a spent tower back | `src/tower.loft::tower_repair_tick` — 20 s of a vehicle standing within one hex, banked ON THE TOWER so a relief crew finishes what a lost one started (plan 17 T1).  `spawn.loft::wave_repair` is the tick's turn, at the END beside the salvage.  ⚠ No key is pressed — repair is a POSITION, so a player parked beside a tower is working on it whether it meant to or not.  ⚠ A FIRING tower REFUSES (`DESIGN.md` § 7), which is what makes upkeep a timing decision.  ⚠ It refills the MAGAZINE and never the CHARGE — get that wrong and the base reads exactly like an undefended one |
| Ask how much a wall has left | `src/damage.loft::structure_hp` — max minus taken.  ⚠ 0.0 answers BOTH "broken" and "never a structure"; ask `structure_breakable` first if you need to tell them apart |
| Ask how strong a wall hex is | `src/damage.loft::structure_max_hp` — the kind's figure scaled by `brace_of`.  ⚠ `numbers.json`'s wall_hp (100) is the BRACED number; a lone plug in a corridor is a STUB and gets 15 |
| Break a wall | `src/damage.loft::break_structure` — the one site, and it does both halves.  The tick calls `damage_resolve` AFTER every enemy has moved, so a breach belongs to the NEXT tick |
| Clear rubble / collect after a tower | `src/vehicle.loft::salvage_at` — the rule, taking a HEX, so the player and every helper read one implementation (`vehicle_salvage` / `helper_salvage` are the two doors).  The counter-play to `ENEMY_MOVEMENT.md` § Bodies are terrain.  ⚠ A crew inside a SEALED base can only reach the ramp by BOOSTING out (V4): the ramp forms outside the wall and an idle vehicle climbs 0.4 m — and no helper has a boost |
| Give a mover a climb that changes while it lives | `src/passable.loft::can_climb` — the rule with the climb passed rather than looked up.  ⚠ Never widen `climb_limit(kind)`: it is a CLASS lookup and a convenience for callers that have a kind.  `vehicle_climb` is the worked example |
| Ask what STARTS the wave list | `src/spawn.loft::wave_provoke_step` — a live vehicle standing on a spawn marker `WAVE_1_PROVOCATION_HEXES` (12) or more from the core, read at the TOP of the tick and fired ONCE (plan 16 W3).  ⚠ Two thresholds: under 10 a marker is silenced entirely, 10–11 it sends enemies and cannot be poked, 12+ it does both — the middle band is what makes the distance test a rule rather than a restatement of "is this marker active".  ⚠ Never an occupancy test: a wave spawns ON its marker, so "is anybody here" lets wave 1 provoke wave 1 |
| Ask how far an enemy moves in a tick, or make a class FASTER | `src/spawn.loft::enemy_speed` for the CLASS's rate (plan 23 K2b — scout 2.5, miner 1.0, everybody else 1.5), then `enemy_bank` for what a timestep owes it: `speed × elapsed` banked per enemy in exact integer units (plan 26 L2), whole hexes released to `enemy_step`.  ⚠ **A tick is no longer a hex**: `TICK_SECONDS` HOLDS the timestep at one regular's hex, it does not force it (`@X058`).  ⚠ **The epsilon is GONE since plan 26 L2** and a new speed no longer has to be picked against it — `@M013`'s reading is history: 1.0 / 1.2 / 1.8 / 2.0 / 2.5 each lost a hex every forty ticks without the guard, and 1.5 / 2.25 / 3.0 could not see it at all, which is why 2.5 was picked partly on testability (`@X063`).  ⚠ What DOES constrain a new speed is `bank_rate`'s millionths: a rate with more than six decimals is quantised (`@X080`).  ⚠ The lookup is at the CALL SITE and not in the bank, because *"a damaged robot moves slower"* makes speed a property of a CONDITION (`@X061`).  ⚠ A hex the ground refuses is SPENT, not re-banked — the opposite of `helper_bank`, and deliberate (`@X059`) |
| Ask whether a mover survives a SHORTER tick, or change `TICK_SECONDS` | ⚠⚠ `tests/26_l0_the_timestep_sweep.loft` and `@M030` — **every mover now holds its rate at seven tick lengths**, and until plan 26 L2 the PLAYER did not (`@D003`, FIXED 2026-08-17): `vehicle_hexes_this_tick` truncated and `Vehicle` had no carry, so the player read **180 / 120 / 180 / 0 / 0 / 0 / 0** hexes a minute against a true 180 and stopped moving entirely at any tick under 250 ms.  ⚠ **So the movers no longer block a shorter tick and `plans/22`'s field cache is the remaining prerequisite** — the one-shot TIMERS followed at L3 and are covered by `tests/26_l3_the_timers.loft`.  ⚠⚠ The instrument is a CROSS-PRODUCT and it has to be: sweep the tick length AND every mover — `@M013` sweeps speeds through movers that carry, `23_k2a` sweeps the tick through an ENEMY, and neither can see this.  ⚠ Timers are a different FAMILY and have their own sweep — `tests/26_l3_the_timers.loft` and `@M033`, where the two that never had an epsilon were the broken ones (`@D004`) |
| Add a one-shot DURATION, or ask why a timer needs no epsilon | `src/tick_timer.loft` (plan 26 L3) — `timer_arm(t, units)` then `timer_spend(t, elapsed)`, which answers whether THIS call fired.  ⚠⚠ **Both directions come off one number** (`timer_left` is `total − spent`), which is why the direction problem is gone rather than guarded.  ⚠ **It is not a `Bank`**: a bank fires again for ever and carries its residue into the next arming (a 5 s cooldown costs 8 ticks then 7) — measured, in `tests/26_l3_the_timers.loft`.  ⚠ A duration is defined in UNITS and the seconds constant is derived, and the units constant is a LITERAL because loft refuses a temporary in a const initialiser |
| Ask whether a TIMER survives a shorter tick | ⚠⚠ `tests/26_l3_the_timers.loft` and `@M033` — every one-shot timer now holds its duration at seven tick lengths, and until plan 26 L3 the two with NO epsilon did not (`@D004`).  ⚠ **The guarded sites were the healthy ones**, which is the opposite of what `plans/26` § 2 predicted; direction is not the discriminator, a guard is |
| Pause, fast-forward, survive an alt-tab, or drive one clock from another | `src/tick_clock.loft` § THE POLICIES (plan 26 L4) — `clock_set_rate(clk, num, den)` (a RATIONAL: pause is `0/1`, and it scales every DURATION door and **no** count door, so a paused game still runs its own tests), `clock_advance_capped(clk, dt, max)`, `clock_pump(clk, now)`, `clock_drive(slow, fast, n)`.  ⚠⚠ **A cap DROPS the excess and must never DEFER it** — clamping the answer while keeping the backlog answers fewer ticks on the stalled frame too and then runs the simulation behind the wall for ever (**4** ticks vs **24**, `@M034`).  ⚠ **dryopea consumes none of it**, deliberately (`plans/26` § FLEXIBLE): a policy skipped because THIS consumer does not need it is how the next consumer comes to write it again.  ⚠ `clock_drive` is a NAME rather than a fix — `n * step` is already exact in integers, which is the opposite of `play_ticks` |
| Advance the game by TIME, or change the tick's length | `src/tick_clock.loft` (plan 26 L1) — `clock_advance(clk, units)` for a duration, `clock_step(clk, n)` for a count, over an integer accumulator.  ⚠ A mover's share of that time is `src/tick_bank.loft::bank_gain` (plan 26 L2), and `enemy_bank` / `helper_bank` / `vehicle_bank` are its three doors.  ⚠⚠ **`TICK_STEP_UNITS` is now the ONE definition of the tick and `TICK_SECONDS` is DERIVED from it** (`spawn.loft`), so change the step and every rate follows — but ⚠ **a step is only safe if `TICK_SECONDS` comes back bit-identical**, which is why the base unit is 1/3 µs and not µs (`@X079`): the recommended 666 667 µs step leaves the 654 measurements untouched and breaks **17 tests** (`@M031`).  ⚠ `main.loft` hands down integer µs; `play_advance` / `play_step` are float wrappers that ROUND, and truncating there would put `19_p1`'s 602 back one layer out |
| Ask why a fresh wave is not moving | `src/spawn.loft::enemy_standing` — the pre-walk window (plan 16 W2), 8 ticks at the marker.  ⚠ Spent ONCE per tick by `wave_stand`, at the END beside `helper_recover_tick`; the predicate only asks.  ⚠ A standing enemy does not move, attacks nothing and blocks nobody — but is NOT immune, which is what "stand visible" means |
| Advance the GAME | `src/play.loft` — `play_ticks(ps, s, n)` for a COUNT, `play_advance(ps, s, seconds)` for elapsed time, `play_step(ps, s, input, seconds)` for a whole frame.  ⚠ Never call `wave_tick` directly: `play_one_tick` is its one caller, and a second one is a second game with the same numbers on it.  ⚠ And never spell a count as `n * TICK_SECONDS` — it is one tick short for 602 of the first 1000 `n` |
| Ask whether a session is LIVE, or start one | `src/play.loft::play_mode` / `play_set_mode` (plan 19 P3).  ⚠ **It gates the CLOCK and never the seam**: `EditorInput.in_playing` says what the KEYS mean this frame, `PlayState.playing` says whether wall time reaches the simulation.  Gate `play_step`'s seconds on either and P1/P2 go red — a scripted frame's time is the SCRIPT's business.  ⚠ The window spends it through `play_frame_seconds`, which is a function rather than an `if` in `main.loft` because an entry point is compiled by nothing |
| Ask whether the run is over | `src/wallet.loft::wallet_broke` — the wallet at zero, and the ONLY end state.  ⚠ Never `core.hp`: it is `null` by design |
| Understand the DIFFICULTY CURVE's shape | [docs/DESIGN.md](docs/DESIGN.md) § It shoots TOWERS — the first real challenge.  Early = a RUSH (volume).  Then the combat boss, which is the first enemy that makes the player POORER rather than merely closer to losing — and the first that invalidates a LEARNED OPTIMUM (the tight funnel that denied a 2×2 repair platform is worthless against something that shoots from outside) |
| Judge what a wave's COMPOSITION is worth | [plans/24](plans/24-the-siege-front/README.md) § W2 and [`docs/ENEMY_MOVEMENT.md`](docs/ENEMY_MOVEMENT.md) § The siege front is the WALL's width — **94 / 101 / 116 / 122 / 126** for twelve robots screened by four of a faster class (`@M020`).  ⚠⚠ **A wave is worth its front class PLUS what the front cannot COVER**: the front is the wall FACE's width, so four screens against five hexes leak exactly ONE miner — worth nothing behind a builder, thirty-nine ticks behind a harvester.  **The screen is arithmetic: bodies against face width.**  ⚠ So price a wave by what is in front of it AND by how much of the face that front can actually cover.  ⚠ [plans/23](plans/23-the-small-robots/README.md) § K3's `@M018` (*a mix is worth its fastest class and no more*, four scouts buying outright immunity) is the SUPERSEDED reading — quote `@M020` |
| Judge whether a DEFENCE is worth building | [plans/12](plans/12-combat-resolution/README.md) § B7 — three scenarios that differ only in their defences, and the measured clock (69 / 112 / 128 since plan 16 W2).  ⚠ A sealed wall nearly doubles it; a wall with a GATE buys nothing at all; and a tower now ADDS 16 ticks where it used to cost 9 — because the pre-walk window moved its kills off the wall's foot, so the ramp that used to bury it no longer forms there |
| Judge whether fetching a lost crew member is worth it | [plans/17](plans/17-tower-hot-swap/README.md) § T3 — **+76 POINTS** over the errand control, on a base with upkeep where nothing falls (~45 / ~41 / ~117 points left).  ⚠ The currency is the WALLET, not the clock: a base that can recover stops falling, so the clock saturates and *points left* is what "how well did you do" means.  ⚠ Earlier readings are history rather than alternatives: [plans/16](plans/16-the-wave-system/README.md) § W4 — **247 / 248 / 248** on a base where the crew member genuinely does come back (tick 187), so it is worth ONE tick.  ⚠ The reason is no longer "the base ends first" (that was [plans/15](plans/15-the-carry-model/README.md) C3's 93/87/87): the JOB is gone by the time they return — the gate is worth 53 ticks while the wave is outside and nothing while it is on the core.  ⚠ The middle run is the control that keeps the drive and the carry apart |
| Judge whether a TRANSPLANT is worth doing | [plans/17](plans/17-tower-hot-swap/README.md) § T3 — **+3 ticks at best, −50 if the donor was firing.**  ⚠ A tower close enough to donate cheaply is close enough to be shooting, which is `DESIGN.md` § The opportunity-cost layer measured.  Its payoff needs swap pits and STRAIN — pulling a top BEFORE it is spent — and neither is built |
| Find out why a base cannot be played to its end | [plans/16](plans/16-the-wave-system/README.md) § W4 — **the tower's 30-shot magazine**.  The authored list is 205 robots = 6150 HP and a tower is 300 HP for the whole run, so seven waves needs 21 perfectly-aimed towers; the best band the suite can build plays FOUR and falls at 321 with every tower black.  ⚠ Nothing lets a base RECOVER between waves, so the lull is a pause rather than a repair window — and that is what blocks retrieval, hot-swap and every mechanic priced across waves.  [plans/17](plans/17-tower-hot-swap/README.md) is the fix |
| Judge what another CREW MEMBER is worth | [plans/14](plans/14-helpers/README.md) § Status — three scenarios that differ only in their crew lines, and the measured clock (⚠ **123 / 135 / 138** since plan 16 W2, from 77 / 214 / 242).  ⚠ A roster buys COVERAGE, not throughput — but the base can now barely express it: the pre-walk window means far fewer ramps form for a crew to clear, so the whole spread is 15 ticks where it was 165 |
| Hurt or kill an enemy | `src/spawn.loft::enemy_hurt` lands damage and never kills; `wave_deaths` (the tick's, after the move loop) is the ONE death path, so B5's tower and a script's `hit` cannot drift.  ⚠ A fatal hit is followed by one last STEP — the tick moves before it kills, so the body lands one hex down the route from where the shot landed |
| Validate the GAME (not a function) | `scripts/validate.sh` — then [plans/08-game-validation/README.md](plans/08-game-validation/README.md) |
| Check a change did not cost anything | `tests/11_f8_the_tick_budget.loft` — a RATIO gate, because a copy changes no behaviour and no other test can see it.  ⚠⚠ **For anything that BUILDS an artefact, count the artefact instead** (`tests/25_m4`, `@M029`): the clock could not carry M4's claim at all — two identical back-to-back calls differed 5.4x — while the floats a mesher uploads are deterministic and are exactly what its cost regression looks like |
| Ask what a mesh EDIT costs, or wire the GL path into play mode | [`plans/25`](plans/25-the-terrain-mesh/README.md) § M4 and `@M028` — ⚠⚠ **a one-hex edit re-bakes ~4 000 hexes' worth**, because `mesh_chunks_touched` names whole tiles and each is re-meshed whole.  At the shipped 32×32 that is **335 ms** against a 50 ms paint stroke (`PAINT_DEBOUNCE_US`) and a 667 ms tick.  ⚠ The sweep is in the plan: 8×8 buys **8.6×** and costs 12× the draw calls, which `loft test` cannot price — so M4 measured it and changed NOTHING.  ⚠ Nothing pays this today: M3 gave the ground a gate, not a window |
| Make the SIMULATION cheaper | [`plans/22`](plans/22-the-field-cache/README.md) — ⚠ the field, not the roster.  `flow_sweep` is **17.6% self and ~69% with its passability family** (re-profiled 2026-08-17, third reading in agreement), it is UNBOUNDED, and it is only read inside the 25-hex bubble, so ~60% of every sweep is never looked at.  ⚠ The field is a pure function of `(pal, pw, hl, climb, core)` and its invalidation surface is **two functions** (`height_raise` / `height_clear`) plus `paint` — so caching is exact, and `11_f8::test_the_field_a_tick_uses_equals_a_fresh_build` is the gate, written in advance and currently vacuous |
| Judge a simulation-LOD idea (coarser away from the player) | ⚠ **Granularity must NOT follow the CAMERA** — if it does, where the player looks changes the outcome, which is unfalsifiable from inside.  The boundary is the interaction radii (tower range 15, bubble 25, nibble reach 1, salvage reach 1, blocker = same hex), which are stable under camera movement.  ⚠ And ticking distant things every N ticks with N× movement is the `n × TICK_SECONDS` defect again — bank progress, never multiply it.  [`plans/22`](plans/22-the-field-cache/README.md) § What this plan does NOT build carries the trigger |
| Find out what the SUITE spends its time on | `LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` — § Profiling the suite.  Read the op count, never the wall clock |
| Speed up frame measurement further | `src/measure.loft::classify_canvas` is already written for the pixel count — do not "tidy" it.  ⚠ And it is no longer where the time goes: `classify_canvas` + the `Canvas` primitives are **~5%** (2026-08-17), against ~69% for the distance field.  [`docs/PROFILING.md`](docs/PROFILING.md) |
| Find out what the SUITE spends its time on, or optimise anything | [`docs/PROFILING.md`](docs/PROFILING.md) — ⚠ re-profile first and quote the DATE; the reading in this file has inverted once already |
| Look up what a `src/` file owns before editing it | [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — the full listing, plus the key data structures.  ⚠ Each `.loft` file's own header is the source of truth |
| Add a script to the gate | drop a `.keys` in `tests/scripts/` — the sweep finds it.  ⚠ every file there must play GREEN; a run that must FAIL belongs in a test as an inline string |

## Branch policy

### Current phase — pre-game-shippable: commit + push directly to `main`

**Until a runnable game build exists, direct commits to `main`
are the normal flow.**  The repo is small, single-author, and
the cost of branching ceremony outweighs its benefit while the
foundation is being laid.  Commit locally, push when the user
asks — no automatic pushes.

**Trigger for switching to the formal flow below:** the moment
there's a runnable game — even a minimum-playable validation —
this section is retired and the **MANDATORY** rules below
become the policy.

### Future phase — once a runnable game exists — MANDATORY

**Direct commits to `main` will not be allowed.**

All changes — features, design updates, plan edits — must land
on a feature branch and reach `main` only through a pull
request.  CI gates each PR.  `main` becomes the release branch.

#### Rules (active once the policy switches)

1. **Never `git commit` directly on `main`.**  If you accidentally
   land on `main`, move the change to a feature branch before
   anything else.
2. **Pushing commits is OK by default — unless there's an open PR
   on the branch that the push would disturb.**  For a long-lived
   working branch with no open PR, push freely after each green
   commit.  When the branch has an open PR, do NOT push without
   an explicit user instruction.
3. **Never create a branch or open a PR unless the user
   explicitly asks.**  "Implement plan 01 phase E1" is *not* a
   PR instruction.  Only run `gh pr create` or `git checkout -b`
   after the user explicitly says "create PR", "open a PR",
   "merge", or "switch to a new branch".
4. Default branch name for general work: a GENERAL slug
   (`work`, `cleanup`, `housekeeping`).  ONLY a substantial plan
   earns a specific branch name.
5. Merging to `main` is via a GitHub pull request — not a local
   `git merge`.

## Git safety — MANDATORY

### Never use `git stash pop` or `git pull` with uncommitted changes

Both can produce unrecoverable working-directory states.  Always
commit before any operation that changes the working tree.  To
compare with main, use `git diff main -- <file>` or `git show
origin/main:<file>` — no branch switch needed.

### Never use `git bisect` or `git checkout HEAD -- <files>`

Both routinely destroy multi-session work-in-progress.  To
investigate a regression, read the relevant code paths directly
or use `git show <commit>` / `git diff <commit>^ <commit>`.

## Documentation validation

We **don't** have a loft-style `@P` tracker + `./scripts/idx`
indexer yet.  Triggers for adding one:

- First dryopea-side P-issue gets numerous enough that prose
  references stop being practical (PROBLEMS.md currently has
  one `@D` row; trigger fires somewhere around ~20).
- Documentation count crosses ~25 (**currently 21** — `docs/*.md`; it read
  "~12" until 2026-08-26, so this trigger is closer than it looked).
- A specific drift incident makes the manual scan painful.

Until then: keep cross-references prose-form (§ section names)
+ explicit relative-path markdown links.  Run `scripts/test.sh`
before committing — it's the only doc-adjacent automation we
have today (validates tests via assert_golden + the loft test
runner).
