<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Worked examples — the TEST is the example, and the source links to it

A public function is documented by **the tests that show how to use it**, cited
by a stable ID that a gate resolves.  `scripts/examples.sh` is that gate.

⚠ **This applies to NEW work only** (project owner, 2026-08-17).  dryopea has
387 public functions and 1201 test functions and there is no retroactive
sweep: a file opts in when somebody is working in it, and the gate is silent
about every file that has not.

## Why not a snippet in the comment

Two failure modes, and the convention exists because both are silent:

- **A snippet in prose ROTS.**  Nothing compiles it, so it goes stale at the
  first signature change and the reader is the one who finds out.
- **An unlinked test is INVISIBLE.**  It is correct, it is maintained, and
  nobody reading the function ever learns it exists — which is how a consumer
  ends up hand-rolling what the library already does.  `plans/26`
  § FLEXIBLE counted **nine** hand-rolled timing sites across dryopea and
  moros, every one of them written beside a library that could have answered.

The pair is the deliverable: the test proves the function works, the citation
makes it the example, and the example cannot drift from working code because
it **is** working code.

⚠ dryopea already teaches its editor seam this way — `tests/scripts/*.keys`
are simultaneously the gate and the worked example, and [`plans/08`](../plans/08-game-validation/README.md)
is the plan that made them so.  This is that idea given an ID so a function
can point at one.

## The marker

A marker is `<abbrev><NNN>` — a registered library abbreviation and a
zero-padded three-digit number.  It goes in the test function's name, right
after `test_`:

```loft
// The test that IS the example for `clock_advance`.
fn test_fs001_a_frame_loop_spends_its_backlog() {
    clk = clock_new(33000);
    ticks = clock_advance(clk, 100000);
    assert(ticks == 3, "100 ms at a 33 ms step is three ticks, got {ticks}");
}
```

and the function it documents cites it:

```loft
// Spend elapsed wall-clock time, and answer how many whole ticks it bought.
//
// Example: fs001, fs004
pub fn clock_advance(clk: TickClock, elapsed_us: integer) -> integer {
```

### The rules

1. **Only a CITED test carries a marker.**  A marker is a contract, not
   decoration — marking all 1201 tests would be churn with no reader.
   ⚠ And the distinction is load-bearing: a marked test is somebody's
   documented example, so renaming or deleting it **breaks the gate**, while
   an unmarked test stays free to refactor.  That is the whole difference
   between a test and an example.
2. **A number is never reused**, even after the test it named is deleted —
   the same rule plan numbers and `@X` codes keep, for the same reason: the
   ID is quoted from commits and prose, so a collision is expensive to unwind.
3. **One to three citations per function.**  One shows the shape; three shows
   the edges.  ⚠ A function needing more than three is usually two functions.
4. **Write a new test if none of the existing ones is CLEAR.**  A test that
   proves a function works is not automatically a test that shows how to use
   it — a fixture built over forty lines of scenario setup proves plenty and
   teaches nothing.

### The abbreviations

⚠ Registered here so two libraries cannot collide.  Add a row before using
one.

| abbrev | owns |
|---|---|
| `dry` | dryopea's own `src/` |
| `fs` | `fixstep` — [`plans/26`](../plans/26-the-fixed-step/README.md)'s clock, **not built yet** |

## The gate — `scripts/examples.sh`

Run by `scripts/test.sh` before `loft test`, because it is a text scan and
costs milliseconds where the suite costs ~177 s.

Three faults are always on, and each is a real way the convention rots:

| failure | what happened |
|---|---|
| **dangling** — a cited marker names no test | the test was renamed or deleted and the citation was left behind |
| **duplicate** — two tests carry one marker | a number was reused, so a citation is ambiguous |
| **orphan** — a marked test nobody cites | the citation was removed and the marker was left behind; the test is no longer anybody's example and should lose its marker or gain a citer |

A fourth — **uncovered** — fires only for a file that has opted in.

⚠ **It skips hidden directories, and that is not tidiness.**  `src/.loft/` is
loft's gitignored build cache; a recursive scan walks into it, and `find -name
'*.loft'` matches the directory itself because `*` matches the empty string.
The visible symptom was a stray `grep: …/src/.loft: Is a directory` in the
gate's own failure output — but the real hazard is a generated source under a
build directory contributing a **phantom citation** that resolves against
nothing a reader can find.

⚠ **So it does NOT require a public function to have an example** unless its
file says it should.

### Opting a file in

Put `// #examples` in a file's header.  The gate then requires every `pub fn`
in that file to cite at least one marker, **or** to say why not:

```loft
// Example: none — a one-line accessor over `ps.playing`
pub fn play_mode(ps: const PlayState) -> boolean {
```

⚠⚠ **The exemption is written down rather than silent, and that is the
point.**  `design-protocol` § step 2 says a design re-asserted at N sites with
silent omission is brittle at exactly `N × silence`; this drives the silence
to zero without driving N there, which is the only half a convention can
reach.  An exemption a reader disagrees with is an argument they can have.

⚠ **The opt-in is a RATCHET.**  A gate that went red on 387 functions the day
it landed would be switched off within a week; one that locks in each finished
file can only go up and is never red for work nobody has started.

### ⚠ The gate has a self-test, because it would otherwise pass vacuously

`scripts/examples.sh --self-test` runs the checker against
`scripts/examples_fixtures/` and asserts each of the three failures **fires**.

⚠⚠ That is not belt and braces.  On the day it landed there was not one
`Example:` line in the repo, so the gate was green over an empty set — which
is `plans/21` § R1's exact trap, where a camera gate reported **perfect**
agreement twice while iterating over nothing.  The generic control is *can
this gate produce a non-trivial reading at all?*, and for a linter the honest
form of it is a fixture per failure mode.

## ⚠ Starting here — what exists today, and what is next

**Built and green (2026-08-17):** this document, `scripts/examples.sh` with
its five fixtures, and the wire-in at the top of `scripts/test.sh`.

```
scripts/examples.sh --self-test    # 5 fixtures: ok + the four faults
scripts/examples.sh                # the repo: "ok — 0 citation(s), 0 file(s) opted in"
```

⚠⚠ **The repo reading is 0 citations, and that is correct rather than
broken.**  Nothing has opted in yet, because the convention is for NEW work.
The self-test is what makes that zero mean *nothing to check* instead of
*the checker cannot see anything*.

**What is next, in order:**

1. **The first real citation is [`plans/26`](../plans/26-the-fixed-step/README.md)'s
   library** — every door in its § A DOOR PER USE CASE gets a marker under
   abbreviation `fs`, and its L6 gate is exactly this convention.  Register
   further abbreviations in § The abbreviations *before* using them.
2. **Any file somebody is already working in** may opt in with `// #examples`
   in its header.  ⚠ Opt in only when the whole file is done — the ratchet's
   value is that a locked file cannot regress, and a file half-annotated with
   `Example: none — TODO` is worse than one that never opted in.
3. **Nothing else.**  ⚠ There is deliberately **no sweep** of the 387 existing
   public functions (project owner, 2026-08-17).  A citation added to an old
   function because it was easy, rather than because somebody was working
   there, is unreviewed documentation — and it costs a marker number
   permanently under rule 2.

**What is NOT decided**, and belongs to whoever picks this up:

- ⚠ **Whether the registry libraries adopt this.**  `hex_grid`, `graphics`,
  `mesh3d` and the rest are owned by their own projects, so this is an
  outbound proposal rather than a change dryopea can make — and
  `CLAUDE.md` § Relationship to loft says where a proposal goes.  ⚠ The
  honest order is to ship one library under it first (item 1) and propose it
  with a worked consumer, not before.
- ⚠ **Whether `// Example:` is the right spelling.**  It was chosen to read
  as prose in a header that is already dense with `⚠` lines, and nothing
  depends on it beyond `scripts/examples.sh`'s two greps.  Changing it is a
  one-line edit **plus every citation written by then**, so it is cheap now
  and expensive later.
- ⚠ **Whether the gate belongs in `test.sh` or `validate.sh`.**  It is in
  `test.sh` because it is about source and tests rather than about scenarios.
  If it ever needs the running game to answer, it is in the wrong script.

## See also

- [`plans/26`](../plans/26-the-fixed-step/README.md) § A DOOR PER USE CASE —
  where this convention came from, and the first library to ship under it.
- [`plans/08`](../plans/08-game-validation/README.md) — `.keys` scenarios,
  which are gate and worked example at once.
- [`docs/DECISIONS.md`](DECISIONS.md) — the other greppable ID conventions
  (`@X`, `@M`, `@D`) and the never-reuse rule they share.
