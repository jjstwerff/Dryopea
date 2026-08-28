<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Worked examples — the TEST is the example, and an index TAG points at it

A public function is documented by **the tests that show how to use it**,
pointed at by a stable index tag.  `scripts/examples.sh` is the gate that
keeps the pointer honest.

⚠⚠ **It has a sibling since 2026-08-28**: `scripts/tags.sh` (`@X325`) does
the same job for the `@X###` and `@M###` families — every citation must
resolve to a row in [`DECISIONS.md`](DECISIONS.md).  ⚠ Same argument, one
family over: *a citation naming a rule that does not exist reads as
authoritative and answers nothing.*  ⚠ Both run at the top of
`scripts/test.sh`, before the suite, because a dangling pointer is a
two-second fix and finding it after three minutes is three minutes
wasted.

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
  ends up hand-rolling what the library already does.  [`plans/26`](../plans/26-the-fixed-step/README.md)
  § FLEXIBLE counted **nine** hand-rolled timing sites across dryopea and
  moros, every one of them written beside a library that could have answered.

The pair is the deliverable: the test proves the function works, the tag makes
it findable, and the example cannot drift from working code because it **is**
working code.

⚠ dryopea already teaches its editor seam this way — `tests/scripts/*.keys`
are simultaneously the gate and the worked example, and [`plans/08`](../plans/08-game-validation/README.md)
is the plan that made them so.  This is that idea given an ID.

## The tag

`@XXX-###` — an `@`, a **three-letter acronym**, a hyphen, three digits.  The
same family as loft's `@P367` and dryopea's `@X072` / `@M013` / `@D002`, so
**one indexer carries them all**.

⚠ **Exactly three letters** (project owner, 2026-08-17) — `DRY`,
`FIX`.  Two read as noise beside `@X072` and give a thin namespace for a
registry that has to cover every library at once; four start competing with
the word they abbreviate.

⚠ **The hyphen is what keeps the families apart.**  The existing codes have
none, so a tag can never be read as one of them.

A tag is an **index tag in a comment**, never part of an identifier.  It goes
directly above the test it names:

```loft
// @XXX-001 — a frame loop spends its backlog
fn test_a_frame_loop_spends_its_backlog() {
    clk = clock_new(33000);
    spent = 0;
    for _ in 0..clock_advance(clk, 100000) { spent += 1; }
    assert(spent == 3, "100 ms at a 33 ms step is three ticks, got {spent}");
}
```

and the function it documents cites it:

```loft
// Spend elapsed wall-clock time; answer how many whole ticks it bought.
//
// Example: @XXX-001, @XXX-004
pub fn clock_advance(clk: TickClock, elapsed_us: integer) -> integer {
```

⚠ A tag binds to the `fn` that **follows** it, within one comment block — a
blank line breaks the block, so a tag in a file header cannot drift down and
claim an unrelated test.

⚠⚠ **The tag anchors to the comment rather than the test's name, and that is
the better contract.**  Deleting the test takes its tag with it, so a citation
to it goes **dangling** — real breakage, caught.  *Renaming* a test for
clarity leaves the example just as valid, and the gate stays quiet, which is
what you want.

### The rules

1. **Only a CITED test carries a tag.**  A tag is a contract, not decoration —
   tagging all 1201 tests would be churn with no reader.
2. **A number is never reused**, even after the test it named is deleted — the
   same rule plan numbers and `@X` codes keep, for the same reason: the ID is
   quoted from commits, prose and other repos, so a collision is expensive to
   unwind.
3. **One to three citations per function.**  One shows the shape; three shows
   the edges.  ⚠ A function needing more than three is usually two functions.
4. **Write a new test if none of the existing ones is CLEAR.**  A test that
   proves a function works is not automatically a test that shows how to use
   it — a fixture built over forty lines of scenario setup proves plenty and
   teaches nothing.

### ⚠⚠ The acronym namespace is the ECOSYSTEM's

(project owner, 2026-08-17.)  The indexer covers the **registered libraries**
as well as this tree, so an acronym is claimed **globally**: a tag must name
one test *everywhere*, and two libraries claiming the same three letters
collide in the index even though neither repo can see the other.

| acronym | owns |
|---|---|
| `DRY` | dryopea's own `src/` and its algorithm examples |
| `FIX` | `fixstep` — [`plans/26`](../plans/26-the-fixed-step/README.md)'s clock, **not built yet** |
| `TST` | the gate's own fixtures, `scripts/examples_fixtures/` |
| `XXX` | ⚠ **RESERVED — the specimen.**  Never a real tag; see below |

⚠ **This table is dryopea's copy of a shared decision, not the decision.**
Registering an acronym is an ecosystem-level act; until there is one place
that holds it, a new acronym goes here **and** is announced where the indexer
is defined.

#### ⚠⚠ `@XXX-###` is the specimen, and this document can use nothing else

A document that TEACHES this convention has to show the shape — and a shape
shown in prose is not a claim that a test exists.  Since prose IS scanned for
citations (§ A tag is not only an API example), every real-looking tag written
here would resolve to nothing and report **dangling**.

⚠ Measured the moment prose citations were admitted: this file's own examples
turned the gate red, naming three tags that were never meant to be claims.

So `@XXX-###` is reserved, ignored by the gate wherever it appears, and is
what every example here is written in.  ⚠ A real tag belongs beside a real
test, never in a passage explaining the format.

### ⚠⚠ A tag is not only an API example

(project owner, 2026-08-17.)  A **first-class program** — dryopea, moros —
tags a test because the **algorithm** is worth reading, not because it
documents a public function.  Such a tag has no `pub fn` to cite it.

So a citation is **any reference**: an `// Example:` line above a function, or
plain prose in a doc or a plan saying *see `@XXX-012`*.

⚠ That is what keeps the `orphan` rule meaning *a tag nothing anywhere points
at* rather than *a tag no function points at* — without it the gate would go
red on exactly the use the tag family is being widened for.

## The gate — `scripts/examples.sh`

Run by `scripts/test.sh` before `loft test`, because it is a text scan
costing milliseconds where the suite costs ~177 s.

Three faults are always on, and each is a real way the convention rots:

| failure | what happened |
|---|---|
| **dangling** — a cited tag names no test | the test was deleted and the citation was left behind |
| **duplicate** — two tests carry one tag | a number was reused, so a citation is ambiguous |
| **orphan** — a tagged test nothing cites | the citation was removed and the tag was left behind |

A fourth — **uncovered** — fires only for a file that has opted in.

### Where it looks

| | default | override |
|---|---|---|
| citations in source | the `SRC` tree | argument 1 |
| citations in prose | `docs/` + `plans/` | `EXAMPLES_CITE_ROOTS` (colon-separated) |
| tag definitions | the `TESTS` tree | argument 2, plus `EXAMPLES_TEST_ROOTS` |

⚠ `EXAMPLES_TEST_ROOTS` is how a consumer resolves a citation into a
**registered library's** tests — so dryopea can point at `hex_grid`'s own
worked example instead of copying it.  ⚠ `orphan` is asked only of the
primary tree: a library's tags are its own repo's business, and calling them
orphans here would make every consumer red for its dependencies.

### Opting a file in

Put `// #examples` in a file's header.  The gate then requires every `pub fn`
in that file to cite a tag, **or** to say why not:

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

`scripts/examples.sh --self-test` runs eight controls over
`scripts/examples_fixtures/` and asserts each fault **fires**.

⚠⚠ That is not belt and braces.  On the day it landed there was not one
`Example:` line in the repo, so the gate was green over an empty set — which
is [`plans/21`](../plans/21-the-renderer/README.md) § R1's exact trap, where a
camera gate reported **perfect** agreement twice while iterating over nothing.

⚠⚠ **And the self-test earned its keep immediately.**  The first traversal was
`grep -r --exclude-dir='.*'`, and GNU grep applies `--exclude-dir` to the
command-line directory too — so **any checkout under a hidden path scanned
zero files and reported a cheerful `ok — 0 citation(s)`**.  Measured: the
identical fixture read 5 lines under `/tmp` and **0** under `~/.cache`.
⚠ That is not a corner case, because **every registered library lives under
`~/.loft/`** — the ecosystem scope above would have been an empty set for
ever.  The traversal now runs *from* the root, and
`examples_fixtures/.hidden/` is asserted on its **citation count** rather than
on being fault-free, because a tree nobody opened is perfectly clean.

## ⚠ Starting here — what exists today, and what is next

**Built and green (2026-08-17):** this document, `scripts/examples.sh` with
its eight controls, and the wire-in at the top of `scripts/test.sh`.

```
scripts/examples.sh --self-test    # 8 controls
scripts/examples.sh                # the repo: "ok — 0 citation(s), 0 file(s) opted in"
```

⚠⚠ **The repo reading is 0 citations, and that is correct rather than
broken.**  Nothing has opted in yet, because the convention is for NEW work.
The self-test is what makes that zero mean *nothing to check* instead of *the
checker cannot see anything*.

**What is next, in order:**

1. **The first real citations are [`plans/26`](../plans/26-the-fixed-step/README.md)'s
   library** — every door in its § A DOOR PER USE CASE gets a tag under
   acronym `FIX`, and its L6 gate is exactly this convention.
2. **Any file somebody is already working in** may opt in with `// #examples`.
   ⚠ Opt in only when the whole file is done — the ratchet's value is that a
   locked file cannot regress, and a file half-annotated with
   `Example: none — TODO` is worse than one that never opted in.
3. **Nothing else.**  ⚠ There is deliberately **no sweep** of the 387 existing
   public functions (project owner, 2026-08-17).

**What is NOT decided**, and belongs to whoever picks this up:

- ⚠⚠ **Where the acronym registry actually lives.**  § The acronym namespace
  makes it ecosystem-wide, and this file is only dryopea's copy.  One place
  has to hold it, and that place is wherever the indexer is defined — not
  here.
- ⚠ **How the indexer consumes these.**  `CLAUDE.md` § Documentation
  validation records that dryopea has no `@P` tracker + `scripts/idx` yet and
  lists the triggers for adding one.  This convention makes the tag family
  bigger, which moves those triggers closer.
- ⚠ **Whether a program's algorithm tags want their own acronym**, or share
  the program's.  `DRY` currently covers both.
- ⚠ **Whether `// Example:` is the right spelling.**  Nothing depends on it
  beyond two greps in `scripts/examples.sh`, so it is cheap now and expensive
  once citations exist.

## See also

- [`plans/26`](../plans/26-the-fixed-step/README.md) § A DOOR PER USE CASE —
  where this convention came from, and the first library to ship under it.
- [`plans/08`](../plans/08-game-validation/README.md) — `.keys` scenarios,
  which are gate and worked example at once.
- [`docs/DECISIONS.md`](DECISIONS.md) — the `@X` / `@M` / `@D` families this
  shares an indexer with, and the never-reuse rule they share.
