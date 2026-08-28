<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# The toolchain, and how it fails

⚠ **Everything below is a way the GATES go red for a reason that is not a
defect in dryopea.**  Each is written down because it reads exactly like a
real failure — and two of them read exactly like each other.
[`CLAUDE.md`](../CLAUDE.md) § Status carries the headlines and the current
gate numbers; this file carries the symptoms, the falsified explanations and
the recovery.

## The gate numbers, and what moves them

### The 300 s hard-kill

⚠⚠ **`loft test` HARD-KILLS AT 300 s BY DEFAULT, and the suite is close
enough to it that a busy box kills the run** — `[timeout] hard-kill after
300s+2s grace: phase=parse fn=? file=tests/__loft_test_base.loft`, which
reads exactly like the cdylib fault below and is not it.  Measured
2026-08-17: the suite is **224 s** with another project's `dotnet` at
100%, and it died at 302 s when that project started a `cargo` build.
⚠ **`LOFT_TIMEOUT=1500 scripts/test.sh` is the way through it**;
`LOFT_TIMEOUT_GRACE` sets the grace.  ⚠ It is also a real budget
constraint on new tests — plan 25 M4's first version cost 63 s on its
own and pushed the whole run over the cliff, and profiling it found ONE
test re-deriving an expensive value twice to print it (63 s → **5 s**,
every reading preserved).

### The three gates

**Suite: 1268/1268 green under `scripts/test.sh`** (~180 s re-measured
2026-08-17 — the `frame` measurements classify full 960x720 frames, the
cost gate ticks a radius-40 world twice, and since plan 13 a dozen tests
run whole scenarios to their fall.  ⚠ This line carried "~35 s" from
plan 12 until H2 re-measured it and "~150 s" until plan 23 K3; the
figure grows with the SCENARIO tests, not with any one phase.  ⚠ The
two most expensive files are both closing measurements and they are
**35 s (plan 23 K3)** and **13 s (plan 16 W4)** — where a file that
runs no simulation costs 3.8 s, which is the compile baseline every
single-file run pays.  ⚠ **The corpus went 1161 → 1156 test
FUNCTIONS with no assertion lost** — five were folded into siblings
because each was re-deriving an expensive value a neighbour had already
computed, and the assert counts are byte-identical before and after.
That cut the suite **10.1%** (5 983 456 → 5 377 562 samples);
[`docs/PROFILING.md`](PROFILING.md) has the per-file table and the
one refactor that measured as FREE).
**Gate: 33 scripts green under `scripts/validate.sh`** (~14 s, 654
measurements).  ⚠ Plan 24 W2 moved **8 of the 33** — a steering change
re-prices scenarios rather than breaking them, and the numbers of record
are `@M020`.  ⚠ Plan 21 R1 **and** R2 moved **none of them**, which is
the point: a camera is not a simulation, and the day it re-prices a
scenario is the day something is reading it that should not be.  ⚠ Nor
did any of plan 25's four phases: a MESHER is not a simulation either.

**Third gate: 2 fixtures green under `scripts/validate_gl.sh`** (26
measurements) — the ground actually DRAWN, through real GL under `xvfb`,
captured and classified.  ⚠⚠ **It is deliberately NOT part of
`validate.sh`** (`@X076`): folding it in would put all 33 headless
scripts behind an X server, and `docs/RENDERER.md` § R0 went out of its
way to prove the readback needs no display.  A machine with no xvfb
still runs the 654.  ⚠ `make validate-gl`, or
`make validate-gl FIXTURE=the-ground`.

## ⚠⚠ The BINARY moves under you, and its version string will not say so

(2026-08-18, plan 20 A2.)

`which loft` on this box is **`~/.local/bin/loft`**, and `/usr/local/bin/loft`
is a **different, older** binary.  Both report `loft 2026.8.0`.

A2 spent a session blocked on a heap corruption that made a measured part come
back with **1 limb instead of 8** and then SIGSEGV in `OpLengthVector`.  It was
real, it was deterministic, and it was **fixed by a rebuild that happened during
the session** — the phase was re-tested against a binary two hours newer than the
one that produced the evidence, and every reading changed:

| binary | built | runs | result |
|---|---|---|---|
| `/usr/local/bin/loft` | 2026-08-16 | 10 | **0 clean, 10 corrupt** |
| `~/.local/bin/loft` | 2026-08-18 11:45 | 25 | **25 clean** |

⚠ **So `loft --version` is not an instrument.**  Before believing any toolchain
symptom — and especially before filing one — check `which loft` and its **mtime**,
and re-run against every binary on the box.  ⚠ The cost of not doing it here was
a phase declared blocked and an issue filed against a fixed build
([loft#969](https://github.com/loft-lang/loft/issues/969)); the cost of doing it
is two commands.

⚠ **And the same care applies to a measurement that "stopped reproducing"** —
`loft_repros/README.md`'s rule is that such a repro is deleted, but that rule
assumes you know WHICH build it stopped reproducing on.

### ⚠⚠ It moves the SUITE TOTAL too, and nothing goes red when it does

(2026-08-26, BACKLOG A1, `@M044`.)

`CLAUDE.md` § The three gates said **1407 green / 101 files**, measured
2026-08-18 (`@M043`).  On the same tree with the `loft` on PATH dated
**2026-08-25**, `scripts/test.sh` measures **1395 green / 101 files** —
**twelve fewer tests, no test file changed, and a green run either way.**

The cause is the collection rule.  `loft test` collects *zero-argument
functions in a test file* as tests, helpers included (`CLAUDE.md`
§ Loft language gotchas — a one-line `fn m2_edge() -> integer` once made
`25_m2` report 17 tests where it has 16).  The tree holds **213** zero-argument
non-`test_` functions in `tests/`, and how many of them count is the runtime's
decision, not the repo's.

⚠⚠ **So a suite total is a reading about a BINARY, not only about a tree**, and
the number in a doc ages the moment the binary is rebuilt.  ⚠ The practical
rule when a delta does not reconcile: **measure the baseline on the binary in
front of you** — hold the new test file out, run, put it back — rather than
subtracting from a documented figure.  *A suite total that does not reconcile
is a finding* (`@M041`), and this is what one looks like when the finding is
the toolchain rather than the code.

## `18_s3` is a DETECTOR, not a test to fix

⚠ **[loft#939](https://github.com/loft-lang/loft/issues/939) is FIXED
and CLOSED** (loft `ac8fb1dc`, *"A vector field assigned from a view
frees what it only names"* — which is exactly `crop_state`'s
`cs_out.crew = state.crew`).  For about a day it made
`tests/18_s3_the_crop.loft` fail and the suite SIGSEGV: returning a
large struct by value poisoned the store, and the next unrelated call
read a plain `integer` field back as a pointer.  ⚠ **`18_s3` is the
detector for it** — it is not something to "fix" if it ever goes red.
⚠ It closed labelled `both-backends`, so *"`--native` looked clean"* was
wrong at the time and the tell was in the reading: native emitted 255
characters where the interpreter emitted 1017, i.e. it never ran the
same workload.  **A backend answering differently on a different
workload is not a backend answering correctly.**

## ⚠⚠ Run the gates DETACHED — `scripts/gate.sh`

⚠⚠ **A long gate polled from its log is wrong three ways, and dryopea
hit all three inside one session.**  The obvious pattern — start
`scripts/test.sh` in the background and keep grepping the output —
costs a look a minute, answers *still running* most of them, and:

- **a run that DIES writes no result**, so a waiter grepping for one
  blocks for ever on a process that is already gone;
- **the PREVIOUS run's result is still in the file** while the next one
  is compiling, so the grep answers about the wrong run;
- **a marker file left behind by a killed run means nothing.**

⚠ The fix is loft's, and `scripts/gate.sh` is it ported:
**record the run's own identity and check the PROCESS, not the log.**
`.gate-verdict` holds one line — `STATE PID EPOCH ELAPSED [detail]` —
and `status` re-reads the pid, so a run that vanished reports **DIED**.

```bash
scripts/gate.sh start     # detach and return at once (make gate)
scripts/gate.sh wait      # exits ONCE, when a verdict exists (make gate-wait)
scripts/gate.sh status    # RUNNING / PASSED / FAILED / DIED (make gate-status)
scripts/gate.sh report    # the last run's failures again
GATES=test|validate|full scripts/gate.sh start
```

⚠⚠ **`wait` is the one to launch in the background from an agent**: it
exits exactly once, so the harness re-invokes once — where polling costs
one turn per look.  ⚠ And what it prints is **the failing assertions**,
not fifteen thousand lines of log.

⚠ `LOFT_TIMEOUT` defaults to **1500** here rather than loft's 300,
because § The 300 s hard-kill is close enough that a busy box trips it.

## Two suites at once clobber each other

⚠ Do not run two `scripts/test.sh` at once — both pre-clean
`tests/actual/`, so they clobber each other and fail for no reason.
⚠⚠ **`scripts/gate.sh start` REFUSES to** — it re-reads the running
gate's pid and declines rather than clobbering, which is this rule
enforced instead of documented.

## The wall clock is not yours alone

⚠⚠ **And the suite's WALL CLOCK is not yours alone.**  Plan 25 M1 timed
it at **293 s** twice against a documented ~177 s and nearly rewrote the
figure; `ps` showed a `rustc` at 336% CPU, a `dotnet` at 101% and
another project's `loft` probe, all from unrelated sessions on the same
box.  The figure above was NOT changed on the strength of those
readings.  ⚠ **Nor on plan 25 M2's**: 197 s, with another project's
`loft` harness pinning a core at 99.8% throughout.  Two sessions have now
declined to rewrite this number and both were right to.  ⚠ Before
believing any timing here, look at what else is running — this is the same rule [`HARD_WON_RULES.md`](HARD_WON_RULES.md) § Profiling the suite gives for the
profiler (read the SAMPLE COUNT, never the seconds), arriving one level
up.

## The `graphics` cdylib fault — recognise it, do not theorise about it

⚠⚠ **Both gates can be taken out by the `graphics` cdylib, and it is a
TOOLCHAIN fault every time — but the cause is NOT pinned, so do not
trust a tidy story about it.**  ⚠ **Not reproducing as of 2026-08-17**:
both gates run clean with no flags.  It has come and gone twice, each
time around a fresh `loft` install, so treat this as a thing to
RECOGNISE rather than a thing that is currently broken.  Symptoms, all
seen 2026-08-15/16:
every PNG/GL test failing with *"native function not loaded"*; a
`[timeout] hard-kill after 300s` in an unrelated file's PARSE phase (a
cdylib build in flight); a `SIGABRT` at the end of an otherwise green
run; and `validate.sh` refusing to start with

```
rust-lld: error: unable to find library -lloft_graphics_native
```

which is a DIFFERENT library's auto-cdylib (`hex_grid`) linking against
`graphics` while `libloft_graphics_native.so` is absent.  loft rebuilds
graphics 2-3 times in a single run and the artefact ends up missing.

⚠ **Seen again 2026-08-17** (plan 25 M3), as `gridmesh`'s auto-cdylib
failing to link with `libloft_graphics_native.so` simply gone from
`~/.loft/build-cache/graphics-0.5.2/release/`.  ⚠ Two things worth
knowing: **`ps` showed another project (`moros`) mid-`cargo build`** on
the same box, which is the correlate every occurrence has had; and
**plan 25 M3 added `imaging` to `loft.toml`, which widens the link line**
(`-lloft_graphics_native -lloft_imaging`) without being the cause — the
missing artefact was `graphics`, which was already the documented one.
⚠ The hand rebuild below fixed it, and it emitted a `.fingerprint` write
error while still producing a good `.so` — another sign of a concurrent
writer rather than of a broken build.

⚠ **Two explanations were tried and FALSIFIED, so skip them**: it is not
simply *two loft binaries sharing `~/.loft/build-cache`* (it reproduces
with one binary, installed and in-tree byte-identical), and it is not
the stamped loft-ffi fingerprint alone — pinning `.loft-build-fp` to the
expected value and setting `LOFT_NO_AUTO_REBUILD=1` do not stop the
rebuild.  ⚠ A fresh `loft` install is what has triggered it each time.

⚠ **What has worked, when it works**: build the cdylib by hand and
re-run, checking the `.so` actually survives.

```bash
(cd ~/.loft/registry/graphics-<ver>/native && \
   CARGO_TARGET_DIR=~/.loft/build-cache/graphics-<ver> cargo build --release)
ls -l ~/.loft/build-cache/graphics-<ver>/release/libloft_graphics_native.so
```

⚠ It is a loft-side problem and belongs upstream, not in a dryopea
workaround.

## Why both gates run INTERPRETED

⚠ **Both gates run INTERPRETED**, and that is not a preference.  On the
NATIVE backend `load_palette` answers 0 entries — a silent `text as
vector<Struct>` miscompile, filed in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) — which no test could
see, because `loft test` runs the interpreter only.
