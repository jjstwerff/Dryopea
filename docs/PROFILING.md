<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Profiling the dryopea suite — and why the wall clock cannot do it

Extracted from `CLAUDE.md`, which keeps the headline warnings and
points here for the method.  ⚠ `CLAUDE.md` § Cost is a DIFFERENT
gate — the tick budget — and is not about this.

### Profiling the suite — and why the wall clock cannot do it

`LC_ALL=C LOFT_PROFILE=1 loft test > out.txt 2>&1` gives one merged
per-function + per-line + call-path report over every run in the suite.

- ⚠ **The report goes to STDERR.**  A plain `> out.txt` keeps the test
  results and silently drops the profile, which reads as "the profiler
  says there is nothing to see".
- ⚠ **Read the SAMPLE COUNT, not the seconds.**  It is an op counter, so
  it is *deterministic* — two runs of an unchanged suite agree exactly
  (1 421 358 twice, measured).  The wall clock has **~3.5 s of run-to-run
  variance on a ~33 s suite**, so it cannot see a 2.4 s improvement at
  all: `classify_canvas`'s 2.6x landed as 32.6 s → 32.8 s, i.e. inside
  the noise and pointing the wrong way.  Quote the op count.
- `LOFT_NO_NATIVE_LIBS=1` makes no difference here — both ways give
  identical counts, so loft's "a `use`d library is a cdylib the sampler
  cannot see into" inversion trap does not apply to `loft test`.
- ⚠ **`loft test --check` is not a compile-only measurement** — it falls
  through to rustc and took 72 s, twice the suite.  loft's own
  `doc/claude/PERFORMANCE.md` § Profiling a run warns about this.
- `ticks()` is in **microseconds** (`default/02_files.loft`), so a probe
  that prints it as ms overstates by 1000x.

**Where the time goes (re-profiled 2026-08-17: 5 983 456 samples,
125.40 s interpreted across 1161 runs):**

| function (self) | 2026-08-15 | 2026-08-17 |
|---|---|---|
| `flow_sweep` | 16.1% | **17.6%** (22.08 s) |
| `painted_ground` | 11.9% | **10.1%** (12.66 s) |
| `hex_neighbor` | 8.5% | **8.6%** (10.70 s) |
| `hex_walkable` | 6.7% | **7.1%** (8.85 s) |
| `lat_neighbour` | 6.5% | **6.6%** (8.20 s) |
| `hex_ground` / `lookup_painted` / `height_rise` / `painted_height_of` | 19.3% | **16.3%** |
| `classify_canvas` | — | **3.9%** (4.82 s) |

⚠ **The distance field is still the suite and the shape has not moved**
— **~69%** across the field family, against the ~75% read two days
earlier, and the top eight *hottest paths* are all
`wave_tick → wave_fields → flow_build → flow_sweep → …`.  So this
re-profile does NOT retire the 2026-08-15 conclusion; it confirms it a
third time and [`plans/22`](../plans/22-the-field-cache/README.md) is
still where the win is.

⚠⚠ **What DID move is the absolute size: the suite's interpreted work
roughly DOUBLED, 2 780 440 → 5 983 456 samples, on twelve more tests.**
Measured per file, the most expensive in the repo are now:

| file | samples | share of suite |
|---|---|---|
| `23_k3_what_composition_is_worth` | 739 187 | **12.4%** |
| `17_t3_what_upkeep_is_worth` | 712 523 | 11.9% |
| `16_w4_the_real_length` | 382 353 | 6.4% |
| `23_k0_the_classes` | 320 344 | 5.4% |
| `23_k1` + `23_k2b` together | 41 458 | 0.7% |

⚠ **Plan 23 accounts for at most a third of the doubling** (~1.10 M
samples of the +3.20 M), so **most of it is unattributed** and it is an
open question rather than a known cost.  ⚠ The hypothesis worth
checking first — and it is a HYPOTHESIS, not a finding — is plan 23
K2a's banked mover: it added an `enemy_bank` call per enemy per tick to
**every** scenario in the suite, and its gate was *the corpus does not
move* (`@M015`), which is a BEHAVIOUR gate.  A per-tick cost that
changes no measurement is exactly what that gate cannot see, and
`11_f8_the_tick_budget` cannot either — it is a RATIO, so a uniform
increase divides out.  ⚠ That is `CLAUDE.md` § Cost's own rule
recurring: *a copy changes no behaviour, only cost*.

⚠ **Read the older numbers below as the 2026-08-15 reading**, kept
because the INVERSION they record is the durable lesson.

**The 2026-08-15 reading (2 780 440 samples, 65.5 s interpreted of a
~130 s wall — the other half is compilation, below):**

⚠⚠ **This INVERTED between plan 12 and plan 17, and the old reading is
the trap.**  The 2026-08-12 profile said *58% is `graphics`'s `canvas()`
… the largest remaining win, an upstream ask*, and *"all of plan 11 …
is under 15% put together"*.  Both are now false and each points a
reader the wrong way:

| | 2026-08-12 | 2026-08-15 |
|---|---|---|
| the flow field + its passability lookups | <15% | **~75%** |
| `classify_canvas` + `Canvas` primitives | 58% | **7.5%** |

**The distance field is the suite**, and every hot path reads
`wave_tick → wave_fields → flow_build → flow_sweep → can_climb → …`.
`flow_sweep` alone is 16.1% self, `painted_ground` 11.9%,
`hex_neighbor` 8.5%, `hex_walkable` 6.7%, `lat_neighbour` 6.5%, and
`lookup_painted` / `height_rise` / `painted_height_of` / `hex_ground`
another 19.3%.  It grew because plans 13-17 added tests that run whole
bases to their fall, and the field is rebuilt from scratch **every
tick**.

⚠ So the moral is not the numbers, it is that a profile ages: this one
was three plans stale and still being quoted as the place to look.
**Re-profile before optimising, and quote the date.**

⚠⚠ **And quote the SAMPLE COUNT, because the wall clock lied again.**
The `flow_sweep` hoist below measured **2 941 011 -> 2 780 440 samples,
a 5.5% cut** — while two clean foreground runs of the whole suite read
2m25 and 2m01, i.e. a 16% "improvement" that is mostly noise.  Half the
suite is compilation and does not move at all, so 5.5% of the
interpreted half is ~4% of the wall.  The op counter is deterministic;
the clock is not.

⚠ **What the hoist actually bought, and why the estimate was 3x too
high.**  `flow_sweep`'s six-direction loop tests `cells[n] != null`
BEFORE the expensive question, so in a BFS most neighbours are already
labelled and the "6x recomputation of the frontier hex" was really 1-2x
in practice.  What did pay was the neighbour's height, which
`can_climb` computed and the `FlowCell` then computed again —
`painted_height_of` fell 3.32 s -> 2.15 s and `can_climb` left the table
entirely.  ⚠ Estimate the multiplier from the code path a profile
actually walks, not from the loop bounds.

⚠ **The other half of the wall clock is COMPILATION, not execution.**
Measured 2026-08-15: a test file with no `use` costs 40 ms, one with
`use lattice;` costs 52 ms, and one with `use dryopea;` costs **~490 ms**
— so ~450 ms per file is rebuilding the aggregator, and all 67 test
files pay it.  That is ~31 s of the suite for nothing, and it grows with
BOTH the file count and the size of `src/`.  Filed as
[loft#925](https://github.com/loft-lang/loft/issues/925); it is not
dryopea's to fix, and the workaround it tempts you into — `use`ing
single modules instead of the aggregator — makes tests stop exercising
the entry point the program uses.

⚠ **`ticks()` is loft's clock builtin — never shadow it**, not even as
a parameter name.  A probe that took `ticks` as a parameter compiled
clean and reported a tick 4x cheaper than it was; the same trap `now`
sets, and a blind stopwatch is worse than none because it fails in the
reassuring direction.

