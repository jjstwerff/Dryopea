<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# A `const` initialised from a sibling module, imported through the aggregator, panics the compiler

```
loft entry.loft
```

**Expected:** `857142857142`

**Observed** (loft 0.8.x, interpreter *and* `--native-emit`):

```
error: index out of bounds: the len is 1 but the index is 65535
       [src/variables/mod.rs:1245]
  --> src/two.loft:5:30
  |
5 | pub fn two_use(n: integer) -> integer {
  |                              ^

error: aborting due to 1 previous error

thread 'main' panicked at src/variables/mod.rs:1245:23:
index out of bounds: the len is 1 but the index is 65535
```

## The trigger — three conditions, all required

1. `src/two.loft` imports the **aggregator** (`use repro;`) rather than
   the sibling that owns the const (`use one;`);
2. it declares a **file-scope `const`** whose *initialiser* reads that
   sibling's const;
3. the program being compiled is a **consumer entry**, not the
   aggregator.

Break any one and it compiles and runs — `src/two.loft`'s header lists
the four variants that were tried.

⚠ **Compiling the aggregator itself produces no diagnostic at all**, so
a library can look healthy while every consumer of it fails.  In
dryopea that was the difference between `loft --native-emit
src/dryopea.loft` (clean) and `loft test` (panics on the first test
file).

## Why it costs time to diagnose

The diagnostic points at a **function's return type**, which has nothing
to do with the const that failed to resolve, and `65535` is `u16::MAX` —
so it reads as an unresolved-variable sentinel escaping into a slot
index rather than as a name-resolution problem.  It is a `panic!` rather
than a diagnostic, so there is no span to work back from.

## ⚠⚠ The workaround MOVES the bug, it does not remove it

Import the sibling directly and the consumer entry compiles — and the
**aggregator** starts panicking instead.  The two are mutually
exclusive:

| `two.loft` says | `loft entry.loft` | `loft --native-emit src/repro.loft` |
|---|---|---|
| `use repro;` | **PANIC** | ok |
| `use one;` | ok | **PANIC** |

⚠ **And the panic is not local to the const that triggers it.**  In
dryopea the site named is a *different file's* const:
`spawn.loft::TICK_SECONDS` reads `tick_clock`'s `CLOCK_UNITS_PER_SECOND`,
and the panic is reported against `per_tick` two declarations later.
Substituting one const at a time: `TICK_SECONDS` derived panics whether
or not `tick_bank`'s `BANK_WHOLE` is; `TICK_SECONDS` as a literal is
clean with `BANK_WHOLE` still derived.  **One such const anywhere in the
package is enough, and a second adds no second failure** — which is what
makes it look like a global ordering problem rather than a per-site one.

## What dryopea does

`src/tick_bank.loft` uses `use tick_clock;`, because that is the side
that keeps **every real entry point and the whole test suite** working:
`src/main.loft`, `src/validate_main.loft`, `src/gl_gate_main.loft` and
all 91 test files compile clean.  The cost is that
`loft --native-emit src/dryopea.loft` — parse-checking the aggregator
itself — panics, so parse-check a real entry instead.
