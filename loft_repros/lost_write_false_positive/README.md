<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# REPRO — an ambiguous struct name emits a FALSE `lost-write` against a library

A directory rather than a single `.loft` file, because the trigger needs
**two libraries declaring the same struct name** — that cannot be
expressed in one file.

```bash
cd prog
loft --interpret amb.loft    # the bug: error + a false lost-write
loft --interpret ok.loft     # the control: clean, and prints `true`
```

## Trigger

Three things together:

1. Two `use`d libraries each declare a struct of the same name
   (`slotlib::Slot` and `otherlib::Slot`).
2. The entry names that struct **bare**, so it is ambiguous — the
   compile aborts with loft's own clear diagnostic.
3. Somewhere in one of those libraries, a `for` loop variable mutates
   a field (`for lw_t in lw_v { lw_t.taken = true; }`).

## Observed (loft 2026.8.0, interpreter and `--native`)

```
error: `Slot` is declared by more than one package here — write otherlib::Slot or slotlib::Slot to say which
warning[lost-write]: 'lw_t' is mutated but its value is never read — the write is LOST. A whole-value bind (`lw_t = …`) COPIES the heap value (C86), so the mutation lands in the copy, not the source.
error: aborting due to 1 previous error
```

The warning is **false**.  `ok.loft` compiles the same library with the
ambiguity removed and prints:

```
loop bind -> true   (want true)
```

on both backends — the write persists.  A `for` loop variable is not a
whole-value bind, and `lw_v` *is* read afterwards, two lines down.

## Attribution — what it is NOT

Measured while reducing, so the issue is not filed against the wrong
cause:

| variant | false `lost-write`? |
|---|---|
| ambiguous struct name, two libraries | **yes** |
| `undefined_function()` — same two libraries | no |
| `undefined_function()` — one library | no |
| the mutation in the ENTRY file, ambiguity or not | no |

So it is not "any compile error" and not "library code": it is the
**ambiguous-struct-name path specifically**, and only for a mutation
that lives in a library.

## Expected

No `lost-write` for a loop-variable mutation that persists.  Failing
that, the ambiguity error alone — resolving it is what the reader has
to do, and an extra warning pointing at correct code in a *different
package* sends them to the wrong file.

## Why it matters more than it looks

`lost-write` is the diagnostic that catches loft's most expensive real
bug class: a whole-value bind that copies a heap value, so the
mutation lands in the copy.  dryopea lost four development phases to
exactly that — a `FlowField` bound to a local in a per-enemy path, 2250x
the intended cost, with 490 green tests sitting over it (plan 11 F8).

A false positive in that class is worse than a missing one.  It teaches
the reader that `lost-write` over library code is noise, which is the
habit that lets the real one through.  It is also unreachable by a
warning-clean gate: a green suite never aborts, so the warning cannot
be kept clean and cannot be trusted when it does appear.

Hit by dryopea while probing plan 09 phase I0 — `camera::InputState`
vs `input::InputState`.  The dumped warning pointed at
`src/spawn.loft::move_order`, code that 497 green tests exercise every
run, and cost a probe to clear.

## See also

- [`../../QUESTIONS_FOR_LOFT.md`](../../QUESTIONS_FOR_LOFT.md) — the
  outbound entry.
- [`../../plans/09-lattice-conversion/README.md`](../../plans/09-lattice-conversion/README.md)
  § I0 — the probe that hit it.
