<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Loft reproducer files

Minimal, self-contained `.loft` reproducer scripts for bugs
dryopea has surfaced upstream.  Each file:

- Is runnable standalone via `loft --interpret <file>` (or
  `loft --lib /path/to/loft/lib --interpret <file>` if loft's
  stdlib lives elsewhere).
- Documents the **trigger** (the specific code shape that fires
  the bug) + the **observed vs expected** output inline.
- Cross-references the dryopea-side workaround in
  [`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) so the
  upstream maintainer can see the impact + retire the
  workaround when the fix lands.

When a bug ships fixed upstream, the entry in QUESTIONS_FOR_LOFT.md
moves Open → Resolved, the workaround is retired in dryopea
code, AND the reproducer file is deleted from this directory.
The dryopea test suite then carries the regression coverage
going forward (no need to keep the standalone repro once it's
no longer reproducing).

## Ready to file

| File | Bug | Status |
|---|---|---|
| [`mutating_a_returned_struct_is_lost.loft`](mutating_a_returned_struct_is_lost.loft) | A struct RETURNED from a function is a copy, so mutating it through a parameter is a silent no-op — and `lost-write` does not fire, though the write is to a temporary discarded one instruction later.  The SAME element indexed inline, or reached through a loop variable, writes through.  Identical on both backends. | **Repro verified, issue not yet filed** — awaiting the go-ahead |

## Currently filed

| File | Bug | Filed upstream |
|---|---|---|
| [`json_vector_cast_native_tail_return.loft`](json_vector_cast_native_tail_return.loft) | Native codegen SILENTLY answers `[]` for a `text as vector<Struct>` cast in tail-return position.  Empties dryopea's palette natively; no panic, no diagnostic. | [loft#866](https://github.com/loft-lang/loft/issues/866) |
| [`struct_cast_via_text_local_returned.loft`](struct_cast_via_text_local_returned.loft) | A `text as Struct` cast returned out of a function trips guard #306 and SIGSEGVs on the interpreter; the native build fails to compile the emitted Rust. | [loft#867](https://github.com/loft-lang/loft/issues/867) |
| [`format_struct_with_hash_field.loft`](format_struct_with_hash_field.loft) | `"{s}"` where `s` is a struct with a `hash<…>` field: SIGSEGV in `OpFormatDatabase` on the interpreter, silent exit 1 on `--native`.  Fires inside assertion messages, so a failing test loses its diagnostic. | [loft#873](https://github.com/loft-lang/loft/issues/873) |
| [`json_null_into_non_null_scalar_field.loft`](json_null_into_non_null_scalar_field.loft) | A `text as vector<Struct>` cast stores JSON `null` into a field declared plain (non-null under DN1) — and `redundant-coalesce` then advises deleting the `?? 0.0` that guards the read.  Both backends. | [loft#870](https://github.com/loft-lang/loft/issues/870) |
| [`struct_through_two_tail_calls.loft`](struct_through_two_tail_calls.loft) | A struct returned through TWO nested tail-position calls, with a struct LITERAL as an argument, loses everything its `while`+vector loop wrote.  1 cell interpreted, 0 native, 13 expected — silent on both. | [loft#880](https://github.com/loft-lang/loft/issues/880) |
| [`lost_write_false_positive/`](lost_write_false_positive/README.md) | An ambiguous bare struct name aborts the compile correctly — and dumps a FALSE `warning[lost-write]` beside it, against a loop-variable mutation in a *different package* whose write persists on both backends. | [loft#883](https://github.com/loft-lang/loft/issues/883) |

⚠ **A reproducer may be a DIRECTORY.**  `lost_write_false_positive/` is
one, because its trigger needs two libraries declaring the same struct
name and that cannot be written in a single file.  Its own README carries
the run command and the attribution table.

Five reproducers were **deleted on 2026-08-12** after re-running them
against loft 2026.8.0 showed their bugs no longer reproduce — per the rule
above, a repro that has stopped reproducing has no evidentiary value and
git history keeps it.  They were `dup_struct_type_across_libs`,
`canvas_store_leak_struct_param`, `u8_vector_in_wrapper`,
`const_param_store_lock` and `struct_with_hash_native_return`; what each
one was observed to do now is recorded in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) § Resolved.
