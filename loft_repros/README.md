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

## Currently filed

| File | Bug | Filed upstream |
|---|---|---|
| [`const_vector_with_a_negative_is_empty.loft`](const_vector_with_a_negative_is_empty.loft) | A file-scope `const vector<integer>` / `vector<float>` whose literal holds a NEGATIVE number reads back EMPTY — `len()` 0, every index `null(oob)`, no diagnostic on either backend.  ⚠ The SIGN is the whole trigger, not the length or the position; a LOCAL with the same literal is correct.  ⚠⚠ A loop over an empty vector runs zero times, so every assertion inside it holds VACUOUSLY — it made a camera gate report perfect agreement while iterating over nothing. | [loft#955](https://github.com/loft-lang/loft/issues/955) |
| [`forward_call_bound_to_a_local.loft`](forward_call_bound_to_a_local.loft) | A local bound to a call whose callee is declared LOWER in the file PANICS the parser (`H5 two-pass contract … cross-pass divergence`), on both backends.  ⚠ Neither half fires alone.  ⚠ It bites because binding a call to a local is the documented workaround for loft#877 and loft#880 — so fixing one bug is the trigger for this one, and the panic names neither. | [loft#918](https://github.com/loft-lang/loft/issues/918) |
| [`omitted_field_is_silently_zero.loft`](omitted_field_is_silently_zero.loft) | A struct literal that OMITS a field takes the type's zero silently — dangerous wherever zero is a meaningful value (dryopea's palette index 0 is sea, which erases).  ⚠ loft HAS declared field defaults (`= -1`), which is the fix and is undiscoverable; the ask is a lint pointing at it. | [loft#914](https://github.com/loft-lang/loft/issues/914) |
| [`loop_variable_is_function_scoped.loft`](loop_variable_is_function_scoped.loft) | A `for`-loop variable outlives its loop and collides across the whole function, so 122 of dryopea's 131 loops carry a per-function prefix.  ⚠ The ERROR is loft#690's fix and is correct; what is filed is the SCOPE. | [loft#915](https://github.com/loft-lang/loft/issues/915) |
| [`missing_file_struct_return.loft`](missing_file_struct_return.loft) | A function that reads a **MISSING** file's content AND returns a struct double-frees: `BUG (#306)` then SIGABRT on `OpFreeText`.  ⚠ Interpreter ONLY — native is correct, and both dryopea gates run interpreted.  Same guard as loft#867, which stays fixed; the residual hole is the missing-file path. | [loft#908](https://github.com/loft-lang/loft/issues/908) |
| [`json_vector_cast_native_tail_return.loft`](json_vector_cast_native_tail_return.loft) | Native codegen SILENTLY answers `[]` for a `text as vector<Struct>` cast in tail-return position.  Empties dryopea's palette natively; no panic, no diagnostic. | [loft#866](https://github.com/loft-lang/loft/issues/866) |
| [`struct_cast_via_text_local_returned.loft`](struct_cast_via_text_local_returned.loft) | A `text as Struct` cast returned out of a function trips guard #306 and SIGSEGVs on the interpreter; the native build fails to compile the emitted Rust. | [loft#867](https://github.com/loft-lang/loft/issues/867) |
| [`format_struct_with_hash_field.loft`](format_struct_with_hash_field.loft) | `"{s}"` where `s` is a struct with a `hash<…>` field: SIGSEGV in `OpFormatDatabase` on the interpreter, silent exit 1 on `--native`.  Fires inside assertion messages, so a failing test loses its diagnostic. | [loft#873](https://github.com/loft-lang/loft/issues/873) |
| [`json_null_into_non_null_scalar_field.loft`](json_null_into_non_null_scalar_field.loft) | A `text as vector<Struct>` cast stores JSON `null` into a field declared plain (non-null under DN1) — and `redundant-coalesce` then advises deleting the `?? 0.0` that guards the read.  Both backends. | [loft#870](https://github.com/loft-lang/loft/issues/870) |
| [`struct_through_two_tail_calls.loft`](struct_through_two_tail_calls.loft) | A struct returned through TWO nested tail-position calls, with a struct LITERAL as an argument, loses everything its `while`+vector loop wrote.  1 cell interpreted, 0 native, 13 expected — silent on both. | [loft#880](https://github.com/loft-lang/loft/issues/880) |
| [`mutating_a_returned_struct_is_lost.loft`](mutating_a_returned_struct_is_lost.loft) | A write through a struct RETURNED from a function is silently discarded, while the SAME element indexed inline or reached through a loop variable writes through.  No diagnostic — and `lost-write` is exactly the analysis that should have caught a write to a temporary discarded one instruction later. | [loft#894](https://github.com/loft-lang/loft/issues/894) |
| [`const_from_aggregator_import/`](const_from_aggregator_import/README.md) | A file-scope `const` whose INITIALISER reads a sibling module's const — where the sibling is reached through the package AGGREGATOR rather than imported directly — panics the variable allocator (`index out of bounds: the len is N but the index is 65535`) in any CONSUMER entry.  ⚠ Three conditions and each alone defuses it: the aggregator import, the read being in a const initialiser rather than a function body, and the compiled program being a consumer.  ⚠⚠ **Compiling the aggregator itself is completely clean**, so the library looks healthy and every consumer fails.  ⚠ The diagnostic points at an unrelated FUNCTION's return type. | [loft#962](https://github.com/loft-lang/loft/issues/962) |
| [`lost_write_false_positive/`](lost_write_false_positive/README.md) | An ambiguous bare struct name aborts the compile correctly — and dumps a FALSE `warning[lost-write]` beside it, against a loop-variable mutation in a *different package* whose write persists on both backends. | [loft#883](https://github.com/loft-lang/loft/issues/883) |

⚠ **A reproducer may be a DIRECTORY.**  `lost_write_false_positive/` and
`const_from_aggregator_import/` are, because their triggers need more
than one file — the first two libraries declaring the same struct name,
the second a package with an aggregator, two submodules and a consumer
entry.  Its own README carries
the run command and the attribution table.

Five reproducers were **deleted on 2026-08-12** after re-running them
against loft 2026.8.0 showed their bugs no longer reproduce — per the rule
above, a repro that has stopped reproducing has no evidentiary value and
git history keeps it.  They were `dup_struct_type_across_libs`,
`canvas_store_leak_struct_param`, `u8_vector_in_wrapper`,
`const_param_store_lock` and `struct_with_hash_native_return`; what each
one was observed to do now is recorded in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) § Resolved.
