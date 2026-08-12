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
| [`json_vector_cast_native_tail_return.loft`](json_vector_cast_native_tail_return.loft) | Native codegen SILENTLY answers `[]` for a `text as vector<Struct>` cast in tail-return position.  Empties dryopea's palette natively; no panic, no diagnostic. | [loft#866](https://github.com/loft-lang/loft/issues/866) |
| [`struct_cast_via_text_local_returned.loft`](struct_cast_via_text_local_returned.loft) | A `text as Struct` cast returned out of a function trips guard #306 and SIGSEGVs on the interpreter; the native build fails to compile the emitted Rust. | [loft#867](https://github.com/loft-lang/loft/issues/867) |

Five reproducers were **deleted on 2026-08-12** after re-running them
against loft 2026.8.0 showed their bugs no longer reproduce — per the rule
above, a repro that has stopped reproducing has no evidentiary value and
git history keeps it.  They were `dup_struct_type_across_libs`,
`canvas_store_leak_struct_param`, `u8_vector_in_wrapper`,
`const_param_store_lock` and `struct_with_hash_native_return`; what each
one was observed to do now is recorded in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) § Resolved.
