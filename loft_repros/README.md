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

| File | Bug | Open in QUESTIONS_FOR_LOFT |
|---|---|---|
| [`u8_vector_in_wrapper.loft`](u8_vector_in_wrapper.loft) | `vector<Struct-with-u8>` corrupts on `:j` when reached via `hash → for-iter → vector → wrapper → :j` | § Open #1 |
| [`const_param_store_lock.loft`](const_param_store_lock.loft) | Two `const` struct params + writing through a third (non-const) param's vector trips `Claim on read-only store` | § Open #2 |
| [`struct_with_hash_native_return.loft`](struct_with_hash_native_return.loft) | Native codegen loses struct type info when a function returns a struct containing a `hash<…>`.  Blocks `make play` (interpret-mode workaround in place). | § Open #3 |
| [`json_vector_cast_native_tail_return.loft`](json_vector_cast_native_tail_return.loft) | Native codegen SILENTLY answers `[]` for a `text as vector<Struct>` cast in tail-return position.  Empties dryopea's palette natively; no panic, no diagnostic. | § Open #4 |
