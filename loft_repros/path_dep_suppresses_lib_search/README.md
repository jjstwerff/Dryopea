<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# A declared PATH dependency suppresses the `--lib` search

Found 2026-08-17, plan 26 L6, interpreter.  `bash run.sh` prints the
matrix.

```
1. no dep,   --lib lib/                  test result: ok
2. no dep,   no flag                     test result: FAILED
3. path dep, --lib lib/                  test result: FAILED
4. path dep, no flag                     test result: FAILED
```

**Rows 1 and 3 differ only by a `[dependencies]` block**, and row 1 is
the one that works.  Declaring

```toml
[dependencies]
mylib = { path = "lib/mylib" }
```

makes `use mylib;` fail with

```
Error: Library 'mylib' not found — searched lib/, lib_dirs, and sibling packages
```

*even though `--lib lib/` resolves that exact package when the
declaration is absent.*  So the declaration is strictly worse than
saying nothing: it does not resolve the dependency itself, and it
prevents the mechanism that would have.

⚠ **This is the documented registry-dep trap, reaching path deps too.**
`loft-libs-world/hex_draw/loft.toml` already carries a comment saying
registry-version deps *"break `--lib` multi-library consumption — loft
resolves the dep from the registry instead of the `--lib` path"*, which
is why that repo declares none.  The same suppression happens for a
`{ path = ... }` dep, where there is no registry entry to resolve from
at all.

## ⚠ What this repro REFUTED

The first diagnosis was *"`loft test` ignores `--lib`"*, because that is
what the failure looked like from inside a consumer that had just
declared the path dep.  **It is false** — row 1 is `loft test --lib` and
it passes.  A three-line repro was enough to overturn it, which is the
whole reason the repro comes before the issue.

## Layout

```
loft.toml              the consumer package (row 3/4's manifest is left in place)
src/consumer.loft      `use mylib;` — the site that fails
lib/mylib/             the package `--lib lib/` finds when nothing declares it
tests/t2.loft          `use consumer;`
run.sh                 the four-case matrix
```
