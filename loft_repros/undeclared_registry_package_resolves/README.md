<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `use <pkg>;` resolves a registry package the manifest never declared — and writes it into `loft.lock`

**Found while:** dryopea plan 20 A1, whose phase gate is a *negative* one —
*"drop the `hex_body` dependency and A1's tests must fail to compile"*.  It did
not fire, and this is why.

## What happens

`loft.toml` here has **no `[dependencies]` section at all** and the directory
ships **no `loft.lock`**.  `src/consumer.loft` says `use hex_grid;` and calls
`hex_neighbor`.  It compiles, runs, and leaves a lock behind:

```
$ ./run.sh
--- what loft thinks this project depends on ---
== project dependencies (loft.toml) ==
  (none)

--- and what it compiles anyway ---
[registry] resolving hex_grid from registry
hex_grid resolved with NO [dependencies]: hex_neighbor(0,0,0) = (1, 0)

$ cat loft.lock
[[package]]
name = "hex_grid"
version = "0.1.0"
…
```

`loft api` and the compiler disagree in one run: the first says the project
depends on nothing, the second resolves `hex_grid` from `~/.loft/registry/`
and then **adds it to the lock**.

## The same thing measured in a real consumer

dryopea, with `hex_body` newly adopted by one module
(`loft test tests/20_a1_the_part.loft`, 18 tests):

| what was removed | result |
|---|---|
| nothing — the shipped state | 18 passed |
| the `hex_body` line from `loft.toml` | **18 passed** |
| that line **and** the `[[package]]` block from `loft.lock` | **18 passed** |
| `use hex_body;` from `src/part.loft` | `Error: Undefined type Rig` |

So the **import** is load-bearing and the **declaration is not**.

⚠ **FILED 2026-08-18 — [loft#968](https://github.com/loft-lang/loft/issues/968)**, as a `question` rather than a `bug`, for the reason below.

## The ask, and it is a question rather than a bug report

⚠ The auto-resolve may well be deliberate — `loft api`'s own help says a
qualified `lib::fn()` auto-loads a library, and self-healing the lock is a
reasonable convenience.  What the measurement shows is the **cost** of it to a
consumer:

- **A dependency cannot be proven load-bearing.**  Removing a declaration
  changes nothing, so nothing in a project can distinguish *"we depend on this"*
  from *"this happens to be installed on the box that built it"*.
- **A dropped dependency is invisible to every gate.**  Delete a line from
  `loft.toml` by accident and the suite stays green.
- **A phase gate of the form *drop the dependency and it must break* cannot be
  written.**  That is the concrete thing that failed here.  dryopea's A1 now
  uses *remove the `use` line*, which does fire — but that proves the import is
  real, not that the manifest is.

So: should `use <pkg>;` for an **undeclared** package be an error, or carry a
warning naming the fix (`loft install <pkg>`), rather than resolving silently?

## What is NOT measured here, and would sharpen it

- **Which VERSION an undeclared package resolves to** when several are
  installed (this box has `graphics` 0.1.0 through 0.5.2).  If it takes the
  newest rather than a declared range, a pin is advisory too — a separate
  measurement this repro does not make.
- **Whether a clean box fails.**  The inference is obvious and is deliberately
  not asserted: nothing here tested a machine with an empty registry.

## Family

The third dryopea has hit where **the manifest is not authoritative**:

- [loft#963](https://github.com/loft-lang/loft/issues/963) — a declared PATH
  dependency *suppresses* the `--lib` search, so declaring is worse than
  saying nothing.
- [loft#966](https://github.com/loft-lang/loft/issues/966) — bare
  `loft install` installs the project, not its dependencies.
- [loft#968](https://github.com/loft-lang/loft/issues/968) — this one: an
  undeclared package resolves anyway, and joins the lock.
