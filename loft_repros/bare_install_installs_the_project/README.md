<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Bare `loft install` installs the PROJECT, not its dependencies

Found 2026-08-17, plan 26 L6, interpreter.  `bash run.sh` — self-cleaning.

```
package name in loft.toml : instprobe
directory name            : bare_install_installs_the_project

--- loft api names the fix for the missing dep ---
  moros_map  NOT INSTALLED — run `loft install`

--- running exactly that command ---
installed bare_install_installs_the_project (3 files) → ~/.loft/lib/bare_install_installs_the_project

--- what it actually did ---
  ~/.loft/lib/bare_install_installs_the_project EXISTS   <- the DIRECTORY name
  ~/.loft/lib/instprobe absent                           <- the manifest name was NOT used
  dependency still:   moros_map  NOT INSTALLED — run `loft install`
```

Three facts in one run:

1. **`loft api`'s own remediation hint names this command** for an
   unresolved dependency.
2. **The command does not resolve it** — the dependency is still
   `NOT INSTALLED` afterwards.
3. **What it does instead is install the current project** into
   `~/.loft/lib/`, under the **directory's** name rather than the
   manifest's `[package] name`.

## ⚠ Why it is more than a surprise

A copy in `~/.loft/lib/<name>` **shadows the registry copy of the same
name** — that is [loft#667](https://github.com/loft-lang/loft/issues/667),
closed, where a locally installed `web` lost its `wasm/` bridge and
shadowed a good published one.  Bare `loft install` is a **new route into
that trap**, reached by a command whose name reads like *install my
dependencies* and which the tool itself recommends.

Living evidence rather than hypothetical: while developing `fixstep` this
put a `~/.loft/lib/fixstep` in place that shadowed the registry, and the
published package could not be verified until it was removed by hand.  It
happened **twice**, both times from a command run for another purpose.

## The workaround

`loft install <pkg>@<version>` — the explicit form — resolves the package
and updates `loft.lock`.  Bare `loft install` is the one to avoid.
