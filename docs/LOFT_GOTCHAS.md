<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Loft language gotchas dryopea has hit

Extracted from `CLAUDE.md`, which keeps a by-name index of these and
points here.  Full reproducers and issue links live in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md); minimal
standalone cases live in [`loft_repros/`](../loft_repros/README.md).

⚠ **Almost every entry here compiles clean and fails SILENTLY.**  That
is what earns them a file rather than a lint rule — a green suite
cannot see any of them.

## Values, copies and tail calls

⚠ **A struct RETURNED from a function is a COPY, so mutating it is a
silent no-op.**  `hurt(first(state), 10)` — where `first` returns the
roster's element — lands 0 damage; indexing the vector inline
(`state.enemies[0] ?? Enemy {}`) lands it, and so does a `for e in
state.enemies` loop variable.  Measured all three, plan 12 B4.  It
compiles, it type-checks, there is no warning, and the read-back looks
like the mutation never happened — which reads as a bug in the thing
being mutated rather than in the accessor.  A one-line "get me the
element" helper is fine to READ through and never to write through.
Filed as [loft#894](https://github.com/loft-lang/loft/issues/894); the
ask there is the missing `lost-write`, not the value semantics.

⚠ **A struct returned through TWO nested tail calls loses what its
loop wrote** — 1 cell interpreted, 0 native, silent on both
([loft#880](https://github.com/loft-lang/loft/issues/880)).  It bites
when an algorithm is factored out of a function into a shared helper,
because every CONSUMER's one-line wrapper then becomes the second tail
call — so the defect appears at call sites nobody edited.  Bind the
inner call to a local.

⚠ **Never index a call's result in TAIL position** — `steps(a, b)[0] ??
fallback` as a function's last expression reads the absent sentinel, so
it answers the fallback on the interpreter and PANICS on native
(loft#877).  Bind the call to a local, then index it.  It bites hardest
where the fallback is a sane default, because a function that returns
only its default still looks like a working function.

## The parser

⚠ **But bind it ABOVE the callee's definition and the parser PANICS**
([loft#918](https://github.com/loft-lang/loft/issues/918), both
backends).  A local bound to a call whose callee is declared lower in
the file aborts with `H5 two-pass contract … a real cross-pass
divergence` — a raw Rust panic, so there is no line number for the
offending call.  ⚠ Neither half fires alone: the callee declared first
with the same local is clean, and the forward reference returned
directly is clean.  ⚠ **So the fix for the two warnings above is the
trigger for this one** — move the callee up, or inline the call into the
return expression.

⚠ **Never interpolate a struct that has a `hash` field** — `"{f}"`
SIGSEGVs the interpreter (loft#873) and exits silently on native.
It bites hardest inside an assertion message, where it replaces the
diagnostic of a failing test with a crash three lines from the real
site.  Format the fields: `{flow_count(f)}`, never `{f}`.

## Literals

⚠ **There is no vector-of-TUPLES literal.**  A table written the
obvious way —

```loft
for row in [("drive_north", 119), ("drive_west", 97)] { … }
```

— fails with `fatal: cannot build this record — its type never
resolved`, pointing at the LAST element rather than at the construct.
The element type is never inferred, so nothing tells you the shape
itself is the problem.

Write a one-line struct instead; it also reads better at the call site:

```loft
struct KeyRow { name: text, code: integer }
rows: vector<KeyRow> = [
    KeyRow { name: "drive_north", code: 119 },
    KeyRow { name: "drive_west",  code:  97 },
];
for row in rows { … }
```

⚠ Tuples themselves are fine — a function may RETURN one, and
`play_core` does.  It is only the vector literal of them that has no
element type.  (Found in plan 19 P2; nothing else in `src/` or `tests/`
uses the shape, so it had never come up.)

⚠ **A vector literal passed straight as an argument needs a typed
local too** in the same situation — `f(["a", "b"])` resolves, but
binding it first (`names: vector<text> = ["a", "b"]; f(names)`) is what
the repo does everywhere and what stays readable when the element type
is a struct.

## The rest

### Loft language gotchas we hit

The following are dryopea-side workarounds for known loft
behaviour.  Full reproducers + loft-side issue refs live in
[`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md):

- **A local may shadow a builtin, and that is now BY DESIGN** —
  so a shadowing mistake is silent rather than caught.
  [loft#852](https://github.com/loft-lang/loft/issues/852) made a
  local carry a function's name in every binding form while the
  function stays reachable as a call, so `ticks = 4` and `ticks()`
  coexist in one scope and mean different things (measured
  2026-08-14: `4` and the clock).  ⚠ The old note here — that
  `now = ticks()` ends up holding a `fn() -> integer` — **no longer
  reproduces**; `src/main.loft`'s `tnow` rename is a scar, not a
  live workaround.  What survives is the § Profiling warning: a
  parameter named `ticks` beside a `ticks()` call compiles clean
  and measures the wrong thing, and nothing warns.
- ⚠⚠ **A `vector<Struct>` local in a VERY LARGE function corrupts the
  heap, and an unrelated test file is what dies**
  ([loft#935](https://github.com/loft-lang/loft/issues/935), plan 23
  K1).  Building one inside `script_command` (~700 lines, complexity
  246) gives `realloc(): invalid next size` with `last op: OpArgText`,
  deterministically, inside `tests/12_b1_rubble.loft` — a file that
  never reaches the branch and never mentions the type.  The crash site
  is in loft's own `default/01_code.loft`, so the damage is written at
  COMPILE time and merely detected wherever the allocator next touches
  it.
  ⚠ **Bisected at full-suite scale**: the whole nested data structure
  (`vector<WavePart>` in a struct in another struct) without that branch
  is 1107 green, so the structure is innocent and the enclosing
  function's SIZE is the ingredient.
  ⚠ **Two false leads, both measured**: not the trailing `u8` field
  (`integer` aborts identically), and not one inline expression —
  binding `tok[i + 1] ?? ""` merely MOVED the abort to a different
  unrelated file.  Any perturbation relocates the damage, which is
  exactly why the first two "fixes" looked like fixes.
  **Workaround:** give the vector its own small function
  (`compose_fault` / `compose_parts` / `script_compose`).  ⚠ A green
  suite cannot see a violating call site.
- **`graphics::KEY_*` need explicit qualification.** Bare-name
  UPPER_CASE constants without `pub` don't re-export across
  `use` chains.  `gl_key_pressed(graphics::KEY_W)` works;
  `gl_key_pressed(KEY_W)` doesn't.
- **JSON cast HANGS on ≥8 declared fields with a
  `vector<Struct>`.**  `text as MapFile` with 10 fields hangs
  forever; 7 fields work.  MapFile is constrained to 6 fields
  until the loft fix ships.
- **`:j` formatter omits empty fields** (empty strings, empty
  vectors, zero ints under some conditions).  Round-trip
  `save → load` of a struct with empty fields can produce JSON
  the cast can't reload.  We avoid empty fields in MapFile.
- **Empty `[]` after a text field in JSON corrupts the prior
  field on cast.**  `{"name":"b","items":[]}` reads back as
  `name=""`.  We keep vectors non-empty (or put them first).
- **Early `return (a, b)` of a tuple of two struct types fails
  type-check**, despite the if-else *expression* form of the
  same tuple working.  In `load_map_or_empty` we use the
  if-else expression form, not early return.
- **`text as Struct` cast IGNORES unknown JSON fields**
  (lenient — @P366 fixed).  We rely on this for forward-compat
  saves.
- **A missing `use` reports as `Expect token ;` on a tuple
  access.**  Calling a function from a module the file didn't
  import leaves its return untyped, so the *next* line's `.0`
  fails to parse — and the whole aggregator goes red with
  "parse errors" while the real mistake (the absent `use`) is
  never named.  `Expect token ;` on a `.0` / `.1` line means the
  tuple's producer didn't resolve; check the imports first.
- **A struct literal that omits a field takes that field's
  default silently** ([loft#914](https://github.com/loft-lang/loft/issues/914)
  — both backends, and `loft --check` says ok).  So in any struct
  that callers build field-by-field — `EditorInput` above all —
  the NEUTRAL value must be the ZERO value.  A "none" sentinel of
  `-1` becomes `0` in every partial literal, which for a palette
  index means "select sea", which erases.  Build from the
  `*_empty()` factory, not from a literal.
  ⚠ **loft HAS declared field defaults** — `palette_pick: integer
  = -1` is honoured by a literal (measured 2026-08-14) — so the
  rule above is a workaround for not knowing, and `EditorInput`'s
  `in_select_palette` / `in_palette_index` PAIR could be one
  field.  ⚠ Literal-only: a `text as Struct` cast IGNORES a
  declared default ([loft#876](https://github.com/loft-lang/loft/issues/876)),
  so nothing dryopea loads from JSON may lean on one.
- **Loop variable name reuse must keep consistent type per
  function-scope** ([loft#915](https://github.com/loft-lang/loft/issues/915))
  — different types in different loops fails ("loop variable 'i'
  has type text but was previously used as integer"), and the
  variable OUTLIVES its loop.  Prefix loop vars per function; 122
  of `src/`'s 131 loops do.
- **Two libraries may declare one struct name; qualify at the use
  site.** `camera::InputState` / `input::InputState` both work, and
  the bare name is a clean error naming its own fix.  The old
  `Double structure type …` panic is GONE — so plan 07 W1's stated
  blocker is stale, and no `Hex` → `Axial` rename is needed.
  ⚠ **But that error dumps a FALSE `warning[lost-write]` against
  `src/spawn.loft::move_order`** ([loft#883](https://github.com/loft-lang/loft/issues/883)).
  The write is fine — measured on both backends.  Qualify the type
  and the warning goes with the error; do **not** go "fix"
  `move_order`.  It bites because `lost-write` is the one warning
  class that catches loft's most expensive real bug (plan 11 F8),
  so it reads as urgent, and because a green suite never aborts —
  the warning is unreachable by the warning-clean gate.

