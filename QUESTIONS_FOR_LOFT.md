<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Questions for loft

Outbound queue from dryopea to the
[loft](https://github.com/jjstwerff/loft) project: questions,
language / runtime / stdlib feature requests, and problem-fix
asks that dryopea has surfaced and that need to be addressed in
loft.

Each entry is its own short section. When an entry is handed
over to loft (filed as an issue, mentioned in a session, or
otherwise actioned upstream), move it from **Open** to
**Submitted** with a note of what was done. When loft ships the
fix / feature, move it to **Resolved**.

## Entry template

```markdown
### <short title>

- **Found while:** <what dryopea was doing when this surfaced>
- **Kind:** question | feature | bug
- **What dryopea needs:** <one or two sentences>
- **Workaround in dryopea (if any):** <…>
- **Loft pointer:** <P-issue / plan / doc section, if known>
```

## Open

### A missing `use` reports as `Expect token ;` at a tuple access, not as an unknown function

- **Found while:** plan 08 V0b — moving the editor's reload action into
  `src/editor_step.loft`, which called `load_map_or_empty` (returns
  `(PaintedWorld, EditorCamera)`) without `use save;` in the file.
- **Kind:** bug (diagnostic quality)
- **What dryopea needs:** the error should name the unresolved call. Today
  the whole aggregator fails to parse and the two errors point at the
  *tuple accesses* on the following lines:

  ```
  error: Expect token ;
    --> src/editor_step.loft:377:33
      |
  377 |         es_pw_new  = es_loaded.0;
      |                                 ^
  ```

  The cause is one line above and not mentioned: `es_loaded` came from an
  unresolvable function, so it has no type, so `.0` cannot parse as a tuple
  index. Every test file went red with "parse errors" while the actual
  mistake was a missing import in a library file — the reported location is
  a consequence, and the real one is invisible.
- **Workaround in dryopea:** add the missing `use`. Recognising the shape is
  the whole cost: **`Expect token ;` on a `.0` / `.1` line means the tuple's
  producer did not resolve.**
- **Loft pointer:** none yet. Related to the resolved
  § Tuple-component cast `local.N as Type` — parse path, which was also a
  tuple-access site reporting someone else's problem.

### `use` does not namespace struct TYPES per library — two libraries defining the same struct name panic at registration

- **Found while:** Plan 07 W1 — trying to adopt `moros_map`'s
  `Map` as dryopea's world model.  dryopea's `world` lib defines
  `struct Hex { q, r }` (an axial coord); `moros_map` defines
  `struct Hex { h_height, h_material, … }` (a world cell).  Both
  are valid; loading both is impossible.
- **Kind:** bug (module system — type identity is not namespaced
  by library).
- **Behaviour:** `use world; use moros_map;` panics at
  `src/database/types.rs:53:9` — `Double structure type Chunk`
  (or `… Hex`, or a parse cascade inside moros_map, depending on
  `use` order).  An internal Rust panic, not a clean diagnostic.
- **Why it's a bug, precisely:**
  - Qualified ACCESS already works: with `use moros_map;` alone,
    `moros_map::map_empty()` / `moros_map::map_get_hex(...)`
    resolve fine.  So `use` namespaces *function* access.
  - But type REGISTRATION is a flat GLOBAL table keyed by bare
    name.  Two `use`d libraries each registering `Hex` collide
    there, **before** any access — so `world::Hex` vs
    `moros_map::Hex` qualification cannot rescue it.
  - Contrast: a USER struct clashing with a `use`d lib gives a
    CLEAN error ("struct 'Hex' conflicts … pick a different
    name").  loft already detects clashes gracefully in that
    path; the lib-vs-lib path panics instead.
  - Related facet: a standalone script that `use`s a library
    which is also a transitive package dependency double-
    registers that library's own structs → `Double structure
    type Chunk` (moros_map's `Chunk` via two load paths).  So the
    registry isn't idempotent per library either.
- **Reproducer:** [`loft_repros/dup_struct_type_across_libs.loft`](loft_repros/dup_struct_type_across_libs.loft)
  — `use world; use moros_map;`, panics at load.
- **What dryopea needs:** `use` should namespace struct types per
  library so two libraries can each define `Hex`, disambiguated
  at the use site as `world::Hex` / `moros_map::Hex` (mirroring
  function-access qualification).  At minimum, a clean compile
  error instead of an internal panic.
- **Workaround in dryopea (under consideration):** rename
  dryopea's coordinate type `Hex` → `Axial` so no two loaded
  libraries share a struct name.  Unblocks W1 without the loft
  fix, and arguably more correct (moros_map's `Hex` is the world
  cell; dryopea's was an axial coordinate).  Sizeable mechanical
  rename across `world` / `camera` / `render` / `painted` /
  `marker_render` / `save` + tests; golden PNGs unaffected
  (render output is identical).  A `hex_distance` FUNCTION-name
  clash with moros_map remains to be checked after the rename.
- **Loft pointer:** `src/database/types.rs:53` (struct-type
  registration) — make the type table per-library / qualified
  rather than a flat global keyed by bare name; and make the
  registration idempotent so a library reachable via two load
  paths registers once.

### Converge gridmesh on ONE ground-level grid (axial flat-top); migrate audience_crystal off the offset-pointy placeholder

- **Found while:** Evaluating why the dryopea editor feels
  sluggish — the renderer rebuilds the whole world every frame
  (no chunk system, no dirty mechanism; `src/render.loft::
  render_to_canvas` re-rasterises every painted hex per frame).
  The fix is to adopt gridmesh's chunk + dirty pipeline
  (`ChunkField`, `field_mark_dirty`, `collect_dirty_inputs`).
- **Kind:** feature / architectural decision (coordinate-layout
  convergence across the shared hex libraries).
- **Decision (agreed with the project owner):** the loft hex
  ecosystem must share ONE ground-level grid convention so the
  libraries can borrow rules/meshes from each other.  That
  convention is **axial flat-top** — the model `moros_map` /
  `moros_render` (the real-world consumer) and dryopea already
  use, and the layout gridmesh's own header names as its target
  ("a coordinate-layout adapter for moros's axial flat-top
  grid, land in later phases").  `audience_crystal`'s
  **offset-pointy** coords are explicitly the Phase-A
  extraction placeholder ("extracted verbatim from
  audience_crystal so its output is unchanged"), NOT the
  destination.
- **What dryopea needs (loft-side work):**
  1. Wire gridmesh's planned **axial-flat-top layout adapter**:
     consume the currently-unread `layout` field so
     `step_x`/`step_y` branch to the plain-axial form
     (`nbr_q = q + k·axial_dq(d)`, `nbr_r = r + k·axial_dr(d)` —
     no offset/parity round-trip).  `axial_dq`/`axial_dr` and
     the chunk-bucketing math (`chunk_of`, pure integer) are
     already layout-invariant, so this is a small, localised
     change.
  2. **Migrate `audience_crystal` (and `tools/audience-demo`)
     onto the shared axial layout** — it's the prototype / odd
     one out; converging it removes the only offset-pointy
     consumer.
- **Why dryopea isn't the one to change:** conforming dryopea
  to offset-pointy would rewrite ~600 lines of `world.loft` /
  `render.loft` / `marker_render.loft`, reindex the 6-way
  direction tables, re-coordinate the example maps, and
  rebaseline all 16 golden PNGs — *and* make dryopea diverge
  from moros, the exact divergence we're eliminating.
- **dryopea-side follow-up (not blocked):** dryopea can consume
  gridmesh's chunk + dirty engine *now* for the per-cell-
  independent ground fill (feed axial `(q,r)` as `(x,y)`,
  `halo_k = 0`, supply its own filled-hex rule — `tools/
  audience-demo/crystal_render.loft::draw_filled_hex` is the
  template).  The axial layout adapter is only required once
  ground rendering becomes **neighbour-dependent** (coastlines,
  slope seams, autotiling, extrusion borders) — which is
  expected soon but doesn't gate the initial adoption.
- **Loft pointer:** `lib/gridmesh/src/gridmesh.loft` — the
  `layout` field on `ChunkField` (unread today), `step_x` /
  `step_y` (lines ~56-64, offset↔axial parity round-trip),
  `axial_dq`/`axial_dr` (already correct); `lib/audience_crystal`
  + `tools/audience-demo` as the prototype to migrate; the
  Phase-A note in the gridmesh header.

### Function returning a freshly-allocated Store leaks one Store per call when it has a struct-typed value parameter

- **Found while:** Debugging the editor's exit-time warning
  `2 stores not freed at program exit: kt=106 Canvas×59, kt=85
  main_vector<single>×1`.  Each rendered frame leaked one Canvas
  (`Canvas×59` ≈ ~1s of running); the count grows unbounded for
  as long as the editor window is open (~960×720×4 bytes each).
- **Trigger (specific + minimal):** a function that takes a
  **struct-typed parameter by value** AND returns a **newly
  allocated user-data Store** (e.g. `canvas(...)`), called
  repeatedly, leaks one returned Store per call.  The *identical*
  function with only **scalar** parameters frees the returned
  Store correctly.  Bisected away every other suspect:
  - scalar-param helper returning a Canvas — CLEAN (control).
  - struct-param helper returning a Canvas — **LEAKS** ×N.
  - hash field / hash-iteration in the body — not required
    (a plain scalar-only struct param leaks just the same).
  - early `break` out of the call loop — not required (leaks on
    a loop that runs to completion too).
  - canvas allocated **inline** in the caller (not via a
    returning helper) — CLEAN.
- **Minimal reproducer:** [`loft_repros/canvas_store_leak_struct_param.loft`](loft_repros/canvas_store_leak_struct_param.loft)
  — runs via `loft --interpret`.  Core shape:
  ```loft
  struct P { a: integer not null }
  fn render_struct(p: P) -> Canvas { cv = canvas(64, 48, p.a); cv }   // LEAKS ×N
  fn render_scalar(a: integer) -> Canvas { cv = canvas(64, 48, a); cv } // CLEAN
  ```
  Exit report for the struct-param loop (9 calls):
  `Warning: 1 stores not freed at program exit: kt=97 Canvas×9`.
- **Kind:** bug (Store lifetime / drop accounting — the struct
  value parameter appears to skew the returned Store's refcount
  or drop bookkeeping so the per-call Store is never released).
- **Concrete dryopea impact:** `src/render.loft::render_to_canvas(
  cam: EditorCamera, pw: PaintedWorld, …) -> Canvas` matches the
  shape exactly (two struct params, returns a fresh Canvas) and
  is called every frame from `src/main.loft`'s render loop, so
  the live editor leaks one full-screen Canvas per frame.
- **Workaround in dryopea (not yet applied):** hoist the
  framebuffer out of the loop — allocate one Canvas before the
  render loop and `clear()` + redraw into it each frame, so no
  Store is allocated/returned per frame.  Requires a
  `render.loft` API tweak (draw-into-existing-Canvas variants
  alongside the current allocate-and-return ones).  Retire once
  the runtime frees the returned Store.
- **Loft pointer:** Store drop/refcount accounting for a
  function's freshly-allocated return value, in the presence of
  a by-value struct parameter (interpret backend; `kt=97`
  Canvas store type).

### `vector<Struct>` with trailing `u8` fields — corrupts when wrapped in a parent struct and serialised via `:j`

- **Found while:** Plan 03 M3 — saving the marker sidecar
  (`marker_world_to_file` in `src/save.loft`).  Earlier
  verification (2026-05-27) marked this Resolved based on a
  partial probe; refined probe shows the bug is **still
  present** for a specific path.
- **Trigger (specific):** the bug only fires on the path
  `hash<Struct[k]>` → `for`-iterate-and-append into a fresh
  `vector<Struct>` → embed in a wrapper struct → `:j` the
  wrapper.  Pulling any one of those steps apart makes the
  output correct.  Concretely:
  - Standalone `{vec:j}` — WORKS (the vec serialises right).
  - Building the vector directly (no hash, no for-loop) and
    wrapping → `{wrapper:j}` — WORKS.
  - Building via hash-iteration → wrapping → `{wrapper:j}` —
    **CORRUPTS**: every u8 field zeroes, AND the leading
    integer fields zero too.
- **Minimal reproducer:** [`loft_repros/u8_vector_in_wrapper.loft`](loft_repros/u8_vector_in_wrapper.loft),
  runs via `loft --interpret`.  Code inline below for context:
  ```loft
  struct Pair {
      q:         integer not null,
      r:         integer not null,
      kind:      u8      not null,
      direction: u8      not null,
  }
  struct Bag { items: hash<Pair[q, r]> }
  struct Wrapper {
      version: integer not null,
      name:    text    not null,
      items:   vector<Pair>,
  }

  fn main() {
      bag = Bag { items: [] };
      bag.items[1, 1] = Pair { q: 1, r: 1, kind: 0 as u8, direction: 0 as u8 };
      bag.items[2, 2] = Pair { q: 2, r: 2, kind: 0 as u8, direction: 3 as u8 };

      out: vector<Pair> = [];
      for e in bag.items {
          out += [Pair { q: e.q, r: e.r, kind: e.kind, direction: e.direction }];
      }
      println("standalone: {out:j}");           // CORRECT
      w = Wrapper { version: 1, name: "test", items: out };
      println("wrapped:    {w:j}");              // ALL ZEROS
  }
  ```
  Observed output:
  ```
  standalone: [{"q":1,"r":1,"kind":0,"direction":0},{"q":2,"r":2,"kind":0,"direction":3}]
  wrapped:    {"version":1,"name":"test","items":[{"q":0,"r":0,"kind":0,"direction":0},
                                                   {"q":0,"r":0,"kind":0,"direction":0}]}
  ```
- **Kind:** bug (`:j` formatter or vector-of-struct copy
  semantics — context-sensitive: the wrapping struct's `:j`
  walk reads vector members from a different / stale location
  than the standalone walk does).
- **Workaround in dryopea:** `marker_file.loft` declares a
  wider on-disk shape `MarkerSaveEntry { q, r, kind: integer,
  direction: integer }`; `save.loft`'s `marker_world_to_file`
  widens u8 → integer when building the save vector.  Same
  idiom as painted.loft / map_file.loft (PaintedHex.kind: u8
  in memory ↔ GroundEntry.kind: text on disk).  Retirable
  once the wrapper-struct path serialises correctly.
- **Loft pointer:** `:j` formatter's nested-struct vector
  walk vs. standalone vector walk — different code paths
  diverge for u8 fields.

### `const` parameter store-lock blocking unrelated writes — multi-const + write-through-other-param shape

- **Found while:** Plan 03 follow-up history work
  (`src/history.loft::clear_and_record`).  Initial verification
  (2026-05-27) of an earlier filing marked this Resolved based
  on a partial probe (single `const Bag` + write to a separate
  `Out` param worked).  Refined probe shows the bug is **still
  present** for the dryopea-faithful shape.
- **Trigger (specific):** function with TWO `const` struct
  parameters, each holding a hash, AND writing through a
  third (non-const) parameter whose path includes a nested
  vector field.  Single-const probes don't trigger; only the
  multi-const + nested-vector-write combination does.
- **Reproducer:** [`loft_repros/const_param_store_lock.loft`](loft_repros/const_param_store_lock.loft)
  — runs `loft --interpret`, reliably panics with `Claim on
  read-only store (size=2) (locked by: lock_store(store_nr=5,
  rec=1))`.
- **Kind:** bug (lock granularity for `const` parameter is
  Store-wide rather than parameter-scoped, and only the
  multi-const path widens the lock far enough to bite an
  unrelated write).
- **Workaround in dryopea:** dropped the `const` qualifier on
  `clear_and_record`'s pw + mw params (history.loft).
  Function still doesn't mutate them — convention enforces
  the intent.  Signature is documentation-weaker; retire when
  the bug ships fixed.
- **Loft pointer:** narrow the `const`-param lock to the
  specific record(s) named, not the whole Store.

### Native codegen panics returning a hash-bearing struct from an `if…else` expression whose condition uses `file()`

- **⚠️ Re-opened (2026-05-28) — earlier closure was a FALSE
  NEGATIVE.**  loft commit `d3906ef6` closed this as "not a loft
  bug — the repro used `struct E`, a stdlib-constant collision."
  True for that repro, but the *real* editor shape still panics
  natively.  The old reproducer was a trivial single-branch
  passthrough (`fn passthrough(p) -> W { w_empty() }`) that does
  **not** reproduce; rebisected to the precise trigger below.
- **Found while:** native `make play MAP=a`.  Native binary
  panics at `src/keys.rs:251:6`: `index out of bounds: the len
  is N but the index is 65535` — the `u16::MAX` "unknown type"
  sentinel surfacing as a real `DbRef.store_nr` (the branch-
  result struct's type info is lost).
- **Kind:** bug (native codegen — type tracking for a hash-
  bearing struct returned from an if/else expression is poisoned
  by the `file()` builtin in the same function).
- **Trigger (bisected, precise):** a function returning a
  hash-bearing struct via an **`if … else` EXPRESSION** whose
  condition uses the **`file()` builtin**.  Native panics;
  `--interpret` works.  Bisection:
  - PANICS: `fn f(p) -> W { if file(p).exists() { build() }
    else { w_empty() } }` (W contains a `hash<…>`).
  - WORKS: same shape with a **plain bool** condition.
  - WORKS: same shape with a **user fn** returning boolean as
    the condition (so it is `file()` specifically, not "a
    bool-returning call").
  - STILL PANICS: binding `ex = file(p).exists();` to a local
    first, then `if ex {…}` (so it is `file()`'s presence in
    the function, not the inline condition).
  - WORKS: declare once + statement-`if` reassign + single
    return (`out = w_empty(); if file(p).exists() { out =
    build(); } out`) — the dryopea-side workaround shape.
  The tuple variant (`-> (W, Cam)`) panics too when the W comes
  via a JSON cast (`text as MapFile` → `mapfile_to_painted`),
  but works when built without the cast — the bare-struct form
  above is the cleaner minimal case.
- **Reproducer:** [`loft_repros/struct_with_hash_native_return.loft`](loft_repros/struct_with_hash_native_return.loft)
  — `loft <repro>` panics at `keys.rs:251`; `loft --interpret
  <repro>` prints `items = 0`.
- **Concrete dryopea impact:** `src/save.loft::load_markers_or_empty`
  is exactly this shape (`if file(path).exists() {
  markerfile_to_world(...) } else { marker_empty() }`), and
  `load_map_or_empty` is the tuple-via-cast variant.  The native
  editor panics at startup before the GL window opens.
- **Workaround in dryopea:** `Makefile`'s `play` target stays on
  `--interpret`.  The single-return restructure (above) would
  unblock native per-function, but is held off pending the
  upstream fix (the user asked for a clean repro rather than a
  dryopea-side patch); a `play-native` target keeps the native
  invocation ready for testing the fix.
- **Loft pointer:** native codegen — the `file()` builtin's
  result type appears to clobber the inferred store type of a
  hash-bearing struct returned from an if/else expression in the
  same function.  Related to the @P374 tuple-return machinery
  but distinct (bare struct return + `file()` interaction).

### Div-by-zero warning still fires on `float / int_literal`

- **Found while:** Re-verifying the @P368 fix on 2026-05-27.
  The headline cases (`x / 0.75`, `x / 2.0`, `n / 4`, `n / 2`)
  no longer warn — but `12.0 / 3` (float dividend, integer
  literal divisor) still emits the rewritten warning.
- **Kind:** bug (partial-fix follow-up to @P368)
- **What dryopea needs:** `lit_nonzero` in
  `src/parser/operators.rs` recognises Int/Long/Float/Single
  literals, but the mixed-type `float / int_literal` path
  appears to widen the literal to float (or insert an `as
  float` cast) *before* the warning check reaches the
  divisor, so the literal-detection misses it.  Either lift
  the check above the widening, or also match the cast-
  wrapped literal.
- **Reproducer:**
  ```loft
  fn test() {
      x = 12.0;
      _ = x / 3;        // warns (expected: no warn — 3 is a non-zero int literal)
      _ = x / 3.0;      // no warn
      _ = 12 / 3;       // no warn
  }
  ```
- **Workaround in dryopea:** write `3.0` instead of `3` when
  dividing a float by an integer-valued constant.  Trivial
  but slightly fewer-bytes-on-disk-warts than the original
  precomputed-reciprocal workaround.
- **Loft pointer:** `src/parser/operators.rs::lit_nonzero` —
  add the float-coerced-int-literal arm.

## Submitted

Filed upstream as GitHub issues on `loft-lang/loft`; kept here as
dryopea's own record until the fix ships, then moved to Resolved.

### Native backend silently returns an EMPTY vector for a `text as vector<Struct>` cast in tail-return position

- **Filed:** [loft-lang/loft#866](https://github.com/loft-lang/loft/issues/866) on 2026-08-12
  (`bug`, `wa:clean`, `sev:medium`, `area:native`, `area:codegen`, `hit-by:dryopea`).
- **Found while:** plan 08 V4 — building `scripts/validate.sh`, which
  runs dryopea as a PROGRAM rather than under `loft test`.  The first
  probe of the gate printed `palette: 0 entries` where the same call
  under `--interpret` prints 11.
- **Kind:** bug (native codegen — **silent wrong answer**, no panic and
  no diagnostic)
- **Trigger (bisected):** a function whose body IS the cast, with the
  path arriving as a **parameter**:
  - WRONG (`[]` natively, right interpreted):
    `fn load(path: text) -> vector<Row> { file(path).content() as vector<Row> }`
  - RIGHT: the same cast bound to a local first, then returned.
  - RIGHT: the same cast written inline at the call site.
  - RIGHT: the same tail-return shape reading a file-scope `const`
    path instead of a parameter — which is why this does not show up
    in small test programs.
- **The generated Rust says it too:** building the native binary warns
  `unused return value of loft::codegen_runtime::db_from_text that must
  be used` at exactly these call sites — the cast is emitted as a
  statement and the function returns the untouched (empty) destination.
- **Reproducer:** [`loft_repros/json_vector_cast_native_tail_return.loft`](loft_repros/json_vector_cast_native_tail_return.loft)
  — self-contained; `loft <repro>` prints `tail-return -> 0`,
  `loft --interpret <repro>` prints `tail-return -> 2`.
- **Why this one hurts more than the panic above:** it is silent.  The
  hash-return bug (§ above) aborts the native editor before the window
  opens, so nobody ships on it by accident.  This one hands back an
  empty palette and lets the editor open: no picker entries, every hex
  renders sea, and nothing anywhere says why.  dryopea's 318-test suite
  cannot see it at all, because `loft test` runs the interpreter.
- **Concrete dryopea impact:** `src/palette.loft::load_palette` is
  exactly this shape.  Natively `load_palette` → 0 entries →
  `picker_default` → 0 entries.
- **The struct-shaped twin is worse and is filed separately** — see
  § "`text as Struct` returned out of a function corrupts the store"
  below.  Two things follow for whoever picks this up: inside the full
  dryopea program the direct `text as MapFile` form answers a
  null-filled struct natively (so the local-binding workaround above
  has no struct twin), and binding to a local — the obvious thing to
  reach for — is the *trigger* of the other bug rather than a fix.
- **Workaround in dryopea:** none applied in source.  `scripts/validate.sh`
  runs `--interpret` (the same choice `make play` already made for the
  panic above), and `validate_all` refuses to run at all when the
  palette loads 0 entries — so the failure is loud from now on
  wherever it happens.
- **Loft pointer:** native codegen, `db_from_text` emission for a cast
  in tail-return position — the result is discarded rather than
  returned.

### `text as Struct` returned out of a function corrupts the store (interpreter SIGSEGV, native rustc error)

- **Filed:** [loft-lang/loft#867](https://github.com/loft-lang/loft/issues/867) on 2026-08-12
  (`bug`, `wa:partial`, `sev:high`, `area:store-lifetime`, `area:native`,
  `both-backends`, `hit-by:dryopea`).
- **Found while:** plan 08 V4 — looking for a dryopea-side workaround
  for the `vector<Struct>` bug above.  Binding the cast to a local is
  the obvious fix there; for the struct-shaped cast it is the trigger.
- **Kind:** bug (**memory unsafety** on the interpreter; emitted-Rust
  compile failure on native)
- **Trigger:** a `text` local cast to a struct, where the result
  crosses a **function return**:
  ```loft
  fn load(p: text) -> S {
      t = file(p).content() ?? "";   // a text local …
      t as S                         // … cast, and returned
  }
  ```
  The interpreter prints
  `loft: BUG (#306): refused to free the stack store (#0) (rec=0,
  pos=0, var='') — a stack-record ref was treated as an owned heap
  store` and then takes SIGSEGV at `OpAppendText`.  The native backend
  never gets to run it: the emitted Rust fails with 4 ×
  `error[E0308]: mismatched types`, assigning `var_t = "".to_string()`
  into a binding typed `DbRef`.
- **It is the RETURN, not the cast.** Bisected 2026-08-12:

  | shape | interpreter | native |
  |---|---|---|
  | `fn load(p) -> S { file(p).content() as S }` | correct | runs, answers a **null-filled struct** |
  | `fn load(p) -> S { t = …content() ?? ""; t as S }` | **BUG #306 → SIGSEGV** | **rustc E0308** |
  | `fn load(p) -> S { t = …; m = t as S; m }` (non-tail) | BUG #306, no crash, answers **null** | runs, answers null |
  | the same cast inside a fn that returns nothing | correct | correct |
  | a text local with no cast | correct | correct |

  Neither the `?? ""` nor a `vector<…>` field is needed — a two-field
  flat struct with a pre-declared `t: text` does it.  Whether it
  segfaults or silently corrupts tracks tail vs non-tail position,
  which makes the quiet half the dangerous one.
- **Reproducer:** [`loft_repros/struct_cast_via_text_local_returned.loft`](loft_repros/struct_cast_via_text_local_returned.loft)
  — self-contained, two-field struct, no dryopea types.
- **Concrete dryopea impact:** none in source today —
  `src/save.loft::load_map_file` / `load_marker_file` use the direct
  form.  Filed because the direct form is what the sibling bug above
  pushes you off of.
- **Loft pointer:** store ownership — a stack-record ref from the cast
  destination escaping through the return path and being freed as an
  owned heap store (guard #306).  Probably the same area as
  OWNERSHIP_MODEL.md's store-lifetime work.


## Investigated — no bug

### Vector-in-struct pass-by-value (false alarm)

Observed during E2 picker work: passing a `Picker { palette,
active }` by value to `render_picker(p: Picker, ...)` produced
`len(p.palette) == 0` inside the callee, even though the
caller's `len(picker.palette) == 11`.

Filed as a suspected fourth bug; reproducer was constructed
and **the bug did not reproduce**.  Plain `vector<integer>`,
`vector<Struct>`, and "inline call inside struct ctor"
patterns all behave correctly: caller and callee see the same
elements.

```loft
struct Item { name: text, n: integer }
struct Wrap { items: vector<Item> }

fn make_wrap() -> Wrap {
    Wrap { items: [Item { name: "a", n: 1 }, Item { name: "b", n: 2 }] }
}
fn consume(w: Wrap) {
    println("consume: items.len = {len(w.items)}");  // → 2 (correct)
}
```

**Root cause:** the apparent "empty vector inside the struct"
was actually the **JSON-cast-with-extras bug** (now § Resolved
as @P366) hiding a load that was returning zero entries from
the start.  Once `GroundType` declared all four extra optional
fields the JSON has (`variant`, `color_status`,
`height_override`, `end_drivable`), the cast started returning
11 entries, the struct correctly carried them across the
value-pass, and the picker rendered.

Notable: **@P366** (silent JSON-cast empty on strict-reject) and
**@P367** (test runner not failing on assert) were **compound**
— they masked each other for ~half an hour of debugging,
producing a green test suite while every assertion was running
against a 0-length palette.  Both fixed in loft commit `42f8228`.

## Resolved

### `graphics::fill_triangle` never fills — integer division before multiply collapses every scanline

- **Found while:** bringing dryopea's suite back to green after the
  library migration.  All 10 golden-image tests that draw a *painted*
  hex failed; sea-only renders passed.  `src/render.loft::draw_hex`
  builds each hex from six `cv.fill_triangle` calls, so every hex
  rendered as a cross.
- **Kind:** bug (library — `loft-libs-graphics/graphics`).
- **The defect:** each edge interpolation divided before it multiplied,
  in integer arithmetic, so `(tr_y - ta_y) / (tc_y - ta_y)` was 0 on
  every scanline but the last.  The fill degenerated to a vertical line
  at the apex plus the base row; a flat-top triangle degenerated the
  other way and filled its bounding rectangle.
- **RESOLVED — released as `graphics v0.5.1`** and published to the
  registry (index `updated` 2026-08-12T09:34:56Z).  Fixed by scaling the
  run before the divide at all three interpolation sites.
- **dryopea is 189/189 green** on 0.5.1, with the committed goldens
  matching byte-for-byte — no re-baselining.  The goldens were correct
  all along, which is mutual confirmation the fix restores the intended
  output.
- **Why the library's own tests missed it:** every assertion in
  `test_fill_triangle` sampled the apex column or the base row — exactly
  the two lines the broken code still drew.  A centroid check would not
  have caught it either: for that test's triangle the centroid sits *on*
  the apex column.  The release adds
  `test_fill_triangle_interior_off_axis` and
  `test_fill_triangle_flat_top_not_a_rectangle`, both verified to fail on
  the old code and pass on the new.

> Earlier batch verified against `~/Documents/loft/target/release/loft`
> built from commit 42f8228 ("Fix @P366/@P367/@P368") on 2026-05-27.
> Dryopea suite 60/60 then green; per-bug reproducers in
> `$TMPDIR/p_followups/loft_fixes/`.
>
> Later batch (@P372–@P375 + `store_persist_bind`) tracked on the
> loft `libraries` / `bumper_plane` branches as the upstream agent
> ships fixes; the local loft binary at
> `~/Documents/loft/target/release/loft` always carries the latest
> resolved set per the cross-project coordination note.  Dryopea-
> side workarounds retire as the relevant code touches them; not
> all retirement happens in one sweep.

### Tuple-component cast `local.N as Type` — parse path

- **Verified fixed:** 2026-05-27 via three-form probe
  (`/tmp/tuple_cast_probe.loft`): unparen `p.0 as float`,
  parenthesised `(p.0 as float)`, and bind-then-cast all
  parse-check cleanly (`exit 0`).  No specific `@P` number
  in loft's recent log — fix may have been absorbed into the
  broader parser-fix batch (84b6592 et al.) rather than
  filed on its own.
- **Workaround retirement (pending):** `marker_render.loft`
  inlines world→canvas projection math to keep floats end-
  to-end and dodge the bug.  Can revert to a
  `world_to_canvas(...)` call + `(tuple.N as float)` cast,
  saving ~12 duplicated lines across `draw_marker_arrow` and
  `draw_target_marker`.

### Cannot pass a literal/expression to a non-`&` parameter

- **Original observation:** during plan 03 follow-up M3 tests,
  `takes_four_worlds(cur_pw, cur_mw, ld_pw, marker_empty(), h)`
  failed with `Cannot pass a literal or expression to a '&'
  parameter — assign to a variable first` despite NONE of the
  world params being declared `&`.  Workaround was to bind
  every struct-valued call expression to a local first;
  ~4 extra `let` per reload-record test.
- **Verified fixed:** 2026-05-27 via
  `/tmp/probe_literal_to_param.loft` — `pl_takes_four(x1, x2,
  x3, w_empty())` ran cleanly, printed `sum = 0`.  Function-
  call expressions pass directly to value-typed parameters
  now without the intermediate-binding ceremony.
- **Workaround retirement (pending):** test bindings in
  `tests/03_qol_history.loft`'s reload-record tests
  (~lines 300-360) can simplify — inline the `_pw` / `_mw`
  args directly.  Cosmetic test cleanup; ~16 lines removed.

### @P375 — `{x:j}` / `to_json()` omitted present-but-empty fields

- **Loft commit:** 83ebd55 ("Fix @P375: …")
- **Found by dryopea:** Plan 01 E4 — `paint_to_mapfile` produced
  a MapFile with `description: ""`, `markers: []`, `waves: []`;
  `{m:j}` dropped all three from the output, so the round-trip
  cast → load got partial JSON that either default-filled or
  hung (until @P372 also shipped).  Reproducer:
  ```loft
  struct S { a: text not null, b: vector<integer>, c: integer not null }
  s = S { a: "", b: [], c: 0 };
  println("{s:j}");
  // pre-fix: {}      post-fix: {"a":"","b":[],"c":0}
  ```
- **Fix:** `{x:j}` and `to_json()` now emit EVERY declared field
  including empty strings / empty vectors / zero integers.
- **Workaround retirement (pending):** `src/save.loft::save_markers`
  no longer needs its "skip-write-if-empty + delete-on-empty"
  branch for the EMISSION reason (the empty-vector reload would
  no longer trip the cast bug — @P373/@P375 both fixed).  The
  delete-on-empty behaviour is still cleaner UX (no zombie sidecar
  on disk after clearing), so the BEHAVIOUR stays but the
  bug-driven rationale is gone.

### @P374 — `return (tuple-of-structs)` rejected vs final-expression tuple

- **Loft commit:** 84b6592 ("Fix @P374: …")
- **Found by dryopea:** Plan 01 integration smoke test —
  `load_map_or_empty(path, palette) -> (PaintedWorld,
  EditorCamera)` rejected the early-return `return (pw, cam);`
  but accepted the same tuple as a final expression.  Identical
  textual halves: `expected __tuple<...>, got (...)`.
- **Fix:** function declaring `-> (A, B)` (structs, rewritten to
  `Reference(__tuple<A, B>)`) now accepts the equivalent
  `return (A{…}, B{…});` form, matching the final-expression
  behaviour.
- **Workaround retirement (pending):** `src/save.loft::load_map_or_empty`
  uses an if-else expression form to dodge this; can rewrite
  with `return` if cleaner reads, but the existing form is
  fine — purely cosmetic retirement.

### @P373 — `text as Struct` corrupts the field before an empty `[]` array

- **Loft commit:** 27560e6 ("Fix @P373: …")
- **Found by dryopea:** Plan 01 E4 — empty `markers: []` /
  `waves: []` in MapFile JSON wrecked the field immediately
  before them.  Reproducer:
  ```loft
  struct Box { name: text not null, items: vector<Item> }
  json = `{"name":"b","items":[]}`;
  b = json as Box;   // observed name=[]  expected name=[b]
  ```
- **Fix:** the empty-array branch in `walk_parsed_into` was
  writing the collection's default to the struct's BASE (field
  0) instead of the collection field's slot.  Corrected.
- **Workaround retirement (pending):** MarkerFile's "non-empty
  vectors only" discipline is no longer bug-driven; can carry
  empty vectors in the on-disk JSON once dryopea decides to.
  Combined with @P375, `save_markers` could simplify
  significantly.

### @P372 — `text as Struct` hangs (infinite loop) for structs over 56 bytes

- **Loft commit:** 58a3167 ("Fix @P372: …")
- **Found by dryopea:** Plan 01 E4 — `text as MapFile` hung
  forever when MapFile had 10 fields.  Originally suspected a
  field-count threshold; root cause turned out to be struct
  BYTE size > 56 (the fixed `database(8)` claim = 64 bytes, with
  the 8-byte header leaving 56 for the payload).  8+ integer
  fields trigger it deterministically; a vector field is one
  reliable way to push past.
- **Fix:** `db_from_text` now sizes the claimed record by the
  declared struct size instead of the fixed 64-byte default;
  larger structs no longer corrupt the heap walker → no
  infinite loop.
- **Workaround retirement (pending):** the 6-field cap on
  MapFile (`src/map_file.loft`) is no longer required.  Plan 04
  § L1 can land its full schema (markers, waves, objective,
  description, …) without splitting into the multi-file
  workaround pattern.  Decision on whether to *actually* fold
  the marker sidecar into MapFile is independent — the sidecar
  has cross-consumer value (@PLAN50 reads markers without
  parsing the rest of MapFile) regardless of whether the cast
  bug is fixed.

### `store_persist_bind` — path-backed user-data Store binding

- **Loft commit:** 4a7e775 (@PLAN38 phase 01c, on `origin/main`)
- **Filed by dryopea:** Designing the persistence destination —
  the world will grow with stencils; serialising every save is
  wasted IO when the runtime already keeps the data in a Store
  buffer that could just as easily be path-backed mmap.  Filed
  asked for an `.loft`-level way to declare *"the user-data
  Store for these records lives at this file path."*
- **Fix:** `pub fn store_persist_bind(r: hash, path: text) ->
  boolean;` (`default/02_images.loft:366`).  Per-instance
  runtime call rather than the declarative `#persist` syntax
  sketched in the original ask, but functionally what was
  asked.  Two modes:
  - Fresh path (not on disk yet) — serialises the current
    in-memory Store at the hash's slot, pads to ≥1024 words
    with a valid tail-free block, mmaps the file.  Existing
    DbRefs remain valid.
  - Existing path — opens via `Store::open`, drops in-memory
    contents in favour of the on-disk image.  Type layout
    must match.
  Fail-soft: returns `false` on any I/O / format error.  Pair
  with `store_durable_check(p)` / `store_durable_seal(p)` for
  crash-safety bracketing.
- **Workaround retirement (pending):** `src/save.loft`'s
  JSON-marshal save path (`save_world`, `save_map_file`,
  `paint_to_mapfile`, `mapfile_to_painted`, plus the marker
  sidecar equivalents) is replaceable with a one-line
  annotation on `PaintedWorld.painted` / `MarkerWorld.markers`.
  When this migration lands, the `MarkerSaveEntry` / `MapFile`
  / `MarkerFile` structs and most of `src/save.loft` go away.
  Strategy carried forward in [`plans/ROADMAP.md` § Persistence
  destination](plans/ROADMAP.md).

### @P367 — Test runner now surfaces assertion / runtime_error failures

- **Loft commit:** 42f8228 (`src/test_runner.rs`)
- **Fix:** test runner now extracts `had_fatal` +
  `runtime_error.message` from the run closure and routes
  typed-runtime-error halts through `matches_expect_fail`,
  so `assert(false, msg)` / `panic` / div-by-zero / any C66
  fault scores FAILED.  Side-effect: also repaired
  `@EXPECT_FAIL` for typed-error paths the panic-only code
  had silently broken.
- **Verified by dryopea:** `loft --tests
  $TMPDIR/p_followups/loft_fixes/p367_assert_fail.loft` now
  prints `FAIL  p367_assert_fail.loft::test_failing_assert
  — assertion failed: this should fail the test`, exit 1.
  A passing neighbour test in the same file still reports
  `ok` correctly.
- **Workaround retired:** `scripts/test.sh`'s marker-file
  grep (`_FAILED_*.txt`) is still active because
  `assert_golden` writes markers as a side-effect — but the
  marker is now a **redundant safety net**, not a primary
  failure signal.  The test runner alone is now sufficient.
  Marker-file path will be removed in a future cleanup.

### @P366 — `text as vector<Struct>` accepts JSON with extra fields

- **Loft commit:** 42f8228 (`src/database/structures.rs`)
- **Fix:** `walk_parsed_struct` now skips unknown JSON keys
  lenient-ignore style (one shared site, both backends),
  matching the dynamic `JsonValue` walker.  Missing declared
  fields still default-fill.  The strict-reject assertion in
  `tests/data_structures.rs::record` was flipped to expect
  the new behaviour.
- **Verified by dryopea:** dropped the 4 workaround fields
  (`variant`, `color_status`, `height_override`,
  `end_drivable`) from `GroundType` in `src/palette.loft`.
  All 18 `tests/01_e2_palette.loft` tests still green,
  golden renders byte-match.  Reproducer:
  `$TMPDIR/p_followups/loft_fixes/p366_json_extras.loft`.
- **Workaround retired:** GroundType matches the design intent
  (9 fields) instead of mirroring every key in palette.json.

### @P368 — No warning on division by a non-zero literal constant (PARTIAL)

- **Loft commit:** 42f8228 (`src/parser/operators.rs`)
- **Fix:** `lit_nonzero` now matches Int/Long/Float/Single
  literals (was Int-only), so `x / 0.75`, `x / 2.0`, `n / 4`,
  `n / 2` no longer warn.  Warning wording also reworded
  ("integer division/modulus" → generic "division").
- **Verified by dryopea:** all-float and all-int literal-
  divisor forms suppress cleanly.  Reproducer:
  `$TMPDIR/p_followups/loft_fixes/p368_div_warn.loft`.
- **Remaining gap:** `float / int_literal` (e.g. `12.0 / 3`)
  still warns.  Filed as a new § Open entry above.  Not
  blocking — the residual is one-character workaround
  (`3.0` instead of `3`).
- **Workaround partially retired:** the mid-precision
  precomputed-reciprocal pattern in `src/world.loft` is no
  longer strictly necessary, but kept for clarity / standard
  graphics idiom.  The other warn-suppressor — `1.0 / ppm`
  in `src/render.loft` — would still warn (variable
  divisor) and is unrelated to @P368.
