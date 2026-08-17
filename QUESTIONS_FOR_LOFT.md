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

### `imaging::Pixel` has no alpha, so a PNG cannot round-trip a sprite

- **Found while:** designing [`docs/PARTS.md`](docs/PARTS.md) (plan 20) —
  entity sprites are cut-outs blitted over the hex ground, so every one
  of them is mostly transparent.
- **Kind:** feature
- **What dryopea needs:** an alpha channel on `imaging::Pixel`.  It is
  `{ r, g, b }` today (`imaging` 0.2.1), and `png(self: File) -> Image`
  is the registry's only PNG **decoder** — so a sprite written with
  transparency comes back opaque.  ⚠ The asymmetry is the sharp part:
  `graphics::Canvas` already *writes* alpha (`save_png` "automatically
  uses RGBA if any pixel has alpha < 255") and already *composites* it
  (`blend_pixel`).  So loft can produce a file it cannot read back.
- **Workaround in dryopea (if any):** **the design avoids needing it** —
  `docs/PARTS.md` § D5 renders part-trees straight into `Canvas` sprites
  and treats PNGs as an ARTEFACT rather than a runtime input, so nothing
  decodes one.  ⚠ A colour key (magenta = transparent) would also work
  and is deliberately not taken: it is a 1990s workaround for a problem
  this design does not have to have.
  ⚠ **What it does block** is the other pipeline — hand-authored art
  PNGs from an artist, loaded at runtime.  That is a real future ask,
  not a hypothetical, which is why this is filed before it bites.
- **Loft pointer:** `imaging` 0.2.1, `src/imaging.loft` — `Pixel`,
  `value()` (which packs 24-bit `0xRRGGBB`), `load_png` / `n_load_png`.
  ⚠ Belongs to whichever repo owns `imaging`; `graphics` moved to
  `loft-libs-graphics`, so check there before filing against
  `loft-lang/loft`.

## Submitted

Filed upstream as GitHub issues; kept here as dryopea's own record until
the fix ships, then moved to Resolved.

### A file-scope `const vector` holding a NEGATIVE literal reads back EMPTY

**Filed 2026-08-17 as
[loft#955](https://github.com/loft-lang/loft/issues/955)** — `bug`,
`sev:high`, `wa:clean`, `area:codegen`, `both-backends`,
`hit-by:dryopea`.  Repro:
[`loft_repros/const_vector_with_a_negative_is_empty.loft`](loft_repros/const_vector_with_a_negative_is_empty.loft).

A `const` at file scope declared `vector<integer>` or `vector<float>`
and initialised from a literal is EMPTY if any element is negative —
`len()` of 0, every index `null(oob)`, no diagnostic on either backend
and none from `--native-emit` either.  ⚠ **The sign is the whole
trigger**: `[10, 9, 5, 0]` is fine and so is a twelve-element positive
literal, while `[10, -5, 9]`, `[-1, 2, 3]` and `[1.0, -2.0]` are all
empty.

⚠⚠ **What makes it worth a `sev:high` argument is that a loop over an
empty vector runs zero times, so every assertion inside it holds
VACUOUSLY.**  It cost plan 21 R1's
`tests/21_r1_the_camera.loft` § The overview IS the editor's view: the
ring of twelve hexes it projects two ways came back as twelve copies of
`Hex { q: null, r: null }`, which land on the screen centre, so the
bearings agreed perfectly and the gate reported a worst disagreement of
**exactly 0.0 rad**.  The only thing that caught it is that an exact
zero is not a value a comparison between integer pixels and a
floating-point projection can produce.

**Workaround (clean):** a LOCAL vector with the identical literal is
correct, so move the value inside the function that reads it.  That is
what `21_r1` does, with a ⚠ at the site saying why.

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

### Verified fixed on 2026-08-17 — the two store-lifetime bugs plan 23 filed

Both closed upstream 2026-08-16, and dryopea's whole corpus confirms it:
**1161/1161 under `scripts/test.sh`** and **33 scripts / 652 measurements
under `scripts/validate.sh`**, on the installed loft, with no workaround
flags.  ⚠ `tests/18_s3_the_crop.loft` is the live detector for the second
one and passes 8/8 — do not "fix" it if it ever goes red again.

⚠ **loft#939 was labelled `both-backends` when it closed**, which retires
this file's own note that `--native` looked clean: it did not, and the
255-vs-1017 crop length was the tell that native never ran the same
workload.  A backend that answers differently on a different workload is
not a backend that answers correctly.

### Returning a large struct by value poisons the store — the NEXT unrelated call corrupts an already-returned struct

- **Filed:** [loft#939](https://github.com/loft-lang/loft/issues/939)
- ✅ **FIXED** by loft `ac8fb1dc` (2026-08-16 00:59), *"A vector field
  assigned from a view frees what it only names"* — which is exactly
  `crop_state`'s `cs_out.crew = state.crew` / `cs_out.cargo =
  state.cargo`.  `tests/18_s3_the_crop.loft` passes 8/8 again.  ⚠ Kept
  here rather than moved to Resolved until a full `scripts/test.sh` run
  confirms it end to end — the graphics cdylib is blocking that, and a
  single green test file is not the corpus.
- **Found while:** inspecting the installed loft after plan 23 K3, when
  `scripts/test.sh` went red on a tree whose two gates were green.
- **Kind:** bug (memory safety), interpreter, **regression**
- **What happens:** `emit.loft::crop_state` builds a whole `WaveState`
  in a local and returns it **by value**.  That call is clean on its
  own — but the **next** `script_run`, which never names the first
  run's state, corrupts its `pick_cursor`: a plain `integer` field that
  nothing in the run ever writes reads back as a pointer.

  ```
  assertion failed: the state survives the crop: 'pick_cursor: 16673063043072 vs 0'
  ```

- ⚠ **Six controls, and the pair is the whole finding:**

  | # | sequence | `pick_cursor` |
  |---|---|---|
  | 1 | two `script_run`, no crop | 0 |
  | 2 | `emit_keys` + a second run | 0 |
  | 3 | crop **refused** (early return) + a second run | 0 |
  | 4 | crop accepted, nothing after | 0 |
  | 5 | crop accepted + 200 allocations | 0 |
  | 6 | **crop accepted + a second `script_run`** | **garbage** |

  (2) is the control that isolates it: `emit_keys` is the same emitter
  over the same data and differs only in not calling `crop_state`.

- ⚠ **The garbage is SHAPED** — `0xf2a00000000`, `0xf0500000000`,
  `0x53c00000000`: low 32 bits always zero, upper half a small counter.
  A field read landing one word off, not arbitrary freed memory.
- ⚠ **Regression window `0d46efef..a5ce016b` (45 commits), not
  bisected.**  The suite was **1161/1161 green** on a loft built
  2026-08-12 21:58 and is red on every build since.
  ⚠ **It is NOT loft#935's fix** — `b80fd632` post-dates a build that
  already failed, so that commit neither caused nor cured it.
- ⚠ **`--native` is INCONCLUSIVE, not correct.**  It answers 0, but
  loft#866 empties the palette natively so the crop emits 255 chars
  where the interpreter emits 1017 — a different workload.
- **Dryopea-side workaround:** none.  The two calls it needs are the
  feature (plan 18 S3), and `tests/18_s3_the_crop.loft` is what fails.
- ⚠ **No standalone reproducer**, same wall as loft#935: a small
  program with the identical shape runs clean, because the enclosing
  frame sizes are the ingredient.  `loft test 18_s3_the_crop.loft` from
  the dryopea root reproduces 3/3.

### A `vector<Struct>` local in a very large function corrupts the heap — and an unrelated test file is what dies

- **Filed:** [loft#935](https://github.com/loft-lang/loft/issues/935)
- **Found while:** plan 23 K1 — adding a `compose` branch to
  `src/script.loft::script_command` (~700 lines, complexity 246).
- **Kind:** bug (memory safety), interpreter
- **What happens:** the branch builds a `vector<WavePart>` local from
  the command's tokens.  Adding it turns
  `tests/12_b1_rubble.loft` — a file that never says `compose`, never
  reaches the branch and never mentions a wave schedule — into a
  deterministic abort:

  ```
  realloc(): invalid next size
  === loft crash (loft) SIGABRT caught ===
    last op:  OpArgText (op=109)
    pc:       1265
    fn:       (?) (d_nr=362)
    at:       <loft>/default/01_code.loft:1269:13
  ```

- ⚠ **Bisected at full-suite scale, and the result is clean:**

  | src under test | `scripts/test.sh` |
  |---|---|
  | HEAD (no K1 at all) | 1107 passed |
  | K1's whole data model — `WavePart`, `vector<WavePart>` nested in `WaveSchedule` in `WaveState`, a new spawn loop, emitter and comparer — **without** the `compose` branch | **1107 passed** |
  | the same, **plus** the `compose` branch building its vector inline in `script_command` | **abort** |
  | the same, with the parsing moved into three small helper functions | 1107 passed |

  So the data structure is innocent and the enclosing function is the
  ingredient.  Moving the `vector<Struct>` local and its loop out of
  `script_command` into `compose_fault` / `compose_parts` /
  `script_compose` is a complete fix.
- ⚠ **Two things it is NOT**, both measured rather than assumed:
  - not the trailing `u8` field — `WavePart.kind` as `integer` aborts
    identically (that is the already-fixed `vector<Struct>` u8 bug
    below, and this is not it);
  - not one particular expression — an earlier draft blamed an inline
    `tok[i + 1] ?? ""`, and binding it merely moved the abort from
    `tests/18_s4_the_reduce.loft` to `tests/12_b1_rubble.loft`.  Any
    perturbation of the function relocates the damage.
- **Why it does not reduce:** the corruption is written where the code
  is COMPILED — the crash site is inside loft's own `01_code.loft` —
  and detected wherever the allocator next touches the mangled chunk.
  A standalone program with the same expressions survives 4000
  iterations; so does a standalone `reduce_keys` over the same fixture.
  The size of the enclosing function appears to be required, and that
  is not something a small file reproduces.
- **Workaround in dryopea:** **do not add a `vector<Struct>` local to
  `script_command`** — give it a helper.  `src/script.loft` carries the
  rule and this bisection beside the `compose` branch.
- ⚠ **What makes it dangerous rather than merely annoying:** a green
  suite is not evidence that a violating call site is absent.  The
  branch that provokes it never runs, and the file that dies has
  nothing to do with the file that was edited.
- **Loft pointer:** interpreter, `OpArgText`; the abort surfaces at
  `<loft>/default/01_code.loft:1269`.

### `loft test` recompiles the consumer library once per test file

**Filed:** [loft#925](https://github.com/loft-lang/loft/issues/925)
(`enhancement`, `sev:medium`, `area:packages`, `wa:none`,
`hit-by:dryopea`).

Measured 2026-08-15, three test files differing only in what they `use`:
no `use` at all costs 40 ms, `use lattice;` costs 52 ms, and
`use dryopea;` costs **~490 ms**.  So ~450 ms per file is rebuilding the
aggregator, and all 67 dryopea test files pay it — **~31 s of a ~130 s
suite**, spent compiling the same unchanged library 67 times.

⚠ It is superlinear in project growth: a module added to `src/` slows
every test file, and a test file added re-pays for every module.

⚠ **No workaround worth taking.**  The obvious one — `use`ing single
modules instead of the aggregator — is measured to be nearly free
(52 ms), and it is exactly wrong: tests would stop exercising the entry
point the program uses, which is the property `CLAUDE.md` § Architecture
keeps the aggregator for.

### A local bound to a FORWARD-declared call panics the parser

**Filed:** [loft#918](https://github.com/loft-lang/loft/issues/918)
(`bug`, `sev:high`, `area:parser`, `wa:clean`, `hit-by:dryopea`).
**Repro:** [`loft_repros/forward_call_bound_to_a_local.loft`](loft_repros/forward_call_bound_to_a_local.loft)
— identical on both backends.

```loft
fn wrapper(n: integer) -> text {
    w_t = inner(n, 12);     // `inner` is declared BELOW
    w_t
}
fn inner(a: integer, b: integer) -> text { "{a}/{b}" }
```

aborts with `H5 two-pass contract: def n_wrapper grew a pass-2-only
attribute w_t … a real cross-pass divergence` — a raw Rust panic at
`src/parser/mod.rs:1904`, so there is no line number for the offending
call.

⚠ **Neither half fires on its own**: the callee declared first with the
same local is clean, and the forward reference returned directly is
clean.  Both controls are in the repro.

⚠ **It bites because binding a call to a local is dryopea's documented
workaround for [loft#877](https://github.com/loft-lang/loft/issues/877)
and [loft#880](https://github.com/loft-lang/loft/issues/880)** — so
applying either fix above the callee's definition turns a working
program into a compiler abort, and the panic names neither.

⚠ Same guard as [loft#763](https://github.com/loft-lang/loft/issues/763)
(closed), which needed an interface forwarding to a generic bounded by
it.  There are no interfaces and no generics here, so this is a residual
neighbouring shape rather than a regression — the same relationship
loft#908 has to loft#867.

**dryopea-side:** move the callee above the caller, or inline the call
into the return expression.  Surfaced in a plan 16 W4 probe; no shipped
dryopea file carries the shape.

### A struct literal that omits a field silently takes the type's zero

**Filed:** [loft#914](https://github.com/loft-lang/loft/issues/914)
(`enhancement`, `wa:clean`, `sev:medium`, `area:parser`, `both-backends`,
`hit-by:dryopea`).
**Repro:** [`loft_repros/omitted_field_is_silently_zero.loft`](loft_repros/omitted_field_is_silently_zero.loft)
— identical on both backends; `loft --check` says `ok`.

A literal that omits a field takes that field's zero with no diagnostic,
and nothing tells the omission apart from a deliberate zero.  Dangerous
exactly when zero is a MEANINGFUL value: `EditorInput`'s palette pick
wants a `-1` sentinel, and 0 is palette entry 0, which is sea, which
erases.

⚠ **The finding that made it worth filing: loft HAS declared field
defaults** — `palette_pick: integer = -1` works in a literal — and
dryopea did not know.  So both of dryopea's workarounds are for a
missing signpost rather than a missing feature:

- the two-field pair `in_select_palette: boolean` + `in_palette_index:
  integer` in `src/editor_step.loft`, and
- the `CLAUDE.md` § Loft language gotchas rule *"in any struct that
  callers build field-by-field, the NEUTRAL value must be the ZERO
  value"*.

**The ask is a lint**, not a semantics change: warn when a literal omits
a field that has NO declared default — the case where the value is the
type's zero and nobody chose it.  A field with a default is the author
saying the omission is fine.

⚠ **A dryopea-side simplification falls out of this and is NOT yet
done**: `EditorInput` could carry `in_palette_index: integer = -1` and
drop `in_select_palette`.  It touches the seam every `.keys` script and
the GL loop read, so it wants its own change rather than a drive-by.
⚠ And it is literal-only — [loft#876](https://github.com/loft-lang/loft/issues/876)
records that a `text as Struct` cast IGNORES a declared default, so no
struct dryopea loads from JSON may rely on one.

### A `for`-loop variable is function-scoped, so 122 of 131 loops carry a prefix

**Filed:** [loft#915](https://github.com/loft-lang/loft/issues/915)
(`enhancement`, `wa:clean`, `sev:low`, `area:parser`, `both-backends`,
`needs-design`, `hit-by:dryopea`).
**Repro:** [`loft_repros/loop_variable_is_function_scoped.loft`](loft_repros/loop_variable_is_function_scoped.loft).

`for i in …` binds an ordinary function-scoped local: it outlives its
loop (`leaked i = 1` after the closing brace), and a second loop over a
different element type anywhere in the same function is a compile error.

⚠ **The ERROR is right and is not what was filed** — it is
[loft#690](https://github.com/loft-lang/loft/issues/690)'s fix, which
turned a silent corruption of the second loop into a diagnostic.  What
was filed is the SCOPE, which #690 did not cover.

**What it costs dryopea:** `CLAUDE.md` § Naming's per-function loop
prefix, on **122 of the 131 `for` loops in `src/`** — `wt_i`, `wd_e`,
`ent_t`, `tslr_w`.  The prefix carries no meaning; it is collision
avoidance invented once per function.  ⚠ And it bites where the error
cannot help: adding a loop to a long function fails on a name a hundred
lines away, and the rename lands on the edit that did nothing wrong.

### Reading a MISSING file and returning a struct from one function SIGABRTs the interpreter

**Filed:** [loft#908](https://github.com/loft-lang/loft/issues/908)
**Repro:** [`loft_repros/missing_file_struct_return.loft`](loft_repros/missing_file_struct_return.loft)
**Hit building:** plan 16 W1's wave-list loader (`src/waves.loft`).

A function that both reads `file(p).content() ?? ""` for a path that does
not exist **and** returns a struct double-frees:

```
loft: BUG (#306): refused to free the stack store (#0) (rec=0, pos=0, var='')
      — a stack-record ref was treated as an owned heap store
free(): invalid pointer
=== loft crash (loft) SIGABRT caught ===  last op: OpFreeText (op=124)
```

⚠ **Interpreter only — `--native` answers correctly.**  That is the worst
direction for dryopea: both gates run interpreted (`CLAUDE.md` § Both
gates run INTERPRETED), so it is a crash the suite hits and a shipped
native build would not.

⚠ **Neither the `return` nor an empty-vector field is required.**  It
reproduces with a plain `struct Box { n: integer }`, and the **if-else
expression form crashes too** — so this is *not* the early-return-of-a-
composite issue `save.loft::load_map_or_empty` already dodges, and that
workaround does not apply here.

**Workaround (`wa:partial`, in use):** split the read from the build so
no single function does both — `wave_file_text` returns `text`,
`wave_file_parse` takes `text` and returns the struct, `wave_file_load`
calls the two.  Clean enough that it arguably improves testability, but
it is a shape you have to know about, and the failure mode when you do
not is a SIGABRT rather than an error.

### Mutating a struct RETURNED from a function is a silent no-op, and `lost-write` stays quiet

**Filed:** [loft#894](https://github.com/loft-lang/loft/issues/894)
(`bug`, `sev:medium`, `wa:clean`, `both-backends`,
`area:store-lifetime`, `area:parser`, `hit-by:dryopea`).
**Repro:** [`loft_repros/mutating_a_returned_struct_is_lost.loft`](loft_repros/mutating_a_returned_struct_is_lost.loft)
— verified on the interpreter and on `--native`, identical output on
both.

Found in dryopea plan 12 B4 (2026-08-13).  The same element, reached
three ways, and only two of them are a mutation the caller can see:

```
hurt(first(s), 10.0);            // through a helper that RETURNS it → 0
hurt(s.es[0] ?? E {}, 10.0);     // indexed inline                   → 10
for e in s.es { hurt(e, 10.0); } // through a loop variable          → 20
```

Nothing distinguishes them at the call site: same types, no warning,
no error.  Six dryopea tests failed at once on it, and each failure
read as a bug in the thing being mutated rather than in the one-line
accessor — because the read-back is simply the value from before the
call.

**The ask is probably a diagnostic rather than a semantics change.**
Value semantics for a returned struct is defensible; what is not is
that `hurt(first(s), …)` writes into a temporary discarded one
instruction later and `lost-write` — the warning class that exists for
exactly this — says nothing.  Note dryopea has also seen `lost-write`
fire FALSELY (§ Submitted, loft#883), so the two together suggest that
analysis is worth a look as a whole.

**Dryopea's own workaround** is a rule in `CLAUDE.md` § Loft language
gotchas: a one-line "get me the element" helper is fine to READ
through and never to write through.

### `input` 0.2.0 ships a PARKED banner citing a CLOSED bug — and the library works

- **Filed:** [loft-lang/loft-libs-game#9](https://github.com/loft-lang/loft-libs-game/issues/9) on 2026-08-13
- **Found while:** plan 09 phase I1 — adopting `input` as dryopea's one
  key table.  The library's own header says it is unusable, so the
  phase started with a probe of every write path before anything was
  built on it.
- **Kind:** bug (documentation)
- **What dryopea needs:** the banner dropped or rewritten.  It cites
  `@P391` (cross-package constructor return lands in CONST_STORE, so
  writes through `&InputState` panic), which is
  [closed](https://github.com/loft-lang/loft/issues/248) — and every
  write path works: `input_new`, `input_tick_from_state`,
  `input_set_bindings`.  A held key reads pressed 5/5 with one edge; a
  rebind takes effect immediately and the old key goes dead.
  ⚠ Measured on the INTERPRETER only — dryopea's native backend is
  blocked for unrelated reasons, so the other two targets are still
  unverified by us.
- **Workaround in dryopea:** none needed; `input` is consumed as of
  I1.  The probe is the workaround for the banner.
- **Also asked for:** a supported `keys_for_action` (replay callers
  have no API to turn an action name back into key codes — dryopea
  reads `bnd_actions` directly, which works only because struct
  fields happen to be readable), and a doc line about where a
  MODIFIER rule is meant to live, since `ActionBinding` has no
  concept of one and every consumer with a Ctrl combo will hit it.

### `registry-sign.sh` aborts a publish on one dropped connection — no retry

- **Filed:** [loft-lang/loft#887](https://github.com/loft-lang/loft/issues/887) on 2026-08-12
- **Found while:** publishing `graphics 0.5.2` — the constant-fill optimisation
  dryopea's own suite profile asked for (58% of its interpreted time was
  `canvas()` building a pixel buffer one element at a time).
- **Kind:** bug (tooling robustness — a correct check with a single-attempt
  fetch).
- **Behaviour:** `registry-sign.sh:278` re-downloads each new tarball to hash
  it — the right check, and the trust-root backstop — but with
  `urllib.request.urlopen(...)` once and no retry.  The
  `github.com → objects.githubusercontent.com` redirect drops most of
  urllib's connections on this box (**1/4**, against **4/4** for
  `raw.githubusercontent.com`), while `curl` and `gh release download`
  cope at the same moments.  Publishing took **5 full
  `registry_maintain.sh` runs plus 7 sign attempts** for one good fetch.
- **Fail-safe, never unsafe:** every failure refused to sign and left
  `index.json` untouched with its signature intact — no half-published
  state.  The cost is time, not correctness.
- **Secondary ask:** the diagnostic says *"does the release exist with the
  named asset?"* when the transport failed.  It did exist, every time; that
  question cost a round of verifying the release and re-proving `loft
  package` of `main` reproduced the tarball byte-for-byte.
- **Workaround (dryopea-side, and the thing that made it tractable):** pass
  `--registry-dir <persistent checkout>` to `registry_maintain.sh` so a
  failed sign leaves the staged index in place, then re-run
  `registry-sign.sh` alone — seconds per retry instead of ~8 minutes of
  re-cloning every `loft-libs-*` repo and re-running `compat check --full`.
  ⚠ **Not** `--no-download`: it skips the backstop, and
  `registry_maintain.sh` deliberately does not plumb it through.

### An ambiguous bare struct name dumps a FALSE `lost-write` against a library

- **Filed:** [loft-lang/loft#883](https://github.com/loft-lang/loft/issues/883) on 2026-08-12
- **Found while:** plan 09 phase I0 — probing whether `input`'s edge model
  matches the editor seam's.  `camera.loft` declares `pub struct InputState`
  and so does `input`, so naming the bare type with both loaded is
  ambiguous.
- **Kind:** bug (diagnostics — a false positive in the one warning class
  that catches loft's most expensive real bug shape).
- **Behaviour:** the ambiguity error is correct and names its own fix.
  Beside it the abort dumps
  `warning[lost-write]: 'mo_t' is mutated but its value is never read —
  the write is LOST` against `src/spawn.loft::move_order` — a selection
  sort 497 green tests exercise every run.  The write is **not** lost:
  a purpose-built probe shows a `for` loop variable's field mutation
  persisting on the interpreter and on `--native`.
- **Attribution — corrected 2026-08-12, after filing.**  The first
  reduction said "the ambiguous-struct path specifically" on the strength
  of one negative, and that was wrong: a **type mismatch** and **too many
  parameters** trigger it too, while unknown-function, too-few-parameters
  and syntax errors do not.  The pattern is errors raised AFTER the
  use-analysis pass; the ambiguous name is one instance.  So the bug is
  broader than filed — a routine type error in any project with a
  warning-emitting library produces it.  Correction posted to the issue.
  ⚠ The lesson is the cheap one: **one negative control is not an
  attribution.**  It was found again by accident, while profiling.
- **Reproducer:** [`loft_repros/lost_write_false_positive/`](loft_repros/lost_write_false_positive/README.md)
  — a directory, because the trigger needs two libraries declaring one
  struct name.  `prog/amb.loft` is the bug, `prog/ok.loft` the control.
- **Why it matters more than it looks:** dryopea lost four phases to a
  real member of this class (plan 11 F8 — a `FlowField` bound to a local
  in a per-enemy path, 2250x the cost, 490 green tests over it).  A false
  positive teaches the reader that `lost-write` over library code is
  noise.  It is also unreachable by a warning-clean gate: a green suite
  never aborts, so the warning cannot be kept clean and cannot be
  trusted when it does appear.
- **Workaround in dryopea:** none needed — write `camera::InputState` /
  `input::InputState` and both the error and the warning go away.  The
  cost is the probe it took to establish the warning was false.

### A struct returned through TWO nested tail calls loses what its loop wrote

- **Filed:** [loft-lang/loft#880](https://github.com/loft-lang/loft/issues/880) on 2026-08-12
  (`bug`, `sev:high`, `area:store-lifetime`, `area:codegen`,
  `both-backends`, `wa:clean`, `hit-by:dryopea`).  Repro:
  [`loft_repros/struct_through_two_tail_calls.loft`](loft_repros/struct_through_two_tail_calls.loft).
- **Found while:** plan 11 F7 — factoring `flow_build` onto a shared
  `flow_sweep` so the routing field and the desire field are one BFS
  with one number changed.
- **Kind:** bug (silent wrong result, and the two backends disagree
  about how wrong)
- **What dryopea needs:** a struct to survive being returned through a
  chain of tail-position calls.  Today, with the outer call passing a
  struct LITERAL as an argument and the sweep filling its hash inside a
  `while` loop that reassigns a `vector` frontier, the result arrives
  with **1** cell interpreted and **0** natively where 13 are expected.
  Binding the inner call to a local fixes it, and that is the
  workaround in [`src/flow.loft`](src/flow.loft).

  ⚠ **What made it expensive was the shape of the refactor, not the
  bug.**  Moving an algorithm out of a function into a shared helper
  turns every *consumer's* one-line wrapper into a second tail call —
  so the defect appears at call sites nobody edited.  Every flow field
  on the map came back empty and the game stopped moving; the nine red
  tests all named the movement they no longer did, and none named the
  wrapper.  A four-way boundary matrix (wrapper binds/tail-returns ×
  caller binds/inlines) is what located it.

### Indexing a call's result in TAIL position — fallback on interpret, panic on native

- **Filed:** [loft-lang/loft#877](https://github.com/loft-lang/loft/issues/877) on 2026-08-12
  (`bug`, `sev:high`, `area:store-lifetime`, `area:codegen`,
  `both-backends`, `wa:clean`, `hit-by:dryopea`).  Repro:
  [`loft_repros/index_of_returned_vector.loft`](loft_repros/index_of_returned_vector.loft).
- **Found while:** plan 11 F5c — factoring `flow_step` onto the new
  `flow_steps` so the mover and the arrow share one preference ordering.
- **Kind:** bug (silent wrong result; crash on native)
- **What dryopea needs:** `callee(...)[i]` to mean the same thing as a
  function's tail expression that it means one line earlier.  Today the
  index reads the absent sentinel (65535), so `--interpret` answers the
  `??` fallback and `--native` panics with `index out of bounds: the len
  is 6 but the index is 65535` (`src/database/allocation.rs:1643`).

  ⚠ **The cost is in how plausible the wrong answer is.**  A `??`
  fallback is written to be a sane default, so a function that returns
  *only* its default looks like a working function.  `flow_step` became
  "the hex I am standing on" for every enemy on the map, and the nine
  tests that caught it blamed a three-line function that reads correctly.

- **Workaround in dryopea:** bind the call to a local, then index it.

### Interpolating a struct with a `hash` field SIGSEGVs the interpreter

- **Filed:** [loft-lang/loft#873](https://github.com/loft-lang/loft/issues/873) on 2026-08-12
  (`bug`, `wa:clean`, `sev:high`, `area:runtime`, `area:native`,
  `both-backends`, `hit-by:dryopea`).  Repro:
  [`loft_repros/format_struct_with_hash_field.loft`](loft_repros/format_struct_with_hash_field.loft).
  Possibly the same formatter-selection root cause as loft#845, but with
  no generics involved.
- **Found while:** plan 11 F2 — writing the flow-field tests.  `FlowField`
  carries `cells: hash<FlowCell[q, r]>`, and an assertion message said
  `"sizes differ: {a} vs {b}"`.
- **Kind:** bug (crash)
- **What dryopea needs:** either a formatted record or a compile-time
  refusal.  `{x}` on a hash-bearing struct exits 139 under `--interpret`
  (`OpFormatDatabase`) and dies with no output at all under `--native`.

  ⚠ **The expensive part is where it fires: inside an assertion message.**
  A test that was failing for an ordinary reason loses its diagnostic and
  reports a segfault instead, and the crash report's "nearest span"
  pointed 3 lines away.  An interpolation only evaluated on failure is the
  least likely place for a suite to catch this.

- **Workaround in dryopea:** format the fields, not the record —
  `{flow_count(f)}`, never `{f}`.
- **Loft pointer:** `OpFormatDatabase` (op 145); loft#845 for the
  generic-type-variable case.

### A `text as vector<Struct>` cast stores `null` into a DN1 non-null scalar field

- **Filed:** [loft-lang/loft#870](https://github.com/loft-lang/loft/issues/870) on 2026-08-12
  (`bug`, `wa:clean`, `sev:medium`, `area:parser`, `area:runtime`,
  `both-backends`, `hit-by:dryopea`).  Repro:
  [`loft_repros/json_null_into_non_null_scalar_field.loft`](loft_repros/json_null_into_non_null_scalar_field.loft).
- **Found while:** plan 11 F1 — building the enemy-passability rule, which
  reads `GroundType.height_override` to decide whether a 3 m wall stops a
  robot.
- **Kind:** bug
- **What dryopea needs:** a field declared plain `float` is non-null under
  @PLN25 DN1, so either the JSON cast should honour that (error, or the
  type's default) or the `?? 0.0` that defends the read should not be
  reported as a **redundant coalesce**.  Today it is both: the cast stores
  `null`, and the compiler advises deleting the guard against it.

  It hides well.  A null height compares `<= climb` as **true** and
  `> climb` as **false**, so `if height > climb { blocked }` reads flat
  terrain as passable — the right answer, for the wrong reason, until
  null-comparison semantics ever move.

- **Workaround in dryopea:** declare the fields that `palette.json` writes
  `null` into as nullable — `slope: integer?`, `drop: integer?`,
  `height_override: float?` (`src/palette.loft`).  The type then matches the
  data, the `?? 0.0` in `src/passable.loft` is honest, and the lint is
  quiet.  `tests/01_e2_palette.loft` already asserted `slope == null`, so
  this only made the declaration agree with what the suite measured.
- **Loft pointer:** @PLN25 DN1 (default non-null); the `redundant-coalesce`
  lint.

### A missing `use` reports as `Expect token ;` at a tuple access, not as an unknown function

- **Filed:** [loft-lang/loft#868](https://github.com/loft-lang/loft/issues/868) on 2026-08-12
  (`bug`, `wa:clean`, `sev:low`, `area:parser`, `both-backends`, `hit-by:dryopea`).
  Re-verified against loft 2026.8.0 first, and minimised further than the
  original report: no library and no missing `use` are needed — an unknown
  function whose result is tuple-accessed reproduces it, and the unknown
  function is never named at all.
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

### Converge gridmesh on ONE ground-level grid (axial flat-top); migrate audience_crystal off the offset-pointy placeholder

- **Filed:** [loft-lang/loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24)
  on 2026-08-12 (`enhancement`).  Filed on **loft-libs-graphics**, not
  loft-lang/loft — gridmesh migrated out of the monorepo.  Re-verified
  against gridmesh 0.2.0: `layout` is still stored and read nowhere, and
  `step_x`/`step_y` are still the unconditional offset-pointy round-trip.
  The `audience_crystal` half of the original ask has lapsed — it is no
  longer in the registry — so the issue asks only for the adapter.
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

### Verified fixed on 2026-08-12 (loft 2026.8.0)

The eight entries that had accumulated under **Open** were re-run against
the current toolchain before filing any of them upstream.  Six no longer
reproduce — they are below, each with what was actually observed.  Filing
them would have been noise; the two that survived are § Submitted.

### `use` does not namespace struct TYPES per library — two libraries defining the same struct name panic at registration

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).  No panic: two libraries
  may each declare `Hex`, and the diagnostic now says
  ``` `Hex` is declared by more than one package here — write types::Hex or
  world::Hex to say which ```.  Qualified literals (`liba::Hex { … }`) and
  functions returning either type both work — checked with a purpose-built
  two-library probe, not just the stale `moros_map` reproducer.
  **Consequence: dryopea's proposed `Hex` → `Axial` rename is unnecessary.**
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

### Function returning a freshly-allocated Store leaks one Store per call when it has a struct-typed value parameter

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).
  `loft_repros/canvas_store_leak_struct_param.loft` runs its 9 struct-param
  calls and exits with NO `stores not freed` warning.  The dryopea-side
  workaround (hoist the framebuffer out of the render loop) was never
  applied and is not needed.
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

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).
  `loft_repros/u8_vector_in_wrapper.loft` now prints the wrapped form
  identical to the standalone one — `{"q":1,"r":1,…},{"q":2,"r":2,"direction":3}`
  where it used to print all zeros.  **Workaround RETIRED 2026-08-12:**
  `marker_file.loft`'s `MarkerSaveEntry` is `u8` again and
  `marker_world_to_file` copies fields straight across.  It turned out
  not to be a save-format change at all — u8 and integer both serialise
  as a bare JSON number, so old sidecars load unchanged; there is a test
  asserting exactly that.  `tests/03_m1_markers.loft` now carries the
  deleted reproducer's coverage.
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

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).
  `loft_repros/const_param_store_lock.loft` prints `entries=1 ds=3` — its
  documented expected output — instead of panicking with `Claim on
  read-only store`.  **Workaround RETIRED 2026-08-12:** the `const`
  qualifiers are back on `history.loft::clear_and_record`'s pw + mw
  params, so the signature documents again that it reads both layers and
  writes only the history.
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

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).
  `loft --native loft_repros/struct_with_hash_native_return.loft` prints
  `items = 0` instead of panicking at `keys.rs:251`, and dryopea's real
  shapes — `load_markers_or_empty` and the `load_map_or_empty` tuple-via-cast
  variant — both survive natively in a direct probe.
  ⚠ **`make play` still cannot go native**, but for a different reason now:
  the silent empty-palette miscompile (loft-lang/loft#866, § Submitted) makes
  the native editor unable to paint.  The Makefile comment naming *this* bug
  as the blocker has been corrected.
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

- **✅ Verified FIXED 2026-08-12** (loft 2026.8.0).
  `x = 12.0; _ = x / 3;` compiles and runs with no diagnostic of any kind.
  **Retirable:** the "write `3.0` not `3`" habit is no longer needed
  (nothing in dryopea's source depends on it — no code change owed).
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
