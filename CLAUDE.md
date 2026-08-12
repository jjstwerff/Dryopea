<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Claude Code Instructions for the dryopea Project

## What dryopea is

**dryopea** is a sci-fi free-build / tower-defence game built on
[loft](https://github.com/jjstwerff/loft).  The defining mechanic
is **scramble-and-salvage**: when a base is about to be overrun,
the player fires a rocket out of the core building and evacuates
key components — each carried-out component disables the tower
it came from, so grabbing salvage *hastens* the overrun.
Evacuated components give an advantage at the next base.  A run
is a sequence of bases, chained by what you carry out.

**Status: active implementation.**  Plan 01 (in-game ground-type
editor) has shipped E1–E4 + an integration smoke test + the
runnable E1-live editor (`src/main.loft`).  Plan 03 (marker layer
+ multi-direction spawns) has shipped M1-M5 — sidecar JSON save,
Tab-toggled Ground/Marker editor mode with HUD badge, spawn
placement + R/Shift+R rotation, hot-pink triangle render
overlay, and a runtime wave engine + spawn director with
approach-mode enemy tick.  Plan 07 (shared world substrate) has
W0 partially landed — `gridmesh` adopted as the chunk/dirty
layer (`src/chunks.loft`).  Plan 08 (game validation) has V0 +
V1 shipped — the editor input seam (`src/editor_step.loft`) so
EVERY editor action is driven headlessly and `src/main.loft` is
a GL shell that polls, steps and renders; the `.keys` script
runner (`src/script.loft`) that plays a written-down run through
that seam; and `snap`, which photographs the editor's own frame
(`src/editor_view.loft`, shared with the GL loop).  V2p answered
the separability question with no code — the palette separates,
world renders contain ONLY exact palette colours, and the real
hazard is HUD contamination, so the `frame` measurement reads
the world layer rather than the composited shot.  V2 built the
instrument on that answer: six measurement commands, the
classifier in `src/measure.loft`, and a wave for `count alive`
to count.  V3 shipped the five scenario scripts in
`tests/scripts/*.keys` — including `a-wave-approaches`, the
first thing here that asserts the GAME works rather than that a
function returns.  V4 closed the plan with the gate itself:
`scripts/validate.sh` / `make validate` sweeps every `.keys`
script, prints each measurement beside its band, and exits
non-zero on one out of band.

**Plan 08 is complete.**  Its first real run also caught what it
was built to catch: on the NATIVE backend `load_palette` answers
0 entries (a silent `text as vector<Struct>` miscompile — filed
in [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)), which no
test could see because `loft test` runs the interpreter only.
Both gates therefore run interpreted, as `make play` already did.

Plan 11 (flow field) has F0 + F1 + F1b + F2 + F3 + F5 + F5b shipped.  F1 is the
instrument, not the movement: `enemy <i> <q> <r>` and `enemies
passable` say where an enemy is and whether its CLASS may be there,
and `src/passable.loft` is the height-step rule they read.  **F1b is
the first wall in dryopea that works** — `enemy_tick` consults that
same rule and stops in front of what it cannot cross.  F2 is the
distance field (`src/flow.loft`): a BFS out from the core, one field
per class, where **no-route is a LARGE value and never 0** — 0 is
"at the core", and "smallest distance wins" must refuse a cell with
no route rather than prefer it.  F3 is the arrow on top of it, gated
by an exhaustive sweep: from EVERY reachable cell, following the
arrows reaches the core in exactly `distance` steps.  **F5 makes
enemies follow it** — `wave_tick` rebuilds the field ONCE per tick
before anybody moves (one per class in the roster), and `enemy_tick`
steps down it; **F5b made the scrambler bubble the mode
selector** it was always specified to be: inside 25 hexes the field
steers, outside it the spawn heading does.  ⚠ The bubble is a
STRAIGHT-LINE distance, never a route length — it is a jamming
sphere, so an enemy with no route at all is still inside it.

⚠ **A 1-hex-wide corridor cannot tell a flow field from a fixed
heading** — both give the identical path, so every enemy test dryopea
had was blind to F5.  A scenario that means to exercise routing needs
a route that leaves the heading's line: a heading of 4 is `(-1, 0)`,
so `enemy 0 3 -1` is a hex no heading can reach.  That is the shape
to reach for when gating a movement change.

⚠ **The neighbour relation lives in `src/world.loft` and nowhere
else.**  `hex_offset` / `hex_neighbor` / `hex_neighbours` are the only
place a hex coordinate may be stepped.  Everything that computes
adjacency, reach or a route calls them; a `+ 1` on a `q` or `r`
outside them is the bug (it is how moros#10 sheared every reach
computation).  That is also what keeps plan 11's distances
independent of plan 09: convert the table, and no distance moves.

⚠ **A walking test must paint the ground it walks on.**  An unpainted
hex IS sea, so after F1b a wave over a blank map does not move at
all, and `enemies passable` over one is red.  Every scenario that
walks enemies drags a corridor first; that is the game's rule, not a
harness quirk.

**Suite: 406/406 green under `scripts/test.sh`** (~20-30 s — the
`frame` measurements classify full 960x720 frames).
**Gate: 11 scripts green under `scripts/validate.sh`** (~11 s).

⚠ **Never interpolate a struct that has a `hash` field** — `"{f}"`
SIGSEGVs the interpreter (loft#873) and exits silently on native.
It bites hardest inside an assertion message, where it replaces the
diagnostic of a failing test with a crash three lines from the real
site.  Format the fields: `{flow_count(f)}`, never `{f}`.

Plan 06 (editor-to-stencil pipeline) is drafted and waits on the
shared substrate.  The full design lives in [`docs/DESIGN.md`](docs/DESIGN.md);
the fiction in [`docs/SETTING.md`](docs/SETTING.md); the full
feature roadmap in [`plans/ROADMAP.md`](plans/ROADMAP.md).

## Relationship to loft

loft is the language + runtime; dryopea is a consumer project.
Dryopea is also the **second partner** for loft's universal
hex-world editor (loft `lib_plan 24`) — moros is the first;
dryopea drives the bug-hunt phase that hardens the shared
libraries.

When dryopea surfaces a need from loft — a language feature, a
stdlib gap, a runtime bug — **file it as a GitHub issue on
`loft-lang/loft`** (`gh issue create --repo loft-lang/loft`;
`jjstwerff/loft` redirects there).  A write-up that stays in this
repo is not filed: `QUESTIONS_FOR_LOFT.md` is dryopea's outbound
queue, not loft's inbox.

The flow, in order:

1. Cut the minimal reproducer into
   [`loft_repros/`](loft_repros/README.md) and check it fails
   standalone on the backends you claim.
2. `gh issue create` with the repro **inline** in the body —
   dryopea is a separate repo, so a link into `loft_repros/` is
   not self-contained.  Label it: `bug` / `enhancement`, plus
   `sev:*`, `area:*`, `wa:*` and `hit-by:dryopea`
   (`gh label list --repo loft-lang/loft` for the set).  Search
   open AND closed issues for the shape first.
3. Record it in [`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md)
   under **Submitted** with the issue link, per that file's own
   Open → Submitted → Resolved convention.

Do **not** fix it locally by patching loft from this repo; loft
has its own contribution flow.  Internal-to-dryopea bugs go in
[`PROBLEMS.md`](PROBLEMS.md) with `@D<NNN>` IDs.

## Key commands

dryopea uses the **installed** `loft` binary (`loft` on PATH —
`/usr/local/bin/loft`).  There is no local loft build step: the
libraries it depends on resolve from the loft package registry
via `loft.toml` + `loft.lock`, so no `--lib` path is passed
anywhere.

```bash
# Run dryopea's test suite (canonical entry — DO NOT run `loft test` directly)
scripts/test.sh

# Play every tests/scripts/*.keys and gate on what they measure —
# the SECOND gate (plan 08 V4).  Prints each measurement beside its
# band, writes a PNG per `snap` into shots/, exits non-zero on a
# reading out of band.  `make validate` is the same thing.
scripts/validate.sh                  # all of them (~11 s)
scripts/validate.sh paint-a-base     # just one, while iterating

# Run the interactive editor (E1-live; opens a 960x720 GL window).
# Use `make play` — it passes --interpret, and the NATIVE backend is
# broken for dryopea today: it panics on the marker load, and where it
# does not panic it silently loads an EMPTY palette (both filed in
# QUESTIONS_FOR_LOFT.md).  `loft src/main.loft` is `make play-native`,
# kept for testing the eventual fix.
make play
make play MAP=starter_01

# Parse-check a single .loft file without running it
loft --native-emit /tmp/check.rs src/<file>.loft

# Inspect a dependency's public API (never guess a signature)
loft api                 # every reachable library + its path
loft api graphics        # one library's full public surface
```

`scripts/test.sh` is the canonical test runner.  It:
- Creates `tests/actual/` — it is gitignored, so a fresh
  checkout does not have it, and neither `save_png` nor the file
  writer creates parent directories.  Without it every write
  silently goes nowhere and the golden tests fail as a
  "mismatch" against a file that was never written.
- Pre-cleans `tests/actual/*.png` and `tests/actual/*.json`
  between runs so stale artefacts can't masquerade as current.
  **Running `loft test` directly skips this** and leaks a save
  file into the next run's cold-start assertions.
- Invokes `loft test` against the dryopea `tests/` directory,
  with warnings VISIBLE (the suite is kept warning-clean).
- Exit code 0 = all green; non-zero = failures (the loft test
  runner surfaces assertion failures as FAIL since `@P367`
  shipped on the loft side).

### Relative paths resolve against the PROGRAM's directory

A relative path in a `.loft` file resolves against
`source_dir()` — the directory of the program entry, not the
process cwd, and not the directory of the file containing the
`file()` call.  Under `loft test` the entry is the test file, so
`source_dir()` is `tests/`.

dryopea's paths (`examples/palette.json`, `tests/golden/…`,
`maps/…`) are all repo-root-relative, so every entry point
declares the **`#cwd`** directive at the top of the file, before
the first declaration.  That restores cwd-relative resolution,
and both `scripts/test.sh` and the `Makefile` run from the repo
root.  A new test file needs `#cwd` or its palette load and
golden compare will silently miss.

⚠ **`#cwd` is legal only in a program ENTRY.**  A file carrying it
cannot be `use`d as a library — the import fails to parse with
`Syntax error: unexpected '#' at <file>:1:2`, and the aggregator
goes red naming the importer rather than the directive.  So an
entry point cannot also be an aggregator member, which means it is
compiled by nothing and every entry must stay a shell with no
decisions in it: `main.loft` over `editor_step.loft`,
`validate_main.loft` over `validate.loft`.

## Architecture — src/ layout

```
src/
  dryopea.loft     library aggregator — `use dryopea;` brings every
                   submodule into scope (tests use this entry)
  main.loft        interactive editor entry point — `fn main()`,
                   NOT in the aggregator (runs via `loft src/main.loft`).
                   The GL shell only: open window, poll input,
                   call the seam, render.  Parse-check it by hand
                   after every edit — `scripts/test.sh` can't see it
  editor_step.loft the input seam (plan 08 V0) — EditorState (all
                   session state) + EditorInput (one frame of intent)
                   + editor_step(s, input).  EVERY action runs through
                   it.  No GL and no clock, ever; disk only via the
                   save / reload actions, and only when a path is
                   attached (editor_state_attach)
  editor_view.loft render_editor_frame(s, w, h, ppm) -> Canvas —
                   what the player sees, composed ONCE: world, hover
                   preview, markers, ghost, picker, save indicator,
                   mode badge.  Both the GL loop and the script
                   runner's `snap` ask for it, so a shot is the
                   editor's frame and not a harness renderer's.
                   Also owns VIEW_W / VIEW_H / VIEW_PPM (the window
                   size IS the shot size).  Never mutates the state
  measure.loft     frame measurement (plan 08 V2) — classify_canvas
                   / classify_world -> FrameCounts.  Reads the WORLD
                   layer, never the composited shot (the HUD puts a
                   floor under every bucket — V2p).  Classification
                   is an EXACT lookup, not nearest-colour: the
                   rasteriser does not blend, so a pixel that is not
                   a palette colour lands in `unknown` and is a
                   FAULT.  The colour table comes from render.loft's
                   `palette_color` — the function that drew the
                   pixels — with palette.json drift caught by its
                   own test
  validate.loft    the gate (plan 08 V4) — validate_all(scripts_dir,
                   shots_dir, palette[, only]) -> ValidateReport.
                   Sweeps a directory of `.keys` scripts, plays each
                   in a session of its own, sums the measurements and
                   reports the FIRST failure with the number that
                   moved.  Refuses to be green over nothing: no
                   palette, no directory, no scripts, or no
                   measurements taken are each a named failure
  validate_main.loft  the gate's entry point — `fn main()`, NOT in the
                   aggregator (runs via `scripts/validate.sh`).  Six
                   lines, no decisions: a file carrying `#cwd` cannot
                   be `use`d as a library, so anything written here is
                   compiled by nothing.  Parse-check it by hand
  script.loft      the `.keys` script runner (plan 08 V1) —
                   script_run(s, source[, shots_dir]) /
                   script_run_file(s, path[, shots_dir]) -> ScriptRun.
                   Commands name ACTIONS, never keys
                   (`do toggle_mode`), so no key table exists to
                   drift from the GL poll's.  Reaches the editor ONLY
                   through editor_step — even `at` walks the camera
                   with pan frames.  An unknown command / action /
                   number / arity is an ERROR, never a skipped line.
                   `snap <name>` writes <shots_dir>/<name>.png
                   (default `shots/`, gitignored) and CHECKS what
                   save_png answers.  V2 added the measurements
                   (count / kind / marker / frame — each ASSERTS and
                   ends the run when out of band) plus `wave` /
                   `tick`; WaveState lives on ScriptRun, not on
                   EditorState — an edited session has no enemies.
                   V3 added `range <lo> <hi>` (how far the live
                   enemies are from the core) and the five scenario
                   scripts in `tests/scripts/`
  world.loft       hex math (axial flat-top); HEX_DIAMETER = 1.5m;
                   cube_round_axial, world_to_hex, visible_hexes
  camera.loft      EditorCamera { pos: Hex, zoom: integer }
                   + InputState (moros-style: factories + pure tick
                   + struct of booleans)
                   + camera_update(c: &EditorCamera, input: InputState)
  painted.loft     PaintedHex { q, r, kind: u8 }
                   + PaintedWorld { painted: hash<PaintedHex[q, r]> }
                   + paint(), lookup_painted(), paint_line()
                   (sea-default sparse storage — un-painted hex is sea)
  palette.loft     GroundType { name, color, sub_palette, slope, drop,
                   drainage, walk_*, buildable }
                   + load_palette(path) via `text as vector<GroundType>`
                   + parse_hex_color().  `slope` / `drop` /
                   `height_override` are declared NULLABLE because
                   palette.json writes null in them — see the file's
                   own warning
  flow.loft        the distance field (plan 11 F2) — flow_build(pal,
                   pw, kind, core) -> FlowField, a BFS out from the
                   core over what that CLASS can occupy, plus
                   flow_distance / flow_reachable / flow_count, and
                   flow_step (F3): which neighbour is closest to the
                   core, COMPUTED from the distances and never stored
                   — F5c needs the ordering over all six neighbours
                   at move time, which a baked direction cannot give.
                   Ties break by lowest direction index, because a
                   scripted run has to be repeatable.
                   ⚠ no-route is FLOW_UNREACHABLE, a LARGE value:
                   0 means "at the core", and every "closest
                   neighbour" search must refuse a routeless cell
                   rather than prefer it.  Built from world.loft's
                   neighbour relation only, which is what makes it
                   independent of plan 09
  passable.loft    may a class of enemy be on this hex? (plan 11 F1)
                   — the enemy KIND discriminants + climb_limit()
                   + hex_height() + occupancy_fault() / can_occupy().
                   TWO questions, and a hex must answer both: is the
                   SURFACE one this class stands on (`walk_ground`),
                   and is the step onto it within its climb.
                   ⚠ `walk_ground` alone is the BUG — `wall` and
                   `wall_high` are walk_ground=true (a wall's walkable
                   part is its TOP), so the one-field predicate walks
                   robots through 3 m walls
  picker.loft      Picker { palette, active }
                   + picker_default(), picker_set_active(),
                   render_picker(cv, p, x0, y0) — Canvas-painted UI
  render.loft      software rasterizer using graphics::Canvas
                   + render_to_canvas, render_with_hover, palette_color,
                   draw_hex, draw_hex_outline,
                   world_to_canvas, screen_to_world, screen_to_hex
  golden.loft      assert_golden(cv, name) — writes tests/actual/<n>.png,
                   asserts byte-equality against tests/golden/<n>.png;
                   FAILs via loft's now-working assert (@P367 fixed)
  map_file.loft    MapFile { version, name, cam_q, cam_r, cam_zoom,
                   ground: vector<GroundEntry> }
                   — 6 fields, flat, vector LAST — see § Known constraints
  save.loft        paint_to_mapfile, save_map_file, load_map_file,
                   mapfile_to_painted, palette_index_of,
                   save_world, load_map_or_empty (returns tuple)
```

Tests live in `tests/<plan>_<phase>_*.loft` (one file per phase).
Goldens live in `tests/golden/` (committed); actuals in
`tests/actual/` (gitignored).  `.keys` scripts live in
`tests/scripts/` (committed — they are source, not output);
scripted-run shots land in `shots/` (gitignored, written fresh
each run — a shot a doc cites is copied into `docs/`), and the
suite redirects its own shots into `tests/actual/`.

## Key data structures

| Type | File | Purpose |
|---|---|---|
| `Hex` | `world.loft` | `{ q, r }` axial flat-top coord |
| `EditorState` | `editor_step.loft` | the whole editor session — layers, camera, picker, mode, history, chunk dirty set |
| `EditorInput` | `editor_step.loft` | one frame of player intent (hover hex + action flags) |
| `EditorCamera` | `camera.loft` | `{ pos: Hex, zoom: integer }` |
| `InputState` | `camera.loft` | per-frame camera flags (in_pan_*, in_zoom_*) — folds into `EditorInput` in plan 08 V0b |
| `PaintedHex` | `painted.loft` | `{ q, r, kind: u8 }` — one painted cell |
| `PaintedWorld` | `painted.loft` | wrapper holding `hash<PaintedHex[q, r]>` |
| `GroundType` | `palette.loft` | one row from `examples/palette.json` |
| `Picker` | `picker.loft` | palette UI state |
| `MapFile` | `map_file.loft` | save record (6 fields; see Known constraints) |
| `GroundEntry` | `map_file.loft` | one persisted hex with kind as text name |
| `ScriptRun` | `script.loft` | one `.keys` run — ok / failing line / message / counts, plus the pointer, the shots directory and the wave it is playing |
| `FrameCounts` | `measure.loft` | one classified frame — pixels per bucket, `unknown` (not a palette colour = a fault), `total` |
| `WaveState` | `spawn.loft` | the enemy roster + round-robin cursor — runtime, not editor state |
| `ValidateReport` | `validate.loft` | one `make validate` sweep — scripts / passed / failed / measurements / shots, and the FIRST failure with the number that moved |

## Important conventions

### Hex convention

Axial flat-top hex grid throughout — matches moros and loft
`lib_plan 24`.  HEX_DIAMETER = 1.5m vertex-to-vertex.  World
+y grows **south** (same direction as canvas +y); there is no
y-flip in the render path.

### Naming

- Functions, variables: `lower_case`
- Types, structs, enums: `CamelCase`
- Constants (file-scope): `UPPER_CASE`
- Loop variables prefixed per function (`tslr_w`, `tpi_pal`)
  to dodge the flat-namespace gotcha
- `dryopea_*` save path is local-cwd-relative + gitignored

### Test discipline (moros-style)

- Factories for state construction (`camera_default()`,
  `painted_empty()`, `picker_default(path)`).
- Pure tick functions: `camera_update(c: &EditorCamera, input: InputState)`.
- `InputState` is a struct of named boolean fields, not a flag
  bitmask.  Tests construct it directly + assert on field changes.
- Golden-image tests via `assert_golden(cv, name)` — render to
  Canvas, write to `tests/actual/<n>.png`, compare bytes to
  `tests/golden/<n>.png`.  Bootstrapping a new golden: run, FAIL,
  review `tests/actual/<n>.png`, copy to `tests/golden/<n>.png`.

### Loft language gotchas we hit

The following are dryopea-side workarounds for known loft
behaviour.  Full reproducers + loft-side issue refs live in
[`QUESTIONS_FOR_LOFT.md`](QUESTIONS_FOR_LOFT.md):

- **`now` is a builtin** (`default/02_images.loft`).  Don't
  use as a local variable name — shadowing confuses type
  inference and your `now = ticks()` ends up holding a `fn() ->
  integer` reference rather than its result.  We renamed to
  `tnow` in `src/main.loft`.
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
  default silently.**  So in any struct that callers build
  field-by-field — `EditorInput` above all — the NEUTRAL value
  must be the ZERO value.  A "none" sentinel of `-1` becomes `0`
  in every partial literal, which for a palette index means
  "select sea", which erases.  Build from the `*_empty()`
  factory, not from a literal.
- **Loop variable name reuse must keep consistent type per
  function-scope** — different types in different loops fails
  ("loop variable 'i' has type text but was previously used as
  integer").  Prefix loop vars per function.

### Save path

The interactive editor saves to `dryopea_save.json` in the
cwd.  Tests write to `tests/actual/*.json` (also gitignored).
Both paths are blown away between runs by `scripts/test.sh`.

**Eventual destination:** path-backed mmap'd `Store` (the hash
IS the file — no save loop).  Rust side ships; loft `.loft`
language surface for binding user-data Stores to a path is
missing.  Filed in [`QUESTIONS_FOR_LOFT.md` § Path-backed
user-data Store binding](QUESTIONS_FOR_LOFT.md); strategy in
[`plans/ROADMAP.md` § Persistence destination](plans/ROADMAP.md).
**Don't take the manual binary `file()` + `#read` detour** —
it's strictly worse than the JSON we have today.

### Plan structure

dryopea follows **moros's plan conventions** — see
[`plans/README.md`](plans/README.md) for the binding, and
[`plans/_TEMPLATE.md`](plans/_TEMPLATE.md) to start one.  The
essentials:

- **Layout is FLAT**: `plans/<NN>-<slug>/`.  There is no
  `future/` · `finished/` · `deferred/` — lifecycle is a field
  in the plan's own `## Status` section, so a plan that ships
  does not move on disk and invalidate every link to it.
- **Never renumber existing plans.**  New plans take the next
  unused integer.  Numbering carries no priority —
  `plans/ROADMAP.md` carries the ordering.
- **Most work is not a plan.**  A plan earns its directory only
  when the work is genuinely multi-phase; cap active plans at
  2–3.  See § Pick the lightest workflow that fits.
- Every phase names a **gate** — how you *see* it works.
  "It compiles" is not a gate.
- Value tags `S/R/G/F/U/C/Q/N` and effort letters
  `XS/S/M/MH/H/VH`, the same letters as moros and loft.

## Plans, ROADMAP, docs

```
plans/
  README.md       — plan conventions + index
  _TEMPLATE.md    — copy this for a new plan
  ROADMAP.md      — comprehensive feature roadmap (5 tiers,
                    A validation → E narrative arcs)
  DEFERRED.md     — parked plans (none yet)
  01-ground-editor/         — Active (E1-E4 + smoke + E1-live shipped)
  02-solver-validation-viewer/
  03-marker-layer-and-spawns/
  04-map-library/
  05-validation-scenario/
  06-editor-stencil-pipeline/ — hex_* substrate now published
  07-shared-world-substrate/  — Active (W0 partial)
  08-game-validation/         — Complete (V0-V4 shipped):
                    scripted play, measured effects, PNGs for
                    inspection, and `make validate` over the lot
  11-flow-field/              — Active (F0 + F1 shipped): enemies
                    route round walls to the core.  F0 answered it: an
                    "entrance" needs no detecting, the field finds
                    gaps by itself — and walls are walk_ground=true,
                    so the obvious passability predicate is the bug.
                    F1 built the instrument that can SEE that bug
                    (src/passable.loft + `enemies passable`); F1b
                    made approach mode stop at the wall — the first
                    wall here that works.  F2 (the distance field)
                    is next
  09-lattice-conversion/      — Active (C0 shipped): dryopea moves
                    to pointy-top odd-r offset, the convention every
                    hex_* library and moros already speak.  Checked
                    against hex_grid as an ORACLE, because a
                    rebaselined golden agrees with a shear

docs/
  DESIGN.md             — master design (mechanics, towers, walls,
                          combat dynamics, scramble loop, run shape)
  SETTING.md            — fiction (AI-driven robots, faction lore,
                          surface-vs-underground, future contact gates)
  DESIGN_HISTORY.md     — 2023 prototype design seeds
  GROUND_TYPES.md       — 11-type palette (water + land + structure)
  NUMBERS.md            — tunable values
  PROXY_ART.md          — placeholder shapes for entities

PROBLEMS.md             — dryopea-internal bugs (@D-prefixed; none yet)
QUESTIONS_FOR_LOFT.md   — outbound queue to loft (Open / Submitted / Resolved)
README.md               — public project intro
loft.toml               — package manifest (depends on graphics)
```

## Loft consumer relationship + library dependency

**Reuse is the rule.**  Do not write a dryopea-local version of
a routine a library already provides, and do not work around a
library bug with a private copy — fix it upstream (or file it)
and consume the release.  Libraries are owned by their
first-class projects; dryopea may ADD to them under their
existing contract, which is the right move when dryopea needs
something adjacent to what a library already does.

**Always check the real surface before writing against a
library** — `loft api <name>` prints its full public API, and
`.loft/api/<name>.api` holds the generated stubs.  Never guess a
signature.

- **Today:** `graphics` and `gridmesh` resolve from the loft
  package registry (`loft.toml` + `loft.lock`); they migrated
  out of loft's monorepo to `loft-libs-graphics`.  `moros_map`
  is a path-dep into the moros checkout (`../moros/lib/moros_map`)
  — it is not published, and is declared but not yet consumed.
- **The shared hex substrate now EXISTS as published libraries.**
  What the docs still call `lib_plan 24` shipped as the `hex_*`
  family in the registry: `hex_field` (exact-integer hex cell
  sets + outlines — the base), `hex_grid` (geometry: axial/pixel,
  neighbours, distance, corners), `hex_shape` (line / box / arc),
  `hex_form`, `hex_place`, `hex_draw`, `hex_edge`, `hex_way`,
  `hex_roof`, `hex_fit`, `hex_recover`, `hex_world` (sparse
  32×32-chunk world model with binary save/load), `hex_terrain`,
  `hex_body`.  moros additionally carries `moros_map` /
  `moros_render` / `moros_sim` / `hex_editor` / `hex_mesh` in
  `../moros/lib/`.
- **Convention mismatch — SETTLED 2026-08-12: dryopea adopts the
  libraries' convention.**  The question was which lattice is
  authoritative.  Answer, from the source: the entire `hex_*`
  family and `moros_map` are **pointy-top, odd-r offset** —
  `hex_grid` calls it "THE CONVENTION (shared with moros — the
  single executable source of it)", `hex_field`'s neighbour table
  is "odd-r offset, same SET as `hex_grid::hex_neighbor`", and
  `moros_map` carries a fixed bug from applying axial cube
  distance to odd-r coords (moros#10).  Plan 07's note that
  moros_map is axial was the stale one, and dryopea's
  **axial flat-top** is the odd one out.
  **The decision (project owner, 2026-08-12): dryopea converts**
  — one lattice across the ecosystem, and it is not the libraries
  that move.  Everything below § Hex convention is therefore the
  OLD convention until the conversion phase lands; see
  [`plans/07-shared-world-substrate/README.md`](plans/07-shared-world-substrate/README.md)
  § W0c.
- **Plans 06 and 07 should be re-read against this.**  Both were
  written waiting on an extraction that has since happened, so
  their "blocked on lib_plan 24" framing is stale.

## Documentation index

| File | Topic |
|---|---|
| [README.md](README.md) | Public-facing project intro |
| [docs/DESIGN.md](docs/DESIGN.md) | Master design — towers / walls / waves / scramble / camera / HUD / economy / run shape |
| [docs/SETTING.md](docs/SETTING.md) | Fiction — autonomous AIs (girl-hacker imprint), faction wars dormant, surface-vs-underground, future contact gates, crew-doesn't-walk justification, combat-bot escalation |
| [docs/DESIGN_HISTORY.md](docs/DESIGN_HISTORY.md) | 2023 prototype seeds |
| [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) | Enemy movement — two steering modes, passability as a height step, bodies as terrain, sealing punished not forbidden, structural wall HP, the tick resolving once |
| [docs/GROUND_TYPES.md](docs/GROUND_TYPES.md) | Palette spec |
| [docs/NUMBERS.md](docs/NUMBERS.md) | Tunable values |
| [docs/PROXY_ART.md](docs/PROXY_ART.md) | Placeholder shapes |
| [plans/README.md](plans/README.md) | Plan conventions (moros-style) + index |
| [plans/_TEMPLATE.md](plans/_TEMPLATE.md) | Template for a new plan |
| [plans/ROADMAP.md](plans/ROADMAP.md) | Comprehensive feature roadmap (5 tiers) |
| [plans/01-ground-editor/README.md](plans/01-ground-editor/README.md) | Plan 01 — Active. E1-E4 + smoke + E1-live shipped |
| [plans/06-editor-stencil-pipeline/README.md](plans/06-editor-stencil-pipeline/README.md) | Plan 06 — editor-to-stencil pipeline (two purposes, three audiences) |
| [plans/07-shared-world-substrate/README.md](plans/07-shared-world-substrate/README.md) | Plan 07 — go 3D on the shared hex substrate |
| [plans/08-game-validation/README.md](plans/08-game-validation/README.md) | Plan 08 — scripted play, measured effects, PNGs for inspection |
| [plans/09-lattice-conversion/README.md](plans/09-lattice-conversion/README.md) | Plan 09 — dryopea moves to the libraries' lattice (pointy-top odd-r offset) + adopts `input` |
| [plans/10-extract-local-libraries/README.md](plans/10-extract-local-libraries/README.md) | Plan 10 — dryopea's own reusable code becomes published libraries |
| [plans/11-flow-field/README.md](plans/11-flow-field/README.md) | Plan 11 — enemies route round walls to the core (replaces the straight-line tick) |
| [PROBLEMS.md](PROBLEMS.md) | Dryopea-internal bugs (`@D<NNN>`) |
| [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) | Outbound queue to loft |

## Reading by goal

| Goal | Start here |
|---|---|
| Understand the game | [README.md](README.md) → [docs/DESIGN.md](docs/DESIGN.md) |
| Understand the fiction | [docs/SETTING.md](docs/SETTING.md) |
| Pick next work to do | [plans/ROADMAP.md](plans/ROADMAP.md) — 5-tier feature list |
| Continue plan 01 work | [plans/01-ground-editor/README.md](plans/01-ground-editor/README.md) § Implementation status |
| Add a regression test | `tests/01_*.loft` for patterns; `golden.loft::assert_golden` for image tests |
| Script a run of the editor | `tests/scripts/*.keys` for the vocabulary; `script.loft::script_run_file` to play one; `snap <name>` for a picture |
| Add a validation scenario | a new `tests/scripts/<name>.keys` + one test in `tests/08_v3_scenarios.loft` (pin its check count — a scenario with its measurements deleted still reports ok) |
| Change what a frame contains | `editor_view.loft::render_editor_frame` — the GL loop and `snap` both draw it, so edit it there, not in `main.loft` |
| Write/edit a `.loft` file | Loft language conventions: see § Important conventions above + loft's own `loft-write` skill |
| Run the editor | `loft src/main.loft` |
| File an outbound loft request | [QUESTIONS_FOR_LOFT.md](QUESTIONS_FOR_LOFT.md) |
| File a dryopea-internal bug | [PROBLEMS.md](PROBLEMS.md) (`@D<NNN>` convention) |
| Understand library extraction | The `hex_*` family is published — `loft api --registry` |
| Change how enemies move | [docs/ENEMY_MOVEMENT.md](docs/ENEMY_MOVEMENT.md) — the whole spec.  [plans/11](plans/11-flow-field/README.md) is what it costs to build |
| Ask whether an enemy may be on a hex | `src/passable.loft` — ONE rule, both questions.  Never `walk_ground` on its own |
| Validate the GAME (not a function) | `scripts/validate.sh` — then [plans/08-game-validation/README.md](plans/08-game-validation/README.md) |
| Add a script to the gate | drop a `.keys` in `tests/scripts/` — the sweep finds it.  ⚠ every file there must play GREEN; a run that must FAIL belongs in a test as an inline string |

## Branch policy

### Current phase — pre-game-shippable: commit + push directly to `main`

**Until a runnable game build exists, direct commits to `main`
are the normal flow.**  The repo is small, single-author, and
the cost of branching ceremony outweighs its benefit while the
foundation is being laid.  Commit locally, push when the user
asks — no automatic pushes.

**Trigger for switching to the formal flow below:** the moment
there's a runnable game — even a minimum-playable validation —
this section is retired and the **MANDATORY** rules below
become the policy.

### Future phase — once a runnable game exists — MANDATORY

**Direct commits to `main` will not be allowed.**

All changes — features, design updates, plan edits — must land
on a feature branch and reach `main` only through a pull
request.  CI gates each PR.  `main` becomes the release branch.

#### Rules (active once the policy switches)

1. **Never `git commit` directly on `main`.**  If you accidentally
   land on `main`, move the change to a feature branch before
   anything else.
2. **Pushing commits is OK by default — unless there's an open PR
   on the branch that the push would disturb.**  For a long-lived
   working branch with no open PR, push freely after each green
   commit.  When the branch has an open PR, do NOT push without
   an explicit user instruction.
3. **Never create a branch or open a PR unless the user
   explicitly asks.**  "Implement plan 01 phase E1" is *not* a
   PR instruction.  Only run `gh pr create` or `git checkout -b`
   after the user explicitly says "create PR", "open a PR",
   "merge", or "switch to a new branch".
4. Default branch name for general work: a GENERAL slug
   (`work`, `cleanup`, `housekeeping`).  ONLY a substantial plan
   earns a specific branch name.
5. Merging to `main` is via a GitHub pull request — not a local
   `git merge`.

## Git safety — MANDATORY

### Never use `git stash pop` or `git pull` with uncommitted changes

Both can produce unrecoverable working-directory states.  Always
commit before any operation that changes the working tree.  To
compare with main, use `git diff main -- <file>` or `git show
origin/main:<file>` — no branch switch needed.

### Never use `git bisect` or `git checkout HEAD -- <files>`

Both routinely destroy multi-session work-in-progress.  To
investigate a regression, read the relevant code paths directly
or use `git show <commit>` / `git diff <commit>^ <commit>`.

## Documentation validation

We **don't** have a loft-style `@P` tracker + `./scripts/idx`
indexer yet.  Triggers for adding one:

- First dryopea-side P-issue gets numerous enough that prose
  references stop being practical (PROBLEMS.md currently has
  zero `@D` rows; trigger fires somewhere around ~20).
- Documentation count crosses ~25 (currently ~12).
- A specific drift incident makes the manual scan painful.

Until then: keep cross-references prose-form (§ section names)
+ explicit relative-path markdown links.  Run `scripts/test.sh`
before committing — it's the only doc-adjacent automation we
have today (validates tests via assert_golden + the loft test
runner).
