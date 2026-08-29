<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea architecture — the `src/` layout and its data structures

Extracted from `CLAUDE.md` so the always-loaded file stays a router.
Its § Architecture — src/ layout carries a one-line index of the same
files and points here for the detail.

⚠ **Each file's own header comment is the source of truth.**  What is
written here is a navigational summary of it, and a summary can drift —
when the two disagree, the `.loft` file is right and this is stale.

## Architecture — src/ layout

```
src/
  dryopea.loft     library aggregator — `use dryopea;` brings every
                   submodule into scope (tests use this entry)
  bindings.loft    the ONE key table (plan 09 I1) — EditorAction
                   {name, keys, ctrl rule, palette index} +
                   editor_actions() + editor_input_from(), the single
                   door from keys to the seam.  The GL loop polls it
                   and a `.keys` run FEEDS it, so `do undo` presses
                   the keys a player presses and a wrong binding
                   fails the gate.
                   ⚠ ELEVEN palette hotkeys over a TWELVE-entry
                   palette: `rubble` is deposited by the runtime and
                   painted by nobody, so plan 12 B1 deleted the `=`
                   binding it would otherwise have had.  An authored
                   rubble hex is a second representation of a pile that
                   `height_clear` could not take away.
                   ⚠ The ctrl rule is DATA, not resolver code:
                   `input::ActionBinding` has no modifier concept,
                   and a rule written once in the resolver and once
                   in the runner is a second table wearing a hat.
                   ⚠ EDGES are NOT here — plan 08 V0 put edge
                   detection in the seam and I1 kept it, so this
                   reads LEVEL state and `input`'s `is_action_just_*`
                   are deliberately unused.  Two edge detectors is
                   the drift this file exists to prevent.
                   ⚠ THREE readers since plan 19 P3, not two: the six
                   PLAY rows are `play_actions`' and the one SHELL row
                   (`toggle_play`, P) is the WINDOW's.  `playing`
                   fills the drive set or the pan set, never both —
                   but `toggle_play` is filled either way, or there is
                   no way out of play mode
  main.loft        interactive entry point — `fn main()`, NOT in the
                   aggregator (runs via `loft src/main.loft`).
                   The GL shell only: open window, measure the frame,
                   poll input, call `play_step`, render.  Parse-check
                   it by hand after every edit — `scripts/test.sh`
                   can't see it.
                   ⚠ Since plan 19 P3 it runs the GAME, and its whole
                   input-and-time half is three lines mirrored in
                   `tests/19_p3_the_clock.loft` § The window's frame:
                   read the mode, resolve the keys, hand over
                   `play_frame_seconds(ps, elapsed)`.  It owns the
                   CLOCK and decides nothing else
  editor_step.loft the input seam (plan 08 V0) — EditorState (all
                   session state) + EditorInput (one frame of intent)
                   + editor_step(s, input).  EVERY action runs through
                   it.  No GL and no clock, ever; disk only via the
                   save / reload actions, and only when a path is
                   attached (editor_state_attach).
                   ⚠ `s.prev` is READ-ONLY for the whole of a step and
                   written ONCE at the end.  It records a frame that
                   already happened, so a mid-step write does not
                   cancel an edge — it FORGES one for every branch
                   below it.  That was @D001: four writes clearing
                   `prev.in_mouse_left` to "drop a held button" made
                   Tab / Ctrl+R / Ctrl+N place a marker the player
                   never asked for.  An action that wants to end a
                   gesture sets the GESTURE's state (`s.painting`),
                   never the input history
  editor_view.loft render_editor_frame(s, w, h, ppm) -> Canvas —
                   ⚠⚠ VIEW_PPM IS PRIVATE and `view_ppm(cam)` is
                   the one door (BACKLOG C7, @X285, @D002).  The
                   base scale is 24.0 px/m at the closest zoom;
                   the scale anything may DRAW, MEASURE or invert
                   a click through is `VIEW_PPM / cam.zoom`.
                   ⚠ Four paths read it and they must AGREE — the
                   GL loop's frame, `snap`'s frame,
                   `classify_world` behind the `frame`
                   measurement, and `screen_to_hex`, which
                   inverts a pointer back to a hex.  A frame drawn
                   at one scale and clicked through another paints
                   the wrong hex; drawn at one and CLASSIFIED at
                   another, every band measures a picture nobody
                   saw.  ⚠ The `zoom` clamp is [loft#914]: a
                   partial `EditorCamera { }` takes 0, and
                   clamping to ZOOM_MIN draws what it always drew.
                   what the player sees, composed ONCE: world, hover
                   preview, markers, ghost, picker, save indicator,
                   mode badge.  Both the GL loop and the script
                   runner's `snap` ask for it, so a shot is the
                   editor's frame and not a harness renderer's.
                   Also owns VIEW_W / VIEW_H / VIEW_PPM (the window
                   size IS the shot size).  Never mutates the state
  ⚠⚠ tick_clock.loft, tick_bank.loft and tick_timer.loft LEFT src/ at
     plan 26 L6.  They are the **`fixstep` PACKAGE** now, in
     loft-libs-game beside `input` — TickClock (clock_advance /
     clock_step / clock_alpha / the L4 policies), Bank (bank_gain) and
     Timer (timer_arm / timer_spend), plus the `approach` ease that
     render_camera.loft used to own.
     ⚠ `loft api fixstep` is the surface and the package's own README
     is the guide.  **Do not restate either here** — the whole reason
     the extraction was worth doing is that there was ONE copy of this
     arithmetic instead of nine, and a second copy of its DOCS is the
     same mistake one layer up.
     ⚠ dryopea's own numbers stay here: TICK_STEP_UNITS and
     TICK_SECONDS are in spawn.loft, and @X079 is why the base unit is
     a third of a microsecond rather than a microsecond.
  play.loft        the GAME's seam (plan 19 P1) — PlayState { wave,
                   clock, ticks, prev, playing, cam } +
                   play_state_new / play_core / play_ticks /
                   play_advance_units / play_advance / play_step_units
                   / play_step, plus the play ACTIONS (P2) and the MODE
                   (P3: play_mode / play_set_mode / play_begin /
                   play_frame_seconds / play_frame_units).
                   ⚠ **The bank is a TickClock since plan 26 L1**, and
                   the doors come in PAIRS: `*_units` are exact,
                   `play_advance` / `play_step` take float seconds and
                   ROUND at the boundary.  Truncating there would put
                   19_p1's 602 back one layer out.  `main.loft` hands
                   down integer µs and no longer divides.
                   ⚠ `cam` is the game's CameraRig (plan 21 R2,
                   @X014).  `play_step` ends by stepping it — on
                   EVERY frame, including frames that spend no tick,
                   because the ease is a function of elapsed time.
                   Putting it inside `play_advance`'s tick loop is
                   the mistake: at 60 fps it would run on one frame
                   in forty and stutter with the right average.
                   ⚠ **`play_one_tick` is the ONE call to `wave_tick`
                   in the repo**, and that is the whole invariant: a GL
                   loop that ticked the game itself, or a script verb
                   that kept its own loop, would be a second game
                   wearing the same numbers.
                   ⚠ **TWO ways to ask, and they are NOT
                   interchangeable.**  `play_ticks(n)` is a COUNT (what
                   `tick` and `fall` ask, and what all 520 gate
                   measurements are pinned to); `play_advance(seconds)`
                   is a DURATION (what a frame has, and the only one
                   that banks a remainder).  `play_advance(n *
                   TICK_SECONDS)` is one tick SHORT for 602 of the
                   first 1000 `n` — see § Testing something that moves.
                   ⚠ `play_ticks` does not touch `banked`, or a
                   scripted tick would steal part of a live frame —
                   sub-tick drift no measurement could see.
                   ⚠ **No epsilon on the accumulator, and its absence
                   is a DECISION**: this timer REPEATS and its
                   remainder CARRIES, unlike the one-shot timers in
                   `helper.loft` / `tower.loft` / `vehicle.loft` whose
                   boundary is decided once and for ever.
                   ⚠ **No clamp on the tick count** either — a frame
                   that delivers 20 s really does play 30 ticks, and a
                   clamp would make the simulation frame-rate dependent
                   at whatever length it chose.
                   ⚠ **PlayState does NOT own the EditorState**: a
                   struct in a field is a COPY, so composing the
                   caller's session would fork the world.  The world is
                   PASSED, the game is OWNED.
                   ⚠ No core, no game — and the seconds are DROPPED
                   rather than banked, or placing a core would replay
                   the whole wait as one burst.
                   ⚠ **The MODE gates the CLOCK, never the seam**
                   (P3).  `input.in_playing` says what the KEYS mean
                   this frame; `PlayState.playing` says whether wall
                   time reaches the simulation at all
                   (`play_frame_seconds`).  Every P1/P2 test hands
                   `play_step` real seconds on a session that has
                   never been in play mode, so confusing the two
                   breaks the corpus.
                   ⚠ **The mode is remembered by the SESSION, not by
                   `main()`** — `plans/19` § Open questions 2 said the
                   SHELL, and the seam's half of that stands (nothing
                   is baked in), but a local in an entry point
                   carrying `#cwd` is a decision no test can reach.
                   ⚠ `play_begin` puts the crew at the CORE and is a
                   stand-in for plan 05's landing flow; it refuses an
                   occupied chassis, so the toggle is a PAUSE rather
                   than a restart
  compare.loft     are two runs in the same state? (plan 18 S0) —
                   state_diff(a, b) answers the FIRST difference and
                   both values, "" when identical; states_equal is
                   defined in terms of it so the two cannot disagree.
                   ⚠ Deliberately knows NOTHING about how a state is
                   written down: defining equality as "emit both and
                   compare the text" makes the round-trip gate circular.
                   ⚠ Every helper is declared ABOVE its caller and the
                   one door is at the BOTTOM — loft#918 panics the
                   parser on a local bound to a forward-declared call
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
                   (`do toggle_mode`); ⚠ `do Tab` must keep FAILING —
                   a key name that starts working means a second
                   table was built.  Since plan 09 I1 the runner
                   TYPES on `bindings.loft`: an action name becomes
                   key codes, which go through `input`, which the
                   same resolver the GL loop uses turns back into an
                   EditorInput.  The round trip looks pointless
                   written down and is the whole point — before it, a
                   binding could be wrong in the editor with all 14
                   scripts green.  Reaches the editor ONLY
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
                   enemies are from the core — a SPAN, because a
                   walking wave is strung out) and the five scenario
                   scripts in `tests/scripts/`.  Plan 11 added `enemy
                   <i> <q> <r>` / `enemies passable` (F1) and `enemies
                   distinct` (F5c — no two live enemies on one hex,
                   RED until a freshly-spawned wave has walked apart).
                   F6 added `wave <n> [class]` (robot / insect — an
                   unknown name is an ERROR, because a script that
                   silently got robots would assert the opposite of
                   what it says) and `raise <q> <r> <metres>`, which
                   piles runtime height onto a hex the way a body does.
                   Plan 12 B1 gave `raise` an optional `[source]`
                   (wreckage / carapace / masonry — named, never
                   numbered, and a typo is an ERROR) and added
                   `clear <q> <r>`, which takes a pile away and is how
                   a run states the layer's defining property.
                   F7 added `target <i> <q> <r>` and `count targets
                   <lo> <hi>` — the SET of hexes under attack, which is
                   the only measurement that can tell a spread siege
                   from one collapsed onto a single chokepoint, and the
                   only enemy measurement that does not depend on
                   spawn order.
                   Plan 12 B2 added `damage <q> <r> <hp>` (which cannot
                   BREAK anything — only a tick does, so it stays one
                   code path) plus the `hp` and `pile` band
                   measurements; B4 added `hit <i> <hp>`, the same rule
                   for an ENEMY — a separate verb rather than an
                   overload, because `damage 4 10` reading as either a
                   hex or an index is a line whose meaning depends on
                   knowing which.  ⚠ `hp` over a hex with nothing
                   breakable on it is an ERROR, because "at 0 HP" and
                   "no wall here" are the two states a break moves
                   BETWEEN and one number for both is green before the
                   siege and after the wall is gone.
                   B6 added `wallet <lo> <hi>` — POINTS LEFT, so
                   `wallet 0 0` is how a run says "the base fell".
                   ⚠ It needs no core marker and no wave: the budget
                   belongs to the RUN, not to the battlefield, so 200
                   is the honest answer before a single enemy exists.
                   Plan 15 C2 added `take <who>` / `drop <who>` (`player`
                  or a crew index) plus the `cargo` and `roster` bands.
                  ⚠ **Two verbs for `DESIGN.md` § 11's ONE key** — the
                  same choice `park` / `drive` made: the key is
                  context-resolved on the carrier's state, and a script
                  SAYS what it means so a line asserting a pickup cannot
                  quietly have been a deposit.  What a script does NOT
                  get to decide is where the cargo ends up: `drop` at
                  the core retrieves where `drop` one hex out merely
                  puts down.
                  ⚠ `roster` counts crew STANDING, not enrolled —
                  `len(crew)` never falls (a wreck keeps its slot) and
                  `helper <i> <q> <r>` is true whether it is standing
                  there or lying there, so neither can see a loss.  A
                  crew member in RECOVERY is not standing either, which
                  is what makes the 60 s visible to a script.
                  Plan 16 W1 added `schedule <counts…>` + the `waves`
                  band, and W3 made the first of those ARM rather than
                  start: the run's list is authored and waits for the
                  design's trigger, so a scenario starts its own waves
                  by DRIVING onto a spawn marker 12+ hexes out, exactly
                  as a player does.  ⚠ A verb that also started it would
                  be the one shortcut letting every scenario in the gate
                  skip the rule — so `schedule 3 4` with nobody to poke
                  it plays nothing, and says so on stdout.
                  Plan 23 K1 added `compose <wave> <count> <class> …`,
                  which says what ONE wave of the armed list is made
                  of.  ⚠ It REPLACES that wave, so re-running a fixture
                  reaches the same state — and a `schedule` line AFTER
                  it wipes it, which is why `emit.loft` writes
                  `schedule` → `compose` → `pending` in that order.
                  ⚠ Its parsing lives in `compose_fault` /
                  `compose_parts` / `script_compose` and NOT in
                  `script_command`: a `vector<Struct>` local in that
                  function corrupts the interpreter heap
                  (loft#935).
                  B7 added `fall <max>` (tick until the wallet empties
                   — ⚠ still standing after `<max>` is an ERROR, or a
                   later `ticks` band would read a collapsed premise as
                   a measurement) and `ticks <lo> <hi>`, the run's
                   CLOCK, which `ScriptRun` now carries
                   plans/30 R3 added `now <seconds>` — the moment the
                   run has reached, AUTHORED rather than simulated (a
                   `tick` advances one, this states one).  ⚠⚠ It exists
                   because `compare.loft`'s `now` row went RED on its
                   first full run: `18_s2`'s round trip replays what
                   `emit_keys` wrote and every tick was advancing a clock
                   nothing wrote down.  ⚠ WRITER and READER are a PAIR
                   (@D007) and deleting either half turns the round trip
                   red, which is checked.
                   ⚠ **A new coordinate-carrying verb needs a row in
                   `convert.loft::keys_schemas`**, or a future lattice
                   conversion leaves it in the old labels — silently,
                   because an unknown command passes through unchanged.
                   `tests/09_c5a_converter.loft` § The schema is
                   complete is the gate, and it only fires if its
                   vocabulary list is updated too
  maps.loft        a MAP, as repo content (BACKLOG A2) — map_slot_path /
                   map_marker_slot_path (the two files one map is stored
                   in, defined ONCE because the launcher and the builder
                   both compose them), map_names, map_play_source,
                   map_build_one / map_build_all, and map_fault.
                   ⚠⚠ **`map_fault` is the point of the file.**  The
                   failure it exists to catch is silent: a map you can
                   OPEN, drive around, and never start a run on, because
                   no spawn marker is `WAVE_1_PROVOCATION_HEXES` from the
                   core to poke or none of them can walk to it.  `MAP=`
                   loads it, P lands the crew, and the waves simply never
                   come — nothing else in the tree notices.
                   ⚠ It is a PURE function of the two layers: no run, no
                   clock, no `WaveState`.  A map IS its ground and its
                   markers, and a check that needed a running game could
                   not be asked of a file on disk — so it reads the same
                   whether a map was built from a source, painted in the
                   editor, or written by hand.
                   ⚠ The BUILDER asks it and REFUSES to write a map that
                   fails, which keeps the failure out of the repo rather
                   than merely reporting it once it is in;
                   `tests/a2_the_maps.loft` over the shipped `maps/` is a
                   SECOND reading, because a map edited in the editor
                   never went past the builder.
                   ⚠⚠ `maps/<n>.keys` is the SOURCE and `maps/<n>.json` +
                   `<n>_markers.json` are BUILT — and all three are
                   committed, because `make play MAP=` loads the JSON and
                   a fresh checkout has no builder run in it (`@X265`).
                   The build SAVES and the source does not: a `do save`
                   line is ceremony that can be forgotten, and it keeps
                   the source pure authoring, which is what lets the same
                   file be played directly through `SCRIPT=`.
                   ⚠ A map holds the GROUND and the MARKERS and NOTHING
                   ELSE.  A `crew` or `schedule` line in a source is
                   authored, played, and then silently dropped by the
                   save, so every shipped map is played solo
  mapbuild_main.loft  the builder's entry point — `fn main()`, NOT in the
                   aggregator (runs via `scripts/build_maps.sh`).  Holds
                   no decision, for `validate_main.loft`'s reason.
                   ⚠ It WRITES repo content, which is why it is not part
                   of `scripts/test.sh`
  scenario.loft    a `.keys` scenario, opened as a live STARTING
                   POSITION (BACKLOG A1) — scenario_open(s, name) ->
                   ScriptRun, whose `.play` is the game the window
                   keeps.  The seam between `script_run_on` and
                   `main.loft`, which until it existed had no path
                   between them: `maps/` is empty, `.keys` has no
                   `save` verb, and `make play` opened a paint brush.
                   ⚠⚠ **It TRUNCATES the file at its first `tick` /
                   `fall`** (`@X263`).  A scenario's tail is its
                   ANSWER — a fallen base, an exhausted wave list, a
                   wall already broken — and a player wants its
                   QUESTION: the ground, the wall, the towers, the
                   crew and the armed schedule, first wave un-poked.
                   It is also the repo's existing use of the word,
                   since `emit.loft` writes a situation as "an
                   authored STARTING POSITION with no `tick` in it".
                   ⚠ It TRUNCATES rather than FILTERS: a measurement
                   written after a tick is true about the end and
                   false about the beginning, so cutting leaves every
                   surviving line still saying what it said.
                   ⚠⚠ `tick` and `fall` are the WHOLE advancing set —
                   `drive` / `send` / `boost` / `park` only set a
                   destination or a flag, and a scripted frame is
                   worth 0.0 seconds of game time — and that claim is
                   not left to a comment: `tests/a1_the_scenario`
                   opens EVERY `.keys` file in the tree and asserts
                   the clock reads t0, so a verb that grew a tick goes
                   red naming the file that reached it.  ⚠ Paired with
                   its own vacuity control, because an EMPTY position
                   also reads t0.
                   ⚠⚠ It also owns the COMMAND LINE's reading —
                   `script=<name>`, never `--script <name>`, because
                   loft takes a leading `--` argument as one of its
                   OWN and strips it, so the flag form would open a
                   MAP of that name, silently (`@X264`).  It lives
                   here rather than in `main.loft` for the reason
                   every decision does: an entry point is compiled by
                   nothing.
                   ⚠ A bare name resolves `tests/scripts/` then
                   `tests/gl/`; a name ending in `.keys` is a path,
                   used as given.  A name that resolves to nothing is
                   a named FAILURE, never a quietly empty world
  lattice.loft     THE lattice (plan 09) — pointy-top odd-r offset, the
                   convention every hex_* library and moros speak.
                   Owns `Hex { q, r }` (q is a COLUMN, r a ROW),
                   HEX_DIAMETER = 1.5m, HEX_FLAT_TO_FLAT, and the
                   `lat_*` verbs: lat_neighbour(s), lat_direction,
                   lat_edge_corners, lat_distance, lat_line, lat_disc,
                   lat_to_metres / lat_from_metres, lat_corner_*,
                   lat_to/from_axial.
                   ⚠ `src/world.loft` and its axial arithmetic are
                   DELETED (C6).  The `lat_` prefix is a scar from the
                   period when both existed; it stays because every
                   call site reads it.
                   ⚠ It DELEGATES to `hex_grid` — never a second
                   implementation, which is what makes the lattice
                   right by construction rather than by two copies
                   agreeing.  It adds only what the library cannot
                   know: dryopea's `Hex` type and dryopea's METRES
                   (one hex_grid unit = 0.75 m = one circumradius).
                   ⚠ `hex_offset` has NO counterpart — in odd-r the
                   neighbour delta depends on row parity, so a constant
                   (dq, dr) table does not exist.  The operation is
                   deleted by the conversion, not translated.
                   ⚠ `lat_edge_corners(d)` (plan 25 M1, @X073) takes
                   NO Hex, and the split is the point: the neighbour
                   LABEL delta is parity-dependent, the corner
                   relation is not — a hexagon is the same hexagon on
                   both parities and only its coordinates move.  Same
                   shape as lat_direction_unit.  ⚠ Delegated, never
                   tabulated; `tests/25_m1` re-derives the pairing
                   GEOMETRICALLY (the two corners nearest the
                   neighbour's centre, both parities) rather than
                   restating hex_grid's six rows, so the check is made
                   in dryopea's own frame instead of assumed to
                   survive its two y-negations.
                   ⚠ `hex_grid::hex_round` answers AXIAL, not offset —
                   `lat_from_axial` is what stops that shearing a cell
                   silently.
                   ⚠ The metre conversions NEGATE y, because dryopea
                   follows hex_grid's COMPASS and hex_grid's +y is
                   north while dryopea's is south.  So dir 5 really is
                   NE on screen — and existing maps will render
                   vertically mirrored, which is the accepted cost.
                   The metre round-trip cannot see this (a consistent
                   flip is invisible to it); the compass sign test is
                   what gates it
  relabel.loft     old label → new label (plan 09 C2) — the bijection
                   from every axial coordinate dryopea ever wrote to
                   disk to its odd-r offset name, plus the direction
                   permutation `new = (old + 5) % 6` (DERIVED from
                   geometry, uniform on both row parities).  C5 runs
                   it over the real files.
                   ⚠ The invariant is DISTANCE, not adjacency — a
                   relabel can keep neighbours neighbours and still
                   fold the plane, and two painted hexes landing on
                   one is silent.  Distance implies adjacency AND
                   injectivity, and is what keeps plan 11's flow-field
                   distances still.
                   ⚠ The picture moves by a mirror AND a 60° hex
                   rotation — flat-top → pointy-top is itself a
                   re-orientation.  Old dir 0 was due SOUTH; it
                   relabels to new dir 5, which renders NORTH-EAST.
                   A converted map does not look "upside down"
  camera.loft      ⚠ pan NORTH is `r += 1` since plan 09 C3 — north is
                   LARGER r in the new lattice, the opposite of axial.
                   `script_walk_camera`'s convergence test must agree,
                   or every `at` fails as "more than 4096 camera steps
                   away" rather than as anything naming the cause.
                   EditorCamera { pos: Hex, zoom: integer }
                   + InputState (moros-style: factories + pure tick
                   + struct of booleans)
                   + camera_update(c: &EditorCamera, input: InputState)
  render_camera.loft
                   THE GAME's camera (plan 21 R1) — moros's
                   `RenderCamera`, ported: an orbit camera in
                   spherical coordinates around a target.
                   ⚠⚠ Its world frame is x EAST, y NORTH, z UP —
                   RIGHT-handed, and NOT dryopea's `+y` south.
                   That one is a CANVAS convention; it is
                   left-handed once z points up, `mat4_look_at`
                   builds a right-handed basis, and the product
                   is a MIRROR that no azimuth undoes (@M021).
                   `lat_to_world` is the ONE place that negates,
                   and its negation cancels `lat_to_metres`' —
                   so the camera's world is `hex_grid`'s own.
                   ⚠ Two presets, not two cameras:
                   `camera_overview` (el 89°, az 270° — the
                   EDITOR's view, measured at 0.0014 rad and
                   0.56% of scale against the software
                   rasteriser, @M022) and `camera_follow`.
                   ⚠ A follow bearing is `facing + 180°` and
                   comes from the VELOCITY — never moros's
                   `270 − facing_deg`, which is right in moros's
                   frame and puts the eye ABEAM in dryopea's.
                   ⚠ Assert on `camera_eye_of_view` (eye = −Rᵀt),
                   never on the struct: a solve nobody is told
                   about is a solve that did not happen.
                   RenderCamera { target: Vec3, azimuth,
                   elevation, distance, fov_y, near, far,
                   up: Vec3 }  (mesh3d owns Vec3 / Mat4)
                   ⚠ And since plan 21 R2 the EASE — CameraRig
                   { cam, boom, rested } + camera_rig_step, which
                   is what `PlayState.cam` holds.
                   ⚠⚠ The approach is `1 − e^(−k·dt)`, NOT moros's
                   `k·dt`: the linear form is frame-rate dependent
                   and `play.loft` is built on the opposite
                   property (19-P0).  Exponential composes, so any
                   subdivision of a second lands on the same bits
                   (@M023).
                   ⚠⚠ THREE valves ease, not just the boom — the
                   vehicle is a LATTICE position and jumps 1.299 m
                   on the tick it steps, so the target and the
                   azimuth are what make the picture move at all.
                   ⚠ The azimuth eases the SHORT way round: A then
                   A+S is a real −300° swing otherwise (@M024).
                   ⚠ Rest SNAPS — an asymptote stopped by a
                   tolerance rests wherever the frames fell.
                   ⚠ Occlusion asks `passable.loft::
                   sight_first_block`, the same walker the towers
                   ask; the boom's free length is quantised to hex
                   steps and smoothed in TIME.
  ground_mesh.loft
                   THE GROUND, as triangles (plan 25 M0 + M1).
                   `ground_top_face` emits one hex's top as a
                   six-triangle fan around its centre, and
                   `ground_side_faces` the vertical quads it owns —
                   both in the CAMERA's world, so this is the only
                   geometry dryopea produces that is not in the
                   screen frame.
                   ⚠⚠ There is NO blend, and that is measured
                   rather than lazy (@X072).  moros's corner-height
                   mean is what makes terrain slope instead of
                   step; here it is a no-op at every hex in BOTH
                   directions — `height_override` is non-null on
                   two of twelve palette kinds, so across ground
                   every term is 0, and across a structure's edge
                   the mean has to be skipped anyway.  ⚠ It is
                   also the honest picture: the sim asks
                   `can_step`, a height DIFFERENCE, so a sloped
                   mesh would draw a ramp the vehicle cannot
                   climb.  The blend arrives with plan 02, and
                   plan 25 M2's halo gate is the tripwire.
                   ⚠ TWO lookups, and they are different
                   questions: HEIGHT from `hex_height` (the
                   AUTHORED entry plus the layer), COLOUR from
                   `hex_surface_index` (the SURFACE).  Swap them
                   and piling debris on a wall LOWERS it.
                   ⚠⚠ Colour is a UNIFORM, so this emits ONE MESH
                   PER PALETTE KIND (@X074) — a GATE requirement,
                   not a performance choice: a flat-unlit frame
                   drawn that way can only contain palette
                   colours, and `classify_world` is an EXACT
                   lookup.  Put the colour on the vertex and GL
                   interpolates it, so every gradient pixel is a
                   FAULT and R0's zero-drift measurement is gone.
                   ⚠ The fan winds COUNTER-CLOCKWISE (two
                   negations cancel), which is GL's front face —
                   `tests/25_m0` recomputes that from the emitted
                   triangles, because a reversed fan changes no
                   count, no height and no position and draws
                   NOTHING under GL_CULL_FACE.
                   ⚠⚠ A side face is emitted ONCE, by the column
                   that STANDS — `if hh <= nh { continue; }`
                   (@X046).  Both halves are load-bearing and both
                   fail INVISIBLY: without the guard the edge is
                   drawn from either side and the second copy is
                   back-facing (pixel-identical, twice the mesh);
                   with `<` instead of `<=` every hex boundary in a
                   flat world grows a zero-area sliver (also
                   pixel-identical).  So `tests/25_m1` gates it as
                   four COUNTS on four fixtures — 6 lone, 10 for
                   two adjacent, 0 flat, 5-and-6 across a step —
                   in four separate FUNCTIONS, because loft
                   abandons a function at its first failed assert
                   and a count that can never be the diagnosis is
                   decoration.
                   ⚠ Absent is ZERO: PaintedWorld is sparse and
                   sea-default, so a wall at the edge of the
                   painted region has a 0 m neighbour and gets its
                   quad.  A lookup that answered "no such hex" and
                   skipped would draw a base with one open side,
                   and only where the author stopped painting.
                   ⚠ The side's NORMAL comes from the two hex
                   CENTRES (outward by construction), its WINDING
                   from the corner RING — two facts computed from
                   different things, and `tests/25_m1` asserts they
                   AGREE.  A mesh whose normals point out and whose
                   triangles wind in draws nothing under
                   GL_CULL_FACE with every normal reading healthy.
                   ── plan 25 M2 ──
                   `ground_chunk_mesh(m, …, cx, cy, kind)` is one
                   TILE for one palette kind, and
                   `ground_chunk_kinds` says which kinds a tile
                   holds, ASCENDING (a kind list is an upload
                   order, so it must be a function of the map).
                   ⚠⚠ The walk is by COORDINATE, and its reason is
                   COVERAGE rather than determinism (@M025): the
                   drawn region is wider than the painted set, so a
                   walk over `pw.painted` cannot reach the
                   coastline.  Gated at 63 vertices against 0.
                   ⚠ It walks a tile once PER KIND, and
                   `ground_chunk_kinds` walks it again — ~6 passes
                   over 1024 hexes.  The one-pass shape needs a
                   `Mesh` per kind held together and a struct in a
                   container is a COPY; the trigger to solve it is
                   M4's ratio, not a hunch.
  mesh_chunks.loft THE MESHER'S DOMAIN (plan 25 M2) — which hexes
                   get drawn, which tile each lands in, and which
                   tiles an edit invalidates.
                   ⚠⚠ The drawn region is the painted set PLUS A
                   ONE-HEX RING (@X075).  `painted.loft` ERASES a
                   hex painted sea, so meshing only what is stored
                   leaves an erased region as a HOLE in the ground
                   at exactly the height of the land round it — and
                   no side quad covers it, sea and grass both being
                   0 m.  ⚠ Its limit is that a gap wider than TWO
                   hexes still holes, and a test pins that so
                   closing it is deliberate.  The real answer is
                   water's DROP, which is plan 02's.
                   ⚠⚠ TWO reaches share the value 1 and do NOT
                   share a constant: `MESH_HALO_K` is how far an
                   edit's consequences reach (a side quad reads one
                   neighbour), the ring is how far past the paint
                   anything is drawn (sea is stored as absence).
                   Plan 02's blend moves the first alone.
                   ⚠⚠ There is NO `ChunkField` here, and that is a
                   change of plan: `collect_dirty_inputs` SKIPS a
                   dirty chunk owning no cells — which with a ring
                   is a tile that still has sea to draw, stale only
                   ever at a tile edge — and `mark_borders` steps
                   CHUNK coords rectangularly, where only
                   `lat_neighbour` may step a coordinate.
                   ⚠ `mesh_chunks_touched` is ONE mechanism read
                   two ways: as an edit it is the dirty rule,
                   summed over the paint it is the tile list.  Two
                   rules derived separately disagree about an edge.
                   ⚠ What stays gridmesh's is `chunk_of`, whose
                   `chunk_div` FLOORS — the arithmetic a hand-rolled
                   `>>` gets wrong left of the origin.
  mesh_crc.loft    DO TWO MESH BUILDS AGREE? (plan 25 M2) — the
                   geometry folded to one integer, because a COUNT
                   cannot see a mesh with the right number of
                   vertices in the wrong places and a PICTURE
                   agrees with a shear.
                   ⚠⚠ It folds the TRIANGLES too, where moros's
                   port folds vertices only — `ground_top_face`
                   SHARES its six rim vertices between the six fan
                   triangles, so M0's reversed fan moves no vertex
                   at all and a vertex-only fold is blind to
                   exactly the defect M0 exists for.
                   ⚠ It ROUNDS where moros's truncates: every
                   palette height (0.0, 3.0, 5.0) sits exactly on
                   truncation's discontinuity.  ⚠ And that guard
                   was unreachable — every comparison here runs
                   identical arithmetic on identical inputs — so
                   `test_the_scale_is_the_tolerance` reaches the
                   branch directly (3.0 vs 3.0−1e-9 must AGREE).
                   ⚠⚠ An EMPTY mesh folds to 0, so a mesher
                   stubbed to emit nothing satisfies every equality
                   — each one needs a non-zero floor.
                   ⚠ It belongs in `mesh3d` (adjacent to
                   `mesh_to_floats`, and moros has a private copy
                   too, which is two).  Trigger to move it: a
                   second non-test caller, or plan 20's entities.
  ground_gl.loft   THE GROUND, DRAWN (plan 25 M3) — one shader,
                   one `graphics::GroupVboSet` per palette kind
                   keyed by chunk, and the kind's colour as a
                   UNIFORM.  `ground_gl_new` / `_upload_all` /
                   `_upload_edit` / `_draw` / `_destroy`.
                   ⚠⚠ FLAT UNLIT is a GATE requirement, not a
                   placeholder: the fragment stage writes one
                   uniform colour and reads nothing, so the frame
                   CAN ONLY CONTAIN PALETTE COLOURS and
                   `classify_canvas`'s exact lookup survives GL
                   (@X074, measured at zero drift — @M026).  Light
                   it and one colour becomes a RANGE; the answer is
                   an ID buffer, never a loosened lookup.
                   ⚠⚠ CULLING IS ON, and this file turns it on —
                   the ground's correctness argument depends on it,
                   and a reversed winding (M0, M1) draws NOTHING
                   with every other valve reading healthy.  Depth
                   testing likewise: the draw order is a PALETTE
                   order, not a depth one.
                   ⚠ `_upload_chunks` clears a kind a tile no
                   longer holds — leave it and the GPU goes on
                   drawing a wall the map says is gone, which no
                   state assertion can see.  It skips tiles with no
                   such group, because an upsert of nothing still
                   makes a VAO.
                   ⚠ An element read out of a `vector<Struct>`
                   ALIASES in loft (measured), which is what lets
                   the sets live in a field; the sibling rule — a
                   struct STORED in a field is a COPY — points the
                   other way.
                   ⚠ `ground_gl_bake` (one tile, one kind, flattened)
                   is the SHARED DOOR the cost gate measures through:
                   `ground_gl_upload_chunks` cannot be called from
                   `loft test` at all (a GL call answers "native
                   function not loaded"), so without it tests/25_m4
                   would price a second implementation.
  gl_gate.loft     THE THIRD GATE (plan 25 M3) — `gl_gate_all`
                   sweeps `tests/gl/*.keys`, draws each through a
                   real context, captures with `gl_screenshot`,
                   decodes with `imaging` and counts with
                   `classify_canvas` ITSELF.
                   ⚠⚠ SEPARATE from validate.sh on purpose (@X076):
                   folding GL into it would put all 33 headless
                   scripts behind an X server.
                   ⚠⚠ The clear colour is NOT a palette colour and
                   the camera sees nothing but ground (@X077) — so
                   `other == 0` means every pixel is an exact
                   palette colour, and an erased-gap HOLE still
                   registers as a fault.
                   ⚠⚠ A per-kind COUNT cannot see a MIRRORED world
                   (@X078, @M027: every count green, landmark 490.8
                   px out), so it also asks where two uniquely-
                   coloured hexes landed vs `camera_screen`.
                   ⚠ A landmark must be FLAT — a column draws its
                   sides in the same colour and they sit between
                   the top face and the screen centre (29 px for a
                   5 m column, 0.6 px flat).
                   ⚠ Expectations live HERE, not in the `.keys`
                   file; a fixture with no case is REFUSED by name.
                   ⚠ Every branch a TEST can reach comes before
                   `gl_create_window` — a GL call inside `loft
                   test` answers "native function not loaded".
                   ⚠⚠ Since plan 20 A5 it also draws the ROSTER, for
                   EVERY fixture including the two with nobody in
                   them — which is what turns `the-ground`'s and
                   `an-island`'s `other == 0` into a statement about
                   the entities too.  `gl_entity_pixels` sums the ten
                   entity colours by name, and
                   `a-defended-base` asserts
                   `unknown - entity pixels == 0`.
  entity_view.loft THE ROSTER, AS TRIANGLES (plan 20 A5) — which
                   entities exist, where they stand, which way they
                   face and what colour each is, walked straight off a
                   `WaveState` and a `MarkerWorld`.  ENT_CREW /
                   ENT_TOWER_BASE / ENT_TOWER_TOP / ENT_CLASSES,
                   entity_class_name / entity_colour /
                   entity_colour_rgb / entity_colours_distinct /
                   entity_min_distance2 / facing_turns / entity_yaw /
                   entity_yaw_heading / entity_hover_metres /
                   entity_bake.
                   ⚠⚠ NOTHING HERE IS STATE, and that is the whole
                   invariant: no drawable list, no per-entity render
                   record, no spawn hook.  A robot is in the frame
                   because `ws.enemies` has a live one — which is what
                   makes "a thing not drawn reads as ZERO" an
                   assertion that can fail.
                   ⚠⚠ A DRAWN CLASS *IS* THE MOVER KIND (@X093):
                   `entity_colour` is indexed by `passable.loft`'s own
                   discriminants, `KIND_VEHICLE` scar and all, with
                   crew / tower base / tower top APPENDED after the
                   last one.  There is no second enumeration to drift.
                   ⚠⚠ THE COLOURS ARE OUTSIDE THE PALETTE ON PURPOSE
                   (@X092): a pixel is an exact palette colour or a
                   FAULT, so an entity in a palette colour would be
                   counted as ground.  They land in `unknown` and the
                   GL gate sums them BY NAME, which makes its claim
                   total.  `entity_colours_distinct` is what stops that
                   being a wish — it caught PROXY_ART's tower-top red,
                   which is `palette_color(9)` to the bit.
                   ⚠ A hover unit is drawn at the height it can CLIMB
                   (@X094) — `vehicle_climb`, so boost is visible for
                   nothing — and `entity_emit_hover` decides clearance,
                   rotor rate AND canopy on the OWNER id alone, which
                   is the integer `CargoLayer` is keyed on.
                   ⚠ ONE emitter for the player and the crew, which is
                   PARTS.md § D8's "same chassis" made structural.
                   ⚠ No interpolation: an entity is drawn on its hex,
                   and @M035 has already priced the fix.
  entity_gl.loft   THE ENTITIES, DRAWN (plan 20 A5) — one
                   `GroupVboSet` per drawn class, re-upserted WHOLE
                   every frame.  entity_gl_new / _ready / _bake /
                   _upload / _draw / _destroy.
                   ⚠ It compiles `ground_gl.loft`'s OWN shader source:
                   flat unlit is a gate requirement (@X074) and two
                   programs from one text is the cheap side of sharing
                   — sharing the program object would make EntityGl
                   borrow something it must not delete.
                   ⚠⚠ EVERY class, EVERY frame, and an absent class is
                   upserted EMPTY rather than skipped: the roster moves
                   on every tick, so there is no clean dirty subset,
                   and a skipped class leaves the GPU drawing last
                   frame's robots after they died.
                   ⚠ It turns culling on itself, and here that is
                   load-bearing rather than defensive — see @D005.
  play_view.loft   WHAT A LIVE SESSION LOOKS LIKE (plan 19 P6) —
                   the composition `main.loft` calls in play mode,
                   `editor_view.loft::render_editor_frame`'s sibling
                   one mode over.  PLAY_SKY, MeshWatch,
                   mesh_watch_new / _built / _reset / _dirty / _tiles /
                   _note, play_view_units / _sync / _draw.
                   ⚠⚠ THE RENDERER DERIVES ITS OWN INVALIDATION
                   (@X095).  `ground_gl.loft` was written for an
                   EDITOR's ground; a live session moves the terrain
                   three ways that never go near `paint` (a body
                   falls, a wall breaks, a heap is cleared).  So
                   `MeshWatch` keeps a SNAPSHOT of the height layer
                   and diffs it — no dirty list on a simulation
                   struct, which would not be state, would leak into
                   `state_diff`, would survive an `emit` and would
                   grow for ever in 1397 headless tests.
                   ⚠⚠ Exact only because EVERY TERRAIN CHANGE A TICK
                   CAN MAKE MOVES THE HEIGHT LAYER — `break_structure`
                   raises masonry BEFORE it repaints.  Asserted
                   against a played base (`tests/19_p6`), which is
                   `11_f8`'s field-cache shape, not quoted.
                   ⚠ `mesh_watch_dirty` reads BOTH directions: a pile
                   that VANISHED has no entry left to walk, and a
                   sweep of the current layer alone leaves the heap
                   drawn after the map says it is gone.
                   ⚠⚠ `play_view_draw` LEAVES THE GL STATE AS IT FOUND
                   IT, and that is load-bearing: the two draw calls
                   each enable depth + cull and neither disables, and
                   the editor's picture is a full-screen TEXTURE BLIT
                   — 691 200 black pixels of 691 200 without the two
                   `gl_disable` lines (@M041).  Gated in `gl_gate.loft`
                   because no fixture can carry it: each draws ONE
                   frame and exits.
                   ⚠ The ROSTER uploads on a tick boundary (10 ms
                   against a 5 ms frame); the frame draws every time,
                   because the camera eases whether or not anything
                   moved.
                   ⚠⚠ THE HUD IS ONE NUMBER (plan 19 P7, @X097) —
                   PLAY_HUD_W / _H / _MARGIN and `play_hud_canvas`,
                   which draws the WALLET and nothing else because
                   `DESIGN.md` § HUD refuses everything else by name.
                   ⚠ Its INK is `hud.loft::hud_wallet_ink`, a ramp from
                   amber to red over the wallet (@X098) — not a
                   constant, so a gate that counted one colour would
                   pass vacuously at every other value.  The overlay is
                   TRANSPARENT except where the digits are: `rgb()`
                   sets alpha 255 and a bare `0x…` leaves it 0, which
                   is the distinction that cost `gl_gate.loft` a false
                   failure (@M041).
                   ⚠ Built and uploaded EVERY frame — 114 x 40 pixels
                   against a frame's 691 200, and the wallet drains
                   continuously, so a cache keyed on the number would
                   miss almost nothing.  The texture is deleted the
                   same frame.
                   ⚠ Not built: interpolation, and an editor in 3-D.
                   Pressing P goes back to the software frame, where
                   the picker, the hover preview and all 654 of
                   `validate.sh`'s measurements live.
  painted.loft     PaintedHex { q, r, kind: u8 }
                   + PaintedWorld { painted: hash<PaintedHex[q, r]> }
                   + paint(), lookup_painted(), paint_line()
                   (sea-default sparse storage — un-painted hex is sea)
  palette.loft     GroundType { name, color, sub_palette, slope, drop,
                   drainage, walk_*, buildable }
                   + load_palette(path) via `text as vector<GroundType>`
                   + parse_hex_color() + GROUND_RUBBLE (11) — the one
                   palette index dryopea's code names, because the
                   RUNTIME produces it.  ⚠ APPENDED, so 0-10 are
                   unsheared: an index is an identity, it is what
                   `painted.loft` stores and `MapFile` round-trips.  `slope` / `drop` /
                   `height_override` are declared NULLABLE because
                   palette.json writes null in them — see the file's
                   own warning
  damage.loft      what a structure has TAKEN, and what happens when it
                   has taken enough (plan 12 B2) — DamageLayer +
                   damage_apply / damage_taken / damage_clear / count,
                   structure_max_hp / structure_breakable / structure_hp,
                   rubble_height_of, break_structure, damage_resolve,
                   plus C6's FOOTING — ground_footing / footing_of /
                   footing_name, FOOTING_BRITTLE / _ORDINARY / _STURDY.
                   plus B4's `enemy_max_hp` / `body_source` / the
                   `BODY_HEIGHT_METRES` a death drops and B5b's
                   `ENEMY_HEIGHT_METRES` / `enemy_height` (the class→
                   number tables live here; the per-enemy verbs live
                   in `spawn.loft`, where `Enemy` is).
                   ⚠ **A body is 0.5 m and a STANDING robot is 1.0 m**,
                   and both numbers are here because they are the same
                   robot in two states.  The body height is the unit
                   the ramp band is counted in, and at 1.0 m two and
                   four bodies land exactly on the band's endpoints
                   with no interior; the standing height is what a
                   tower aims at, and aiming at the FEET instead puts
                   the canonical shot exactly on the LOS boundary.
                   ⚠⚠ A WALL IS AS STRONG AS THE GROUND IT WAS CUT
                   FROM (BACKLOG C6, @X284).  `structure_max_hp` is now
                   kind x bracing x FOOTING, and the pair is one idea
                   said twice: bracing is what holds a wall up
                   SIDEWAYS, footing is what holds it up from BELOW.
                   ⚠⚠ THE ROCK KINDS THE DESIGN NAMES DO NOT EXIST —
                   no granite, no sand rock, no volcanic.  What the
                   palette has is a SLOPE ladder (sand 2, grass 6, hill
                   12, rock 20, steep_rock 40) read by nothing since
                   plan 01, exactly as `drop` was before C5; an angle
                   of repose IS how well a material holds itself up.
                   ⚠⚠ THE FOOTING IS THE GROUND AROUND THE WALL, NOT
                   UNDER IT, AND PERSISTENCE DECIDED IT.  *Under it*
                   was overwritten by the paint (§ What the hex becomes)
                   and `MapFile` cannot remember it; a runtime layer
                   cannot either, because a wall's strength has to be
                   recoverable from the SAVED world or the same wall
                   has two strengths across a reload.  That leaves the
                   wall's own kind or its surroundings, and the crew do
                   not haul — so the stone comes from where they stand.
                   ⚠ THE STURDIEST IN REACH WINS, FOR MONOTONICITY: a
                   mean or a minimum would make a standing wall get
                   WEAKER when the wall beside it broke, because a
                   break paints BROKEN_GROUND where a wall used to be.
                   A maximum cannot fall.  ⚠ The other end is real and
                   recorded: a break RAISES its neighbours' footing, so
                   a brittle perimeter stops being brittle where it has
                   been breached — a self-limiting unzip.
                   ⚠ The band is CHOSEN: 2.0 priced against the TRENCH
                   (@M059 — the best wall and a trench cost the same
                   and buy the same clock), 0.5 against one regular's
                   100 s.  The palette's top three all reach the
                   ceiling deliberately (@X278's saturation).
                   ⚠ 153 / 174 / 220 for the same wall on sand, grass
                   and rock (@M061) — and ONE HEX of sand changes
                   nothing, so brittleness is a property of a REGION.
                   ⚠ It stores damage TAKEN, not HP remaining, for the
                   reason `height.loft` stores a RISE: a miss has to
                   mean something useful, and "HP remaining" reads as
                   ALREADY BROKEN on a sparse map.
                   ⚠ A break is TWO effects and `break_structure` is
                   the ONE site that does both: the wall is REMOVED
                   (repainted to `BROKEN_GROUND`, and that edits the
                   painted world — a broken wall really is gone) and a
                   heap of masonry is DEPOSITED (runtime, clearable).
                   Never ERASE the hex: the painted layer is
                   sea-default, so an erased breach is less passable
                   than the wall it replaced.
                   ⚠ Max HP is keyed on the palette NAME, not the
                   index — an index is storage, a name is what a
                   modder edits around.
                   B3 added `brace_of` / `brace_name` / `brace_factor`:
                   `structure_max_hp` is `structure_base_hp` times how
                   the structures AROUND a hex hold it up.  ⚠ It is
                   computed from the world and never stored, which is
                   what makes a perimeter UNZIP from a breach.
                   ⚠ `structure_breakable` asks the BASE figure and has
                   to — `brace_of` asks it of all six neighbours, so
                   routing it through `structure_max_hp` is an infinite
                   recursion.
                   ⚠ **Only a ROW is straight** — two neighbours brace
                   along one line when their direction indices differ
                   by exactly 3, and odd-r row parity means a
                   constant-`q` COLUMN zigzags and reads as braced

  tower.loft       what a TOWER is (plan 12 B5a + B5b) — the numbers
                   (range 15 hex, 1.0 s interval, 10 HP a shot, 6.0 m
                   tall, 30 shots), TowerState + tower_charge /
                   tower_bank / tower_hold / tower_shots /
                   tower_spend_shot / tower_black / tower_budget_left /
                   count, tower_in_range, and B5b's tower_eye /
                   tower_sees / tower_sight_fault.
                   ⚠ Range is `lat_distance` and
                   NOTHING else; a `+ 1` on a q or an r reaching for it
                   is moros#10 again.
                   ⚠ A tower BANKS charge rather than firing per tick,
                   because a 1.0 s interval is 1.5 ticks and B5b has to
                   COUNT shots against a 30-shot budget.  A shot
                   SUBTRACTS an interval, never resets — and the
                   comparison needs `TOWER_CHARGE_EPSILON`, because
                   `1/1.5` has no exact float form and a bare `>=`
                   silently drops every third shot.
                   ⚠ **LOS is ONE straight line and no table.**
                   `tower_sees` runs `lat_line` from the eye (the
                   tower's hex plus 6.0 m) to the target's top and
                   refuses any hex whose `hex_height` rises above the
                   ray.  Both ENDPOINTS are skipped — a tower on a wall
                   is not blinded by its own hex — and the comparison
                   needs `TOWER_SIGHT_EPSILON`, because the canonical
                   geometry lands exactly ON the boundary.
                   ⚠ **Do not add a "what blocks" lookup.**  A
                   `wall_high` beside the tower does NOT block and a
                   `wall` near the target DOES; the kind never decides
                   on its own.  `tests/12_b5b_los_budget.loft` § The
                   difference is the HEIGHT fails both ways round if
                   anyone tries.
                   ⚠ Shots FIRED, never shots remaining — zero is the
                   neutral value, so a tower nobody ticked is ready
                   rather than black (the same choice `damage.loft`
                   makes).
                   ⚠ `tower_pick` and `wave_fire` are in `spawn.loft`,
                   where `WaveState` is: this file must not depend on
                   the wave engine, because the tick calls INTO it

  vehicle.loft     the PLAYER (plan 13 V1) — VEHICLE_SPEED_HEX_PER_SECOND
                   (3.0, twice an enemy), Vehicle + vehicle_empty /
                   _place / _drive / _present / _arrived /
                   _hexes_per_tick / vehicle_tick.
                   ⚠ **Two hexes a tick, and it is a RATE** — the tick
                   is DEFINED by an enemy's speed, so "one hex per
                   tick" is what every other mover does and would
                   silently halve the player.
                   ⚠ **No passability code here.**  A hover unit's
                   climb is its clearance (`CLIMB_VEHICLE`, 0.4 m) and
                   the player is a third KIND in `passable.loft`.
                   ⚠⚠ THE CHASSIS FLOATS (BACKLOG C10, @D006, @X286):
                   `drive_along` asks `can_hover`, which reads
                   `walk_vehicle` rather than `walk_ground`.  The two
                   columns differ for the four WATER kinds and nothing
                   else, so the vehicle's whole difference from a robot
                   is that it floats on water — and the HEIGHT step
                   still decides everything else: a 3 m wall stops it,
                   a 3.0 m boost clears it.  ⚠ A trench takes it (a
                   drop is free) and the climb OUT is what 0.4 m has
                   not and a boost has.
                   ⚠ **It DRIVES, never routes** — `lat_line` to where
                   it is pointed, stopping at the first refused step.
                   A flow field would be the machine choosing the way,
                   which `DESIGN.md` § 11 rejects.
                   ⚠ `vehicle_tick` takes the tick's DURATION as a
                   parameter: `TICK_SECONDS` is in `spawn.loft`, which
                   `use`s this file.
                   V2 added `vehicle_salvage` — one dead robot a second
                   (`VEHICLE_SALVAGE_METRES_PER_SECOND`, derived from
                   `BODY_HEIGHT_METRES`), no key pressed, because
                   clearing is a POSITION.
                   ⚠ **Reach is 1 and it is FORCED**: a hover unit
                   climbs 0.4 m and the ramp that beats a tower is
                   1.5 m, so it CANNOT stand on what it must clear.
                   ⚠ ONE heap a tick, the deepest in reach — clearing
                   is meant to take time you do not have.
                   ⚠ It takes a BITE via `height_raise`, never
                   `height_clear` (which still has no caller).
                   ⚠ Since plan 14 H2 the rule itself is `salvage_at`,
                   which takes a HEX — the second half of the shared
                   chassis, after H1's `drive_along` — so the player
                   and every helper clear one implementation.  There is
                   no helper salvage RATE: `numbers.json` § helper has
                   none, and a second constant would be a tunable the
                   plan invented.
                   ⚠⚠ SPOIL IS NOT SALVAGE (BACKLOG C9, @M059).
                   `collectable` refuses `RUBBLE_SPOIL` and the search
                   skips to the NEXT-deepest heap rather than giving
                   up.  The reason is measured: the bite is
                   `min(rate * dt, pile)`, so a clearer takes the WHOLE
                   pile whenever the pile is smaller than one bite, and
                   a crew member clears FIFTY times faster than a
                   besieger shovels — one helper in reach would hold a
                   trench open for ever at any fill rate the design can
                   tolerate.  ⚠ The counter-play is the kill zone
                   instead (@M060), not the spade.  ⚠ A pile is named
                   by its NEWEST deposit, so a robot killed on a filled
                   trench renames the heap `wreckage` and the player
                   may collect it — which re-opens the trench and pays
                   for it.
                   V4 added boost: `vehicle_boost` / _boosting /
                   _boost_ready / _climb / _speed, four hexes a tick and
                   a 3.0 m climb for 2 s, then 5 s of cooldown armed as
                   the boost EXPIRES.
                   ⚠ **Boost is not a movement mode** — it is the same
                   height rule with a bigger number, and 3.0 m is
                   EXACTLY a `wall`, so a `wall_high` still refuses it.
                   ⚠ It calls `can_climb`, never `can_step`: the climb
                   is the vehicle's STATE, not its class.
                   ⚠ `VEHICLE_TIMER_EPSILON` — 2.0 s over a 1/1.5 s
                   tick is three ticks that sum to 1.9999999999999998,
                   so a bare `> 0.0` gives a FOURTH tick of boost
  helper.loft      the NPC crew (plan 14 H1) — HELPER_SPEED_HEX_PER_
                   SECOND (2.5), _HP, _ROSTER_START / _CAP, Helper +
                   helper_new / _drive / _arrived / _hp / _hurt /
                   _bank / helper_tick.
                   ⚠ **The first mover whose speed does not fit the
                   tick**: 2.5 hex/s is 1.667 hexes, so it BANKS
                   progress and steps the whole hexes out — the pattern
                   `tower.loft` and plan 13 V4 already use, arriving a
                   third time and LOCAL to the mover that needs it.
                   ⚠ **This is NOT "the tick becomes a timestep"** —
                   that warning is about a SHORTER tick, and F8's
                   budget trigger does not fire.
                   ⚠ `HELPER_PROGRESS_EPSILON` was worth 6.7% of the
                   speed, compounding: without it the carry sat on
                   0.99999999999999956 and a hex was deferred for ever.
                   The gate is the 1-2-2 step PATTERN, because both
                   wrong versions still arrive.  ⚠ Plan 26 L2 DELETED
                   the epsilon — the bank is integer now, so the fifth
                   hex in three ticks is `10 000 000 / 3 000 000` and
                   needs nobody's permission.  The pattern gate stays.
                   H2 added `helper_salvage` — the player's clearing
                   rule, on the player's chassis, done by an NPC — and
                   a crew turn in `wave_tick` that earns into the RUN's
                   wallet.  ⚠ It adds no mechanic at all, which is what
                   makes it a gate on the ROSTER rather than on a job.
                   ⚠ **The gate is a RATE**: a crew that is not in the
                   tick and a crew sharing ONE vehicle's bite both
                   empty the heap and both read exactly like one
                   helper, so "the rubble is gone" cannot see either.
                   H3 added `helper_wreck` / `helper_wrecked` — and
                   `alive` IS the wreck, because every verb already
                   asks it, so a downed crew member stops driving,
                   clearing and BLOCKING at once.
                   ⚠ **Nothing puts it back BY ITSELF**, and that is the
                   one rule where a helper is not the player's chassis
                   doing the player's job: `vehicle_respawn` is three
                   lines away in the tick and reads the opposite way.
                   The roster slot is KEPT (never compacted), which is
                   what retrieval needed.
                   Plan 15 C2 added the way back: HELPER_RECOVERY_
                   SECONDS (60.0), HELPER_TIMER_EPSILON, `helper_
                   recovering` / `_lost` / `_begin_recovery` / `_recover_
                   tick`.
                   ⚠ **60.0 s is EXACTLY 90 ticks and a bare `> 0.0`
                   gives 91** — the epsilon trap's FOURTH appearance and
                   its least visible.  The discriminator inverts the
                   intuition: the 5.0 s boost cooldown is 7.5 ticks and
                   is IMMUNE, so the trap fires only where the timer
                   divides the tick exactly — the case that looks
                   safest.  ⚠ And the two conditions over the timer must
                   AGREE: an epsilon in `helper_recovering` but not in
                   the exit test stalls the clock for ever.
                   ⚠ `helper_wrecked` is true during RECOVERY too (a
                   crew member at the core is not driving, clearing,
                   earning or blocking either); `helper_lost` is the one
                   that means *still out there and needs fetching*
  carry.loft       what a vehicle is HOLDING (plan 15 C1) — CARGO_WRECK
                   / CARGO_NONE / CARGO_GONE, CarryObject + CargoLayer +
                   cargo_empty / _spawn / _count / _slots / _held_by /
                   _carrying / _on_ground_near / _get / _owner / _take /
                   _put / _consume / _spill / _follow / _owned_by /
                   _slot_fault.
                   ⚠ **NOT a hash keyed by hex**, and it is the only
                   runtime layer that is not: two carry objects on one
                   hex is REACHABLE (a helper carrying a downed
                   colleague is itself destroyed), and a hash answers
                   with one of them while the other is a crew member
                   deleted with no fault raised.  A vector with stable
                   slots, never compacted — `WaveState.crew`'s shape.
                   ⚠ **Conservation is STRUCTURAL, not maintained**: ONE
                   record with an `owner` field, where "on the ground"
                   is a VALUE of that field rather than a different
                   place to be.  A pickup is a single assignment, so
                   duplication is unrepresentable — the move
                   `damage.loft` makes with *damage TAKEN* and
                   `wallet.loft` with *points SPENT*.  A slot on the
                   carrier PLUS a ground layer makes a pickup two
                   writes, and every path doing one of them duplicates
                   or destroys.
                   ⚠ Owner ids are `occupancy.loft`'s BLOCKER
                   vocabulary — `BLOCKER_NONE` (-1) IS the ground — and
                   never a second numbering, which is the door H3
                   deleted `vehicle_on` for.
                   ⚠ **A KIND is data, not a code path** (the enemy
                   rule): what varies per kind is only what a valid
                   destination is and what arriving there does.  A
                   tower-top or a beacon that needs new CARRYING code
                   has broken the contract in `plans/15` § C0.4.
                   ⚠ `cargo_consume` is the ONE way out of the world;
                   a carrier that DIES calls `cargo_spill` instead, or
                   dying becomes a free retrieval.
                   C2 added the destination half: CARGO_REACH_HEXES,
                   `cargo_destination_ok` (a wreck goes to the CORE and
                   nowhere else) and `cargo_deliver`.
                   ⚠ ONE reach for both halves, because § 11's key is
                   ONE key — two reaches would make it mean two
                   distances depending on what the vehicle happens to
                   hold.
                   ⚠ An unknown kind has NO destination rather than
                   every destination, or a kind added without a rule
                   would be depositable anywhere and consumed silently.
                   ⚠ `cargo_deliver` does NOT apply the effect: what
                   arriving DOES needs the roster, and a carry model
                   that knew about helpers could not serve tower-tops.
                   `spawn.loft::wave_arrived` is the other half
  part.loft        what an entity IS, structurally (plan 20 A1) — Socket +
                   Binding + Part + PartSet over `hex_body::Rig`, with
                   part_new / part_socket / part_bind / partset_new /
                   partset_add / partset_get / socket_index /
                   socket_world / socket_fault / bind_fault /
                   part_cycle_fault / part_fault.
                   ⚠⚠ **It contains no rig, no joint, no limit, no pose
                   and no hitbox** — every one of those is `hex_body`,
                   published, and `PARTS.md` § D1 is the decision to
                   consume it (`part_fault` asks `rig_admissible` before
                   any question of its own).  ⚠ The gate for that is a
                   NEGATIVE one and it is the `use` line, never the
                   manifest: dropping `hex_body` from `loft.toml` AND
                   from `loft.lock` leaves all 18 tests green
                   ([loft#968]), while deleting `use hex_body;` answers
                   `Undefined type Rig`.
                   ⚠⚠ **A socket's position is COMPUTED and stored
                   nowhere** — `socket_world` is the ONE site, and the
                   absence of a coordinate on `Binding` is what makes
                   moving a part move everything in its sockets.
                   ⚠ **A socket carries no POSE either**, where
                   `hex_part::Binding` carries `bd_open`: a dryopea part
                   is a catalogue asset and its angles come from the
                   simulation (§ D3, plan 20 A4).
                   ⚠ The cycle check walks a PATH, never a visited set —
                   a diamond (one part in two sockets) is legal, and the
                   diamond test is the only one that can see the
                   difference.
                   ⚠ The first file in the repo to opt into
                   `docs/EXAMPLES.md` with `// #examples`, so all twelve
                   public functions cite a test (`@DRY-016`..`@DRY-027`).
  catalogue.loft   what each entity is MADE of (plan 20 A3) — cat_hover_unit
                   / cat_helper / cat_robot / cat_tower_base /
                   cat_tower_top, plus catalogue / catalogue_names.
                   ⚠⚠ **It declares no footprint.**  `part_size` DERIVES
                   the extent from the limb table, and that is the only
                   reason PARTS.md § D6's check against VEHICLE_WIDTH_M
                   is not a tautology: the vehicle's 2.28 x 2.05 falls
                   out of where § D7 puts the four rotors.
                   ⚠ The helper IS the hover unit (§ D8), never a fifth
                   entry; and a robot class is a row of DATA, so no
                   colour lives here.
                   ⚠ § D7's four BOOMS are absent: they run diagonally
                   and neither Limb nor hex_body::Rig carries a rest
                   orientation.  A2's to solve — the same gap puts the
                   tower's socket on a zero-length bone at an offset
                   rather than the tip of a 6 m one.
                   ⚠ The constructors are `cat_*` because loft's
                   namespace is FLAT and a bare `robot` collided with a
                   local in a dozen test files.
  part_mesh.loft   A PART, AS TRIANGLES (plan 20 A2) — PART_ROUND_
                   SEGMENTS, emit_box / emit_disc / emit_cone,
                   part_emit / part_emit_at / part_values_rest.
                   ⚠⚠ **No forward kinematics**: every vertex goes
                   through `hex_body::frame_point` over ONE
                   `rig_world_frame3` per bone, held across that bone's
                   limbs.  `rig_world_frame3` was added UPSTREAM
                   (hex_body 0.3.0) rather than re-derived here.
                   ⚠ A NORMAL is a DIRECTION — posing one as a point
                   adds the bone's translation and still normalises to
                   something plausible.
                   ⚠ A box gives each face its own four corners (24
                   vertices, not 8) so the normals stay flat.
                   ⚠ `part_emit_facing` (plan 20 A5) TURNS about the
                   world's `+z` and then MOVES, and `part_emit_at` is
                   it at yaw 0.  ⚠⚠ `frame_place` rotates the BASIS as
                   well as the origin, and forgetting it is silent on
                   this catalogue — at rest every bone's basis is the
                   identity, so only a joint off zero can see it (the
                   canopy's LATERAL axis is the one that does not
                   commute with a yaw).
                   ⚠⚠ And `emit_box`'s three NEGATIVE faces used the
                   same in-plane pair as their positive partners until
                   A5, so half of every box was wound INWARDS — no
                   count, vertex, normal or `mesh_crc` could see it
                   (@D005).  Scaling `u` by the sign does NOT fix it:
                   `us` is derived from `u`, so both flip.
                   ⚠ A socketed child follows the socket's POSITION and
                   not its orientation, because `hex_body` publishes no
                   way to compose two `Frame`s.  Every socket in the
                   catalogue rides an unrotated bone, so today the two
                   are the same picture to the bit; the fix, when a
                   socket turns, is `frame_compose` upstream.
                   ⚠ `frame_shift` builds a Frame LITERAL with all
                   twelve fields, because a returned struct is a COPY
                   ([loft#894]) and an omitted field takes its zero
                   ([loft#914]).
  pose.loft        THE POSE COMES FROM THE SIMULATION (plan 20 A4) —
                   ROTOR_IDLE_TURNS_PER_S, rotor_turns_per_second /
                   rotor_phase / rotor_phase_at / canopy_turns /
                   pose_hover_unit / pose_vehicle / pose_crew /
                   emit_tower / emit_tower_base / emit_tower_top.
                   ⚠ The tower's base and top are SEPARATE doors since
                   plan 20 A5, because they are different COLOURS and
                   the shader carries one uniform colour per draw
                   (@X074) — `emit_tower` still composes them, so A4's
                   gate reads the same tower it always did.
                   ⚠⚠ **Three claims, three owners, and none of them is
                   new state**: the tower's top is `tower_has_top`
                   (plan 17 T2), the canopy is `cargo_carrying` (plan
                   15), the rotors' rate is `vehicle_speed` (plan 13).
                   A field on `Vehicle` saying *canopy open* is the
                   defect PARTS.md § D3 is written against.
                   ⚠ The simulation answers a TARGET and the SWING is
                   presentation: `pose_hover_unit` takes the angle as a
                   FLOAT so a renderer can ease it, because a binary
                   door would re-introduce the two-pose canopy § D4
                   retired.
                   ⚠ The boost rate is DERIVED from `vehicle_speed`'s
                   own ratio (`@X091`), and the IDLE rate is bounded by
                   the FRAME RATE: four-fold blades reverse beyond an
                   eighth of a turn per frame, 3.75 turns/s at 30 fps.
                   ⚠ The rotor PHASE is `rate x t` read fresh, so a
                   change of rate moves it — integrating needs an
                   accumulator on the Vehicle, which plan 20 refuses.
                   ⚠⚠ `emit_tower` indexes `ps.ps_parts[...]` rather
                   than calling `partset_get`, and that is a WORKAROUND
                   ([loft#974]): a record fetched through an accessor
                   reads its vector fields EMPTY after an unrelated
                   allocation, so the SECOND tower drawn has no limbs.
  wallet.loft      the run's budget, and the only END STATE dryopea
                   has (plan 12 B6) — WALLET_STARTING_POINTS (200),
                   NIBBLE_POINTS_PER_SECOND, NIBBLE_REACH_HEXES,
                   Wallet + wallet_new / wallet_left / wallet_spent /
                   wallet_drain / wallet_broke, and nibble_in_reach.
                   ⚠ **The core is invulnerable** (`numbers.json` §
                   core.hp is `null`), so "the heart is destroyed" is
                   spelled the WALLET reaches zero.
                   ⚠ **Reach is a straight-line `lat_distance` of 1**,
                   derived from `core.footprint_layout` — the core is a
                   radius-1 disc, so an enemy within one hex is
                   standing ON it.  Draining for every live enemy
                   passes every rate-and-floor assertion while making
                   walls and towers pointless; that is what
                   `tests/12_b6_wallet.loft`'s perimeter test refuses.
                   ⚠ Points SPENT, never points left — the same
                   zero-is-neutral rule `damage.loft` and `tower.loft`
                   keep, and here it stops a `Wallet {}` literal from
                   starting the run already over.
                   ⚠ The clamp is on the WRITE, not just the read: a
                   ledger allowed past the budget would swallow the
                   first loot credit whole.
                   ⚠ `wallet_earn` (plan 13 V3) is the ONLY income, and
                   "the wallet never refills unattended" still holds —
                   because its only caller needs a VEHICLE, not because
                   the verb is missing.
                   ⚠ `loot_rate` reads the rubble SOURCE: wreckage and
                   carapace pay, MASONRY pays nothing, or demolishing
                   your own wall would be an income stream.
                   V5 added blocker damage — `vehicle_hp` / _hurt /
                   _respawn plus `VEHICLE_HP_BLOCKER` — and
                   `spawn.loft::enemy_blocked_by` is the rule.
                   ⚠ `vehicle_on` is DELETED (plan 14 H3): "who is
                   standing on this hex" is the whole crew's question
                   now, and `occupancy.loft`'s `BlockerMap` is its one
                   door — a per-vehicle predicate beside it is the one
                   a future caller would reach for.
                   ⚠ **Blocking is a property of the MAP**: an enemy
                   with a sidestep goes round and nobody is hurt, so
                   the player is only a liability in a chokepoint.
                   ⚠ A COMPANION blocking the same hex is never
                   attacked — which is why this needed its own
                   predicate rather than reusing occupancy
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
                   rather than prefer it.  Built from lattice.loft's
                   neighbour relation only, which is what kept it
                   independent of plan 09's conversion.
                   `flow_steps` (F5c) is the same answer as a LIST —
                   every strictly-closer neighbour, best first — which
                   is what the mover reads so it can skip an occupied
                   one.  In a BFS field every entry is at `d - 1`, so
                   the ordering is direction order alone.
                   `flow_desire` (F7) is the SAME sweep with the climb
                   lifted (`FLOW_CLIMB_ANY`) — where an enemy wants to
                   go when it has no route.  One field for every class,
                   because the class only ever contributed its climb.
                   ⚠⚠ `sweep_ground` (BACKLOG C9, @X283) is what the
                   two sweeps differ in besides the climb, and it is
                   the desire field's rule said out loud: AN OBSTACLE
                   THE WAVE CAN REMOVE IS PASSABLE IN IT.  A wall
                   always was — its top is walkable, so lifting the
                   climb was enough — and a TRENCH is not a surface at
                   any height, so the NODE rule widens for it.
                   ⚠ Without it the siege cannot SEE a moat: an
                   unadmitted hex is never offered by `flow_steps`, so
                   `enemy_target` names the besieger's own hex and a
                   branch in `wave_damage` would be dead code.
                   ⚠ The SEA is not a moat (`moat_at` is false at drop
                   0), so § Three states' termination argument — every
                   passable hex is a painted one — is untouched.
                   ⚠ ROUTING passes `false` and is unchanged: a routing
                   field that crossed trenches would walk robots INTO
                   the water, because a metre is inside `climb_ok`
                   ⚠⚠ `flow_route` (plans/30 R2) is the whole descent as
                   a LIST — every hex from a start down to the core, in
                   order — and it exists because A LEG IS A PATH AND
                   NEVER A LINE (@M071): only 10 of 90 straight
                   crossings of the authored maps arrive.  ⚠ path[0] is
                   the start and the last entry is the core, so the
                   STEPS are len − 1 and equal `flow_distance`; a route
                   ending anywhere else is one that does not CONNECT,
                   and the caller is left to see that rather than handed
                   a short answer that looks whole
  height.loft      the RUBBLE layer — what runtime has piled on the map
                   (plan 11 F6, named by plan 12 B1).  HeightLayer +
                   height_raise (metres AND a source) / height_clear /
                   height_rise / height_piled / height_source / count,
                   plus RUBBLE_WRECKAGE / _CARAPACE / _MASONRY /
                   _CARGO / _SPOIL.  ⚠ SPOIL is the one nothing died to
                   make — what a besieger shovels into a trench
                   (BACKLOG C9) — so `loot_rate` pays 0 for it and
                   `salvage_at` refuses to pick it up at all.
                   A sparse map of metres ADDED to what the palette
                   paints, so a pile on grass and a pile on a wall are
                   one arithmetic.  ⚠ It ACCUMULATES (bodies do) and a
                   negative rise floors at the ground.
                   ⚠ **An entry means a PILE**: shrinking one to nothing
                   REMOVES it, exactly as `painted.loft` removes a hex
                   painted back to sea.  A zeroed-but-present entry
                   would leave a hex standing on debris that is not
                   there — over water that is a hole in the sea.
                   ⚠ The source is one per hex and the NEWEST deposit
                   names the pile; a withdrawal leaves the name alone.
                   Nothing reads it back yet — B2 and B4 are its three
                   producers.  Runtime state: it rides on `WaveState`
                   and never reaches a save
  occupancy.loft   who is standing where, this tick (plan 11 F5c) —
                   Occupancy + enter / leave / taken / count / stacked.
                   A COUNT per hex, not a boolean set: a wave spawns
                   stacked, so one of a pair stepping off must not free
                   the hex.  ⚠ It is not passability (that is the
                   GROUND, per class) and never a target — a companion
                   blocks a step and is never attacked for it.
                   Plan 14 H3 added the OTHER map: BlockerMap +
                   blocker_empty / _set / _at / _taken / _count /
                   _crew_index, built by `spawn.loft::wave_blockers`.
                   ⚠ **A second map rather than a second count**, and
                   the asymmetry is why: an enemy steps BESIDE a
                   companion and ATTACKS a vehicle, so one structure
                   would be read with a "but which kind?" everywhere.
                   ⚠ It answers WHO (`BLOCKER_PLAYER` is 0, helper `i`
                   is `BLOCKER_CREW + i`, nobody is -1), because with a
                   roster the damage must land on the vehicle that is
                   actually in the way.
                   ⚠ A WRECK is not in it — a downed helper blocks
                   nothing, or the first crew member to die in a
                   corridor would be a free wall with no HP left
  passable.loft    may a class of enemy make this move? (plan 11 F1 +
                   ⚠⚠ TWO DOORS, ONE RULE (BACKLOG C10, @X286):
                   `can_travel` is the rule; `can_climb` asks it
                   for something that WALKS and `can_hover` for
                   something that FLOATS, and `hex_walkable` /
                   `hex_hoverable` are the two surface columns.
                   Plan 11 F1 forbids a second TRAVERSAL, not a
                   second door — two independent rules is how a
                   mover and a field come to disagree about what
                   ground is.  ⚠ Only `vehicle.loft::drive_along`
                   hovers; every field and every enemy walks.
                   F6) — the enemy KIND discriminants + climb_limit()
                   + hex_height() + can_stand() / can_step() /
                   can_occupy(), each with a `*_fault` twin that names
                   the numbers.
                   ⚠ Since plan 21 R2 it also owns the SIGHT line —
                   `sight_first_block` + SIGHT_EPSILON.  `can_step`
                   asks what a mover may cross; this asks what
                   reaches above a line drawn OVER it, and both are
                   questions about `hex_height`.
                   ⚠ It answers WHERE (the blocking cell's index),
                   never whether: `tower_sees` is the boolean door
                   and `camera_boom_free` the distance one.  A
                   predicate cannot say how far to pull a boom in.
                   ⚠ It walks BOTH endpoints, which `tower_sees`
                   used to skip — a looker's eye sits on top of its
                   own hex, so the skip only restated the heights.  TWO questions: is the SURFACE one this
                   class stands on (`walk_ground`), and is the STEP
                   within its climb.
                   ⚠ Since plan 12 B1 every surface question takes the
                   HeightLayer, because a piled hex's surface is
                   `rubble` and not what the map paints.  `hex_height`
                   deliberately does NOT — it reads `painted_ground`
                   and adds the layer, which is what makes a pile on a
                   wall the wall PLUS the pile.
                   ⚠ `walk_ground` alone is the BUG — `wall` and
                   `wall_high` are walk_ground=true (a wall's walkable
                   part is its TOP), so the one-field predicate walks
                   robots through 3 m walls.
                   ⚠ `can_step` is the rule (an EDGE, asymmetric — a
                   drop is free); `can_occupy` is what a POSITION can
                   honestly say with no history — "some neighbour
                   offers a legal step in", i.e. height minus the
                   LOWEST standable neighbour.  It is the measurement's
                   rule and must never be the field's node filter
  picker.loft      Picker { palette, active }
                   + picker_default(), picker_set_active(),
                   render_picker(cv, p, x0, y0) — Canvas-painted UI
  render.loft      software rasterizer using graphics::Canvas
                   + render_to_canvas, render_with_hover, palette_color,
                   draw_hex, draw_hex_outline,
                   world_to_canvas, screen_to_world, screen_to_hex.
                   ⚠ Draws from `lattice.loft` since plan 09 C3 —
                   pointy-top, so a hex is TALLER than it is wide.
                   `draw_hex` reads `lat_corner_offset` rather than
                   carrying a vertex table, so the hexagon drawn IS
                   the lattice's; there is no y-flip here, because
                   the one sign inversion lives in the metre
                   conversion
  build.loft       THE BUILD ORDER (plan 27) — BuildOrder { q, r, what,
                   kind, spent } in a sparse BuildLayer on WaveState,
                   plus order_place / order_fault / order_erase /
                   order_spend / build_at / build_resolve.  The ONLY way
                   a structure comes into existence during a run.
                   ⚠ Work is stored as SPENT, never LEFT, in INTEGER
                   base units.  [loft#914] is why the first: a partial
                   literal must read as *nobody has started*, not as
                   *already finished*.  @M049 is why the second: a float
                   rate accumulated per tick made a wall_high take 31
                   ticks against a true 30, which is @D003/@D004's
                   family in a new mechanic.
                   ⚠⚠ THE TOTAL IS A DURATION DERIVED FROM THE KIND —
                   10 s a wall, 20 s a wall_high, 30 s a tower — and it
                   is FLAT rather than `structure_max_hp`, which would
                   also handle bracing and would build a lone stub in
                   1.5 s (@M048).  A wall is cheaper to BREAK at an end;
                   it is not cheaper to BUILD there.
                   ⚠⚠ `fixstep::Timer` was REFUSED on semantics: it
                   fires once and DISARMS, so a completed timer reports
                   zero progress — and a build order is spent during the
                   tick and raised at the consequence stage, so it must
                   stay readable after it completes.  A one-shot that
                   forgets and a progress bar that must not are two
                   families.
                   ⚠ `order_place` asks `order_fault` itself, so an
                   illegal order cannot be placed by forgetting to
                   check — `maps.loft::map_fault`'s shape.  `buildable`
                   is the palette flag it finally reads, carried since
                   plan 01 and consumed by nothing until now.
                   ⚠⚠ ONE QUEUE, TWO THINGS TO BUILD (@X273): `what` is
                   BUILD_GROUND or BUILD_TOWER, and it is read at
                   exactly ONE site — `build_resolve`, where a tower
                   places a MARKER and a wall PAINTS.  Delete that
                   branch and a tower silently paints SEA.
                   ⚠⚠ A TOWER ORDER REFUSES ERASURE (@X274), and a
                   falsifying probe found that rather than the design:
                   the 100 points go at PICKUP, and the wall trail
                   erases any order it drives over — so without it,
                   driving over your own tower site destroys a beacon
                   you paid for, silently.
                   ⚠ `build_at` picks the MOST ADVANCED order in reach,
                   so a crew converges: a half-built wall stops nothing,
                   and it is what makes *N helpers are N times as fast*
                   observable at all.
                   ⚠ The marker check is NOT here — *is there already a
                   marker on this hex* needs the marker world, so it
                   lives in `spawn.loft::wave_drop`, which is the split
                   `cargo_destination_ok` makes for a tower TOP
  persist.loft     A PLANET — a place that REMEMBERS (BACKLOG B3) —
                   PLANET_ROOT / PLANET_ARG / planet_arg / planet_dir /
                   persist_fault / planet_open / planet_save.
                   `dryopea_planets/<planet>/<player>/world.json`,
                   gitignored; `make play PLANET=<name>` opens it.
                   ⚠⚠ THE PATHS ARE DERIVED FROM THE KEY, NEVER PASSED —
                   a caller that could pass paths could point a session
                   at another player's world, and @X188 is a statement
                   about who can see what.
                   ⚠ The PLAYER is in the path from day one though
                   dryopea has exactly one: a shared planet cannot be
                   retrofitted into a path with no room for a name.
                   ⚠⚠ A PLANET IS THE GROUND AND THE MARKERS AND NOT A
                   RUN — the pair a MAP holds.  A wall the crew RAISED
                   survives because it is ground; the order that made
                   it, the roster and the wallet die with the session.
                   A planet is a place, not a save game.
                   ⚠ It goes through `maps.loft`'s slot naming and
                   `save.loft`'s writer, so there is no second save
                   format and no second loader to drift.
                   ⚠⚠ JSON RATHER THAN THE MMAP `Store`, ON MEASURED
                   GROUNDS (@M052).  `store_persist_bind` shipped and
                   round-trips across processes with no save call — but
                   dryopea's world is a FIELD of `EditorState`, so a
                   bind writes the CONTAINER's store: loft advises
                   `persist-bind-through-field`, the undo history rides
                   along, and the on-disk layout becomes the editor's
                   working struct's, which any new field silently
                   invalidates.  Taking it needs `EditorState`
                   restructured so the world lives in a store-owning
                   container.  ROADMAP's "one-line annotation" is
                   falsified
  errand.loft      A ROBOT GOING ABOUT ITS BUSINESS (BACKLOG B4) —
                   TRAFFIC_RATE_DEFAULT, Traffic { rate, bank },
                   traffic_set / traffic_running / traffic_due,
                   errand_done / errand_depart.
                   ⚠⚠ NO MOVER AND NO SECOND AI, which is DESIGN.md
                   § 10's rule and ROBOT_ECONOMY.md § The governing
                   rule stating it again.  `enemy_walk_heading` has
                   walked a robot along its business since plan 11 F5b,
                   and SETTING.md says a spawn marker's direction IS
                   what they were going before.
                   ⚠⚠ WHAT WAS MISSING WAS AT BOTH ENDS: a robot's
                   business never ENDED — walk into a cliff and it
                   stood there for the rest of the run, so traffic
                   would silt the map up rather than cross it — and
                   nothing ever STARTED one the wave schedule had not.
                   ⚠⚠ THE BUBBLE TAKES THE ERRAND, ONE WAY.  Cleared in
                   `spawn.loft::wave_cutoff`, which sweeps the roster
                   before anybody moves — the same position a mover
                   would test for itself, so the two cannot disagree
                   about which robots are lost; walking back out does
                   not restore a signal the core is still jamming.
                   ⚠ It was one line of `enemy_step` until BACKLOG C3,
                   which needed the JAMMER to gate it — and the jammer
                   is on WaveState, where a mover cannot reach it.
                   ⚠ DEPARTING IS NOT DYING — no body, no salvage, no
                   payment.  A robot that walked on was never the
                   player's to kill, and routing it through
                   `wave_deaths` would pay a wallet for traffic nobody
                   touched.
                   ⚠ A Bank rather than a timer: a rate banked in
                   integer base units releases whole robots exactly at
                   every tick length (@M049 is the same mistake one
                   system over).
                   ⚠⚠ `Enemy.errand` and the rate both default to OFF,
                   so [loft#914]'s silent default lands on the way the
                   game already worked — which is why 679 gate
                   measurements did not move.
                   ⚠⚠ AND SINCE plans/30 R1 IT ALSO OWNS THE ROUTINE —
                   ROLE_* / ANCHOR_* / ERRAND_BAG_*, GUARD_LEG_UNITS,
                   Role { name, empty, laden, period, shift },
                   errand_row /
                   errand_roles / errand_role_of / errand_role_named,
                   Errand { role, home, work, alt, carry, slip } on
                   Enemy, errand_new / errand_at / errand_same,
                   errand_role / errand_anchor / errand_leg /
                   errand_destination_of / errand_destination /
                   errand_arrive, errand_row_fault /
                   errand_table_fault.
                   ⚠⚠ THE BAG STEERS AND NEVER A CLOCK (@FR-E-Bag-Steers,
                   @M073): `carry > 0 ? laden : empty`, so a round trip
                   closes at 4, 40 and 400 hexes, where a row one column
                   away with a period gets 13 hexes out and delivers
                   nothing for ever — ../crawler's measured defect,
                   reproduced rather than imagined.  GUARD is the one
                   role a clock may steer and it has no bag.
                   ⚠ THE RULE HAS TWO HALVES, NOT TWO IMPLEMENTATIONS:
                   errand_leg reads the bag, errand_arrive fills and
                   empties it, and neither is the rule alone.
                   ⚠⚠ A ROLE IS A ROW AND THE TABLE IS INDEXED, NEVER
                   COMPARED (@X333) — tests/30_r1_the_errand.loft sweeps
                   every src/*.loft and fails on a code line comparing a
                   ROLE_ constant, with ROLE_KIND_COUNT the one
                   exemption.  crawler has `role == 7` in eight places
                   and no compiler can refuse one.
                   ⚠ EVERY COLUMN IS READ THE MOMENT IT EXISTS —
                   errand_leg reads four and errand_done reads `shift`;
                   what draws a role off its route is R7's and is
                   deliberately not a column yet (@X112 from the other
                   end).
                   ⚠⚠ READ THE ROLE THROUGH `errand_role`, NEVER THROUGH
                   `route.role` — it answers ROLE_NONE for any robot
                   whose link wave_cutoff has cut, so the bubble's
                   one-way door has no second write to forget.
                   ⚠ THE BAG IS NOT carry.loft's LEDGER (@X334): that
                   file conserves an object ON THE MAP the player could
                   pick up instead; a bag holds material that was never
                   on the map.  R7 is where that stops being true.
                   ⚠ errand_destination answers a task.loft `Job`
                   (@X332) — the RECORD is shared, the SELECTION is not,
                   and `kind` stays TASK_ANY until R7 reads one.
                   ⚠⚠ AND SINCE plans/30 R2 IT OWNS THE CYCLE —
                   Cycle { anchors, lengths, path, period }, cycle_new /
                   cycle_running / cycle_walked / cycle_phase /
                   cycle_at / cycle_where / cycle_build / cycle_fault.
                   ⚠⚠ THE CLOSED FORM SPLITS IN TWO (@FR-E-Closed-Form,
                   @X335): TIME → STEPS is exact arithmetic, because a
                   Bank keeps its remainder — the hexes released by t
                   are floor(rate × t / BANK_WHOLE) however t was spent
                   — while STEPS → HEX is a PATH (@M071), so the round
                   is stored as the hexes themselves, one per step
                   offset, and cycle_at INDEXES it.
                   ⚠⚠ THE BANK DOES NOT RESTART AT A LEG BOUNDARY: how
                   far into this leg am I is walked(t) − walked(t₀) and
                   NEVER walked(t − t₀) — the two differ by a whole hex
                   whenever the carry is non-zero.  ⚠ And it is an
                   obligation on the MOVER too: a mob that walks past an
                   anchor spends its remaining hexes on the next leg,
                   and a DWELL is a LEG with a length, never a pause.
                   ⚠ The period is a count of HEXES when the bag steers
                   and a span of TIME when a clock does — one function
                   reading the same column errand_leg reads (@X329).
                   ⚠⚠ A CLOCK PERIOD MUST BE A WHOLE NUMBER OF TICKS and
                   cycle_fault refuses one that is not (@M074): a leg
                   boundary in TIME lands wherever the timestep puts it
                   and one in DISTANCE cannot — a second, independent
                   argument for @FR-E-Bag-Steers.
                   ⚠ GUARD_LEG_UNITS CANNOT SEE ITS OWN ROUNDING at any
                   shipped speed (10 s is 10, 15 and 25 whole hexes), so
                   the gate sweeps a 16-tick NEIGHBOUR — @M013, and a
                   change to that constant must keep the sweep.
                   ⚠ The round is a FLAT vector of hexes rather than a
                   vector<CycleLeg> holding a vector<Hex> each, because
                   [loft#974] reads a record's vector field as EMPTY
                   through an accessor.
                   ⚠⚠ AND SINCE plans/30 R3 IT OWNS THE MOVER —
                   cycle_hex_units, ErrandField { q, r, climb, field },
                   errand_fields, errand_cycling, errand_step.
                   ⚠⚠ errand_step IS THE ONE DOOR (@FR-E-One-Door):
                   nothing else may write a cycling mob's position, and
                   it owns `slip` — which collapses four of the twelve
                   re-assertion sites, because they cannot forget what
                   they cannot do.  `spawn.loft::enemy_move_to` is the
                   ONE WRITE underneath it and is public for that reason
                   alone; the nameable difference (@X329) is that the
                   write knows how a position is stored and nothing
                   about routines, and this one the reverse.
                   ⚠⚠ THE POSITION IS EXACT IN ITS PHASE AND ONLY
                   CONDITIONALLY IN ITS HEX (@X336): a mob whose first
                   choice is taken walks another route of the same
                   length and loses no time, so hex equality holds where
                   nothing can push a body and the field DISTANCE holds
                   everywhere.  @FR-E-Slip's "re-converges on the same
                   hex" is about the ANCHOR, not the way there.
                   ⚠⚠ A DWELL IS NOT A BLOCK (@X337) — cycle_phase
                   clamps a clock leg's offset, so a guard waiting at
                   its post loses nothing and must not slip; charging it
                   freezes the guard for ever WITH EVERY CONFORMANCE
                   COUNT GREEN, because a frozen rule agrees with a
                   frozen body.  Liveness is a second gate.
                   ⚠⚠ A LOST HEX COSTS BANK_WHOLE / rate AND THE
                   DIVISION MUST BE EXACT — @M074's family, one subject
                   over: rate × slip has to be a whole multiple of
                   BANK_WHOLE or the floor disagrees at some t.
                   cycle_fault refuses a rate without one; every shipped
                   speed divides.
                   ⚠ `now` IS THE MOMENT A STEP BEGINS, never the one it
                   ends at — WaveState.now is advanced at the END of
                   wave_tick, and the wrong order is invisible for every
                   role whose clock leaves it dwelling.
                   ⚠ Steps come from flow_steps then flow_sidesteps and
                   from nothing else (@FR-E-Non-Increasing): a sidestep
                   admitting a FURTHER hex silently breaks R5's bound.
                   ⚠⚠ errand_fields BUILDS ONE FIELD PER ANCHOR OF THE
                   ROW, never per DESTINATION (@D008): a mob that
                   reaches an anchor with hexes left turns and spends
                   them on the NEXT leg, and a vector built from the
                   tick's starting destinations has nothing for it to
                   descend — the hexes then go to `slip` and EVERY
                   equality agrees, because the rule reads now − slip
                   and follows the body down.  ⚠ Invisible at the
                   shipped robot's one-hex-a-tick; a SCOUT turns
                   mid-tick and drifted twelve hexes in three minutes.
                   ⚠⚠ AND SINCE plans/30 R4 A ROUND HAS AN ENDING —
                   Role.shift, GATHER_SHIFT_UNITS, errand_home_done, and
                   errand_depart at the TOP of wave_tick.
                   ⚠⚠ HOME IS A LEG OF THE ROUND, NOT A PLACE A FINISHED
                   MOB WALKS TO (@X338, @FR-E-Home-Is-A-Place): the
                   plan's invariant is three states and ONE exit, so a
                   mob breaking off its cycle would be a fourth state
                   and a second exit.  A mob leaves the roster the tick
                   its own cycle brings it home — the mover, the cycle
                   and the conformance gate are untouched.
                   ⚠ A SHIFT IS A SPAN, NOT A COUNT OF ROUNDS: a count
                   needs the cycle, now − slip is already there, and a
                   shift chooses no ANCHOR so it cannot reproduce @M073.
                   ⚠ A SHIFT NEEDS A BAG-STEERED ROW and
                   errand_row_fault refuses a clock-steered one: its
                   phase is a time where a turn point is a distance.
                   ⚠⚠ AND SINCE plans/30 R4b A ROUND MAY END SOMEWHERE
                   IT DOES NOT PASS (@X341) — Cycle.terminal,
                   cycle_turn, errand_terminal, errand_shift_over,
                   ROLE_HARVEST + HARVEST_SHIFT_UNITS.  A round whose
                   home is off it repeats until the shift and then walks
                   ONE terminal leg to the repair point: `anchors`,
                   `lengths` and `path` describe the whole walk and
                   `period` is the repeating PREFIX alone.
                   ⚠⚠ THE TURN IS A THIRD VALUE OF THE BAG
                   (ERRAND_BAG_HOMEWARD), and that is the decision.  The
                   closed form turns at T = ceil(S / period) × period,
                   but the MOVER has no cycle and cannot know `period` —
                   what it can see is the moment its bag empties at the
                   drop-off, and the first empty leg after the shift IS
                   T.  A separate `am I finished` field would be a clock
                   in all but name.
                   ⚠⚠ THE ENDING IS COMPARED IN HEXES AND NEVER IN TIME
                   (@M077): 12 of 192 swept cases disagree and the
                   failure is a WHOLE ROUND.  cycle_fault refuses a
                   shift that is not a whole number of hexes at the
                   mover's rate — @M074's family, a third subject.
                   ⚠⚠ THE MOVER COUNTS ITS OWN HEXES: the latch reads
                   walked(now − slip) PLUS the hexes released so far
                   this tick, and a version reading only the first is
                   short by up to the whole bank.  A regular robot
                   releases one hex a tick and cannot see it; a scout
                   can (@M078).
                   ⚠ `harvest` is a CATALOGUE ROW beside `haul` and not
                   a change to it (@X322): one mechanism, two rows of
                   data, and `haul` stays the route that does not end.
                   ⚠⚠ THE DEPARTURE IS AT THE TOP OF THE TICK, and that
                   is the claim rather than a tidy-up: at the consequence
                   stage a robot arrives at its nest and is removed in
                   the same tick, so the last frame holding it has it
                   ONE HEX SHORT.  What the player cannot see the gate
                   cannot see either.
                   ⚠ AND THE MOVER STOPS A FINISHED MOB at home, because
                   one releasing more than a hex a tick would step over
                   its own ending; the remaining hexes are DROPPED and
                   never slipped — it arrived, it was not held up.
                   ⚠ errand_bag_for is errand_leg's map read BACKWARDS
                   (plans/30 R6a): materialising a mob has to put
                   something in its hands, and the only true answer is
                   whatever agrees with the leg its own rule has it on.
                   ⚠ cycle_carry is cycle_walked's REMAINDER — what a
                   mover is carrying toward its next hex — and it is
                   what stops a fresh body releasing that hex late.
                   ⚠ INERT: no scenario has a routine, so errand_fields
                   builds nothing and the fork in wave_tick is never
                   taken — 920 gate measurements unmoved, through R6a
                   as well
  poi.loft         A PLACE THAT OWNS MOBS — and the BOUND that is the
                   point of one (plans/30 R5, @X301, @FR-E-Poi-Owns).
                   Poi { kind, q, r, state, since }, the KIND table
                   (poi_kinds / poi_kind_of / poi_kind_named), the five
                   states, poi_state_set, PoiRoute + PoiWorld,
                   poi_errand, poi_bound, poi_route_fault, and
                   Bound + bound_disc_holds / bound_holds / bound_meets.
                   ⚠⚠ THE RECORD IS SCAFFOLDING; THE BOUND IS THE
                   PHASE.  @X299 needs *could this ever be in this
                   window?* answered STATICALLY, and @FR-E-Poi-Owns is
                   what collapses it from N queries to one: the POI IS
                   the bound.
                   ⚠⚠ THE POPULATION IS A SET OF ROUTES AND NEVER A
                   LIST OF BODIES — poi_bound reads PoiRoutes and
                   cannot see a body, so forty haulers on one route
                   have the bound of one.  A `count` is a column it
                   never reads.
                   ⚠⚠ THE CLAIM IS PER-LEG AND THE UNION IS ONLY THE
                   QUERY (@X342).  bound_disc_holds(b, leg, h) is what
                   @FR-E-Non-Increasing and @X336 actually prove;
                   bound_holds is the union over it.  A union is SLACK
                   — against it a radius one hex short, a terminal leg
                   with no disc, an off-by-one rim and a sidestep that
                   could increase the distance ALL read green (@M080).
                   ⚠ Two doors, one implementation, and the difference
                   is nameable: the claim asks a body about its own
                   leg, the query asks whether anything could be here
                   and WANTS the slack.
                   ⚠ LATTICE distance, deliberately: lat_distance is at
                   most any path length, so the lattice disc is a
                   SUPERSET of the field-distance region — the safe
                   direction — and it costs ONE hex across three maps
                   (@M079) while buying a bound of 2 x legs integers
                   that needs no world to read.
                   ⚠ STATE BELONGS TO THE PLACE: poi_state_set is the
                   ONE door and writes `since` with the state, because
                   the closed form goes PIECEWISE at a change.  The
                   bound does not move under any of the five states.
                   ⚠⚠ A POI IS NEVER CULLED (@X304) — there is no verb
                   here that removes one.  *Not materialised* and
                   *culled* are different words.
                   ⚠⚠ AND SINCE R6a IT KNOWS WHAT AN UN-MATERIALISED MOB
                   IS: PoiMob { route, seat, slip, gone }, and only
                   `slip` accumulates (@X343).  The round is an index,
                   the phase is a SEAT whose offset is derived
                   (poi_seat_offset), the hex is `cycle_at`, the BAG is
                   derived from the leg (errand_bag_for) and the BANK
                   from the clock (cycle_carry) — because only a BODY
                   can be pushed, so a mob nobody has looked at needs no
                   memory at all.
                   ⚠⚠ THE THREE TIERS are shaped by where the question
                   is asked: poi_culled is per POI and is one
                   bound_meets for a whole population, poi_tier_at is
                   per mob.  poi_survey's shape IS the architecture —
                   the cull test is outside the loop and is all a
                   distant POI ever costs.  `PoiSurvey.asked` is the
                   COST GATE and it is a COUNT (@M029): 0 against 6 for
                   a POI 200 hexes away.
                   ⚠⚠ poi_materialise HANDS THE BODY FOUR THINGS and the
                   fourth is the one nobody would think of — the hex,
                   the bag, the `slip`, and THE RULE'S BANK.  A fresh
                   body carries nothing where the rule is part-way
                   through a hex, so it releases its next hex late by
                   exactly cycle_carry, and at 1.5 hex/s that is always
                   ZERO (@M014's class, @M081).
                   ⚠ poi_release takes back `slip` and NOTHING else; the
                   bank is re-derived, exactly as the bag is.
                   ⚠ poi_cycles is the SORTIE-LONG cache the cull tier
                   depends on — rebuilt every tick, a culled population
                   would cost two flow sweeps a route to discover it
                   could be skipped.
                   ⚠ INERT: nothing builds a PoiWorld yet, POI_KIND_NONE
                   is 0, and no .keys verb authors one — R6b puts it on
                   WaveState and R7's scenario pair needs the vocabulary
  skill.loft       CREW SKILLS — build, repair, scout (BACKLOG C1) —
                   SKILL_EFFECT_SPAN / _HALF, Skills { build, repair,
                   scout } on Helper, skill_factor, skill_work_units,
                   and the DETECTION rule (DETECT_BASE_HEXES,
                   NOTICE_HEAP_BASE_HEXES, NOTICE_CARGO_HEXES,
                   NOTICE_INTERNAL_HEXES, detect_radius, notice_of_heap,
                   detect_sees).
                   ⚠ A skill SCALES A NUMBER THAT ALREADY EXISTS
                   (@X112) — `build` the helper-seconds DESIGN.md § 13
                   calls the bottleneck, `repair` the standing clock
                   `tower_repair_tick` runs.  No skill adds a mechanism.
                   ⚠ THE CREW HAVE SKILLS AND THE PLAYER DOES NOT
                   (@X119, DESIGN.md § 8): the pilot's own repair is a
                   flat 30 ticks whatever skills exist.
                   ⚠⚠ LEVEL 0 IS BIT-FOR-BIT THE OLD GAME, and the
                   guard for it lives in `skill_factor` — a second one
                   in `skill_work_units` was written first and DELETED,
                   because removing it left the whole gate green: an
                   exact 1.0 multiplied and truncated is already the
                   identity.  Its real job is the NEGATIVE level, which
                   answers −0.5 without it.
                   ⚠ THE CURVE IS CHOSEN AND SAYS SO —
                   `archive/gameplay.data` names the twelve skills and
                   gives no numbers at all — so the gate asserts
                   PROPERTIES (exactly 1.0 untrained, rising,
                   saturating below its span) and tuning the constants
                   moves no test.  Shape is @X189's: no ceiling on the
                   LEVEL, a diminishing return on the EFFECT.
                   ⚠⚠ DETECTION IS TWO RADII THAT INTERACT AND THE
                   SUBJECT OWNS HALF (@X277, owner 2026-08-27): ground
                   items, what is in the ground, what grows on it,
                   salvage — and an INTERNAL (brain, motors, weapons)
                   with the shortest radius in the design.  So a trained
                   scout notices QUIETER things rather than further
                   ones.  ⚠ Nothing produces internals yet
                   (MATERIALS.md is design, not built), so that row is
                   the ruling recorded ahead of its producer.
                   ⚠ DETECTION IS NOT REACH: `salvage_at` and
                   `cargo_take` ask what can be ACTED on; this asks
                   what is KNOWN about, at a different distance
  endure.loft      ENDURANCE — work spends it, rest restores it
                   (BACKLOG C2, @X113) — ENDURANCE_POOL_UNITS /
                   _FRESH_UNITS / _TIRED_FLOOR / _REST_DIVISOR,
                   Endurance { spent } on Helper, endurance_factor /
                   _work / _rest / _scale.  The crew-side of the tower's
                   charge.
                   ⚠ A TIRED PERSON WORKS LESS AND NEVER STOPS —
                   PROGRESSION.md refuses the cliff, so the pool bottoms
                   out at a FLOOR and a spent crew member is slow rather
                   than useless.
                   ⚠⚠ REST IS CONTINUOUS: every alive crew member
                   recovers a third of every tick and working outpaces
                   it.  That is what lets the three jobs (salvage,
                   build, repair) spend independently with nothing to
                   co-ordinate — they resolve at three different points
                   in `wave_tick`, and "restore only somebody who did
                   no work" needs a per-tick scratch flag on a person.
                   ⚠⚠ SKILL MAKES YOU PRODUCTIVE, NOT TIRELESS: a site
                   tires by the RAW elapsed and never by the scaled
                   output, so a trained builder gets more done for the
                   same tiredness and training never makes anybody able
                   to work longer.
                   ⚠⚠ YOU TIRE FROM WORK YOU ACTUALLY DID.
                   `helper.loft::crew_work_units` is PURE for exactly
                   this reason: it is evaluated as an ARGUMENT to
                   `tower_repair_tick`, so spending inside it charged
                   every helper a tick of repair whether or not a black
                   tower was in reach — 1.67x a tick's worth per tick,
                   caught by the gate.
                   ⚠ Stored as SPENT, so [loft#914]'s partial literal
                   reads as RESTED.
                   ⚠ Sized from the design's own sortie (@M055): ~675
                   ticks to tire against a corpus whose longest base is
                   321, so 687 measurements did not move — and the
                   arithmetic is ASSERTED, so a base that gets long
                   enough goes red before the clocks do
  jammer.loft      THE JAMMER SWITCH — turning your own core off
                   (BACKLOG C3) — Jammer { off }, jammer_new /
                   jammer_on / jammer_set / jammer_toggle /
                   jammer_loot_rate; and in `spawn.loft`,
                   `wave_cutoff` and `wave_jammer_switch`.
                   ⚠ THE CORE IS THE JAMMER (SETTING.md § The
                   recruitment), so it does three things and the
                   fiction makes them one: it CUTS a robot off, which
                   is why there are waves at all, which is why there is
                   anything to salvage.
                   ⚠⚠ IT STOPS THE SUPPLY AND NEVER THE SIEGE.  Losing
                   an errand is one way (@X276), so everything already
                   cut off is still walking to the core — a player who
                   hits the switch with a wave on the perimeter has
                   bought nothing for that wave.  A switch that emptied
                   the map would have retired the SCRAMBLE.
                   ⚠⚠ THE SALVAGE STOPS AS INCOME, NEVER AS WORK.
                   `height_clear` is untouched, so a kill zone bodies
                   are ramping shut stays openable and the cost is the
                   points.  It is also why toggling cannot be farmed:
                   both effects are CONTINUOUS, so being paid means
                   leaving it on while you work.
                   ⚠⚠ THE FIELD STORES `off`, and that is [loft#914]:
                   a boolean defaults to false, so storing `on` would
                   make every partial WaveState literal in the repo
                   land with the core DARK — no waves, no income, and
                   a green suite over a game nobody plays.  Every
                   reader asks `jammer_on` and never the field.
                   ⚠⚠ AND THE MOVER'S QUESTION CHANGED WITH IT.
                   `enemy_step` asked `enemy_in_bubble`, because
                   everything inside the bubble had been cut off by the
                   time it moved — so INSIDE and CUT OFF were one
                   question.  A dark core separates them, and the
                   discriminant is now `enemy_engaged` (!errand &&
                   in_bubble), which is bit-for-bit the old reading
                   while the core is jamming because `wave_cutoff`
                   sweeps at the top of every tick.  Without it a robot
                   crossing a DARK bubble turns round and besieges, and
                   every gate still passes.
                   ⚠ jammer_loot_rate is the ONE door the salvage
                   income goes through; `spawn.loft` pays the player
                   and the crew at two sites, and the way two copies of
                   one rule drift is a crew still being paid after the
                   core went dark.
                   ⚠ wave_jammer_switch is POSITION-LOCKED to the core
                   (@X099), reads `state.player` and takes no owner —
                   the crew do the work, ending the run's income is the
                   player's decision.
                   ⚠ It decides no END GAME: DESIGN.md § Shutting down
                   the scrambler gives the switch a second job — the
                   swarm coordinated and pointed at the ancient ones,
                   the bubble as a LURE — and tier 3 does not exist, so
                   the only thing the off state can be priced against
                   today is the player's own base (@M056)
  task.loft        A JOB, AND WHICH ONE A CREW MEMBER GOES TO
                   (plan 29 O1/O3, @X197's assignment pillar) —
                   TASK_ANY / _BUILD / _CLEAR / _REPAIR / _REARM,
                   TASK_KIND_COUNT, task_name / task_from_name /
                   task_named / task_cycle, Job { found, kind, q, r },
                   job_none / job_same / jobs_in_scope / job_pick; and
                   in `spawn.loft`, `wave_assign` (the ONE site where a
                   crew member decides anything) and
                   `wave_direct_nearest`.
                   ⚠ It adds no FIFTH job.  The four are the four a
                   crew member already did by STANDING somewhere —
                   `helper_build`, `helper_salvage`, `wave_repair`,
                   `wave_rearm` — each on a one-hex disc, and every one
                   of them still fires on position exactly as before.
                   This answers WHICH ONE TO WALK TO.
                   ⚠⚠ THE REMIT TRADES BREADTH FOR REACH: undirected is
                   all four inside `detect_radius` (3 hexes untrained),
                   directed is ONE kind across the whole map.  A remit
                   that only narrowed would be a pure loss nobody would
                   ever give — @X253's payoff needs @X124's practice
                   loop, and @M066 measured that nine of twelve skills
                   have no number in the tree.  One that only widened
                   would be free, and a decision that costs nothing is
                   not one.
                   ⚠⚠ THE RADIUS IS THE CREW MEMBER'S ALONE, and that
                   is a MEASURED correction.  The first version asked
                   the full `detect_sees`, so a heap's own
                   `notice_of_heap` widened the work radius from 3 to 6
                   — and it moved 18 tests across 8 files, all in one
                   direction: bases that used to fall stood for 800
                   ticks with 0 enemies alive, and a wallet that ended
                   at 40 ended at 299 of 300.  @X277's two radii answer
                   *is this worth driving to*, which is the PLAYER's
                   question; this asks *what is under my nose*, which
                   is a fact about the person and nothing about the
                   pile.  ⚠ And it is @X198's test that it failed —
                   a default that absorbs the work DESIGN.md § 9 says
                   growth is supposed to CREATE deletes the table the
                   pillar stands on.
                   ⚠ `job_pick` is nearest-wins, ties by kind then q
                   then r — `repair_target`'s rule for the third time —
                   plus ONE line of stickiness: a job already being
                   walked to is abandoned only for one STRICTLY nearer.
                   That is @X198's *a job started is not cheap to
                   abandon* as arithmetic, and mechanically it is what
                   stops a crew member oscillating between two jobs
                   that trade places as they move.
                   ⚠ TASK_ANY is 0 so a partial Helper literal reads as
                   GENERAL ([loft#914]) — the same discipline that
                   spells a field `top_removed` rather than `has_top`.
                   ⚠ It cannot see a `WaveState`: `spawn.loft` uses it,
                   and a `use` imports one way only — `manifest_of` and
                   `moat_depth` were shaped by the same constraint,
                   with the same benefit, which is that a job search can
                   be asked about a world that is not in a run at all
  trap.loft        A TRAP THAT DOES NOT AUTOMATICALLY RESET
                   (BACKLOG C4, @X108) — TRAP_DAMAGE / TRAP_BLAST_HEXES
                   / TRAP_REACH_HEXES / TRAP_REARM_UNITS, TrapCharge {
                   spent, rearm }, trap_state_empty / trap_armed /
                   trap_fire / trap_rearm_tick / trap_rearming /
                   trap_rearm_progress / trap_in_blast / trap_in_reach;
                   and in `spawn.loft`, `wave_traps` and `wave_rearm`.
                   ⚠ It is the tower-repair clock with the COST MOVED
                   TO THE FRONT (plans/17 T1): a tower's servicing buys
                   thirty shots at the END of its life, a plate's buys
                   ONE blast at the start of every one.
                   ⚠⚠ THE TRIGGER IS A CROSSING, NEVER A STANDING
                   POSITION.  A scout is released TWO hexes in two
                   ticks out of three, and the hex in the middle of a
                   stride is one nothing stands on when a tick ends —
                   so a plate asking `occupancy_taken` is walked over
                   by the one class fast enough to matter, and NO TEST
                   USING A REGULAR ROBOT COULD SEE IT (a regular takes
                   exactly one hex a tick).  `occupancy.loft` grew a
                   `visits` count for it: one field and one line,
                   because `occupancy_enter` is already the ONE door
                   every arrival goes through and `Occupancy` is
                   already threaded into the mover.
                   ⚠⚠ THE BLAST IS A DISC OF RADIUS 1, and two
                   derivations land on that number: a crosser ends at
                   most `stride − 1` hexes away so radius 1 catches
                   what set it off (ASSERTED as a cross-product over
                   every class), and it is `hex_disc_radius_1`, the one
                   footprint numbers.json names.
                   ⚠ THE FIELD STORES `spent`, so [loft#914]'s silent
                   default is an ARMED plate — which is what somebody
                   who wrote `flag 7 0 trap` in a map meant.  Every
                   reader asks `trap_armed`.
                   ⚠ It does not fire on the player or the crew:
                   `Occupancy` holds enemies only, and the fiction
                   agrees — the vehicle HOVERS, so there is no weight
                   on the plate.
                   ⚠ Re-arming takes the ONE servicing rate dryopea has
                   (TOWER_REPAIR_UNITS, 20 s) because there is no trap
                   row in numbers.json — the case @X112 says to push
                   back on, CHOSEN and said to be chosen.  It is
                   presence-locked, refuses a CARRIER, refuses an ARMED
                   plate, and scales by the crew's `repair` skill.
                   ⚠⚠ WHAT IT IS WORTH IS THE TRIP, NOT THE BLAST
                   (@M057): one blast nobody goes back for is worth
                   −3 ticks — the bodies are a permanent terrain change
                   (plan 12 B7) — while the same plate re-armed three
                   times is +106, and the control says it is not the
                   crew member standing there (parked for the whole
                   run: +20).  ⚠ And ONE HEX decides it, failing two
                   different ways: in the gate the servicing hex is the
                   hex the wave comes through and the crew member is
                   wrecked; two hexes out it never gets through its own
                   gate at all
  moat.loft        A MOAT — the one hex whose surface is BELOW the
                   ground around it (BACKLOG C5, @X282) —
                   MOAT_METRES_PER_DROP, moat_depth / moat_depth_at /
                   moat_at; and elsewhere, `passable.loft`'s two new
                   readings of it, `build.loft::BUILD_MOAT_UNITS` +
                   `as_moat_kind`, and `play.loft`'s second trail mode.
                   ⚠ It is the palette's `drop` READ — sea 0, water 1,
                   rapids 3, waterfall 8, carried since plan 01 and read
                   by nothing until now (plans/25 § M2 open question 3).
                   ⚠ NOT plan 02's solver: this is the LOCAL reading —
                   one hex, its own drop, no drainage chain — and a
                   river stays flat until plan 02 grows the term a
                   chain rather than replacing it.
                   ⚠⚠ A PROBE FALSIFIED THIS FILE'S HEADLINE BEFORE IT
                   SHIPPED.  It was designed as *the depth is the COST,
                   because the crew and the player HOVER and fall in* —
                   and `walk_vehicle` WAS read by NOTHING (@D006):
                   `can_climb` refused a step whose either end failed
                   `hex_walkable`, which answers `walk_ground` for
                   everybody.
                   ⚠⚠ BACKLOG C10 FIXED IT 2026-08-28, SO THE
                   FALSIFIED VERSION IS RIGHT AGAIN (@X286): the probe
                   was right about the CODE and the design was right
                   about the GAME.  `drive_along` asks `can_hover`, so
                   a hovering mover crosses flat sea for free, falls
                   INTO a trench and owes a climb out that 0.4 m has
                   not and a 3.0 m boost has — THE DEPTH IS THE COST.
                   ⚠ The drop's other job, the WATERLINE, is what this
                   file is for and does not depend on who may cross.
                   ⚠⚠ A PILE IS A SURFACE ONLY ONCE IT CLEARS THE
                   WATER (`passable.loft::hex_ground`), so the depth is
                   how much a moat SWALLOWS — water's 1 m is two bodies
                   at BODY_HEIGHT_METRES, the same one-to-two band
                   § Why a robot climbs 2.0 m lands on for a ramp onto
                   a wall.  Without it the drop decides nothing: a
                   trench would flip to walkable rubble at the first
                   grain however deep it was.  ⚠ On land moat_depth is
                   0.0, so it is `height_piled`'s `rise > 0` unchanged
                   everywhere anybody has ever painted — which is why
                   745 gate measurements did not move.
                   ⚠⚠ `water` and ONLY `water` may be ORDERED, and it
                   is a removability rule: a waterfall trench wants
                   SIXTEEN bodies and nothing in the game can put them
                   there, so the player could dig a barrier nothing can
                   undo.  order_work_units is the refusal, because the
                   list of what may be built at all was already there.
                   ⚠ The trench costs a wall's 10 s deliberately —
                   equal, so the choice is about what it DOES.
                   ⚠⚠ BESIEGERS SHOVEL IT SHUT (BACKLOG C9, @X283) —
                   MOAT_FILL_DAMAGE_PER_METRE, moat_spoil_metres,
                   moat_fill, moat_left_to_fill; and elsewhere,
                   `flow.loft::sweep_ground`, `wave_damage`'s second
                   verb, `height.loft::RUBBLE_SPOIL` and
                   `vehicle.loft`'s refusal of it.  A moat is a TIMER.
                   ⚠⚠ THE BACKLOG ROW'S MECHANISM WAS WRONG AND A PROBE
                   SAID SO.  It read *a besieger's target lands
                   nothing* — but water fails `hex_walkable`, so a moat
                   hex is not a node in the DESIRE field either and
                   `enemy_target` names the besieger's OWN hex.  A
                   branch at `wave_damage` alone would have been dead
                   code; the work is in the desire SWEEP.
                   ⚠ The rule that sweep always followed, said out
                   loud: AN OBSTACLE THE WAVE CAN REMOVE IS PASSABLE IN
                   IT.  A wall is (the lifted climb was how that got
                   expressed); a trench is, and water is not a surface
                   at any height, so the NODE rule is what widened.
                   ⚠ The SEA is not a moat, so the field still stops at
                   the coast and § Three states' unbounded-plane
                   argument is untouched.  Routing is UNCHANGED.
                   ⚠⚠ A TRENCH IS A WALL THAT CANNOT BE UNBRACED — the
                   rate is priced at the FULL WALL_HP per metre, the
                   figure a wall reaches only closed into a ring,
                   because a hole has no ends to unzip from.  ⚠ Metres
                   per DAMAGE, never per depth: that is what keeps the
                   drop the timer, so a waterfall wants 1200 ticks of
                   one regular against a corpus whose longest base is
                   320.  ⚠ ONE door, no per-class table — a besieger
                   digs with the tool it chews with.
                   ⚠⚠ SPOIL IS NOT SALVAGE (@M059).  The row promised
                   `salvage_at` its trip and it measured as an OFF
                   SWITCH: the bite is min(rate * dt, pile), so a
                   clearer takes the WHOLE pile whenever the pile is
                   smaller than one bite, and one helper in reach would
                   hold a trench open for ever at any fill rate the
                   design can tolerate.  ⚠ loot_rate pays 0 for spoil
                   and the DEFAULT pays, so the row is what stops
                   *letting the enemy fill your trench* being income.
                   ⚠⚠ WHAT IT IS WORTH: 130 / 174 / 221 (@M059) — the
                   trench is +47 over the wall the same ten
                   helper-seconds a hex would have bought.  It still
                   EARNS nothing alone, because a wave standing at a
                   trench is a wave nothing is shooting at — ⚠⚠ and
                   with a TOWER behind it that inverts: 335 ticks and
                   NINE of thirteen dead (@M060), because a besieger
                   has to stand at a fixed distance and dig
  font.loft        THE FONT — the ONE seam to graphics::draw_text
                   (BACKLOG B1) — TEXT_FONT_FILE, Font { handle, path,
                   loaded }, font_load / font_load_from / font_ready /
                   text_fault / text_width / text_line_height /
                   text_draw.  dryopea rasterises NOTHING: `graphics`
                   owns the glyphs, and this owns which font, where it
                   lives, and what one that did not load may do.
                   ⚠⚠ THE PATH IS ABSOLUTE, and that is a requirement
                   rather than a tidiness: `gl_load_font` resolves a
                   RELATIVE path against the process CWD in the pinned
                   graphics 0.5.2 and against `source_dir()` in 0.8.0,
                   so `loft install graphics` would silently stop the
                   font loading (@X268).  `{source_dir()}/../assets/…`
                   is the one form both pass through verbatim, and it
                   resolves from BOTH entry shapes — `tests/` and
                   `src/`.  ⚠ `#cwd` does NOT move `source_dir()`.
                   ⚠⚠ `font_load_from` REFUSES a relative path rather
                   than documenting against one: `is_file` answers TRUE
                   for `assets/…` from the repo root, so a relative
                   path passes every check dryopea could make and only
                   then means two different things.
                   ⚠⚠ A FONT THAT FAILED TO LOAD DRAWS IN WHATEVER
                   FONT LOADED FIRST — a `null` handle collapses to 0
                   at the native boundary, which is a live handle, so
                   the failure is the WRONG TYPEFACE and not a blank
                   corner (@M047).  Every door asks `loaded`.
                   ⚠⚠ `loaded` is a SECOND field for one fact, against
                   `carry.loft`'s own rule, and [loft#914] is why: a
                   partial literal takes the silent default, and
                   omitting `loaded` FAILS CLOSED where omitting a
                   `handle` would default to 0 and fail INTO the trap.
                   ⚠ Nothing outside this file may call
                   `graphics::draw_text`.
                   ⚠⚠ It does NOT decide COMPOSITING: 774 of a glyph
                   line's 1324 lit pixels are BLENDED, so text drawn
                   into a classified frame breaks @X077's `other == 0`
                   and @X092's `unknown - entity == 0`.  That is the
                   first consumer's call — @M042's own-canvas count is
                   the precedent, a flat-thresholded door the other
                   candidate, and loosening `classify_canvas` is
                   refused outright
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
| `Hex` | `lattice.loft` | `{ q, r }` pointy-top odd-r offset coord — `q` a COLUMN, `r` a ROW |
| `EditorState` | `editor_step.loft` | the whole editor session — layers, camera, picker, mode, history, chunk dirty set |
| `EditorInput` | `editor_step.loft` | one frame of player intent (hover hex + action flags) |
| `EditorCamera` | `camera.loft` | `{ pos: Hex, zoom: integer }` |
| `RenderCamera` | `render_camera.loft` | `{ target: Vec3, azimuth, elevation, distance, fov_y, near, far, up: Vec3 }` — the GAME's camera (plan 21 R1).  ⚠ Its world is `+y` NORTH with `+z` up, which is not dryopea's metre frame; `lat_to_world` is the one conversion |
| `CameraRig` | `render_camera.loft` | `{ cam: RenderCamera, boom: float, rested: boolean }` — the LIVE camera plus what the world cannot supply (plan 21 R2, `@X070`).  ⚠ **Two booms, and they are different facts**: `boom` is what the player asked for and the wheel writes; `cam.distance` is what the eye HAS after easing and occlusion.  Collapse them and a wall the vehicle drove past permanently rewrites the zoom.  ⚠ Lives on `PlayState.cam`, which closes `@X014` |
| `InputState` | `camera.loft` | per-frame camera flags (in_pan_*, in_zoom_*) — folds into `EditorInput` in plan 08 V0b |
| `PaintedHex` | `painted.loft` | `{ q, r, kind: u8 }` — one painted cell |
| `PaintedWorld` | `painted.loft` | wrapper holding `hash<PaintedHex[q, r]>` |
| `GroundType` | `palette.loft` | one row from `examples/palette.json` |
| `Picker` | `picker.loft` | palette UI state |
| `MapFile` | `map_file.loft` | save record (6 fields; see Known constraints) |
| `GroundEntry` | `map_file.loft` | one persisted hex with kind as text name |
| `ScriptRun` | `script.loft` | one `.keys` run — ok / failing line / message / counts, plus the pointer, the shots directory and the GAME it is playing.  ⚠ `run.play.wave` / `run.play.ticks` since plan 19 P1: the wave and the clock are ONE record, because `play_ticks` takes a `PlayState` and a run holding the pieces separately could only hand over copies |
| `PlayState` | `play.loft` | a game in progress — the roster, the `TickClock`, the time banked toward the next tick, last frame's input, whether the session is LIVE, and the camera it is looking through.  ⚠ What a live session has that an edited one does not; the world is passed in, never owned, because a struct in a field is a copy.  ⚠ `playing` (plan 19 P3) gates the CLOCK and never the seam — `EditorInput.in_playing` is the other, per-frame half and says what the KEYS mean |
| `Timer` | `tick_timer.loft` | a one-shot duration as `{spent, total}` in integer base units — the shared half of every *fires once* clock in the game (`recover`, `repair`, `boost`, `cool`, `stand`, `lull`).  ⚠ `total == 0` is UNARMED **and is the zero-default**, which is what makes `Timer { }` correct-neutral at all six sites and is the opposite conclusion to `Bank`'s (`@X082`).  ⚠ `timer_left` and `timer_spent` are one number read two ways, so a site keeps the direction its field was authored in with no second accumulation to disagree |
| `Bank` | `tick_bank.loft` | the carry toward the next whole unit, as ONE integer — the shared half of every rate in the game, and the reason `Enemy`, `Helper` and `Vehicle` all release whole hexes the same way.  ⚠ It holds neither the rate (`@X061`) nor the scale (`@X080`), which is what makes `Bank { }` correct-neutral in a partial literal.  ⚠ `bank_progress` is what `compare.loft` reads and `bank_fraction` what `emit.loft` writes — an integer inside, hexes on the wire |
| `TickClock` | `tick_clock.loft` | a fixed step and the time banked toward the next one, both INTEGERS — so `advance(n × step) == step(n)` exactly, where the float bank it replaced was wrong for 602 of the first 1000 `n`.  ⚠ `banked` is always in `[0, step)`, which is what makes it the whole of a rollback's timing state.  ⚠⚠ Its base unit is 1/3 µs and that was MEASURED rather than chosen for tidiness (`@X079`, `@M031`)  ⚠ Since plan 26 L4 it also carries the POLICY state — `rate_num` / `rate_den` / `rate_carry` and the pump's `pump_at` / `pumped` — and **every one of them defaults to *the behaviour you already had***, which is this plan's third answer to [loft#914] (`@X084`).  ⚠ `rate_carry` is a SECOND accumulator and a different QUESTION: `banked` carries sim time toward the next step, it carries wall time toward the next sim unit |
| `FrameCounts` | `measure.loft` | one classified frame — pixels per bucket, `unknown` (not a palette colour = a fault), `total` |
| `WaveState` | `spawn.loft` | the enemy roster + round-robin cursor + the runtime rubble layer + the structure damage ledger + every tower's banked charge + the run's wallet + the crew + the cargo — runtime, not editor state |
| `Vehicle` | `vehicle.loft` | the player: where it is, where it is pointed, whether it is in the world at all, and the ground it has banked — ⚠ `parked` is separate because (0, 0) is a real hex and is the core in every scenario.  ⚠⚠ `bank` arrived in plan 26 L2 and its ABSENCE was `@D003`: the player truncated its movement where every other mover carried, so it read 180 / 120 / 180 / 0 / 0 / 0 / 0 hexes a minute against a true 180 (`@M030`) |
| `Wallet` | `wallet.loft` | points SPENT out of the run's 200 — zero is a FULL wallet, and the ledger is clamped at the budget so a later credit is not swallowed |
| `TowerState` | `tower.loft` | per tower: the seconds banked toward its next shot, the shots it has FIRED out of its 30, and the seconds banked toward a REBUILD — runtime, never saved.  ⚠ Three clocks, and repair touches exactly one of them |
| `Enemy` | `spawn.loft` | `{ q, r, kind, heading, alive, taken, stand, bank }` — ⚠ **three of the eight are ZERO-neutral and that is the trap**: `taken` is damage ABSORBED, `stand` is the pre-walk window still owed and `bank` is ground banked but not yet spent, so a literal that omits any of them is a HEALTHY enemy that has finished arriving and is carrying nothing.  ⚠ The carry joined in plan 23 K2a and had no `.keys` setter until K2b, because at 1.5 hex/s it is exactly zero after every tick and nothing in the repo could hold one.  ⚠ It became a `Bank` in plan 26 L2 — and stayed zero-neutral through the nesting only because a `Bank` holds no scale (`@X080`) |
| `CarryObject` | `carry.loft` | one carryable thing — ⚠ `owner` is the WHOLE state machine (ground / a carrier / spent), because two fields that can disagree about one fact is the defect the model exists to make unwritable |
| `CargoLayer` | `carry.loft` | every carryable thing in the run — ⚠ a VECTOR with stable slots, never a hash by hex: two objects share a hex and a hash deletes one |
| `Socket` | `part.loft` | one joint a part OFFERS — a name, a class token, and **a bone plus a `t` along it**.  ⚠ NO SIZE: the class is the contract, and whether the thing physically fits is the simulation's constant to answer (`PARTS.md` § D6, plan 20 A3) |
| `Binding` | `part.loft` | what is in which socket — **two texts and nothing else**.  ⚠⚠ The absence of a coordinate is the invariant, and the absence of a POSE is the deviation from `hex_part::Binding.bd_open`: a dryopea part is a catalogue asset, so its angles come from the sim |
| `Part` / `PartSet` | `part.loft` | a rig, the sockets it offers, what fills them — and the catalogue they are looked up in.  ⚠ A `hash` keyed by name, because every question the file asks is a lookup by name and nothing ever sweeps it |
| `HeightLayer` | `height.loft` | metres of rubble piled on the map at runtime, and what it is made of — never saved |
| `DamageLayer` | `damage.loft` | HP each structure has ABSORBED — runtime, never saved; a miss means undamaged |
| `FlowField` | `flow.loft` | one class's distance field: cells (distance + the height it was swept with), the core, and the CLIMB it was built for |
| `ValidateReport` | `validate.loft` | one `make validate` sweep — scripts / passed / failed / measurements / shots, and the FIRST failure with the number that moved |

