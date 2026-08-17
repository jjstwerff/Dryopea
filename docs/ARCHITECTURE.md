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
                   what the player sees, composed ONCE: world, hover
                   preview, markers, ghost, picker, save indicator,
                   mode badge.  Both the GL loop and the script
                   runner's `snap` ask for it, so a shot is the
                   editor's frame and not a harness renderer's.
                   Also owns VIEW_W / VIEW_H / VIEW_PPM (the window
                   size IS the shot size).  Never mutates the state
  tick_clock.loft  the FIXED STEP, in exact integer time (plan 26 L1)
                   — TickClock { step, banked } + clock_new /
                   clock_advance (a DURATION) / clock_step (a COUNT) /
                   clock_banked / clock_restore /
                   clock_units_from_micros.
                   ⚠⚠ **The base unit is 1/3 of a MICROSECOND**
                   (CLOCK_UNITS_PER_SECOND = 3 000 000), and µs was
                   REFUSED on a measurement (@X079, @M031): dryopea's
                   tick is 2/3 of a second, `1e6 / 1.5` is 666 666.67,
                   and the 666 667 the plan recommended moves 17 tests
                   while the 654 gate measurements cannot see it.  1/3
                   µs is the coarsest unit in which 2/3 s is whole, so
                   TICK_SECONDS derived from it is BIT-IDENTICAL to the
                   `1.0 / 1.5` it replaced.
                   ⚠ **The unit is the CONSUMER's choice** — clock_new
                   takes a step and the accumulator counts whatever the
                   caller counts (128 Hz cannot use 3 000 000 either).
                   What this file promises is the IDENTITY, not a unit.
                   ⚠ **No callback, no cap, no rate scaling, no
                   alpha.**  It never calls your tick; it answers HOW
                   MANY.  A cap is the DRIVER's policy — moros's page
                   has one, its server does not, and `play_advance`'s
                   refusal of one is what 654 measurements rest on.
                   ⚠ **No tick COUNTER**: `PlayState.ticks` already
                   counts, and a second counter is two facts that can
                   disagree.  This owns the STEP and the BANK.
                   ⚠ clock_seconds_from_units is the ONE seam back to
                   float seconds and every caller is a one-shot TIMER
                   (boost, cooldown, recovery, repair, the lull) — L3
                   closes it.  A MOVER that reached for it would be
                   throwing away what tick_bank was built to keep
  tick_bank.loft   a RATE consumed in whole units, exactly (plan 26 L2)
                   — Bank { progress } + bank_new / bank_rate /
                   bank_gain / bank_most / bank_progress /
                   bank_fraction / bank_restore /
                   bank_restore_fraction / bank_reset, over
                   BANK_RATE_SCALE and BANK_WHOLE.
                   ⚠⚠ **The ONE implementation of *do not lose a
                   fraction*.**  There were three copies plus a missing
                   fourth (@D003 — the player truncated), and both
                   mover epsilons are DELETED because integer
                   arithmetic has nothing to nudge.
                   ⚠⚠ **A Bank holds the CARRY and nothing else**
                   (@X080): the RATE arrives per call, because @X061
                   makes it a property of a CONDITION; and `whole` is a
                   PARAMETER, because a Bank carrying its own scale
                   would default to 0 in every partial struct literal
                   and silently freeze that mover ([loft#914]).
                   ⚠ **The rate is scaled and the time is not** — time
                   is exact base units, a rate is an authored float, so
                   bank_rate quantises to millionths.  The reciprocal
                   form (units per hex) needs no scale and is INEXACT
                   at 2.25 hex/s, which @M013 already sweeps.
                   ⚠ **It is NOT a Timer** and must not become one: a
                   bank's remainder is load-bearing for ever, a
                   one-shot's dies at its boundary.  L3 builds Timer
                   beside this, never on top of it.
                   ⚠ bank_most is the CEILING and spends nothing
                   (@X081) — `play_steer_reach` asks once per FRAME

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
                   ⚠ **A new coordinate-carrying verb needs a row in
                   `convert.loft::keys_schemas`**, or a future lattice
                   conversion leaves it in the old labels — silently,
                   because an unknown command passes through unchanged.
                   `tests/09_c5a_converter.loft` § The schema is
                   complete is the gate, and it only fires if its
                   vocabulary list is updated too
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
                   the player is a third KIND in `passable.loft` —
                   `walk_vehicle` is true for all twelve palette
                   entries, so the height step is its whole
                   passability.
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
                   because the class only ever contributed its climb
  height.loft      the RUBBLE layer — what runtime has piled on the map
                   (plan 11 F6, named by plan 12 B1).  HeightLayer +
                   height_raise (metres AND a source) / height_clear /
                   height_rise / height_piled / height_source / count,
                   plus RUBBLE_WRECKAGE / _CARAPACE / _MASONRY.
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
| `Bank` | `tick_bank.loft` | the carry toward the next whole unit, as ONE integer — the shared half of every rate in the game, and the reason `Enemy`, `Helper` and `Vehicle` all release whole hexes the same way.  ⚠ It holds neither the rate (`@X061`) nor the scale (`@X080`), which is what makes `Bank { }` correct-neutral in a partial literal.  ⚠ `bank_progress` is what `compare.loft` reads and `bank_fraction` what `emit.loft` writes — an integer inside, hexes on the wire |
| `TickClock` | `tick_clock.loft` | a fixed step and the time banked toward the next one, both INTEGERS — so `advance(n × step) == step(n)` exactly, where the float bank it replaced was wrong for 602 of the first 1000 `n`.  ⚠ `banked` is always in `[0, step)`, which is what makes it the whole of a rollback's timing state.  ⚠⚠ Its base unit is 1/3 µs and that was MEASURED rather than chosen for tidiness (`@X079`, `@M031`) |
| `FrameCounts` | `measure.loft` | one classified frame — pixels per bucket, `unknown` (not a palette colour = a fault), `total` |
| `WaveState` | `spawn.loft` | the enemy roster + round-robin cursor + the runtime rubble layer + the structure damage ledger + every tower's banked charge + the run's wallet + the crew + the cargo — runtime, not editor state |
| `Vehicle` | `vehicle.loft` | the player: where it is, where it is pointed, whether it is in the world at all, and the ground it has banked — ⚠ `parked` is separate because (0, 0) is a real hex and is the core in every scenario.  ⚠⚠ `bank` arrived in plan 26 L2 and its ABSENCE was `@D003`: the player truncated its movement where every other mover carried, so it read 180 / 120 / 180 / 0 / 0 / 0 / 0 hexes a minute against a true 180 (`@M030`) |
| `Wallet` | `wallet.loft` | points SPENT out of the run's 200 — zero is a FULL wallet, and the ledger is clamped at the budget so a later credit is not swallowed |
| `TowerState` | `tower.loft` | per tower: the seconds banked toward its next shot, the shots it has FIRED out of its 30, and the seconds banked toward a REBUILD — runtime, never saved.  ⚠ Three clocks, and repair touches exactly one of them |
| `Enemy` | `spawn.loft` | `{ q, r, kind, heading, alive, taken, stand, bank }` — ⚠ **three of the eight are ZERO-neutral and that is the trap**: `taken` is damage ABSORBED, `stand` is the pre-walk window still owed and `bank` is ground banked but not yet spent, so a literal that omits any of them is a HEALTHY enemy that has finished arriving and is carrying nothing.  ⚠ The carry joined in plan 23 K2a and had no `.keys` setter until K2b, because at 1.5 hex/s it is exactly zero after every tick and nothing in the repo could hold one.  ⚠ It became a `Bank` in plan 26 L2 — and stayed zero-neutral through the nesting only because a `Bank` holds no scale (`@X080`) |
| `CarryObject` | `carry.loft` | one carryable thing — ⚠ `owner` is the WHOLE state machine (ground / a carrier / spent), because two fields that can disagree about one fact is the defect the model exists to make unwritable |
| `CargoLayer` | `carry.loft` | every carryable thing in the run — ⚠ a VECTOR with stable slots, never a hash by hex: two objects share a hex and a hash deletes one |
| `HeightLayer` | `height.loft` | metres of rubble piled on the map at runtime, and what it is made of — never saved |
| `DamageLayer` | `damage.loft` | HP each structure has ABSORBED — runtime, never saved; a miss means undamaged |
| `FlowField` | `flow.loft` | one class's distance field: cells (distance + the height it was swept with), the core, and the CLIMB it was built for |
| `ValidateReport` | `validate.loft` | one `make validate` sweep — scripts / passed / failed / measurements / shots, and the FIRST failure with the number that moved |

