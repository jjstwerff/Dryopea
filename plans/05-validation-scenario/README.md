<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 05 — Validation scenario

**Status:** Future (drafted 2026-05-26; no code).

## Goal

The **integration plan**.  Plans 01-04 design the *systems*;
plan 05 specifies the *minimum playable thing* that uses them
end-to-end.  Most decisions referenced here are already
settled elsewhere — this document **consolidates** them into a
single buildable spec, not a re-derivation.

Cross-references throughout point to the source-of-truth docs;
when those change, this plan adapts rather than the other way
around.

## Validation tier

For the playable validation milestone:

- **One base, one mission.**  No multi-mission run, no
  carryover effects (mechanic carries; effect deferred per
  DESIGN.md § Q4).
- **Robots only.**  No insects, no elementals, no bosses.
- **One tower type** (placeholder laser).  No tower variants
  yet.
- **One enemy type** (placeholder magenta regular).
- **No sap, no gems, no new tower unlocks.**
- **One starter map** (see § Starter map #1 below).

Validation **passes** when a cold player can pick the game up
and play one full base end-to-end — paint walls, weather
waves, scramble out — with no critical inconsistency in
~30 minutes of play.

## Starter loadout (state at t = 0)

When the rocket finishes landing on the chosen map:

| | Value | Reference |
|---|---|---|
| Core building | Landed and visible | DESIGN.md § Updates: core landing — area + random within + random rotation + 2-hex clearance |
| Starter tower | Touched down via a separate lander, 5-10 hex from the core in a random direction, **already standing + firing-ready** | DESIGN.md § Updates: starter tower lander |
| Helpers | **2**, emerging from the lift-off face within 2-3 s of landing | DESIGN.md § Updates: helper roster 2 starting, 6 max |
| Wallet | **200 points** (default; tunable) | NUMBERS.md |
| Top inventory | Empty (validation has no carryover effect yet) | DESIGN.md § Q4 |
| Walls / additional towers | 0 | Player builds them during play |
| Active spawn markers | All authored markers EXCEPT those within `close_spawn_disable_radius` of the core | DESIGN.md § Updates: free-pick landing |
| Wave list | Per `examples/waves.json` | Plan 03 § Waves |

## Pre-mission flow (cold start)

```
[ Launch the game ]
     ↓
[ Static planet view ]      plan 04 § L3 — teaser of the future
                            rotating-planet hub.  Placeholder
                            sphere with one clickable marker
                            per available map.
     ↓
  player clicks a map-marker
     ↓
[ Landing-spot pick UI ]    DESIGN.md § Updates: free-pick landing
     ↓
  player clicks any hex (within map's playable-area edge buffer)
     ↓
[ Rocket descent animation ]
     ↓                       rocket auto-steers off invalid hexes;
                             searches outward for a valid hex within
                             core_landing_area_radius
     ↓
[ Core lands → starter-tower lander touches down → helpers emerge ]
     ↓                       player gains control
     ↓
[ Play — pre-wave window ]   (no time limit; wave 1 fires on
                              walls-built or distant-marker trigger)
```

## Input scheme (consolidated, single page)

| Action | Keyboard / Mouse | Controller (sketch) |
|---|---|---|
| Move | **W A S D** | Left stick |
| Mouse / right stick | reserved for **UI clicks** (landing-spot pick, editor paint, map select) — NOT camera orbit | (controller equivalent: cursor stick) |
| Pickup / drop (single key) | **E** | A / X |
| Wall paint mode toggle | **Q** | B / circle |
| Boost (vehicle when moving; tower when adjacent — context resolves) | **Shift** (hold) | RT / R2 |
| Mode toggle (editor: ground ↔ marker) | **Tab** | Y / triangle |
| Palette select (editor) | **1 2 3 4 5 6 7 8 9 0 -** | D-pad cycle |
| Pause / cancel | **Esc** | Start |

Source-of-truth: [NUMBERS.md § Input scheme](../../../docs/NUMBERS.md).

## Camera (during play)

**Over-the-shoulder, locked-in-pose.**  ~3 m above and ~5 m
behind the vehicle; slight forward pitch (~10°) so the player
sees a bit further ahead than back.  Camera **does not orbit
with mouse** — it is automatic, not player-driven.

**Auto-reframes** in two cases:

- **Sudden vehicle movement** (sharp turn, boost start) —
  camera swings smoothly to maintain framing.
- **Terrain blocks line-of-sight to the vehicle** (wall,
  `wall_high`, `steep_rock`) — camera swings to a position
  that *can* view the vehicle.

Swing easing is smooth (~0.5 s) so the player reads "the
camera adjusted" rather than "the camera teleported."  Source
of truth: [NUMBERS.md § Camera](../../../docs/NUMBERS.md).
Visible radius is bounded by the atmospheric haze
(SETTING.md § The atmosphere is thick).

## HUD (diegetic + minimal numeric)

**The diegetic principle dominates** (DESIGN.md § Updates:
input philosophy).  Most game state is shown in the world,
not on a HUD layer:

| Information | Where it's read |
|---|---|
| What I'm carrying | Carry object floats above the vehicle/helper (PROXY_ART.md) |
| Tower state (healthy / decayed / boosted) | Tower top colour (red / black / pink) |
| NPC order status | Core's TOP colour (black / red / amber / green / white-flash) |
| Launch countdown | Core's BOTTOM pulse (idle dark / pulsing orange-red, faster as ignition nears) |
| Wave incoming | Active spawn markers pulse during the pre-walk visibility interval |
| Wall outline (pre-construction) | Flat red outline on the painted hex |
| Wall under construction | Wall grows out of the ground; height = progress |
| Stranded helper | Damaged silver-grey cuboid at their wreck hex |

Numeric / state HUD reduced to the bare minimum:

- **Wallet** (points) — single number, screen corner.  The
  one number the player must see to make build decisions.
- **Active palette entry** (editor mode only) — swatch + name
  highlight in the picker.
- **Paint-mode on/off indicator** — **the vehicle body itself
  changes colour**: white when paint mode is off, red-tinted
  near-white when paint mode is on (PROXY_ART.md § Player).
  Visible at any zoom from the over-the-shoulder camera; no
  HUD icon needed.  The appearance of outlines as the player
  drives confirms it.

That's the entire HUD.  No wave-number display, no inter-wave
countdown, no minimap, no carry icon, no boost cooldown bar.
If a missing indicator turns out to be needed during play, add
it then.

## Force-launch UX

| Step | Visual + behaviour | Source |
|---|---|---|
| 1. Player drives through the core's **opening** | The vehicle physically crosses into the core's interior; the opening is the only side the vehicle can pass through | DESIGN.md § Updates: force-launch trigger |
| 2. Bottom pulse activates | Orange-red ring at the base of the cylinder lights up and begins pulsing slowly | PROXY_ART.md § Core § Bottom pulse |
| 3. Countdown elapses | Pulse rate accelerates — slow beat → rapid flutter as ignition nears | (same) |
| 4. White flash | At T = 6 s, the bottom ring flashes white briefly | NUMBERS.md (`6 s` default) |
| 5. Liftoff | The core ascends visually; camera follows up briefly, then fades to the inter-mission screen | (placeholder; settle on fade vs cut in build) |
| Cancel at any time before step 4 | Player drives back out through the opening; pulse fades to dark over ~0.3 s; countdown resets to 0 on next entry | PROXY_ART.md § Core § Bottom pulse |

**Hazard during the countdown:** enemies that reach the core
during steps 2-3 keep nibbling (draining points from the
wallet); the longer the launch takes to fire, the more points
are bled.  This is intentional pressure (DESIGN.md § Updates:
force-launch + core invulnerable).

## Wave UX

1. Wave 1 fires when **EITHER** trigger satisfies:
   - The player has built **N walls** (`wave_1_wall_trigger`,
     default 8), or
   - The player has driven onto a spawn marker that sits
     ≥ `wave_1_provocation_distance` hex from the core
     (default ≥ 12).
2. At wave start, **enemies appear at randomly-picked active
   spawn markers** and **stand visible** for
   `pre_walk_visibility_interval` (default 5 s) before
   beginning to walk along the marker's direction.  Markers
   pulse during this window.
3. Each enemy walks in approach mode until it crosses the
   **scrambler bubble boundary** (`scrambler_bubble_radius`,
   default 25 hex from core).  Inside, it pivots to engage
   mode (flow-field gradient toward the core).
4. Towers in range fire at any enemy with line-of-sight;
   enemies fall; loot drops; helpers auto-collect and deliver
   to the core.
5. Wave clears when its last emitted enemy dies.
6. After `inter_wave_delay` (default 15 s), the next wave
   begins (back to step 2).

## Win / loss / scramble UX

dryopea has **no hard run-loss** (DESIGN.md § Updates).  All
outcomes are forms of "you launched."

| Outcome | Trigger | Visual |
|---|---|---|
| Force-launch (mid-wave) | Player drove into core + countdown elapsed | Standard launch animation; any helpers / loot not aboard left behind |
| Force-launch (after all waves) | Final wave cleared, player chooses to enter the core | "Free scramble" — no more enemies pressuring; player can ferry tops + tour the base at leisure before launching |
| Vehicle destroyed | Blocker-damage HP reaches 0 | Player respawns inside the core (which starts the launch countdown automatically per DESIGN.md § Updates) — drive out to cancel and continue, stay to ratify |
| Final wave cleared (no launch yet) | The wave list runs out with core intact | No banner, no auto-launch.  Just no more enemies; player is in the "free scramble phase" indefinitely |

**Inter-mission state** (briefly): after liftoff, the screen
transitions (cut or fade) back to the map-selection UI.  The
player's persistent wallet (= points carried out) is the
seed for the next mission's starting budget.  Tower-tops
collected stay in inventory but have no effect yet (Q4
deferred).

## Starter map #1 — sketch

A small hand-authored map exercising every system end-to-end.

**Terrain:**
- ~30 × 30 hex playable area
- A central plain (`grass`) the player can build a base on
- A `sand` coast along one edge — exposes the player to easy
  paint variety
- A ridge of `hill` + `rock` along another edge, with a single
  `steep_rock` cliff section — gives a non-buildable barrier
- A small `water` patch (1-2 hex) — tests the no-build-on-water
  rule
- Default sea everywhere else

**Spawn markers (5 placed):**
- 2 markers on the open plain at moderate distance (~15 hex
  from likely landing centre) — wave-1 provocation candidates
- 1 marker behind the hill ridge — forces enemies to path
  around terrain
- 1 marker near the coast — tests another approach axis
- 1 marker close to the centre — gets auto-disabled by the
  `close_spawn_disable_radius`; visible to the player as a
  safety signal

**Wave list:** `examples/waves.json` as-is (5, 8, 12, 20, 30,
50, 80 — 7 waves).

**Objective:** `survive_waves` (the default; clear all waves
with core alive).

**Authoring discipline:** terrain is intentionally simple
(easy to read at a glance, no hidden complexity).  The map's
job is to exercise the *systems*; it doesn't need to be
fun *yet*.  Authoring will surface implicit assumptions the
design hasn't made — capture them as P-issues or DESIGN.md
amendments.

## Explicit scope cuts (out of validation)

The validation explicitly **does not include**:

- **Bosses** (phase 3 — see PROXY_ART.md § Boss enemy)
- **Insects + sap** (deferred; can appear visually as passive
  wildlife in the map if useful, but mechanics deferred)
- **Elementals + gems** (deferred)
- **Tower variants** (only placeholder laser tower)
- **Robot diversity** (all enemies identical magenta cuboids)
- **Helper skills** (helpers interchangeable)
- **Tower-top carryover effect** (mechanic carries; effect
  TBD per Q4)
- **Stranded-helper persistence** (data state only; no UI for
  rescue quests)
- **Sap harvesting, gem collection**
- **Multi-mission run state beyond wallet** (no second
  mission, no inventory beyond wallet)
- **Orbital banking, planet meta, multiplayer, abandoned
  bases** (all future)
- **Multiple maps** (validation ships one starter map)
- **Polish: sound, animations beyond construction-rise,
  particles, UI niceties** (all deferred)

These are tracked future-design items, not bugs to fix during
validation.

## Success criteria

Validation **passes** when:

1. **A cold player can play one base end-to-end.**  Pick a
   map, pick a landing spot, paint walls, see waves arrive,
   defend with towers, salvage tops, scramble out.  ~15-30
   minutes per session.
2. **The loop visibly works.**  Every system shows
   recognisable behaviour:
   - Walls grow out of the ground (construction visual).
   - Enemies pulse at spawn before walking.
   - Towers fire and decay (red → black).
   - Loot drops are picked up by helpers and delivered.
   - The core's bottom pulses during launch hold.
   - Carry objects float above the player / helpers.
3. **Mechanics surface no contradictions in 30 minutes of
   play.**  If contradictions arise, file as issues — the
   design needs amendment, not the validation.
4. **Numbers feel approximately right.**  Document which
   feel wrong (too fast / too slow / too tight / too loose)
   — those are inputs to the next tuning pass, not blockers.

Failure modes (= validation NOT passing):

- The wave triggers don't work (waves never start, or start
  unpredictably).
- The economy doesn't loop (player runs out of points without
  recovery path; or has so many points the choice is trivial).
- The launch UX is unreadable (player can't tell when to
  enter the core, when it's about to fire, how to cancel).
- The wall-paint trail is confusing (paints when not wanted,
  doesn't paint when wanted).

## Implementation milestones

Plan 05 is integration, not a system layer.  Its
"milestones" are checkpoints across the OTHER plans' phases —
when each milestone passes, a chunk of the validation
scenario is alive.  Each milestone produces a testable
artefact.

### Milestone M-S1 — Editor is paintable + persistent

**Depends on:** plan 01 E1-E4 complete.

**Test artefact:** `tests/scripts/05_ms1_paint_persist.loft`

```loft
// Cold start, paint, save, restart, load, verify.
let palette = load_palette("examples/palette.json")
let world = hash<u8[integer]>::new()
paint(&mut world, 0, 0, 5)
paint(&mut world, 1, 0, 9)
save_map(&world, &Camera{q:0,r:0,zoom:1}, "/tmp/m_s1.json")
let (w2, _) = load_map("/tmp/m_s1.json", &palette)
assert lookup(&w2, 0, 0) == 5
assert lookup(&w2, 1, 0) == 9
```

**Pass criteria.** A person can paint terrain in the editor,
save, restart, load.  No combat yet.

### Milestone M-S2 — Spawn markers placeable + the wave engine spins

**Depends on:** plan 01 done + plan 03 M1-M5 complete.

**Test artefact:** `tests/scripts/05_ms2_markers_waves.loft`

```loft
// Author a tiny map with one core position + 2 spawn markers;
// trigger wave 1; assert enemies appear at marker positions.
let (world, markers, _) = load_map_with_markers("tests/data/tiny.json", &palette)
let core = Hex { q: 0, r: 0 }
let state = WaveState::new()
spawn_wave(&mut state, 5, &markers, core)
assert state.alive.len() == 5
// Each enemy on an active marker hex
for e in state.alive {
    assert markers.lookup(pack(e.pos.q, e.pos.r)) != None
}
```

**Pass criteria.** Spawn markers (placed in the editor) emit
enemies at wave start; markers within the auto-disable
radius produce no enemies.

### Milestone M-S3 — Starter map exists + loads from index

**Depends on:** plan 04 L1-L4 complete (at least
`starter_01.json`).

**Test artefact:** `tests/scripts/05_ms3_starter_map.loft`

```loft
let idx = load_index("maps/index.json")
assert idx.maps.len() >= 1
let m = load_map_file("maps/starter_01.json", &palette)
assert m.objective == "survive_waves"
assert m.markers.len() >= 4
assert m.waves.len() >= 1
```

**Plus human test:** launch the game, see the static planet
view, click the marker for starter_01, confirm landing-spot
pick UI appears.

### Milestone M-S4 — Landing flow works end-to-end

**Depends on:** plan 04 + the landing algorithm.

**Test artefact:** `tests/scripts/05_ms4_landing.loft`

```loft
let m = load_map_file("maps/starter_01.json", &palette)
let pick = Hex { q: 0, r: 0 }   // player picked centre

let outcome = simulate_landing(&m, pick)
assert outcome.core_pos != None                 // landed somewhere
assert outcome.core_rotation in 0..6            // random rotation valid
assert outcome.starter_tower_pos != None        // separate lander touched down
assert outcome.helpers_emerged == 2             // 2 starting helpers
assert outcome.wallet == 200                    // starting budget
```

**Pass criteria.** From cold start through landing, the game
state is `{core landed, starter tower placed, 2 helpers,
200 pts, wave 1 not yet triggered}`.

### Milestone M-S5 — Wave 1 triggers + plays out

**Depends on:** plans 01 + 03 + 04 + the wave engine + the
tower engine.

**Test artefact:** `tests/scripts/05_ms5_wave1.loft`

```loft
let game = start_mission(&load_map_file("maps/starter_01.json", &palette))
// Player builds 8 walls (simulate paint)
for i in 0..8 { game.paint_wall(i, 5) }
// Helpers construct them (advance time)
game.advance_time(80.0)   // ample for 2 helpers on 8 walls @ 10s each

assert game.wave_state == Active(wave_index=0)   // wave 1 triggered
// Run the wave for its natural duration
game.advance_time(60.0)   // long enough for the 5 enemies to walk + die or arrive
assert game.wave_state in [Active(_), Cleared(0), Failed(_)]
```

**Pass criteria.** 8 walls triggers wave 1; enemies appear at
active markers and walk toward the core; the starter tower
shoots them.  Wave 1 either clears or the player has a
visible loss-of-points pressure.

### Milestone M-S6 — Force-launch works (the exit)

**Depends on:** all prior milestones + force-launch UX.

**Test artefact:** `tests/scripts/05_ms6_launch.loft`

```loft
let game = start_mission(&load_map_file("maps/starter_01.json", &palette))
game.advance_time(60.0)   // some play

// Player drives into the core's opening
game.drive_vehicle_into_core()
assert game.launch_countdown_active

// Wait the countdown
game.advance_time(6.0)
assert game.launched
assert game.next_mission_ready
assert game.next_mission_wallet_seed > 0  // some unspent points carried
```

**Pass criteria.** Driving into the opening starts the
countdown (bottom pulse); 6 seconds later, the rocket
launches; the game transitions to a "next mission" state
(returns to the planet view for the next pick).

### Milestone M-S7 — Whole loop, 30 minutes of cold-play

**Depends on:** M-S1 through M-S6.

**No automated test.**  A human picks the game up, plays
through one base end-to-end without instructions, and reports
back.  This is the actual validation criterion (see § Success
criteria above).

If M-S7 passes, **plan 05 is done** — validation milestone
shipped.

## Open questions

Almost none — the design has already settled the things plan
05 would need to decide.  A small residue:

1. **Liftoff visual transition.**  Camera follows the rocket
   up + fade to inter-mission UI, or a clean cut?  Settle in
   build; the diegetic-camera follow is the leaning default.

That's it.  Everything else routes back to the existing
source-of-truth docs (paint-mode indication = vehicle body
colour-tint, settled 2026-05-26; camera = over-the-shoulder
locked-pose with auto-reframe, settled 2026-05-26).

## Dependencies

- **Plan 01** ground editor — terrain painting; the editor's
  paint mechanism is also the runtime wall-paint mechanism.
- **Plan 03** marker layer + spawns — spawn placement and
  wave-firing mechanism.
- **Plan 04** map library — file format, selection UI,
  per-map objective + waves field.
- **Plan 02 (optional during validation)** — the
  dual-layer 3D viewer.  Not required to ship validation;
  useful for debugging terrain solver later.
- **loft `lib/graphics`** GL bindings — already shipped.
- The full design corpus
  ([`DESIGN.md`](../../../docs/DESIGN.md),
  [`SETTING.md`](../../../docs/SETTING.md),
  [`GROUND_TYPES.md`](../../../docs/GROUND_TYPES.md),
  [`PROXY_ART.md`](../../../docs/PROXY_ART.md),
  [`NUMBERS.md`](../../../docs/NUMBERS.md)) — referenced
  throughout.

## See also

- [`../01-ground-editor/README.md`](../01-ground-editor/README.md) — the editor + painted-layer foundation
- [`../02-solver-validation-viewer/README.md`](../02-solver-validation-viewer/README.md) — companion debug viewer
- [`../03-marker-layer-and-spawns/README.md`](../03-marker-layer-and-spawns/README.md) — spawn + wave mechanism
- [`../04-map-library/README.md`](../04-map-library/README.md) — map file format + selection UI
- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md) — every Updates 2026-05-26 bullet plan 05 consolidates
- [`../../../docs/NUMBERS.md`](../../../docs/NUMBERS.md) — every tunable referenced
