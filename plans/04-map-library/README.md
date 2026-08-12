<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 04 — Map library + per-map objectives

**Status:** Future (design drafted 2026-05-26; no code).

## Goal

A **curated set of hand-authored maps** the player picks from
before each base.  Every map is hand-made in the editor
(plans 01 + 03) and ships as a content file in the repo; **no
procedural / auto-generation** is in scope, ever — content is
hand-crafted.

Maps can carry:

- **Terrain** (plan 01's painted ground layer).
- **Spawn markers** (plan 03's marker layer — multi-direction
  spawn points authored on the map).
- **Per-map metadata** — name, description, intended
  difficulty.
- **Per-map objectives** — eventually unique per map (validation
  tier defaults to "survive the wave list").
- **Per-map events with variance** — see § Variability below.

The library is the *curated* face of dryopea's content.  Players
choose **which fight** they want; each map answers that with
unique features and a unique objective.

## Two load-bearing design choices

### 1. No auto-generation, ever

Every map is authored by hand.  This is a permanent design
constraint, not a temporary scope cut.  Reasons:

- Hand-authoring is the dogfood loop: each map exercises the
  editor (plans 01 + 03) and surfaces gaps in the authoring UX
  that auto-generation would never surface.
- Curated maps carry **intent**.  A designer composes terrain +
  spawn directions + objective to tell a story.  Procedural
  output can produce variety but not voice.
- Variety per replay is supplied by the **event variance**
  mechanism below — position pools + probabilities — not by
  re-rolling the whole map.  That's the right granularity:
  same shape of fight, different details.

### 2. Events vary per play, even on the same map

A map's content is **stable** (terrain, spawn markers,
objective) but its **events** can vary across plays of the same
map:

- An event lives in a **position pool** of 1-N candidate hexes
  on the map; per play, the runtime picks one (or none) of
  those at random.
- An event has a **probability of occurring at all** (e.g. 60%
  per play); on the unlucky rolls, that event simply doesn't
  fire.

So the same map plays slightly differently each time without
the authoring effort exploding.  The author picks **what could
happen and where it could happen**; the runtime picks **which
of those actually does this time.**

This is the same structural pattern as plan 03's per-enemy
random spawn-marker selection (random pick from an authored
set), now applied to events at the map scale.

## Scope (validation tier)

In:
- A `maps/` directory in the repo holding hand-authored map
  files (JSON or similar; format consolidates plan 01 + plan
  03 layers).
- A `maps/index.json` listing available maps with name,
  description, optional difficulty tag.
- A **map-selection UI** in-game: before a base starts, the
  player sees the list of maps + descriptions and picks one.
- After the player picks: continue with the existing flow
  (plan 03 spawn markers loaded; core lands per DESIGN.md's
  area-pick + random-within rule; starting budget + 2 helpers;
  wave 1 begins).
- Per-map metadata block in each map file: `name`,
  `description`, `difficulty`, `objective` (validation
  default: `"survive_waves"`).
- A small starter set: 2-4 hand-authored maps that exercise the
  full system end-to-end.

Out of scope (deferred to later plans):
- **Unique objectives beyond "survive wave list."**  The
  validation set uses the default; objective *variants* (hold
  for time, escort, reach-a-point, destroy-an-objective) land
  later — see § Objectives variants below.
- **Event variance authoring + runtime.**  Captured here as a
  forward design intent (see § Variability below); not built
  in the validation tier.
- **Map sharing between players, the workshop, modding, online
  libraries.**  Future.
- **Map thumbnail rendering / previews in the selection UI.**
  Validation tier shows name + text description; thumbnails
  later.
- **Procedural / auto-generated maps.**  Out of scope
  permanently per § "No auto-generation" above.

## Phases

| # | Scope | Proves |
|---|---|---|
| **L1** | **Map file format.** Consolidate the plan 01 painted layer + plan 03 marker layer + per-map metadata block (name / description / difficulty / objective) into a single JSON map file in `maps/`. | The on-disk shape; round-trip with the editor. |
| **L2** | **Map index.** `maps/index.json` lists available maps (name + description + optional difficulty); the game reads it at startup. | The library catalog. |
| **L3** | **Map-selection UI — static planet view with markers.** At base-start, show a still planet (placeholder sphere / low-poly globe) with **one clickable marker per indexed map** (positions hand-placed in the index file).  Clicking a marker is equivalent to "selecting that map."  Sets the visual tone of the eventual diegetic rotating-planet hub (SETTING.md § Future UX) before any meta-game state is implemented.  After selection, proceed to the area-pick → core-landing → wave-1 flow. | The library face + the teaser of the future hub. |
| **L4** | **Starter map set.** Hand-author 2-4 maps that exercise the whole system: open terrain + rivers + a perimeter setup with 3+ spawn markers from different directions. | The library has content. |
| **L5** | **(deferred — placeholder)** Per-map objective variants: alternate goal conditions (hold N minutes, destroy specific target, escort, reach point).  Land when the validation default has proven sound. | Objective system extensibility. |
| **L6** | **(deferred — placeholder)** Per-map event variance: position pools + per-event probability; runtime random selection at base start.  See § Variability. | Replay variance without auto-gen. |

## Implementation + testing

### Phase L1 — Map file format

**One format, used by editor + game.**  The editor's save
(plan 01 § E4) and the game's mission load both use the
**same `MapFile` schema** defined here.  Early-phase editor
saves populate only the fields they touch (`ground` +
`camera` for plan 01 E4); later phases fill in `markers`
(plan 03 M1), `waves` + `objective` + `play_area` (plan 04
L1).  Missing fields default; unknown fields ignore.  No
parallel "editor save format" exists.

**Files**

| File | Purpose |
|---|---|
| `src/map_file.loft` | `MapFile` struct + JSON loader / saver.  Shared with plan 01's E4 save path. |
| `maps/` | Directory holding authored map files. |

**Format — `maps/<name>.json`**

```json
{
  "version": 1,
  "name": "starter_01",
  "description": "Coast and hills with three approaches",
  "difficulty": "easy",
  "objective": "survive_waves",
  "play_area": { "min": [-15, -15], "max": [15, 15] },
  "ground": [
    { "q": 0, "r": 0, "type": "grass" },
    { "q": 0, "r": 1, "type": "grass" },
    …
  ],
  "markers": [
    { "kind": "spawn", "q": 10, "r": 0, "direction": 3 },
    …
  ],
  "waves": [5, 8, 12, 20, 30, 50, 80],
  "inter_wave_delay_seconds": 15
}
```

**Key functions**

- `fn load_map_file(path: text, palette: &vector<GroundType>) -> MapFile`
- `fn save_map_file(m: &MapFile, path: text)` — sorted entries
  for deterministic output.

**Test — `tests/scripts/04_l1_format.loft`**

```loft
let palette = load_palette("examples/palette.json")

// Build a tiny map programmatically
let mf = MapFile {
    version: 1,
    name: "test",
    description: "tiny",
    difficulty: "easy",
    objective: "survive_waves",
    play_area: PlayArea { min: Hex{q:-5,r:-5}, max: Hex{q:5,r:5} },
    ground: [GroundEntry { q: 0, r: 0, type_name: "grass" }],
    markers: [MarkerEntry { kind: "spawn", q: 3, r: 0, direction: 0 }],
    waves: [3, 5, 8],
    inter_wave_delay_seconds: 15,
}

save_map_file(&mf, "/tmp/dryo_test.json")
let mf2 = load_map_file("/tmp/dryo_test.json", &palette)

assert mf2.name == "test"
assert mf2.ground.len() == 1
assert mf2.markers.len() == 1
assert mf2.waves.len() == 3
```

**Pass criteria.** Map round-trips; the existing plan-01 +
plan-03 save formats fold into the consolidated MapFile
without regression.

### Phase L2 — Map index

**Files**

| File | Purpose |
|---|---|
| `maps/index.json` | List of available maps with positional metadata for the planet view. |
| `src/library.loft` | Loader for the index. |

**Format — `maps/index.json`**

```json
{
  "version": 1,
  "maps": [
    {
      "name": "starter_01",
      "file": "starter_01.json",
      "description": "Coast and hills",
      "difficulty": "easy",
      "planet_marker": { "lat": 12.3, "lon": -45.6 }
    }
  ]
}
```

`planet_marker` is the {latitude, longitude} on the placeholder
planet sphere where the marker is drawn.

**Test — `tests/scripts/04_l2_index.loft`**

```loft
let idx = load_index("maps/index.json")
assert idx.maps.len() >= 1
for entry in idx.maps {
    // The referenced map file must exist + load
    let m = load_map_file("maps/" + entry.file, &palette)
    assert m.name == entry.name
}
```

**Pass criteria.** The index is loadable; every listed map
file exists and matches its index entry.

### Phase L3 — Static planet-view selection UI

**Files**

| File | Purpose |
|---|---|
| `src/planet_view.loft` | Renders a placeholder sphere + markers; handles clicks. |
| `src/main.loft` | Entry path: launch → planet view → click marker → load map → editor / play. |

**Key functions**

- `fn render_planet(idx: &MapIndex)` — placeholder sphere
  geometry (low-poly globe) + a marker per indexed map at its
  (lat, lon).
- `fn screen_to_marker(idx: &MapIndex, screen_xy: (integer, integer)) -> Option<text>`
  — return the clicked marker's map name, or None.

**Test — `tests/scripts/04_l3_planet.loft`**

Mostly human / visual: launch the game, see the planet, see
one marker per indexed map, click one → game proceeds to
landing-spot pick on that map.

Programmatic test:

```loft
let idx = load_index("maps/index.json")
// Synthesise a click at the screen position the first marker
// would render at:
let sxy = marker_screen_position(&idx, 0)
let result = screen_to_marker(&idx, sxy)
assert result == Some(idx.maps[0].name)
```

**Pass criteria.** The planet view is the cold-start screen
(replacing any abstract menu); markers are clickable and
route to the right map.

### Phase L4 — Starter map set (the content)

**Files**

| File | Purpose |
|---|---|
| `maps/starter_01.json` | First hand-authored map.  ~30×30 hex play area; central plain (grass) + coast (sand) + hill+rock ridge with one steep_rock cliff + a small water patch.  5 spawn markers (2 moderate plain, 1 behind ridge, 1 coastal, 1 close to centre — the close one will be auto-disabled at landing).  Waves `[5,8,12,20,30,50,80]`.  Objective `survive_waves`. |
| `maps/starter_02.json` (optional) | Second map varying terrain shape + marker layout. |
| `maps/starter_03.json` (optional) | Third map. |
| `maps/index.json` | Lists the maps with their planet positions. |

**Authoring workflow.**

1. Run the editor (plan 01 + 03 must already pass).
2. Paint terrain hex by hex.
3. Switch to marker mode, place spawn markers + rotate
   directions.
4. Save (plan 01 E4 / plan 03 M1 save format).
5. Move the resulting JSON to `maps/`.
6. Add an entry to `maps/index.json` with a planet position
   chosen to feel sensible on the placeholder globe.

**Test — `tests/scripts/04_l4_starter_maps.loft`**

```loft
let palette = load_palette("examples/palette.json")
let idx = load_index("maps/index.json")
assert idx.maps.len() >= 1  // at least starter_01

let starter = load_map_file("maps/starter_01.json", &palette)
assert starter.ground.len() >= 50          // non-trivial painted area
assert starter.markers.len() >= 4          // enough markers (close-disable headroom)
// At least one marker > 12 hex from origin (provocation trigger reachable)
let any_far = starter.markers.iter().any(|m| (m.q.abs() + m.r.abs()) >= 12)
assert any_far
```

**Pass criteria.** At least one playable starter map exists;
the index lists it; the planet view shows its marker; the
full cold-start → landing → play flow runs through it.

### Phases L5 + L6 — deferred

Out of validation scope.  Stub specs:

- **L5 Objective variants.**  Extend the `objective` field
  in MapFile to support `hold_time` / `reach_point` /
  `destroy_target` / etc.  Each variant carries its own
  parameter block in the map file.  Win condition becomes
  per-objective.
- **L6 Event variance.**  Extend MapFile with an `events`
  array (per-event probability + position pool + trigger).
  Runtime rolls at base start; firing scheduled by trigger
  condition.

Ship after the validation tier proves the loop.

## Objectives — variants (deferred)

The validation default is **`survive_waves`**: clear the
authored wave list with the core alive.  Future objective
variants (each is its own short authoring sub-spec):

| Tag | Goal | What the player sees |
|---|---|---|
| `survive_waves` | Clear the wave list with core alive | (default; same as today's design) |
| `hold_time` | Keep core alive for N minutes — waves keep coming, no clear condition by count | Timer counts up; survival is the win |
| `reach_point` | Drive the vehicle to a target hex | A goal marker visible on the map |
| `destroy_target` | Tower-kill a specific enemy structure (boss / building) | The target structure marked on the HUD |
| `escort` | Bring an NPC / item from A to B | The escort visible as a follow target |
| `defend_extras` | Defend the core AND a secondary structure | Two HP bars on HUD |

A map's `objective` field selects one of these; later, maps can
carry parametric data (`hold_time: 600` seconds, `target_hex:
[12, -4]`, etc.).  All variants share the same scramble exit
(force-launch by entering the core).

## Variability — events with position pools + probability
(deferred)

The forward design intent — events that vary per play of the
same map.  Captured here so the map file format leaves room.

A map's `events` field is a list of event specs.  Each spec has:

```json
{
  "name": "boss_arrives",
  "kind": "spawn_boss",
  "probability": 0.7,
  "position_pool": [[15, -3], [18, -5], [12, -1]],
  "trigger": { "wave": 6 }
}
```

At base start, the runtime walks the events list:

- Roll `probability` — if it fails, the event doesn't happen
  this play.
- If it passes, pick a hex at random from `position_pool`.
- The event fires when its `trigger` condition is met (wave N
  reached, time T elapsed, hex H reached, etc.).

Effect: the same map authored with two boss-arrival pool hexes
and a 70% probability produces FOUR different feel-flavours
across replays — three with the boss at different hexes, one
without it at all.

Author cost: pick the pool, set the probability.  Runtime cost:
one RNG roll per event at base start.  Replay cost: a fresh
session feels meaningfully different even on the same map,
without straying from the authored intent.

**Out of scope** for L1-L4 (validation tier).  L6 adds it.

## Authoring philosophy — early maps teach, hidden depth rewards

Long-term, **all maps converge on a similar baseline difficulty**
(balanced so picking any map is a fair fight at its difficulty
tier).  But the **first maps the player encounters** carry an
extra burden — they have to teach the game.  Design discipline
for the early set:

- **A guided main road for new players.**  The painted terrain
  + spawn marker layout naturally funnels enemies down a
  visible path; the player learns walls → entrances → towers
  → repair → boost → salvage → launch by following the obvious
  route.  Failure modes are forgiving (the wave list ramps
  gently, the starting budget covers a basic perimeter).
- **Hidden challenges off the main path for advanced players
  to find.**  Extra spawn markers tucked behind terrain (an
  ambush from an unexpected direction), optional secondary
  threats triggered by exploration (a side path leading to a
  bonus mini-boss), pickups in out-of-the-way hexes (extra
  loot, an unlocked tower-top, a rare resource).  Nothing
  on-rails; discovery is the reward.

The same map authoring tools cover both — the "hidden" content
is just normal map content placed off the obvious path, often
tied to the event-variance mechanism (§ Variability — hidden
events with low probability and out-of-the-way position pools
read as "secrets" without any new system).

Later maps in the curated set don't need the guidance scaffold;
they assume the player knows the loop and lean harder into
their unique objectives / features.  But every map stays
**hand-authored** — no auto-gen, no procedural difficulty
curve, just an author choosing what the next fight feels like.

Hidden objectives (a possible later direction): a map's
`objective` field could carry a **visible main objective** AND
a list of **hidden sub-objectives** the player only discovers
by attempting them.  Out of scope for L1-L4; flagged so the
objective system leaves the door open.

## Dependencies

- **Plan 01 — In-game ground-type editor** — the painted-layer
  authoring; map files consume this layer.  L1 depends on plan
  01 E4 (save format).
- **Plan 03 — Marker layer + spawns** — the spawn-marker layer
  authoring; map files consume this layer.  L1 depends on plan
  03 M1 (data layer) and M3 (save / load).
- **No upstream lib-plan dependency.**  Plan 04 is pure dryopea
  content + UI.

## Why this plan

- **The validation scenario needs a map.**  A "first playable"
  base loads a specific terrain + spawn marker layout —
  i.e., a map file.  Plan 04 defines that file format and the
  selection in front of it.
- **The library shape is small but load-bearing for replay
  value.**  The hand-authored discipline + the event-variance
  intent together carry the "every base feels different"
  promise without resorting to auto-gen — which the user has
  ruled out as a permanent constraint.
- **Per-map objectives are how dryopea grows after
  validation.**  Once "survive waves on map A" is proven fun,
  swapping in "hold for 10 minutes on map A" or "destroy the
  enemy supply depot on map B" extends content without
  re-engineering the loop.

## Open questions

1. **Map file extension.**  `.json` (universal, slow to write
   by hand)? `.loft` literal struct (matches the language)?
   `.dmap` (custom)?  Lean **JSON** for L1 (cheap, tooling-
   friendly); revisit if hand-editing becomes common.
2. **Map size bounds.**  Sparse / infinite per plan 01, but a
   map's painted layer + spawns describe a bounded *intended*
   play area.  Should the map carry an explicit playable-area
   bounding box?  Lean **yes** — author specifies the
   rectangle of hexes that matter; the camera frames it on
   load.
3. **Player's "starting LOCATION" choice on a map.**  DESIGN.md
   § Updates says the player picks a starting area, then the
   core lands randomly within ~3 hexes.  On a single map, is
   the starting area FIXED (author specifies it)?  Or does the
   PLAYER pick a hex on the map?  Lean **author-specifies**
   for L1-L4 — keeps the validation flow short.  Player-pick
   lands later.
4. **Difficulty tag granularity.**  Three tiers (easy / normal
   / hard) or a numeric scale?  Lean **three tiers** for
   simplicity.
5. **Index file vs filesystem scan.**  `maps/index.json` is
   explicit but needs maintenance.  Scanning `maps/*.json` at
   startup is automatic but loses curation ordering.  Lean
   **explicit index** — the library is curated; the index
   carries the order the author wants players to see.
6. **Concurrent map editing.**  Out of scope.  The editor's
   workflow assumes one map at a time.

## See also

- [`../01-ground-editor/README.md`](../01-ground-editor/README.md)
  — terrain authoring; map files consume this layer.
- [`../03-marker-layer-and-spawns/README.md`](../03-marker-layer-and-spawns/README.md)
  — spawn marker authoring; map files consume this layer.
- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md)
  § Updates 2026-05-26 — area-pick + random-within core
  landing; starter budget + helper roster; force-launch
  exit.
