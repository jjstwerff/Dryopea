<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea — design history

This file preserves design seed material from the **2023-era
prototype** (the original `Dryopea` repo, now private at
[`jjstwerff/dryopea-archive`](https://github.com/jjstwerff/dryopea-archive)).

The canonical current design lives in [`DESIGN.md`](DESIGN.md);
this document is here so the lineage of ideas is visible — many of
@PLAN46's design choices (scramble-and-salvage, tactical retreat as
core, hex grids with slope-typed materials, multi-layer pathing)
were already drafted in 2023, three years before they were written
up formally. Keeping the trail intact helps future readers see what
was decided and re-decided.

---

## 1. Original 2023 README (game-design paragraphs)

> *Source: README.md of the original `Dryopea` repo, 2023-02-19.
> Reproduced here verbatim. Engine / Rust / language-development
> sections (shell prompts, git-branch tooling, monthly language
> releases, etc.) have been dropped — they belonged to what is now
> the loft project and are obsolete here.*

```
Game development on Dryopea rogue AI

This is an example open-source game for the development of the underlying
game engine.  The goal is a 3d game that fully runs inside a browser or
as a standalone Vulcan executable.

It will feature an overarching tactical campaign with relatively short
missions to the planet's surface.  Each mission will have a randomly
chosen set of, potentially secret, objectives and a time limit due to
increasingly hard encounters.  At any moment the player can decide to
break off the mission and try to salvage as much as possible before
leaving.

When the final encounter is cleared, the mission is also over and the
player can choose to keep their base occupied but relatively dormant on
the planet.  It can then still produce and hinder opposing factions.
It will be possible to defend this base in a future mission against
renewed attacks.

It should be greatly extensible with a quality editor for rapid
prototyping.  This editor can edit full maps or individual assets used
on these maps.  It will be able to load glb files for more in depth
assets and animations but should be fully functional without it.

The game will eventually provide multiplayer support for both
collaborative or competitive missions.
```

**What survived into @PLAN46:**
- "Break off the mission and try to salvage as much as possible" →
  the **scramble phase** (now the signature mechanic).
- "Keep their base occupied but relatively dormant on the planet…
  defend this base in a future mission" → the **planet-scale
  persistent abandoned bases** in DESIGN.md § Future expansion.
- "Tactical campaign with relatively short missions" → the
  **bounded-session / one-sitting** design principle.
- "Multiplayer support for both collaborative or competitive
  missions" → DESIGN.md § Future expansion (planet-scale
  multiplayer over `lib/server` + `lib/web`).
- "A quality editor for rapid prototyping" → the **in-game
  ground-type editor** (now [plan 01](../plans/01-ground-editor/README.md)).

**What did NOT survive:**
- "Vulcan executable" — Vulkan was never used; loft's render path
  is WebGL/WebGL2 + GL. WASM-in-browser remains a goal; the native
  desktop path uses GL, not Vulkan.
- "Editor edits full maps or individual assets including glb
  files" — too broad. DESIGN.md narrows the editor's scope to
  terrain + small markers (per the editor/game split).

---

## 2. Hex / terrain design notes (2023)

> *Source: bottom section of the `todo` file in the original repo
> (everything before this section was loft-engine development
> notes and has been dropped).*

```
click on hex grid: assume vertical view
    x,y to correct hex position & relative position on the hex
    dragging for multiple hexes selection & rotate the camera

sampling png onto hex grid: specific point & 4 pixels around it
    hex center into exact position on the png-grid
    starting with rectangle that approximates the hex
    get the pixels with their share of the tile (potentially more than 4)

draw coast lines:
    walls at 15 degrees based on rough shape of hexes
    (step = 60, 2 steps = 30)
    start with middles of sides towards the sea
    path: (x,y) dir & steps (turn left/right)
    match longest pattern first
    (sets: round30, round15, circles3, circles6, flats30, flats15)
    points (x,y,z) triangles materials

general slopes (flats, gradual, hills, mountain)

water flow, amount of water
    random direction per tile
    breaking circles from the coast with flipping

tree/plant growth
steep sides into rock faces
lakes as non-connected seas
roofs / walls / roads / rails (flatten)

editing height and open terrain
    show pre-build items & place them with position and rotation
    clicking including height and multiple layers

shortest path
    on single layer
    connections between layers
    width of room versus vehicle
    maximum slope (different materials)
    maximum water depth to pass (wading, animals, vehicle)
    following water for boats (ocean, sea, river, lake)
```

**What survived into @PLAN46 / lib-plan 19+20:**
- "General slopes (flats, gradual, hills, mountain)" → lib-plan
  20's `md_slope` (per-material slope value); same idea, formalised
  as a multi-source Dijkstra solver.
- "Editing height and open terrain… clicking including height and
  multiple layers" → the editor/game split + the `cy`-layer model
  in lib-plan 19 / 20.
- "Shortest path… max slope (different materials), max water depth
  (wading, animals, vehicle)" → the **multi-level pathing graph**
  in DESIGN.md system #4, with per-agent-type traversal rules.
- "Steep sides into rock faces" → gridmesh Phase C T4 auto
  slope-faces.
- "Connections between layers" → bridge `cy`-layer decks
  (DESIGN.md system #3, system #4).

**What did NOT survive (yet):**
- "Sampling png onto hex grid" — image-as-input terrain importer.
  Lib-plan 20 uses *painted* ground types (palette + drainage
  seed), not a sampled image. The PNG sampler may return as a
  back-door bulk-import tool but is not core.
- "Coast lines with walls at 15-degree increments" — the
  curved-coast aesthetic. Current lib-plan 19 (T4) uses
  axis-aligned slope faces; sub-hex curvature is deferred.
- "Tree / plant growth" — vegetation simulation. Not in scope for
  the core dryopea game; sea-default world means flora is a later
  addition.

---

## 3. Game data schema (2023, `world.gcp`)

The original repo's `code/overland/world.gcp` defined classes for:

- `Mission { name, description, prerequisite[Item], specials[Item] }`
- `Statistic` enum (skills: boost/build/combat/drive/hack/mine/
  operate/repair/scout/scrounge/social/stealth; statistics:
  agility/charisma/observe/plan/stamina/tinker; unit:
  armor/assembly/bulk/efficiency/flammable/handling/hits/
  isolation/max_speed/resilience/storage/value/weight; weapon:
  acid/bludgeon/cold/cutting/emp/falloff/flaming/flash/lightning/
  piercing/poison/range; state: damage/direction/primed/speed/wear)
- `Faction` enum (spacers, economy, natives, shaman, robots, world,
  oceanic, ancient, aliens)
- `Item { name, type, description, statistics[Stat] }` with
  `ItemType` (knowledge, background, class, drug, upgrade, tower,
  vehicle, building, machine, human, robot, animal, weapon, ammo,
  material, good, fluid)
- `Construct: Item { production[Produce] }`
- `Machine: Construct { fuel[Cost] }`
- `Building: Construct { production[Produce] }`
- `BuildQueue { item, priority, towards: Actual }`
- `Link { to, type: LinkType }` with `LinkType` (pipe, pipes,
  electric, laser, attached, road, path, air, transport)

The full file is preserved at [`../archive/world.gcp`](../archive/world.gcp);
example data at [`../archive/gameplay.data`](../archive/gameplay.data)
(31 KB of filled-in factions / items / missions).

**Relevance.** This is direct foundation material for D4 (economy
/ exploration) and D5 (scramble + run meta): the salvageable
component types, the production / cost graph, and the link
topology between buildings. When D4 starts, mine this schema
first.

---

## See also

- [`../archive/`](../archive/) — preserved 2023 prototype files
  (proto-loft `.gcp`, partial `world.loft`, gameplay/terrain
  data).
- [`../examples/terrain.txt`](../examples/terrain.txt) — the 2023
  ground-type palette (grass / hill / mountain / sea / sand /
  forest with slope values) — directly seeds plan 01.
- [`../examples/map.png`](../examples/map.png) +
  [`../examples/map.xcf`](../examples/map.xcf) — the 2023 map art
  (PNG + GIMP source).
- [`DESIGN.md`](DESIGN.md) — current canonical design.
