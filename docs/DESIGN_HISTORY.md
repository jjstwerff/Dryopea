<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea — design history

This file preserves **design seed material that predates the plans** —
the 2023-era prototype (the original `Dryopea` repo, now private at
[`jjstwerff/dryopea-archive`](https://github.com/jjstwerff/dryopea-archive))
in §§ 1-3, and the owner's later idea dump in §§ 4-5.

⚠ **Each section is a ROUTING TABLE, not a design.**  The raw sources
live in [`../archive/`](../archive/) and are never edited; what follows
says, block by block, **what was adopted and into which document, what
was already canon, what CONTRADICTS canon, and what was refused with the
reason.**  A seed idea rewritten here would be a second copy of a live
design, which is the copy that drifts.

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

## 4. The owner's seed notes (2026-08-26 hand-over)

> *Source: [`../archive/seed-notes.md`](../archive/seed-notes.md),
> preserved verbatim.  Supplied as "my (older) ideas about the game" —
> pre-dates `DESIGN.md`, `SETTING.md` and every plan.*

### 4a. Adopted — where each block landed

| Seed block | Landed in | ⚠ What it added that was genuinely missing |
|---|---|---|
| **Opening** (the Linn Everett / Ian Thorne call) | [`SETTING.md`](SETTING.md) § The recruitment | ⚠⚠ **The justification for the whole game.**  *"very little communication with the surface … so there has to be personnel below to service the different systems"* — the jammer that makes robots beatable is what forbids remote operation.  Also the company, both names, and salvage-as-pay |
| **Lore** — aggressive pollen, hostile to humans AND machines | [`SETTING.md`](SETTING.md) § The pollen | ⚠⚠ the environmental CAUSE of four systems designed separately: the robots' repair economy, why an approaching robot reads as helpful, the wrecks nobody killed, and why a base needs upkeep |
| **Lore** — tree biology (horizontal trunks, hearts, spikes, acid pools, sap→concrete/batteries, ants with alarm odour sacs) | [`SETTING.md`](SETTING.md) § What a tree IS + § The ants are the insect tier | the organ behind `DESIGN.md` § Scouting's *"sap invites insect chase by smell"*, and the biology behind § Trees as terrain |
| **Lore** — the two hackers' motives | [`SETTING.md`](SETTING.md) § Both hackers had a MOTIVE | ⚠ *why the swarm is hostile at all*: hostility was authored by a person for a domestic political reason, and outlived both |
| **Lore + Plot** — settlers today (root bond, suits, scavenging, rock paintings, fear of the player) | [`SETTING.md`](SETTING.md) § The settlers today | ⚠⚠ *"afraid of the player, as a mutation of the AI"* — the player looks like the enemy and cannot step out to show a face.  The best answer to why first contact is hard |
| **Plot** — six biomes + *"layers in the ground protect against radio"* | [`SETTING.md`](SETTING.md) § The biomes | ⚠⚠ the radio note explains, with one physical fact, why the AI is a surface power, why the natives were never found, and what the player's jammer is imitating |
| **Plot** — competing operators, raids, shared infrastructure | [`SETTING.md`](SETTING.md) § The competitors | the cheap version: *their attempts are visible on the planet* — authored terrain, no new mechanism |
| **Ending** — reprogram / shut down / keep mining / set to defend | [`SETTING.md`](SETTING.md) § The endings | all four resolve *what do you do with a machine a person lives inside*, and destroying it in battle is conspicuously absent |
| **Material / Parts / Weapons / Machines** | [`MATERIALS.md`](MATERIALS.md) (new) | the tree behind `DESIGN.md` § 13's flat points scalar |
| **Defensive structures** — curved walls, moat, drawbridge; bridge anchors | [`MATERIALS.md`](MATERIALS.md) § Defensive structures | ⚠ the **moat** is the cheapest of the three: water's DROP is already in the palette and nothing reads it |
| **Minimal Assets** — the builder machine, *"the first cut is time consuming, then cut from the trench"* | [`MATERIALS.md`](MATERIALS.md) § The first cut is the expensive one | ⚠ makes **where you started building** change what the rest of the base costs — the racing line applied to construction |
| **Minimal Assets** — living quarters, loneliness | [`MATERIALS.md`](MATERIALS.md) § The crew are people | ⚠ a **building a long mission needs**, once `PROGRESSION.md` § P2b gives the crew endurance pools.  A morale *bar* is still the failure mode; a pool spent by work and restored by rest is not |
| **Rules** — *"it is possible to turn off the Jammer tower"* | [`plans/ROADMAP.md`](../plans/ROADMAP.md) Tier B | ⚠ passes the design test outright: turning it off stops the waves AND stops the salvage — see § 4c |

### 4b. Already canon — the seed and the current design agree

These needed no change; recording them because agreement reached
independently is evidence, and because someone will otherwise "add" them:

| Seed line | Already |
|---|---|
| *"Enemies will damage a wall when no alternative route is possible"* | `ENEMY_MOVEMENT.md` — and **built** (`plans/12`) |
| *"Enemy bodies will block a path, and might be used as a stepping stone"* | `ENEMY_MOVEMENT.md` § Bodies are terrain — **built**, and the bodies-ramp mechanic is now a headline rule |
| *"Automatic ramps on walls and slopes"* | `DESIGN.md` § Wall topology — drivable ends |
| *"Line of sight from towers towards enemies"* | `tower.loft::tower_sees` — **built** |
| *"Pre-made arena's with problems in them"* | `tests/scripts/*.keys` + [`plans/04`](../plans/04-map-library/README.md) |
| *"With enough time the waves on an installation will falter and the mission is stopped"* | `DESIGN.md` § Wave 7 cleared → free scramble |
| *"Land with a rocket on a chosen spot.  Directly functions as a jammer and emergency vehicle"* | `DESIGN.md` § 15 Landing flow + § 4 The core — **exactly** the shipped design |
| *"Only progression outside missions by launching rockets to space"* | `DESIGN.md` § 14 Scramble exit + § 16 Meta-game hub |
| *"Boss mobs.  Body parts, also on towers"* | `DESIGN.md` § Tower-top salvage + [`PARTS.md`](PARTS.md) § D3 — **built** as `tower_detach_top` |
| *"Speed up travel … without flying but with gliding"* | `DESIGN.md` § Hover + boost — **built** |
| *"Semi random walking pattern of enemies at level start"* | `spawn.loft::enemy_standing` — the pre-walk window, **built** |

### 4c. ⚠ Contradictions and refusals — with the reason

| Seed idea | Verdict | Why |
|---|---|---|
| *"The rockets into the sky are a cry of help from yet another human.  This is not the AI's doing"* | ⚠⚠ **OPEN — owner's call** | Directly contradicts `SETTING.md` § The quarantine, where AI-built rockets are the cordon's primary reason.  The better story and the more expensive retcon; a both-are-true version is proposed in [`SETTING.md`](SETTING.md) § OPEN — whose rockets are they? |
| *"Maximize automatic transport of materials.  Train tracks and lifts"* | ⚠⚠ **refused for the player** | Automation removes the trip, and the trip is the game (§ The recruitment charters it, `plans/17` § T3 measured it).  ⚠ The **enemy's** rails are welcome — `ROBOT_ECONOMY.md` § 3 already makes a route a thing to cut |
| *"Power grid and drain calculations"* | ⚠ **refused for the player's base** | `MATERIALS.md` § Power — a wire replaces a drive, and the drive is the measured mechanic.  Generators (positions) are fine |
| **Detonating flyers** | ⚠ **blocked** | `ENEMY_MOVEMENT.md`'s **ONE AI, per-class DATA** — a flyer needs its own mover, and it deletes the wall.  ⚠ **Wall spiders cost one row** (`can_climb` already takes the limit as a parameter) and are the version to build |
| *"Individual humans can have skills and different pool levels … need rest/sleep"* | ⚠⚠ **ADOPTED** (owner's ruling, 2026-08-26: *"the RPG part here will win"*) | [`PROGRESSION.md`](PROGRESSION.md) § P2 — twelve skills, six statistics, endurance pools.  ⚠ **This row said "refused" for one day**, on a session's derivation rather than the owner's design; `@X016`/`@X017` are superseded and `@X103` reversed |
| *"Only switching to a human that is at the main communication terminal"* | ⚠ **promoted to designed-not-decided** | [`PROGRESSION.md`](PROGRESSION.md) § P8 (`@X115`).  ⚠ With the crew adopted as characters it stops being a curiosity: without a switch a skilled crew is a spreadsheet the player watches, and the switch costs a drive back to the core mid-wave.  Still a change to the control model (`DESIGN.md` § 8, § 9), not an addition to it |
| **Development / Merge limitations / Script / Structures** | **obsolete** | This is the loft-engine origin story: *"directly start with a separation between game and engine"*, files <1000 lines, JSON dumps, an in-memory typed store.  ⚠ **It all happened** — that engine is [loft](https://github.com/loft-lang/loft), and dryopea is the consumer.  `CLAUDE.md` § Relationship to loft is the live version |
| *"Move from tiles to triangles"*, *"scaling/rotation of vehicles"*, *"24 directions walls"* | **shipped or superseded** | [`plans/25`](../plans/25-the-terrain-mesh/README.md) is the terrain mesh; [`plans/20`](../plans/20-entity-art/README.md) is the part-tree.  ⚠ The *"24 directions walls"* stencil idea is superseded by the hex lattice |
| **Round / curved walls, curved roads and rails** | **deferred, and it is a renderer question** | `plans/25` § M0 measured dryopea's ground as *a flat plane with pillars*; a curve is not authorable on this lattice today.  Same fate as § 2's 15-degree coastlines |

## 5. The 2023 gameplay data (mined 2026-08-26)

> *Source: [`../archive/gameplay.data`](../archive/gameplay.data) — 31 KB
> of filled-in factions, statistics and items.  § 3 above said "mine this
> schema first when D4 starts"; this is that mining.*

### 5a. ⚠⚠ The knowledge tree is the find

~55 `type:knowledge` items in ordered arcs — natives, sap and insects,
robots, aliens and the portal, the old ones, and the company's own.
Adopted whole into [`SETTING.md`](SETTING.md) § The knowledge tree,
because it is the structure § Future contact's no-shortcut rule has been
describing without one.

⚠ **Three canon questions get seed answers**, and the source predates
the questions by three years:

| Canon question | The 2023 answer |
|---|---|
| `SETTING.md` § Why the warriors can stand it — *and it is not strength* | **`Alien outcasts` — *"some aliens are somehow fully mentally shielded"***, plus `Alien rescue` — the hive sends squads under pressure.  The immune-response framing, arrived at twice independently |
| `SETTING.md` § What wakes an old one is OPEN | `Lord powers` — *"even during their sleep it is possible to contact these lords"*; `Awaken` — *"unwise though possible"*.  ⚠ So: **talk first, wake later** — the awakening is a consequence of a relationship |
| `ROBOT_ECONOMY.md` § Open questions 4 — what wakes the military? | `Army` — *"the government will send their fearsome armada **when somehow forced to**"* |

### 5b. ⚠ And the planet has a name

The data says **Dryopea** in forty places — *"human natives already
living on Dryopea"*, *"the huge trees on Dryopea"*.  ⚠⚠ **The project is
named after the world it is set on, and no document said so until
2026-08-26.**  Recorded in [`SETTING.md`](SETTING.md) § The recruitment.

### 5c. The catalogues, and the warning in them

| Layer | 2023 count | Verdict |
|---|---|---|
| materials + fluids + goods | **34** | ⚠ [`MATERIALS.md`](MATERIALS.md) § The 2023 catalogue is much bigger — *ship three*.  ⚠ Four rows survive the cut, and the **rock types** (granite sturdy, sand rock brittle, volcanic easy) are the best of them: the ground you dug becomes the wall's strength, over a palette that already exists |
| parts (`type:machine`) | 13 | folded into `MATERIALS.md` § Parts |
| towers | 14 | ⚠⚠ two ideas taken: **traps that do not auto-reset** (place in advance, then drive out mid-wave to re-arm — the design test passed outright) and the **grabber**, kept only in its *move a body* form |
| upgrades | 28 | ⚠ **adopted** — [`PROGRESSION.md`](PROGRESSION.md) § P3, under § P6a's fence (*an upgrade buys friction, never answers*).  ⚠⚠ Two need individual rulings: **`Laser communicator` — *"allows to communicate under the influence of a scrambler"*** survives only as a LINE-OF-SIGHT relay, because a general radio deletes `@X099`; and **`Auto pilot`** keeps the drawback the 2023 note already gave it |
| skills + statistics | 18 | ⚠⚠ **adopted whole** — the RPG layer, and the lattice is worth taking as authored: every skill is bonused by exactly two statistics and every statistic bonuses exactly four skills.  See [`PROGRESSION.md`](PROGRESSION.md) § P2 |
| animals | 19 | ⚠ **the tier-2 and tier-3 bestiary already exists here** — spitting beetle, mantis, ant *"produces glue that can paralyze machines"*, jumper *"can jump most walls but without a decent weapon"*, kraken, sea dragon, and all four elementals.  Mine it when `ROADMAP.md` Tier C starts; the *jumper* is a wall-climber with no weapon, which is a designed unit already |
| factions | 9 | spacers, economy, natives, shaman, robots, **world**, **oceanic**, ancients, aliens.  ⚠ `world` (*"many beasts behave intelligently"*) and `oceanic` (*"often fight the others"*) are the two not yet in canon |

### 5d. ⚠ Weapon damage types — 12 there, 6 here

The 2023 sheet has *acid, bludgeon, cold, cutting, emp, flaming, flash,
lightning, piercing, poison* plus `range` and `falloff`.  `DESIGN.md`
§ Damage TYPE ships a deliberate **six-way triangle** where each type
costs something and two of the costs are self-inflicted.

⚠ **The six are canon and the twelve are not an upgrade to them.**  What
the longer list contributes is `falloff` — *"what range makes hitting
something more difficult"* — which is precisely the § Damage TYPE
insight that **range is a PROFILE, not a number**, and `poison`, which
is *"ignored by machines"* and therefore a purely anti-insect axis.

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
- [`../archive/seed-notes.md`](../archive/seed-notes.md) — the § 4
  source, verbatim.
- [`MATERIALS.md`](MATERIALS.md) — the catalogue §§ 4-5 fed.
