<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# dryopea — roadmap

A logical-order list of remaining features.  **We will diverge
from it.**  The goal is to keep "what could we do next?"
answerable in 30 seconds rather than to lock a strict sequence.

Each row: short feature name, status, source-of-truth slot,
brief one-liner.  Status values:

- **shipped** — code landed, tests green.
- **partial** — some code landed; constrained or behind workarounds.
- **drafted** — design written, no code.
- **sketch** — referenced in docs, design not fully written.

Tiers are ordered by **player-impact-per-line-of-code**: Tier A
delivers a playable thing; subsequent tiers add depth on top.
Within a tier, ordering is a suggestion — you can pick any row.

---

## Tier A — Validation playable

One map, one mission, one tower type, one enemy type — but
end-to-end and *fun-shaped*.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Ground editor (sparse paint, sea default) | **shipped** | [plan 01 E1-E3](01-ground-editor/README.md) | Hex grid + camera + palette + click/drag paint |
| Save/load MapFile JSON | **partial** | [plan 01 E4](01-ground-editor/README.md) | 6-field schema; expanded once loft JSON-cast bugs land |
| Integration smoke test (cold-start cycle) | **shipped** | [plan 01](01-ground-editor/README.md) | 72/72 green under `scripts/test.sh` |
| Interactive GL editor (E1-live) | **shipped** | [plan 01 E1-live](01-ground-editor/README.md) | `src/main.loft`; human playtest pending |
| 3D solver-validation viewer | drafted | [plan 02](02-solver-validation-viewer/README.md) | Painted layer + height-solved mesh overlay, 40% transparent |
| Marker layer + spawn points | **shipped** | [plan 03](03-marker-layer-and-spawns/README.md) | M1-M5 done; sidecar JSON, mode toggle, placement + rotation, render overlay, wave engine + spawn director |
| Map library + browser (planet-view UI) | drafted | [plan 04](04-map-library/README.md) | MapFile schema (L1), map index, content, selector |
| Enemy flow field (route round walls, per class) | **shipped** | [plan 11](11-flow-field/README.md) | F0-F8 done.  Distance field per climb limit, arrows computed not stored, enemies spread rather than stack, passability as a height STEP over a runtime layer, and a desire field for besieging a sealed perimeter.  Does NOT replace the straight-line `enemy_tick` — that is approach mode, and the two modes hand off at the scrambler bubble |
| Combat resolves (damage, death, rubble, towers) | drafted | [plan 12](12-combat-resolution/README.md) | B0-B7.  Towers fire and decay, enemies take damage and die, walls lose structural HP and break into climbable rubble, bodies pile as terrain, nibble drains the wallet.  Headline gate: an authored base with walls and towers, nobody defending it, falls on a clock that gets markedly longer for having the defences |
| End-to-end validation scenario | drafted | [plan 05](05-validation-scenario/README.md) | The "minimum playable thing" spec |

When Tier A is done the game is **playable** — a player can
land, paint a base, defend through some waves, scramble.  Not
deep yet; just real.

---

## Tier B — Combat depth + content pipeline

Skilled play hooks **and** the editor-to-entity content
pipeline.  Two parallel arcs that don't strictly depend on
each other but both gate the depth tiers above them.

### Combat depth (DESIGN § 7)

Skilled-play opt-in mechanics; an entry-level player can
ignore the lot and still complete missions.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Tower attack-count decay + repair | drafted | DESIGN § 7 | 30-shot budget; goes black; refill on repair |
| Repair rule (firing tower can't be repaired) | drafted | DESIGN § 7 | Engineering realism: power-down before maintenance |
| Boost (timed, fire-and-forget) | drafted | DESIGN § 7 | Pink, held-key activation; validation ships free |
| Strain system (shot-density wear) | drafted | DESIGN § 7 | Per-shot wear scaled by output level |
| Boost cooldown + active-maintenance bypass | drafted | DESIGN § 7 | Pickup-drop-repair cycle doubles boost frequency |
| Overload (presence-locked input) | drafted | DESIGN § 7 | High-output mode; player must remain |
| Hot-swap cycle (two-top alternation) | drafted | DESIGN § 5 + § 7 | Sustain overload via swap-pit cycling |
| Swap-pit wall pattern | drafted | DESIGN § 5 | Authored indentation: spare top + safe parking + clear path |
| Tower variants (anti-insect / area / etc.) | drafted | DESIGN § 7 future-tower-types | Unlocked via scouting (Tier D) |
| Ammo for variant weapons | drafted | DESIGN § 7 | Per-shot consumable, distinct from decay |
| Tactical type-swap mid-combat | drafted | DESIGN § 7 | Different-type spare in swap pit |
| New tower order via beacon ferry | drafted | DESIGN § 7 | Carry beacon from core to build site |
| **Wreck decay, blocking + damage types** | **designed 2026-08-13** | ENEMY_MOVEMENT § Bodies are terrain | ONE decay clock driving TWO things: salvage value (a fresh wreck is harvestable, an old one is rubbish) and PASSABILITY (a big robot's body seals its hex until it settles; a small one never does). A plugged chokepoint makes the wave attack the WALL instead, so the player shoots corpses to reopen it — at the cost of shots and salvage, and only while standing at the tower. Damage TYPE picks the trade: laser vaporises, explosive splashes onto the player's own walls, EMP maximises obstruction and destroys the high-value electrics (and barely scratches insects). Needs a decay clock on the rubble layer, per-class body height + breakdown rate, tower damage types, and plan 06 S1's contents layer — see [`plans/12`](12-combat-resolution/README.md) § Wreck decay, blocking, and damage types |

Plan-shaped candidate: `plan-future-XX — Tower mechanics
depth` covering the strain / boost / overload / hot-swap
arc together — they share mechanics and graphics.  Slot
number TBD when the trigger fires.

### Editor-to-entity content pipeline

The whole stencil-from-editor arc — multi-layer painting,
bridges, stencil authoring mode, mesh baker, mesh
composition, entity runtime.  Brings the suite into
**rapid prototyping** posture: think → paint → bake → drop
into a map → run.

Strategic positioning — **two shipping paths from one
pipeline**:

- **Polish path** (big-studio): the developer never waits
  on art to reach a solid playable state; the final art
  push lands at the end of development on shape-correct
  stencils that are already in every position.  The polish
  artist refines what's there.
- **Strike path** (indie / starting devs): the stencil
  output IS the shipped aesthetic.  No polish layer.
  Clean geometric / art-deco / low-poly / block-layout
  art-direction works as a final shipped style, exactly
  as it does for many indie successes.  This expands the
  suite's addressable audience to indies who don't have
  or need an art team — they ship full games on stencils
  alone.

See [`plan 06`](06-editor-stencil-pipeline/README.md)
§ Who this serves for the three-audience breakdown
(dryopea team + studios + indies).

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Multi-layer painting (moros-house style) | drafted | [plan 06 S1](06-editor-stencil-pipeline/README.md) | Stacked layers per hex; vertical structure |
| Bridges as a primitive | drafted | [plan 06 S1](06-editor-stencil-pipeline/README.md) | Multi-layer connecting spans |
| Stencil authoring mode | drafted | [plan 06 S2](06-editor-stencil-pipeline/README.md) | Same editor, bounded region, separate save format |
| Mesh baker (stencil → static mesh) | drafted | [plan 06 S2](06-editor-stencil-pipeline/README.md) | Scales down to entity size; per-stencil colour |
| Mesh composition (mount + pivot) | drafted | [plan 06 S3](06-editor-stencil-pipeline/README.md) | Tower-base + rotating-top; swivel turrets |
| Entity runtime (baked meshes as units) | drafted | [plan 06 S3](06-editor-stencil-pipeline/README.md) | Spawn, tick, render baked entities |
| World-dressing prefabs (place a stencil into a map) | drafted | [plan 06 § 1](06-editor-stencil-pipeline/README.md) | Old habitats / huge trees / bridges / factories / ruins |
| Jointed / leg movement extensions | sketch | plan 06 S4 (deferred) | Walking robots, insect locomotion — triggers with tier-2 plan |

Plan slot: [`plan 06 — Editor-to-stencil pipeline`](06-editor-stencil-pipeline/README.md)
covers all of the above as a single coherent initiative,
with phases S1-S4.  S4 (joints / legs) explicitly deferred
until tier-2 insects trigger it.

---

## Tier C — Enemy diversity

Make tier 1 fully alive, then extend to tier 2 + tier 3.
Currently all enemies render as the same placeholder magenta
cuboid.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Tier 1 economic-bot vs combat-bot wave split | drafted | DESIGN § 10 + SETTING § Combat bots | Typed wave mixes; combat-bots dormant by default |
| Combat-bot wake klaxon (diegetic activation cue) | drafted | SETTING § Combat bots | Audible signal when an AI reactivates its military |
| Boss = mobile repair platform (2×2) | drafted | DESIGN § 7 Boss | Industrial unit, not a soldier; phase 3 |
| Tier 2 — insects + sap | drafted | SETTING § Insects | Passive fauna; smell-tracking; `wall_high` blocks |
| Tier 3 — elementals + stones | drafted | SETTING § Elementals | Dormant; gem-keyed activation; 4 sub-kinds |
| Hacking helpers (subvert robot units) | drafted | DESIGN § 9 Helpers | Coordinator bots = highest-value target |
| Robot diversity — typed wave compositions | drafted | SETTING § Robot diversity | Workers / haulers / scouts / coordinators / etc. |

Likely candidate for two plan slots: `plan 07 — Tier 2 insects`
(largest mechanical novelty: passive fauna + smell-tracking) and
`plan 08 — Tier 3 elementals + stones` (gem mechanic, dormancy).
Tier-1 typing + boss + hacking probably fit into plan 03 or
plan 06 expansions.

---

## Tier D — Between-missions meta

Multi-mission play actually feels like a campaign.  Currently
each mission is independent; persistence isn't shipped.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Central space station hub | drafted | SETTING § Between missions | Rented bay; persistent state; pre-mission UI |
| Persistent inventory (tops, materials, points) | drafted | DESIGN § 13 | Carries across runs |
| Q4 loadout closure | drafted | DESIGN § 7 + § 13 | Pick towers from inventory before each sortie |
| Scouting unlocks new tower types | drafted | DESIGN § 7 + § 13 | Scouted intel persists; variants become orderable |
| Helper rescue quests | drafted | DESIGN § 9 Helpers | Stranded helpers from past missions, rescuable |
| Static planet-view map selector | drafted | [plan 04 L3](04-map-library/README.md) | Clickable markers per available map |
| Rotating planet-view UI (future UX) | sketch | SETTING § Future UX | Day/night terminator, overlay state |
| Bounded sessions + mission chaining | drafted | DESIGN § 14 | Time-windowed run shape |

Likely candidate for `plan 10 — Station hub + persistence`
covering hub UI + inventory + Q4 loadout + scout-unlock.  Mission
chaining (DESIGN § 14) may want its own slot once the persistent
inventory lands and its trigger fires.

---

## Tier E — Narrative arcs (deep content)

The world the validation mechanics live in.  Deliberately
gated behind player demonstrated competence — see
[SETTING.md § Future contact](../docs/SETTING.md#future-contact--humans-ais-and-the-no-shortcut-rule)
for the no-shortcut design rule.

| Feature | Status | Slot | Brief |
|---|---|---|---|
| Faction territory awareness | drafted | SETTING § Robot diversity | Maps tagged with AI faction; affects compositions |
| Side quests — underground human contact | drafted | SETTING § Future contact | Breadcrumb discovery → first contact → trade |
| Direct contact with an AI | drafted | SETTING § Future contact | Deep-lore: meet the girl-hacker AI as a person |
| Truth discovery — government cordon paradox | drafted | SETTING § Future contact | Off-planet leverage |
| Player-faction alignment | drafted | SETTING § Future contact | Ally with one AI vs another |
| Off-planet meta — orbital banking, vendors | sketch | DESIGN § 13 Future expansion | Shop at the station hub |
| Multi-player disruption missions | sketch | SETTING § How mechanics fit | Coordinate against the AI economy |

Likely candidate for `plan 11 — Future contact arcs`, but this
tier is **deferred** by design — it's the cap on the skill
ceiling, not the floor.  Authoring of breadcrumbs sits inside
the maps from plan 04 + plan 07-08, so the *content* lives in
the per-map authoring, while the *triggers + state machine*
deserve a plan when the trigger to start lands.

---

## Persistence destination — path-backed `Store` (deferred, awaiting loft)

Today the editor saves to `dryopea_save.json` via `text as
MapFile` round-trip.  The eventual destination is **the hash IS
the file**: each in-game data structure (painted hexes, marker
layer, stencil instances, …) is a `Store` mmap'd from a path —
lookups are direct memory reads, mutations are durable on the
next OS msync, no explicit save/load loop.

The Rust side already supports this (`Store::open(path)`,
`Store::open_durable(path, mode)`; @PLAN38 phase 01 shipped in
loft commit `d494edc`).  The integrity bracket
(`store_durable_check` / `store_durable_seal`) is exposed to
`.loft` user code as of phase 01b (`8bc4b08`).  **What's missing
is the language surface for binding a user-data `Store` to a path
at program startup** — see [`QUESTIONS_FOR_LOFT.md` § Path-backed
user-data Store binding](../QUESTIONS_FOR_LOFT.md).

Until that lands, we **stay on JSON** for the world file —
the manual binary `file()` + `#read` cursor-IO route is a worse
stopgap than the existing JSON path (still requires hand-rolled
ser/deser, still doesn't get us mmap).  Hybrid split when the
surface lands:

- **Store-backed mmap** for bulk runtime state — painted hexes,
  marker layer, stencil instance lists (everything that mutates
  during play).
- **JSON** for human-edited content — `examples/palette.json`,
  per-map metadata + objective + waves list, stencil library
  definitions.  Diffable, hand-editable, git-friendly.

When the upstream surface lands the dryopea migration is a
**one-line annotation** on `PaintedWorld` / the marker-layer
wrapper / etc.; the rest of the codebase doesn't change.

---

## How to use this

- **"What could we do next?"** — scan the table for the nearest
  drafted row whose dependencies are shipped.  Pick whichever
  seems most appealing.
- **"Is X in the plan?"** — search for X here.  If it's not
  here, either it's not yet design-thought or it lives in a
  lib_plan (loft library scope — outside dryopea).
- **"What's the dependency between X and Y?"** — the tier
  ordering is a hint but not a strict gate.  Concrete
  dependencies live in each plan's `## Dependencies` section.

Diverging from the order is expected.  The dogfood loop
(per [CLAUDE.md](../CLAUDE.md) dev cadence) often pulls a
later-tier feature forward when it sharpens an earlier-tier
demo.  Update this file when something ships or when a new
candidate is added.

---

## See also

- [`README.md`](README.md) — plans admin (workflow, file layout)
- [`DEFERRED.md`](DEFERRED.md) — parked plans
- [`../docs/DESIGN.md`](../docs/DESIGN.md) — master design
- [`../docs/SETTING.md`](../docs/SETTING.md) — fiction
- [`../QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) — outstanding loft-side asks
