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
| Save/load MapFile JSON | **partial** | [plan 01 E4](01-ground-editor/README.md) | 6-field schema; expanded once loft JSON-cast bugs land.  The marker sidecar round-trips all three kinds (spawn / target / tower) |
| Integration smoke test (cold-start cycle) | **shipped** | [plan 01](01-ground-editor/README.md) | Part of the 691-test suite under `scripts/test.sh` |
| Interactive GL editor (E1-live) | **shipped** | [plan 01 E1-live](01-ground-editor/README.md) | `src/main.loft`; human playtest pending |
| 3D solver-validation viewer | drafted | [plan 02](02-solver-validation-viewer/README.md) | Painted layer + height-solved mesh overlay, 40% transparent |
| Marker layer + spawn points | **shipped** | [plan 03](03-marker-layer-and-spawns/README.md) | M1-M5 done; sidecar JSON, mode toggle, placement + rotation, render overlay, wave engine + spawn director |
| Map library + browser (planet-view UI) | drafted | [plan 04](04-map-library/README.md) | MapFile schema (L1), map index, content, selector |
| Enemy flow field (route round walls, per class) | **shipped** | [plan 11](11-flow-field/README.md) | F0-F8 done.  Distance field per climb limit, arrows computed not stored, enemies spread rather than stack, passability as a height STEP over a runtime layer, and a desire field for besieging a sealed perimeter.  Does NOT replace the straight-line `enemy_tick` — that is approach mode, and the two modes hand off at the scrambler bubble |
| Combat resolves (damage, death, rubble, towers) | **done** | [plan 12](12-combat-resolution/README.md) | Walls have structural HP and break into climbable rubble, enemies have HP and die leaving bodies that ramp a kill zone shut, towers fire at 15 hex with a sight line and a 30-shot budget, and a wallet the nibblers drain is the only end state.  ⚠ **B7's clock half-falsified the goal:** a sealed wall takes an unattended base from 61 ticks to 104, a wall with a GATE in it buys one tick, and adding a tower cuts it back to 95 — its own dead ramp over the wall it defends.  (B7 first measured 161/311/180; its findings motivated plan 11 F7b's sidestep, and all three conclusions survived the rebaseline.)  **Answered by [plan 13](13-the-vehicle/README.md):** a crew that clears the ramp takes the towered base from 95 ticks to 121 — past the bare wall's 104 — so the tower is finally worth its own dead.  ⚠ Only a BOOSTING crew can reach it: the ramp forms outside the wall and an idle hover unit climbs 0.4 m |
| A player in the world (drive, boost, clear, earn, be destroyed) | **done** | [plan 13](13-the-vehicle/README.md) | V0-V5.  A hover unit at 2 hexes a tick, boosting over its own 3 m wall, clearing the body ramp that beats a tower and getting paid 20 points a metre for it — which takes the towered base from 95 ticks to 145.  Blocking a wave with nowhere to go round costs 5 HP/s and twenty seconds |
| NPC helpers (the crew becomes a cooperative) | **done** | [plan 14](14-helpers/README.md) | H0-H3 done: a crew on the player's chassis at 2.5 hex/s — ⚠ the first mover whose speed does not fit the tick, so it BANKS fractional hexes (not "the tick becomes a timestep"; plan 11 F8's budget trigger does not fire).  They clear and earn on the shared chassis, and a base with two fronts goes 77 → 214 → 242 ticks as the crew grows to cover them — ⚠ a roster buys COVERAGE, not throughput: a second helper beside the first is worth NOTHING.  A crew member can also be lost for good: the blocker rule covers the whole player's side and a helper that dies WRECKS where it stood while the player respawns.  H4 (retrieve → recover) shipped as [plan 15](15-the-carry-model/README.md) C2, where the CARRY model it was blocked on lives: a lost crew member is picked up, carried to the core and rejoins the roster after exactly 90 ticks — and ⚠ nothing else brings one back, which the scenario gates by having its retrieval cut out |
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
| **⚠ Speed decoupled from the tick** | **designed 2026-08-13** | DESIGN § 10 § Speed must NOT be tied to the tick | Owner instruction. Speed varies by role, by tier and by CONDITION (a damaged robot moves slower) — the scout is quite a bit faster and the BOSS quite a bit slower, the latter deliberately so the player has time to strategize rather than react, which is § 6's pre-walk-visibility principle applied inside the fight. So the tick stops being "the time to cross one hex" and becomes a simulation timestep; every enemy banks movement progress the way a tower banks its fire interval (plan 12 B5a, epsilon included). ⚠ Turns two assertions in `tests/12_b0_probe.loft` into an inverted gate, and makes "the tick got shorter" a THIRD trigger for plan 11's incremental field rebuild — the per-tick budget shrinks in direct proportion, and a from-scratch rebuild that fits at 667 ms does not fit at 100 ms |
| **Builders repair the boss** | **designed 2026-08-13** | DESIGN § 7 § Builders repair the BOSS | Closes a mutual-repair knot with the boss's existing heal-nearby-regulars: the player picks which end to break. Three counters, and TWO are architectural — "near" and "room to reach" are decided by where the walls went, not by DPS. ⚠ Check the arithmetic before it ships: a couple of builders can out-heal one laser, which is what splash and artillery are for |
| **Small-robot roles (scout / harvester / builder / miner)** | **designed 2026-08-13** | DESIGN § 10 § Small robots: four roles, one AI | Four economic roles that differ ONLY in how fast their working tools chew a wall — a miner cuts rock for a living, a scout does not. Same routing, same targeting, same retaliation: one `numbers.json` row each plus one branch in `spawn.loft`'s damage-to-wall lookup, which already carries the note saying it becomes one. Makes wave composition a threat the player can READ without a HUD, and sharpens the bracing rule (a strong attacker at a weak hex is what decides a perimeter). The scout is also notably FASTER and has no real weapon — so it is the unit artillery cannot hit and the one not worth hitting, while the miner is what artillery is for: role composition and tower composition become a matching problem. ⚠ The harvester is the one role with no stated mechanical distinction yet; a hauler's obvious axis is what it CARRIES |
| **Damage types + armour + traverse time** | **designed 2026-08-13** | DESIGN § 7 § Damage TYPE is the axis | A triangle, not a ladder: laser is poor vs armour and vaporises salvage; artillery is good vs armour but single-target and can MISS a mover; explosive splashes onto the player's OWN walls; EMP destroys the high-value electrics, blocks longest, and barely scratches insects. Cross-cutting: a tower TURNS to aim, so switching targets costs damage — and a shot that has become impossible is not fired at all, while one already in flight can be dodged by a fast enemy without trying. A flame thrower is short-ranged and excellent against a swarm of SMALL enemies; a sniper is the heaviest gun, slowest to aim, best far and especially bad up close — so range becomes a PROFILE rather than a number, and the two bookend both axes. Two class properties fall out — armour and size — and size is needed by the corpse-blocking rule anyway. Placement becomes a real decision: a sniper is bad inside the base and excellent on an outer ridge (LOS read as a HEIGHT gives elevation for free). Retaliation is an INFORMATION rule — an enemy attacks a tower that hurt IT while the scrambler is up, and one that hurt anyone's companions once it is down — but never overrides ROUTING, so a tower behind a closed perimeter is unreachable and the wave keeps going for the core. A ridge tower is exposed because it is reachable, and costs the player a drive out through a live wave to service it. The ONE exception under scrambling is the BOSS — and NOT as a second AI: bosses run the same rules with different data (2x2, so it cannot fit a one-hex entrance and is the unit most likely to be stuck outside being shot; plus the option to share what is hurting it). The event is emergent. Needs tower HP and routing for a unit wider than one hex |
| Ammo for variant weapons | drafted | DESIGN § 7 | Per-shot consumable, distinct from decay |
| Tactical type-swap mid-combat | drafted | DESIGN § 7 | Different-type spare in swap pit |
| **The CARRY model (one slot, one pickup/drop key)** | **done** — [plan 15](15-the-carry-model/README.md) C0-C3 | DESIGN § 11 § E + § Carry visibility | ⚠ **A shared blocker, named here because three separate features wait on it**: helper retrieval + recovery ([plan 14](14-helpers/README.md) H4), the tower-top repair / hot-swap arc, and the beacon ferry below.  One slot per vehicle (`numbers.json` § helper.carry_slot_count is 1 for player and helper alike), one context-resolved key (empty hands = pickup, carrying = deposit), and everything carried renders above the carrier.  Building it inside any one of the three would serve that case alone and be re-derived by the next.  **Built by [plan 15](15-the-carry-model/README.md)**: one record per object with an `owner`, so an object is on the ground XOR in exactly one carrier's slot and duplication is unrepresentable rather than prevented.  ⚠ It is the ONE runtime layer that is not a hash keyed by hex — two carry objects share a hex when a loaded carrier is destroyed, and a hash deletes one of them silently.  Helper retrieval is done (C2); **tower-tops and beacons are the two consumers still open**, and the contract they arrive under is a kind row plus a destination rule and NO new carrying code.  ⚠ **C3 measured what retrieval is worth and it is nothing yet** — 85/79/79 ticks on one base, because a 60 s recovery is priced against § wave_system's SEVEN waves with 15 s lulls and dryopea plays one wave at a time.  The wave system is the named trigger for re-measuring it; shortening the recovery would be tuning a number to fit a harness |
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
| **Robot economy — natural wave patterns** | drafted | [docs/ROBOT_ECONOMY.md](../docs/ROBOT_ECONOMY.md) | Six installation types + transport routes; traffic replaces the authored wave list and spawn markers.  ⚠ **Retires [plan 16](16-the-wave-system/README.md)**, which the owner has committed to removing before ship |
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
