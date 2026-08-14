<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Numbers — runtime parameters

**Source of truth:**
[`../examples/numbers.json`](../examples/numbers.json) is the
runtime config the game loads at startup.  **Every parameter
has its value, units, and documentation inline in the JSON.**
Modders edit values there and re-launch — no rebuild required
(DESIGN.md § Moddability).

This document is the *high-level overview*: what's in the file,
why it's organised the way it is, and what the design targets
are.  For an individual parameter's meaning + tradeoffs, read
the JSON.

## What's in numbers.json

The file is a single JSON object with one section per system,
each section's leaves carrying `value` + `units` + `doc`:

| Section | Holds |
|---|---|
| `world` | hex grid scale, layout convention, map extents, atmospheric haze radius |
| `player_vehicle` | dimensions, hover heights, speeds, boost timings, blocker-damage model |
| `enemy_regular` | dimensions, speeds, HP, **body height** (what a corpse raises its hex by — NOT the standing height), damage rates (core / wall / blocker), loot value, pre-walk standstill |
| `enemy_boss_phase3` | 2×2 footprint, speed, HP, wall-break + repair-on-regulars rates, loot value |
| `enemy_boss_combat` | ⚠ **Design only, not built** — the big COMBAT robot (the *other* boss kind).  Weapon range **10** against a tower's 15, which makes a 5-hex dead band and a tower placement window of `10 < D <= 15`; it SHOOTS TOWERS; and a `range_rule` entry stating that range above 15 must be paid for in slow speed plus wind-up |
| `tower` | range, fire interval, damage, shot budget, costs + build/repair/boost timings + boost multipliers |
| `wall` | wall + wall_high heights, HPs, **the four bracing factors**, **the rubble a break leaves**, build times, the end-ramp slope, entrance gap window |
| `helper` | starting + cap roster, speed, HP, order cost, lander delivery + recovery times, construction tick |
| `core` | footprint + dims, invulnerability flag, scrambler bubble radius, launch countdown, all 5 landing geometry params |
| `wave_system` | wave list, inter-wave delay, pre-walk visibility, both wave-1 triggers (wall-count + provocation distance) |
| `economy` | starting budget, carryover ratio, all order / loot values, tower-top carryover effect (validation placeholder) |
| `camera` | over-the-shoulder pose, swing easing, FOV, haze visibility |
| `input` | key + controller binding map; mouse/right-stick reserved for UI clicks (camera is locked) |

## Design targets the parameters anchor

The defaults are picked to produce these *shapes* (verify in
play; tune freely).

⚠ **Five of the seven are now GATED against the running
simulation** — [`tests/numbers_design_targets.loft`](../tests/numbers_design_targets.loft)
pins each one, so a tuned number fails there naming the design
promise it broke.  Before that file existed every target on this
page was prose verified by nobody, in a document whose whole
purpose is to be tuned.  ✓ marks a gated target; the two without
one say what they are waiting for.

- ✗ **Single base session ≈ 15-25 minutes.**  ~45 s pre-wave
  commitment → 7 waves with 15 s gaps → ~5-6 min wave phase
  → free scramble or earlier exit.
  ⚠ **NOT gateable, and it needs a PLAYER.**  Every clock
  dryopea can measure is an unattended one, and an unattended
  base is a measuring instrument rather than the game — it
  falls during wave 2 by design, because nothing kills, repairs
  or collects.  Gateable the day the vehicle lands.
- ✓ **Tower DPS ≈ 10/s.**  Regular at 30 HP = 3-shot kill;
  cluster of 3 enemies dies in ~10 s under one tower.  A whole
  30-shot magazine is 30 s and **ten kills**, which is what
  "pace towers across the wave" costs: one tower cannot clear
  wave 2 (eight) and wave 3 (twelve) without a repair between.
- ✓ **Wall break-through ≈ rare — of a LONE enemy.**  One robot
  needs 100 s to break a braced wall hex (measured: 102 s);
  bosses 5× faster.  Wall-nibbling is the fallback when the
  perimeter is fully closed; player intent is to leave
  deliberate entrances.
  ⚠ **A WAVE is three times faster, and this is the number to
  read before authoring a base.**  Five robots arrive as a
  front and breach a sealed wall in **33 s**, at its 30 HP END
  rather than the 100 HP hex this target's arithmetic uses
  (plan 12 B3's bracing, reachable since plan 11 F7b).  Past a
  handful of attackers the clock **saturates** — thirteen are
  no faster than five — because only so many can reach the hex
  closest to breaking.  So a sealed perimeter is a decision
  with a price, not a countdown that scales with the wave.
- ✗ **Economy ramps via loot.**  ⚠ **NOT gateable:** loot
  COLLECTION needs the vehicle, `enemy_regular.loot_value` is
  read by nobody, and `wallet.loft` deliberately has no credit
  verb — which is what enforces "the wallet never refills
  unattended".  Wave 1 (5 enemies × 10 pts =
  50 pts) + 200 starting = 250 pts → 1 tower + 1 helper.
  Wave 2 (8 × 10 = 80 pts) plus carryover funds further
  expansion.  By mid-game the player should be running ~3-4
  towers + 4-6 helpers.
- ✓ **Movement scale.**  Player at 3 hex/s normal = ~4 m/s; an
  enemy at 1.5 hex/s gives time to react.  Boost (6 hex/s)
  is for crossing the base, not winning fights.
  ⚠ The enemy half is gated twice over, because the TICK's
  duration is derived from it (`spawn.loft::TICK_SECONDS`) —
  and `DESIGN.md` § Speed must NOT be tied to the tick intends
  to break that coupling, so this is one of the assertions that
  should go red the day it does.
- ✓ **Combat economy ≈ 1 wave / 30 s of full-tower fire.**
  Tower shot budget 30 = the player needs to pace towers
  across the wave, salvage or repair between bursts.
  ⚠ **A tower with nobody to clean up after it makes a base
  fall SOONER** — measured, plan 12 B7: its own dead pile into
  a ramp over the wall it defends.  The budget is not the only
  thing a tower spends.
- ✓ **Damage to wallet ≈ slow drain.**  At 1 pt/s per nibbling
  enemy, 5 enemies on the core = 5 pt/s; 200 pts buys 40 s
  before zero.  Encourages keeping enemies AWAY, not just
  outpacing the damage.  **Built and measured** (plan 12 B6):
  five nibblers empty the budget in 60 ticks = 40.0 s exactly,
  and ONE takes 301 ticks ≈ 200.7 s.  ⚠ 301 rather than the 300
  the arithmetic says — `1 / 1.5` has no exact float form, so
  three hundred ticks sum a hair under 200 s.  The floor itself
  is exact; only which tick crosses it moves.
  ⚠ **The drain SATURATES at 7 pt/s**, and every reading of
  `wave_system.wave_list` depends on it: only the core's own
  footprint can nibble (`core.footprint_layout` is a radius-1
  disc), so a wave of eighty is no more dangerous to the wallet
  than a wave of ten.  The rest of a big wave's weight lands on
  walls and towers, never on the budget.

## What gets used by what

Cross-references between the parameter file and the design
docs:

| Parameter | Referenced by | Why |
|---|---|---|
| **every gated target above** | [`tests/numbers_design_targets.loft`](../tests/numbers_design_targets.loft) | ⚠ **Tune a number and this file tells you which design promise you broke.**  It also pins the constants to the figures quoted here, which is the cheapest stand-in for the loader nothing has built: `numbers.json` is read by NOBODY yet, so every value is hand-copied into a `.loft` constant and the drift no test can see is a JSON edited without its twin |
| `world.hex_diameter` | every distance in the design | Canonical unit |
| `world.atmosphere_haze_radius` | SETTING.md § The atmosphere is thick; camera | Caps render + sight |
| `core.scrambler_bubble_radius` | SETTING.md § The core is a scrambling tower; wave engage-mode handoff | The bubble boundary IS the approach→engage trigger |
| `core.close_spawn_disable_radius` | DESIGN.md § Updates: free-pick landing | Auto-silenced spawn markers |
| `wave_system.wave_1_wall_trigger` + `wave_1_provocation_distance` | DESIGN.md § Updates: wave-start triggers; `src/spawn.loft` | Either trigger fires wave 1.  ⚠ Only the PROVOCATION half is built (plan 16 W3) — the wall trigger counts walls BUILT and dryopea has no construction, so it arrives with it.  ⚠ The distance is meaningful only because it is FURTHER OUT than `core.close_spawn_disable_radius`: 10 or 11 hexes is a live spawn source that cannot be poked, and if the two are ever tuned together the rule stops being testable |
| `wall.entrance_gap_recognition_hexes` | GROUND_TYPES.md § Entrances | Two ends form a gate when within this range |
| `tower.shot_budget_per_charge` | `src/tower.loft`; PROXY_ART.md § Tower § lifecycle | Top goes black after this many shots — spent per SHOT and never per tick, so an idle tower never decays |
| `helper.recovery_time_after_retrieval` | `src/helper.loft`; PROXY_ART.md § Helper § Damage | Time before a retrieved helper rejoins the roster.  ⚠ 60.0 s over a 1/1.5 s tick is EXACTLY 90 ticks, and a timer that divides the tick exactly is the ONLY case where the banked-timer epsilon bites — a bare `> 0.0` gives 91.  The 5.0 s boost cooldown is 7.5 ticks and is immune, which is why the safe-looking numbers are the dangerous ones (plan 15 C0) |
| `helper.carry_slot_count` | `src/carry.loft` | One slot, and its doc says *"Same as player"* — so the PLAYER's slot count is stated here and nowhere else.  ⚠ It is 1 for every kind of cargo the design names (loot cube, tower-top, beacon, downed helper), which is what makes a carried kind DATA rather than a code path |
| `economy.tower_top_carryover_effect` | DESIGN.md § Q4 | Mechanic carries, effect deferred — validation = "none" |
| `camera.swing_easing_time` | DESIGN.md § Updates: camera locked | Auto-reframe smoothing |
| `wall.wall_hp` + `wall.brace_factor_*` | `src/damage.loft` | A wall's HP is its kind's figure scaled by how its neighbours brace it; `wall_hp` is the BRACED number and almost nothing on a real map gets it |
| `wall.rubble_height_fraction` | `src/damage.loft` | What a broken wall leaves as a heap.  ⚠ MUST stay under a robot's climb or a breach stops being a way in |
| `enemy_regular.body_height` | `src/damage.loft` | The unit the body ramp is counted in — two to four dead robots get the next one over a wall, five is a heap it cannot climb |
| `enemy_regular.damage_to_wall` | `src/spawn.loft` | What a besieging enemy spends per second; the four small-robot roles will differ in THIS and nothing else |
| `tower.range` + `fire_interval` + `damage_per_shot` | `src/tower.loft` | ⚠ The interval is 1.5 ticks, which is why a tower banks charge rather than firing per tick |
| `tower.height` + `enemy_regular.height` | `src/tower.loft`, `src/damage.loft` | The two ends of a sight line — a shot runs from the tower's hex plus 6.0 m to the target's hex plus 1.0 m, and every obstacle is judged against the line between them.  ⚠ Moving either moves what a tower can see over, and nothing else has to change |
| `enemy_regular.speed_engage` | `src/spawn.loft` | Today the tick's own length is derived from it — ⚠ a coupling the design intends to break (DESIGN.md § Speed must NOT be tied to the tick) |
| `enemy_regular.damage_to_core_wallet` + `economy.starting_budget_first_base` | `src/wallet.loft` | The only end state dryopea has.  `core.hp` is `null`, so a base falls when 200 points reach zero rather than when anything is destroyed |
| `player_vehicle.salvage_rate` | `src/vehicle.loft` | ⚠ **The whole crew's rate, not just the player's** — `§ helper` deliberately has no salvage figure, because DESIGN.md § 9 puts an NPC crew on the same chassis and one rate is one implementation (`salvage_at`).  A per-helper figure arrives with § 9's skill profiles and not before |
| `helper.speed` | `src/helper.loft` | ⚠ 2.5 hex/s is 1.667 hexes a tick — the first speed in the design that does not fit, and why a helper BANKS progress.  It does **not** ask for a shorter tick (plan 14 H0) |
| `core.footprint_layout` | `src/wallet.loft` | ⚠ Read for REACH, not for blocking.  The core is a radius-1 disc, so an enemy within one hex is standing ON it — which is where `NIBBLE_REACH_HEXES` comes from instead of a melee range plan 12 invented.  (`tower.footprint_layout` is the same shape and is still NOT built — a tower stands on one hex) |

## Loading + modding

⚠ **Nothing loads this file yet.**  Every value the engine
consumes today is a constant in loft with a `numbers.json §
section.key` comment pointing back here —
`src/damage.loft`, `src/tower.loft` and `src/spawn.loft` each
carry that note at the top of their constants.  The file is
therefore the *specification* the code is checked against by
hand, and the loader below is the intended flow rather than
the current one.  A value changed here does **not** change the
game until the matching constant moves.

The intended flow:

1. Game starts → reads `examples/numbers.json` (or whatever
   path the install ships at).
2. Each section is bound to a strongly-typed config struct in
   loft code.
3. Player tweaks a value (e.g. raises `tower.range` from 15
   to 20) and re-launches.  Effect visible immediately.
4. Modders ship a forked numbers.json alongside a forked
   palette.json / waves.json / maps for a custom variant.

No code changes needed for any of this; the build is the
*engine*, not the *content*.  See DESIGN.md § Moddability.

## Updating values

When a value changes in `numbers.json`:

- Update the `doc` field if the *meaning* changed (not just
  the value).
- Note here in a one-line bullet if the change is
  *structural* (e.g., adding a new section, removing a
  parameter).
- Defer to the JSON's own inline docs for per-parameter
  rationale.

## See also

- [`../examples/numbers.json`](../examples/numbers.json) — the loadable config (source of truth).
- [`../examples/palette.json`](../examples/palette.json) — companion: ground-type palette.
- [`../examples/waves.json`](../examples/waves.json) — companion: wave count list (will fold into numbers.json eventually; kept separate for now to mirror plan 03's authoring shape).
- [`DESIGN.md`](DESIGN.md) — every mechanic the parameters attach to.
- [`PROXY_ART.md`](PROXY_ART.md) — geometry that some parameters reference.
- [`GROUND_TYPES.md`](GROUND_TYPES.md) — palette-internal slope/drop/height-override values (kept separate from numbers.json — they live with the palette content rather than the engine config).
