<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# How to play what exists

BACKLOG A4.  ⚠ **What is here today**, not what the design promises —
[`DESIGN.md`](DESIGN.md) is the game dryopea is becoming and § What is
not there yet, below, is the gap.

⚠⚠ **The key table in this file is GATED** against the one the game
reads (`src/bindings.loft::editor_actions`) by
`tests/a4_the_controls.loft`, so a binding that moves goes red here.
It was written because the hand-maintained list in `src/main.loft`'s
header had the palette **off by one** and had been wrong for
twenty-five plans.

---

## Run it

```bash
make play MAP=starter_01          # an authored base — start here
make play PLANET=kepler           # a place that REMEMBERS what you did to it
make play SCRIPT=a-defended-base  # any of the 41 `.keys` scenarios
make play                         # the default save slot, empty on a fresh checkout
```

Three maps ship in [`../maps/`](../maps/README.md); `SCRIPT=` opens any
`.keys` file in `tests/scripts/` or `tests/gl/` as a starting position.
**Escape** closes the window.

⚠⚠ **A PLANET is the one that keeps your work.**  `MAP=` and `SCRIPT=`
open authored content — the same base every time, however you left it —
while `PLANET=` opens *your* world: build a wall, press `Ctrl+S`, quit,
and it is there when you come back.  It lives in
`dryopea_planets/<name>/solo/` and is gitignored, because a map is the
repo's and a planet is yours.  ⚠ A planet remembers the GROUND and the
MARKERS and nothing else — the run itself (your crew, the wallet, a
wall still being built) goes when the window does.  ⚠ `PLANET=` wins if
you pass more than one.

⚠ It runs **interpreted** (`make play` passes `--interpret`) because the
native backend loads an empty palette — `make play-native` exists only
to test the eventual fix.

## Two modes, one window

A session opens in the **map editor**.  Press **P** and the window
becomes the **game**: the ground as triangles, every entity as a
part-tree, through a camera that follows your vehicle.  Press **P**
again and the editor comes back — the run is kept, so it is a pause
rather than a restart.

⚠ **The first P re-bakes the world**, which is a visible pause on a
large map.  The console says so.

---

## The keys

### Both modes

| key | action | what it does |
|---|---|---|
| `P` | `toggle_play` | editor ↔ game.  ⚠ A dev-build toggle standing in for plan 05's landing flow |
| `Ctrl`+`S` | `save` | write the map to disk |
| `Ctrl` | `mod_ctrl` | the modifier the five combos below are read against |
| `Shift` | `mod_shift` | anticlockwise to a rotation, boost to a vehicle |
| `Esc` | — | save and exit.  ⚠ Not an action — it is the window's, not the game's |

### In the editor

| key | action | what it does |
|---|---|---|
| `W` `A` `S` `D` | `pan_north` `pan_west` `pan_south` `pan_east` | pan the camera (held → about 8 hexes/sec) |
| `H` | `recentre` | camera back to the origin, leaving both layers alone |
| `Tab` | `toggle_mode` | ground ↔ marker mode.  The badge top-right says which: grass-green ground, hot pink marker |
| `K` | `cycle_kind` | marker mode: spawn → target → tower → trap → spawn |
| `R` | `rotate` | marker mode: turn the place-direction clockwise.  `Shift`+`R` the other way.  No-op for a target |
| `Ctrl`+`Z` | `undo` | one drag is one undo, 50 deep |
| `Ctrl`+`Y` | `redo` | any other action discards the redo branch |
| `Ctrl`+`R` | `reload` | re-read both layers from disk.  `Ctrl`+`Z` undoes it |
| `Ctrl`+`N` | `clear` | empty both layers and reset the camera, as one undoable action |
| mouse left | — | paint the hovered hex, or place / remove a marker.  Drag paints a line |
| mouse wheel | — | ⚠⚠ **nothing.**  It moves a number no renderer reads — `PROBLEMS.md` `@D002`, open |

### In the game

⚠ `W` `A` `S` `D` are **deliberately** the same four keys, and the two
sets are never both live: `DESIGN.md` § 12 locks the play camera, so a
played frame never pans and an edited one never drives.

| key | action | what it does |
|---|---|---|
| `W` `A` `S` `D` | `drive_north` `drive_west` `drive_south` `drive_east` | drive, at 3 hexes/sec |
| `Shift` | `boost` | 6 hexes/sec for 2 s, then 5 s of cooldown — and it climbs **3.0 m** where a rolling vehicle climbs 0.4.  ⚠ The only way out of a base you have sealed |
| `E` | `carry` | pick up / put down.  **One key**, resolved on what your hands hold; dropping at the core delivers.  ⚠⚠ **Pressed at the core with empty hands it BUYS A TOWER BEACON for 100 points** — carry it to where you want the tower, press `E` again to plant it, and the crew raise it over 30 s.  ⚠ The points leave the wallet at PICKUP, so a beacon in your hands *is* the hundred points; a drop somewhere illegal leaves it on the ground rather than refunding you |
| `Q` | `paint_wall` | ⚠⚠ **wall paint on / off — this is how you BUILD.**  With it on, every hex you drive over is ordered as a wall; drive over one again and the outline is rubbed out.  Your crew raise them, **10 s of one helper's time per hex** — two helpers on one site take half as long.  ⚠ You cannot order a wall on water, on a cliff, on a heap or on something already standing.  ⚠ An outline a helper has **started** can no longer be rubbed out |
| `J` | `toggle_jammer` | ⚠⚠ **the jammer switch — turn your own core off.**  Only works while you are **at the core**; pressing it anywhere else does nothing.  With the core dark **no wave is sent and no list is armed**, ambient robots cross the bubble without losing their signal — and **salvage pays you nothing**.  ⚠⚠ It stops the SUPPLY, never the SIEGE: everything already cut off is still walking to your core, so it is not a panic button.  You can still clear heaps with it off, you are just working for free

⚠⚠ **`Q` is the one key here that is not a place you drive to**, and
that is deliberate rather than an oversight — `DESIGN.md` § 11 calls it
*"an acknowledged exception to the spatial principle"*.  Everything else
in play mode happens because of where you are; this changes what driving
MEANS.  It is also the key you are most likely never to find, which is
why the vehicle is meant to tint red while it is on.

⚠ **`J` is half a place**: the switch only answers at the core, so
driving there is the reach — but a toggle cannot BE a position, because
parking beside the core would flip it every frame.  ⚠ Nothing in the
world tells you it is there yet; that is meant to be the crew's job
(`PROGRESSION.md` § P2f) and a core that LOOKS dark (`DESIGN.md`
§ Two surface signals), and neither is built.

⚠ **The editor's mouse still works while the clock runs**, and the
simulation sees it — measured: a `wall_high` painted mid-run makes the
hex robots were stepping through impassable on the next tick.  ⚠ And
since plan 27 C2 **the renderer sees it too**: the live mesh used to
watch the *height layer* only, which could not see a hex you painted, so
what you drew stayed invisible until you left play mode and came back
(`@X269`).

### Painting

The number selects a palette entry, and the entry is what the click
paints.  ⚠⚠ **The key is the index PLUS ONE** — `5` is sand, not grass.

| key | action | paints |
|---|---|---|
| `1` | `palette_1` | `sea` |
| `2` | `palette_2` | `water` |
| `3` | `palette_3` | `rapids` |
| `4` | `palette_4` | `waterfall` |
| `5` | `palette_5` | `sand` |
| `6` | `palette_6` | `grass` |
| `7` | `palette_7` | `hill` |
| `8` | `palette_8` | `rock` |
| `9` | `palette_9` | `steep_rock` |
| `0` | `palette_10` | `wall` |
| `-` | `palette_11` | `wall_high` |
| — | — | `rubble` — ⚠ **no hotkey, deliberately**: the runtime deposits rubble and nobody paints it, so there is nothing to select |

⚠ Painting `sea` **erases** a hex, and an unpainted hex *is* sea.  On
the shipped maps the paint is the geometry: everything outside it is
water no robot can cross.

---

## Starting a run

Pressing **P** starts the clock and puts your vehicle at the core.  It
does **not** start the waves — those are armed and asleep.

⚠⚠ **Drive onto a spawn marker 12 or more hexes from the core** and the
list wakes up: seven waves of 5, 8, 12, 20, 30, 50 and 80 robots, each
arriving after the one before it is cleared plus a lull.  Then drive
home, in front of them.

⚠ A spawn marker closer than 10 hexes to the core is silenced for the
whole mission and never sends anything.

## What you do without pressing anything

⚠⚠ **Most of this game has no key.**  Three of the four things a pilot
does are triggered by *being somewhere*:

- **Repair** — park within one hex of a spent tower for 20 seconds and
  it comes back with a full 30-shot magazine.  ⚠ A tower that is
  *firing* refuses, so upkeep is a timing decision.
- **Salvage** — drive onto rubble and it is collected, at half a metre
  a second, straight into the wallet.
- **Nothing you press attacks.**  You are a noncombatant: the towers
  fight, you keep them fighting.

On `starter_01`, parking between its two towers instead of on the core
is worth **61 ticks and a whole extra wave** — measured,
[`DECISIONS.md`](DECISIONS.md) `@M045`.

## What you see

The ground, the entities, and **one number**: the wallet, in the
**top-left** corner, ramping amber to red as it drains.  It is drawn in
seven-segment rectangles because nothing in this repo can draw a letter
(`@X097`).

⚠⚠ That is the entire HUD, and `DESIGN.md` § HUD says it should be —
no wave counter, no health bar, no minimap, no boost cooldown bar.
Everything else is diegetic: the rotors show boost, the canopy shows
cargo, a tower shows whether it still has a top.

⚠ The console prints a line per tick — wave count, enemies alive,
wallet, where you are.  Today that is the only readout of anything the
HUD refuses.

**In the editor** there are two more, both wordless: a **swatch** of
the palette entry your next click paints, with a dot beside it that is
amber when there are unsaved changes and green when disk matches
memory; and a **badge** in the top-right that is grass-green in ground
mode and hot pink in marker mode — the layer your next click writes to.

## How it ends

The wallet reaching **zero**, and nothing else.  There is no fail
screen and no win state; the base falls, the console says so, and
pressing **P** stops the clock.

⚠ **No base survives its wave list yet.**  The strongest one the repo
can build reaches **wave 5 of 7** (`the_gap_03`), and that is a
property of the game rather than of the maps — nothing lets a base
recover between waves, which is what [`../plans/17`](../plans/17-tower-hot-swap/README.md)
is about.

---

## What is not there yet

| missing | what it means when you play |
|---|---|
| ⚠ **A SEVENTH CREW MEMBER** | The wallet buys towers (`E` at the core) and walls are free, but there is no way to ORDER a helper — `numbers.json` prices one at 100 points and caps the roster at 6, and neither is built.  [`plans/27`](../plans/27-building/README.md) § What this plan does NOT build says why it was left out |
| ⚠⚠ **TEXT** | Nothing can draw a letter (`@X097`), so the wallet is seven-segment rectangles and there is no dialogue, no debrief, no crew chatter — BACKLOG B1 |
| **the landing flow** | P puts you at the core.  Choosing where to land is plan 05 |
| **a crew you can direct** | Helpers exist in the simulation, and a `.keys` scenario can place them — `make play SCRIPT=a-base-that-plays-its-list` gives you one.  ⚠ A **map** cannot carry crew, so every shipped map is played solo |
| **zoom** | The wheel changes a number no renderer reads — `@D002` |
| **sound** | None at all |
| **the scramble** | The evacuation the whole design is named after is not built |

## See also

- [`../maps/README.md`](../maps/README.md) — the three authored bases and what each teaches
- [`DESIGN.md`](DESIGN.md) — the game this is becoming
- [`STATUS.md`](STATUS.md) — one line per shipped phase
- [`../plans/BACKLOG.md`](../plans/BACKLOG.md) — what could be built next
