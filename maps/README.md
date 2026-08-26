<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# maps/ — the authored bases

`make play MAP=starter_01` opens one of these.  BACKLOG A2.

| map | shape | what it is for |
|---|---|---|
| [`starter_01`](starter_01.keys) | one walled neck, five hexes wide, two towers on the lane | **the first base** — it teaches WHERE TO STAND and nothing else |
| [`crossroads_02`](crossroads_02.keys) | that neck mirrored, core in the middle, two spawns | **parking is the wrong answer** — the drive between the lanes is worth more than either lane |
| [`the_gap_03`](the_gap_03.keys) | a `steep_rock` massif with one gap, four towers | **terrain, not masonry** — nothing to repair, nothing that can break, and the strongest base the repo can build |

⚠ What each is worth is **measured, not asserted** —
[`docs/DECISIONS.md`](../docs/DECISIONS.md) § `@M045`.

## Two files per map, and one of them is the source

```
starter_01.keys           ← the SOURCE.  Author this.
starter_01.json           ← BUILT.  ~460 ground entries.
starter_01_markers.json   ← BUILT.
```

⚠⚠ **All three are committed**, because `make play MAP=` loads the JSON
and a fresh checkout has no builder run in it.  The `.keys` is what a
reviewer reads: a map arrives in a pull request as *a wall moved two
hexes east* rather than as four hundred changed lines.

## Adding or changing a map

```bash
$EDITOR maps/my_base.keys      # the same gestures the editor makes
make maps MAP=my_base          # writes the pair beside it
make test                      # tests/a2_the_maps.loft gates both
git add maps/my_base.*         # all three
```

⚠ `make maps` **refuses to write a map nobody could play** — see
`src/maps.loft::map_fault`.  The one that catches people: a run starts
when the player drives onto a spawn marker **12+ hexes from the core**
(`WAVE_1_PROVOCATION_HEXES`), so a map whose spawns are all closer
loads, draws, lands the crew, and never sends a wave.

⚠⚠ And a **second** number decides where a spawn goes: a tower reaches
**15** hexes, so a spawn inside *forward tower + 15* is one the towers
shell as it arrives — wave 1 dies without ever walking and the shape
you built the map around never gets used.  All three maps here were
authored with that wrong the first time and measured out of it.

## What a map does NOT hold

⚠⚠ **The ground and the markers, and nothing else.**  A `crew` or a
`schedule` line in a source is authored, played, and then silently
dropped by the save.  A run's wave list is the seven-wave default every
`PlayState` starts with (`waves.loft::wave_list_default`), so every map
here is played **solo** — which is why `crossroads_02` cannot be held.

## Playing a source without building it

```bash
make play SCRIPT=maps/starter_01.keys
```

That is BACKLOG A1's door, and it works on any `.keys` file.  The
difference is only that it replays the source every launch.

## ⚠ The editor writes back here

`make play MAP=<name>` attaches the session to these files, so painting
and pressing Esc **rewrites the map** — that is how you edit one.  A
session that changed nothing leaves the files alone (`src/main.loft`
§ Save on exit), so panning the camera is safe; `git checkout maps/`
undoes the rest.
