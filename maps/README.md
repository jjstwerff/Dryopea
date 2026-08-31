<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# maps/ — the authored bases, and the one that is not one

`make play MAP=starter_01` opens one of these.  BACKLOG A2.

| map | shape | what it is for |
|---|---|---|
| [`starter_01`](starter_01.keys) | one walled neck, five hexes wide, two towers on the lane | **the first base** — it teaches WHERE TO STAND and nothing else |
| [`crossroads_02`](crossroads_02.keys) | that neck mirrored, core in the middle, two spawns | **parking is the wrong answer** — the drive between the lanes is worth more than either lane |
| [`the_gap_03`](the_gap_03.keys) | a `steep_rock` massif with one gap, four towers | **terrain, not masonry** — nothing to repair, nothing that can break, and the strongest base the repo can build |
| [`the_flats_04`](the_flats_04.keys) | 65 hexes square, open floor, a cliff rim, nothing else | ⚠⚠ **ROOM** — the only one that is not a base.  No lane, no neck, no gate, no tower site: it is where you build your OWN |

⚠ What each is worth is **measured, not asserted** —
[`docs/DECISIONS.md`](../docs/DECISIONS.md) § `@M045`.

## ⚠⚠ Three of them teach a shape.  The fourth is the absence of one.

`the_flats_04` is [`plans/34`](../plans/34-the-drive/README.md) D0 and
[`docs/PUZZLES.md`](../docs/PUZZLES.md) § What CANNOT be measured yet is why
it exists: the other three are **34 x 15**, **49 x 13** and **30 x 17** —
long strips, thirteen to seventeen rows tall — and a rail-legal curve is a
run of hexes whose direction changes at most once every N (`@X356`), so a
radius costs space in **both** dimensions.  ***A player cannot build a round
base on a map fifteen rows tall.***

⚠ It follows from `@X360`: **every base that is seen is built by a player**,
so what the map layer owes the shape work is ROOM, not more authored bases.

⚠⚠ **Its gate is not a base's** — `tests/a2_the_maps.loft` § The flats is
ROOM.  It is in `a2_shipped` and not in `a2_bases`, because *poking a spawn
sends a wave at a defence* is a claim it does not make, and replaying its
source through the seam costs **~12 s** where `starter_01` costs 0.3 s.  What
that replay was checking is asked of it directly instead: the source's own
`count painted` and `kind` measurements, re-asked of the pair that shipped.

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
