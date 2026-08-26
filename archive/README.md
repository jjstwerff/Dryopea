<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# archive

Salvaged artefacts from the 2023-era prototype (the original
`Dryopea` repo, now private at `jjstwerff/dryopea-archive`), plus
later **seed material handed over by the project owner**.
These files are **historical reference only**; they do not build
and are not maintained.

⚠ **Never edit a file here to keep it current.**  Each one is the
record of what was thought at the time; correcting it destroys the
only thing it is for.  Where a file disagrees with
[`../docs/DESIGN.md`](../docs/DESIGN.md) or
[`../docs/SETTING.md`](../docs/SETTING.md), those win — and the
disagreement is listed in
[`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md).

Living design that derives from this material is in
[`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md); the
canonical current design is in
[`../docs/DESIGN.md`](../docs/DESIGN.md).

## Contents

| File | Origin | What it is |
|---|---|---|
| `world.gcp` | `code/overland/world.gcp` | Game data schema in proto-loft (the `.gcp` extension predates loft's `.loft`). Classes for `Mission`, `Faction`, `Item`, `Building`, `Machine`, `BuildQueue`, `Link`; enums `Statistic`, `ItemType`, `LinkType`. Direct foundation for D4 (economy) + D5 (scramble inventory). |
| `main.gcp` | `code/overland/main.gcp` | CLI entry point of the 2023 `overland` generator (`<png-file>` in → `.glb` out). Shows the original "world is generated from a PNG" approach. |
| `gameplay.data` | `code/overland/data/gameplay.data` | 31 KB of filled-in game data — factions, items, missions. ⚠ **Mined 2026-08-26** — the ~55-item knowledge tree, the nine factions, the 19-animal bestiary and the 34-row material catalogue are routed in [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) § 5, and it is where the planet's name comes from. |
| `terrain.data` | `code/overland/data/terrain.data` | Terrain definitions. Inspect alongside `examples/terrain.txt`. |
| `seed-notes.md` | project owner, hand-over 2026-08-26 | The owner's pre-plan idea dump — minimal product, build steps, the **Linn Everett / Ian Thorne opening scene**, planet lore (pollen, tree biology, the settlers), plot, mission and dungeon briefs, campaign progression, endings, and the material / part / weapon / machine catalogues. ⚠ Routed block by block in [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) § 4. |
| `world-prototype.loft` | `archive/world.loft` (in the original repo) | Partial loft port of the world model. Hill formula `(r-t)²(r+t)²`, `Action` enum (Flatten / Add / Subtract / Smooth / Pillar), multi-level `Position`, 32-chunk block addressing. Half-converted from C++ — useful as a thinking record, not as code. |

## What was deliberately NOT salvaged

- The entire loft-engine precursor (`Cargo.toml`, `src/`, `lib/`,
  `default/`, `tests/`, `webassembly/`, `archive/map.rs` etc.) —
  superseded by [loft](https://github.com/jjstwerff/loft) itself.
- Loft language documentation (`doc/*.html`,
  `loft-reference.{pdf,typ}`, `print.html`, `index.html`) —
  current versions live in loft.
- Build / IDE configuration (`Makefile`, `clippy.toml`,
  `.idea/`, `*.iml`, `rusty-tags.vi`).
- `example/todo.json` — that was a *personal* todo list
  (cleaning, exercise, call a friend), not a game file.
- The top section of the original `todo` file (loft-engine
  development notes). Only the bottom section, which contains
  game-design notes, was preserved — and only as quoted material
  in [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) § 2.
