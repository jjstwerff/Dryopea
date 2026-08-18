<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# A record fetched through an ACCESSOR reads its vector field as EMPTY

⚠⚠ **Silent, both backends, no imports, 40 lines.**  `repro.loft` prints

```
a: limbs 2      correct
b: limbs 0      WRONG
c: limbs 0
```

and exits 0.  Nothing is logged and nothing aborts — which is why the
consumer met it as *"the second tower a renderer draws has no limbs"*
rather than as a crash.

## The recipe, and every ingredient is necessary

1. a struct field `hash<Item[name]>`;
2. an **accessor function** `fn bag_get(b: Bag, name: text) -> Item?`
   whose whole body is `b.items[name]`;
3. a caller that binds `it = bag_get(b, "one")?` and reads `it`'s
   `vector<…>` field;
4. an **unrelated struct with a vector**, allocated in the caller
   *between* two such calls.

Take any one away and the reading is correct:

| variant | reading |
|---|---|
| `it = b.items["one"]?` **inline at the use site**, no accessor | 2, 2, 2 ✅ |
| no unrelated allocation between the calls | 2, 2, 2 ✅ |
| `if it == null { … }` instead of `?` | 2, **0**, **0** ❌ (so the discharge is innocent) |
| `vector<Struct>` instead of `vector<float>` | 2, **0**, **0** ❌ (so the element type is innocent) |
| the caller never writes to the unrelated struct | 2, **0**, **0** ❌ (so it is the ALLOCATION, not the write) |

⚠ The second and third rows are the useful ones: **the `?` and the
element type are not the trigger**, and a reader who assumes either will
"fix" it without moving the reading.

## Where dryopea met it, and what the louder form looks like

`src/pose.loft::emit_tower` (plan 20 A4) draws a tower by fetching
`tower_base` out of a `PartSet` — a `hash<Part[pt_name]>` — and emitting
its limbs into a `mesh3d::Mesh` the caller allocated.  The first tower
draws; the second reads **0 limbs and emits 0 triangles**.

With `mesh3d::Mesh` as the unrelated allocation the same recipe
sometimes aborts instead of answering zero:

```
Store access out of bounds: rec=4 fld=507376628 width=4 store_bytes=9856
type=103 — the reference is corrupt, not merely out of range
```

⚠ Which of the two you get depends on the allocation history, not on the
source: the same program answers 0 in one arrangement and aborts in
another.  **So a green suite is not evidence** — this shape passed 1361
tests while `emit_tower` was the only caller that had the ingredients.

## The workaround dryopea ships

**Index the hash at the USE SITE** rather than through the accessor:

```loft
base = ps.ps_parts["tower_base"]?;      // correct
// base = partset_get(ps, "tower_base")?;   // the same lookup, empty
```

`src/pose.loft` carries that with a pointer back here; when the fix
ships, it goes back to `partset_get` and `tests/20_a4_the_joints.loft`
is what proves the change.

## Provenance

⚠ Checked against **both** binaries on this box before filing — the
lesson `@M038` cost a session: `~/.local/bin/loft` built 2026-08-18
13:26 and a fresh `target/release/loft` built 13:48 (two commits later,
one of them a heap-correctness PR) **fail identically**.  Filed as
[loft#974](https://github.com/loft-lang/loft/issues/974), 2026-08-18.

⚠ It is *not* [loft#969](https://github.com/loft-lang/loft/issues/969)
(closed, fixed the same day): that one needed **two functions
interleaved** over one part and truncated `8 → 1`; this one needs
**one** function and an unrelated allocation, and empties the vector
completely.  The sibling directory
[`emit_then_measure_corrupts/`](../emit_then_measure_corrupts/) records
969's exclusion list.
