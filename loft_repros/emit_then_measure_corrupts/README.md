<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Iterating one struct's `vector<Struct>` field in two library functions truncates it

⚠⚠ **THE BUG THIS RECORDS WAS FIXED IN LOFT ON 2026-08-18**, the same day it was
filed ([loft#969](https://github.com/loft-lang/loft/issues/969)) — the binary
built at 11:45 is clean over 25 runs where the 2026-08-16 one corrupts 10 of 10.
⚠ Both report `loft 2026.8.0`, so **the version string does not separate them**.
This directory is kept anyway, because what it holds is the EXCLUSION LIST
below, and that is what anyone would need if the shape recurs.

⚠⚠ **THIS DIRECTORY IS A NEGATIVE CONTROL.**  It does **not** reproduce the bug.
It is kept because it records which ingredients are **not** sufficient, which is
most of the value anyone chasing this needs — seven of them were tested and
excluded before the recipe below was found in the consumer.

## What reproduces, in dryopea

Two library functions over the same `Part`, which holds a `vector<Limb>` field:

- `part_box` — walks `p.pt_limbs`, walks `p.pt_rig`'s parallel vectors per limb,
  returns a 6-tuple;
- a **ten-line** emitter that walks `p.pt_limbs` and appends one vertex per limb
  to a `mesh3d::Mesh`:

```loft
use mesh3d;
use part;

pub fn tiny_emit(m: Mesh, p: Part) -> integer {
  n = 0;
  for l in p.pt_limbs {
    add_vertex(m, vertex(vec3(l.lb_cx, l.lb_cy, l.lb_cz), vec3(0.0, 0.0, 1.0), vec2(0.0, 0.0)));
    n += 1;
  }
  n
}
```

Called in this order on a part fetched from a `hash<Part[pt_name]>`:

```
1 tiny_emit   -> 8 verts                   correct (the part has 8 limbs)
2 part_box    -> z -0.37..0.56             correct
3 tiny_emit   -> 1 vert                    THE VECTOR HAS BEEN TRUNCATED
4 part_size   -> (0, 0, 0)                 and the part now has no size
```

⚠ **Each function alone is fine, repeated indefinitely.**  `tiny_emit` three
times in a row answers 8, 8, 8.  `part_box` twice in a row answers correctly
twice.  Only the interleaving corrupts, and the damage then **spreads to other
parts in the same set** — a different part fetched afterwards reads 0 limbs.

⚠ Under `loft test` the suite SIGSEGVs in `OpLengthVector` inside the stdlib, in
whichever file runs next.

## Ingredients tested and EXCLUDED — this is what the directory is for

Each was added to `run.loft` here and did **not** reproduce:

1. a struct with a `vector<Struct>` field, iterated by two library functions;
2. a `hash<Struct[field]>` those parts are fetched from;
3. an aggregator the consumer reaches both modules through;
4. a struct from a **registry library** (`hex_body::Rig`) stored in a field, and
   its parallel vectors indexed through it;
5. posing every vertex through that library (`rig_world_frame3` / `frame_point`);
6. volume — 288 triangles rather than 12;
7. the `graphics` cdylib loaded by the aggregator;
8. the extra `Part` fields (`pt_fits`, `pt_socks`, `pt_binds`) and a set holding
   four parts rather than one.

So the trigger needs something further from the consumer's context that seven
attempts did not isolate.  ⚠ It is **deterministic** in dryopea — the same three
calls give the same wrong numbers every run.

## Running this

```
loft run.loft
```

It prints four lines and all four are correct, which is the point.
