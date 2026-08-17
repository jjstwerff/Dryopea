<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# probe/ — cheap experiments that could kill a design

A **probe** is the smallest thing that can prove a plan's load-bearing
assumption wrong, run *before* the plan is written.  It is not a test and is
deliberately not in `scripts/test.sh`.

⚠ **A probe is kept when its answer is load-bearing**, so the claim can be
re-checked rather than believed.  Most probes are throwaway and leave only a
row in a plan (`plans/19` § P0 is one).  A probe earns a directory here when a
later reader would otherwise have to take a number on trust — or when the
environment it measured could change under the project.

| probe | question | answer |
|---|---|---|
| [`r0/`](r0/) | can a GL frame be **gated with no display**? | **yes** — and with **zero** colour drift over 76 800 px.  [`docs/RENDERER.md`](../docs/RENDERER.md) § R0 |
| [`m3/`](m3/) | does a **SHADER-written** palette colour survive that round trip **exactly**? | **yes** — drift **0** over 691 200 px, on the real mesher and camera with culling on.  [`plans/25`](../plans/25-the-terrain-mesh/README.md) § M3, `@M026` |

⚠ **`m3` is not `r0` again, and that is the point of it existing.**  `r0`
measured a canvas BLIT: pixels that were already 8-bit integers, handed to GL
and read back.  A fragment shader writes a **float**, which can lose a bit to
`GL_DITHER` (on by default in the GL spec), to an sRGB-encoding framebuffer, or
to the driver's float→unorm8 rounding — and one bit would have made plan 25
M3's `other == 0` a gate that could not be written.  ⚠ It depends on dryopea by
path on purpose: a hand-written triangle would have answered for a triangle
nobody ships.

## Running one

```bash
bash probe/r0/run.sh
```

⚠ Each probe carries its own `loft.toml`.  That is not duplication to be
tidied: a probe may need a dependency dryopea does not have — `r0` needs
`imaging` — and **adding it to dryopea's manifest on the strength of a probe is
how an experiment becomes a commitment nobody decided to make.**  The dependency
joins `loft.toml` when a phase needs it, with that phase as the reason.

## Why these are not tests

They spawn things a test must not: `r0` starts an X server.  ⚠ And their value
is in having been run *at a moment* — before a design was committed to — which
a green suite cannot express.  A probe that becomes load-bearing for the
shipped code should graduate into `tests/` as a real gate; until then, the plan
that cites it is where its answer lives.
