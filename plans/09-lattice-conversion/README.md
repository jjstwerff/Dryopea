<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `09` — Lattice conversion: dryopea moves to pointy-top, odd-r offset

**Value:** `F` (foundation — unblocks plan 07 and every future library
borrow) · **Effort:** `MH`

## Status

**Active — C0 shipped 2026-08-12; C1 is next.** This is plan 07's `W0c`,
cut into its own plan because it is multi-phase, stands alone, and plan 07
is long enough already. It is a **precondition** for plan 07's asset
interchange, not a part of it: converting the lattice is worth doing whether
or not dryopea ever reads a `.hxw`, because every library it might borrow
from speaks the other one.

**The decision this executes** (project owner, 2026-08-12): dryopea is the
only axial-flat-top consumer in the ecosystem, so **dryopea converts** — the
libraries do not move. The upstream ask for a second `gridmesh` layout was
withdrawn and closed on the strength of it
([loft-libs-graphics#24](https://github.com/loft-lang/loft-libs-graphics/issues/24)).
The evidence that dryopea is the odd one out is in
[`plans/07`](../07-shared-world-substrate/README.md) § Correction: the
coordinate convention.

## Goal

Every coordinate dryopea owns is **pointy-top, odd-r offset** — the
convention `hex_grid` calls "THE CONVENTION (shared with moros — the single
executable source of it)" — and that is **proved against `hex_grid` itself**,
cell for cell, rather than against pictures that were rebaselined to agree.

## The gate is an oracle, not a golden

⚠ **This section is the plan.** Everything else is sequencing.

A lattice conversion has an obvious-looking gate — *rebaseline the goldens
and look at them* — and that gate is worthless. A rebaselined golden agrees
with whatever was drawn, **including a shear**. It cannot be surprised,
which by [`plans/README.md`](../README.md) § What makes a step SAFE means it
is not validation at all.

moros paid for this. Axial cube distance — `max(|dq|, |dr|, |dq+dr|)` — was
applied to coordinates that are odd-r offset, so `(0,0)` and its SW
neighbour read as **two** steps apart instead of one, and *"the editor's road
width, scatter reach, storey footprint and house outline were all sheared
blobs rather than discs"* until someone measured one (moros#10). Every
picture looked plausible the whole time.

So every phase below compares against **`hex_grid` as an oracle**, and the
comparison is cell-for-cell over a swept window, not by eye.

**C0 already proved the oracle can surprise us.** Run 2026-08-12:

```
neighbours of (0,0):  d=0 (1,0)  d=1 (0,-1)  d=2 (-1,-1)  …
neighbours of (0,1):  d=0 (1,1)  d=1 (1,0)   d=2 (0,0)    …   ← parity shift
hex_distance((0,0), (-1,-1)) = 1        ← dryopea's axial math answers 2
hex_to_px(0,1) = (0.866, 1.500)         ← 0.866 = √3/2, the odd-row half-shift
```

That last disagreement is the point: the oracle and dryopea's current
answers **differ today**, so a sweep comparing them is a test that can fail
for a real reason. A gate that agreed with dryopea before the conversion
would be measuring nothing.

## Two changes, not one

The conversion is usually described as one thing. It is two independent
ones, and separating them is what makes the phases cuttable:

| | what changes | what notices |
|---|---|---|
| **Geometry** | flat-top → pointy-top: where a hex's corners are | the 16 goldens, `frame` shares |
| **Labels** | axial → odd-r offset: which `(q, r)` names a cell | maps, `.keys` scripts, direction tables, every state assertion |

They are separable because the relabel is a pure bijection
(`col = q + (r - (r & 1)) / 2`) that preserves adjacency, and the redraw
touches no coordinate. Keeping them apart gives each its own exact
comparison; doing them in one edit gives neither.

## Phases

Cut against [`plans/README.md`](../README.md) § What makes a step SAFE. The
**Shape** column names which of the three safe shapes each step uses.

| Phase | Effort | Shape | Verify | Status |
|---|---|---|---|---|
| **C0** — probe: can `hex_grid` be the oracle? | XS | a probe first | its answers for a hand-checked cell set, AND that they **disagree** with dryopea's current axial math — an oracle that already agrees proves nothing | **Shipped** |
| **C1** — `lattice.loft` beside `world.loft` | S | parallel run | sweep ±16 cells: dryopea's neighbour / distance / corner answers equal `hex_grid`'s cell for cell. Negative control: run the sweep against the CURRENT axial functions — it must go RED, or the sweep cannot see the bug it exists to catch | Open |
| **C2** — the relabel, and what it must preserve | S | parallel run | `axial_to_offset` ∘ `offset_to_axial` = identity over the sweep; and **adjacency is preserved** — every axial-adjacent pair maps to a `hex_grid`-adjacent pair. An off-by-one in the parity term goes red on odd rows only, which is exactly the shape that hides | Open |
| **C3** — the renderer draws pointy-top | M | one site at a time | hex centres equal `hex_grid::hex_to_px` under the camera transform (to a float ε); `screen_to_hex ∘ world_to_canvas` = identity over the sweep; goldens rebaselined ONCE and reviewed; plan 08's `frame` bands still hold | Open |
| **C4** — `paint_line`, marker arrows, spawn approach | M | one site at a time | per site: `paint_line` equals `hex_grid`'s line over the sweep; each marker arrow points at the neighbour it names; `a-wave-approaches` still shows `range` decreasing monotonically | Open |
| **C5** — migrate the data | S | parallel run | C2's adjacency check applied to every real map + `.keys` script; painted counts identical before and after; scripts converted BY the converter, never by hand | Open |
| **C6** — delete the axial layer | XS | — | `grep` finds no axial helper; suite + `scripts/validate.sh` green | Open |

⚠ **No phase is `H`.** Plan 07 carried this as one `H` phase, which fails
[`plans/README.md`](../README.md) § The two mechanical checks — an `H` step
has no half-done state with anything exact to compare against. The split
above is what that check was for.

### C1 — the new layer, and why it can go red alone

`src/lattice.loft` implements the target convention: `nb(q, r, d)`,
`distance`, `line`, `corner`, `to_px` / `from_px`. Nothing calls it yet.

That would normally be the classic un-cuttable phase — *something built and
called by nobody cannot fail*. It is a real phase here **only because of the
sweep**: `tests/09_c1_oracle.loft` compares every answer to `hex_grid`'s over
a bounded window, so a wrong direction table is red immediately, with no
consumer in sight. The negative control in the Verify column is what makes
that claim honest — the same sweep pointed at today's `world.loft` must fail.

### C3 — the one place the goldens are allowed to move

Rebaselining 16 goldens is the loudest part of this plan and the least
informative. They move once, in C3, and the review is by eye for *plausible*
— the load-bearing check in that phase is the centre-point comparison
against `hex_grid::hex_to_px`, which no rebaseline can launder.

Plan 08's measurements are the second net: `kind`, `marker`, `count` and
`range` are exact and label-only, so C3 must leave every one of them
untouched. A renderer change that moves a `kind` assertion has changed
something other than the drawing.

### C5 — the data, and the one decision left open

Maps, `examples/*.json` and the five `.keys` scenarios all carry axial
coordinates in their text.

**Open:** whether converted maps bump `MapFile.version` and the loader
migrates on read, or the files are converted once and old files are refused.
dryopea has no released maps and one save slot, so refusing is defensible and
much simpler — but plan 04 (map library) is where saved content starts
mattering, and this is the cheapest moment to decide it. **Not decided here.**

## Invariant gate

| Phase | Concrete expected result | Invariant pinned | Negative control |
|---|---|---|---|
| **C0** | `hex_distance((0,0), (-1,-1)) == 1` | the oracle disagrees with axial, so it can surprise us | an oracle that already matched dryopea would be measuring nothing |
| **C1** | the ±16 sweep is green against `hex_grid` | dryopea's lattice IS `hex_grid`'s | the same sweep against today's axial code must go RED |
| **C2** | every axial-adjacent pair stays adjacent | the relabel is a bijection that preserves structure | an off-by-one parity term breaks odd rows only — and must be caught there |
| **C3** | hex centres equal `hex_grid::hex_to_px` | the picture follows the lattice, not the other way round | a rebaselined golden agrees with a shear; the centre check does not |
| **C4** | `a-wave-approaches` range still decreases | the game's own behaviour survives the move | enemies that reach the core in a different number of ticks means the metric moved |
| **C5** | converted maps keep painted counts + adjacency | a relabel is not a content change | a map that gains or loses a hex was converted wrong |

## Open questions

1. **Does `HEX_DIAMETER = 1.5 m` survive?** `hex_grid` fixes its own world
   scale (`L = √3` per hex step). dryopea's metres are a rendering constant,
   but plan 02's solver and plan 07's 3D path both want real units — settle
   whether dryopea scales `hex_grid`'s output or redefines its metre.
2. **Do the six direction NAMES survive?** dryopea's spawn directions are
   0..5 with a documented meaning (`R` rotates through them). `hex_grid`'s
   `hex_neighbor` dir order is its own. If they differ, every saved marker's
   `direction` is remapped in C5 — and § C5's open decision covers it.
3. **Does `gridmesh` become correct-by-construction?** `src/chunks.loft`
   currently feeds axial `(q,r)` as `(x,y)` with `halo_k = 0`, which is
   sound only because the fill is per-cell-independent. After C6 that stops
   being a coincidence, and neighbour-dependent rules (coastlines, slope
   seams) become available without an adapter — the thing #24 asked for,
   obtained by moving instead.

## See also

- [`plans/07-shared-world-substrate`](../07-shared-world-substrate/README.md)
  § Correction: the coordinate convention — the evidence and the decision;
  § Asset interchange — what this unblocks.
- [`plans/08-game-validation`](../08-game-validation/README.md) — the
  `scripts/validate.sh` gate C3–C5 lean on.
- [`CLAUDE.md`](../../CLAUDE.md) § Hex convention — rewritten by C6, and
  **stale until then**.
