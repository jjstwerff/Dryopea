<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `09` — Stop hand-rolling: the lattice, and the input layer

**Value:** `F` (foundation — unblocks plan 07 and every future library
borrow) · **Effort:** `MH`

Two subjects, one plan, because they touch the same three files:
`camera.loft` (which carries both a lattice `Hex` and the pan flags),
`editor_step.loft` and `main.loft`.  The **C** phases convert the lattice;
the **I** phases retire dryopea's hand-written input layer for the `input`
library.  I0–I1 land FIRST — they are lattice-independent, and doing them
first means the bigger change happens once, on a settled seam.

## Status

**Active — C0 + I0 shipped 2026-08-12, and I0's finding @D001 is fixed
the same day; I1 is next and now has the corrected seam it needs.** This is plan 07's `W0c`,
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

And dryopea stops carrying its own input layer: named actions, key
bindings and edge detection are `input`'s job, not `camera.loft`'s +
`main.loft`'s 28 `gl_key_pressed` calls + `editor_step`'s remembered
previous frame.

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
| **I0** — probe: does `input`'s edge model match the seam's? | XS | a probe first | reproduce the three semantics plan 08 pinned — a tap fires once, a HELD action fires once, a level action repeats — against `input`'s `is_action_just_pressed`. **The deliverable is the answer**: `input_new` documents "first frame counts as a transition", which the seam does not do, so if they differ I1 changes shape before it is built | **Shipped** — they MATCH; see § I0 |
| **I1** — the seam takes its input from `input` | S | parallel run | both paths run side by side and the resulting `EditorState` is compared field by field — the V1a gate, reused — then the old path is deleted. Second net: plan 08's edge tests unchanged and green, and `scripts/validate.sh` reports the SAME count it did before the swap (233 over 14 scripts as of 2026-08-12 — read it, do not trust this number) | Open — its prerequisite (@D001, § The forge is deleted) is **done** |
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

### I0 — the answer: the edge models MATCH, and the divergence is somewhere else

Measured 2026-08-12, loft 2026.8.0, interpreted.  Three throwaway probes:
`input` alone, the seam alone, and both loaded together.

```
        input                                  the seam
A1  first tick, key DOWN -> fires        B1  first frame, Tab DOWN -> toggles
A2  held 5 ticks         -> 1 edge       B2  held 5 frames         -> 1 toggle
A2  pressed              -> 5/5         B3  held palette key 5x   -> idempotent
A3  down, up, down       -> 2 edges
A4  first tick, key UP   -> no fire
```

**The mismatch this phase was written to find does not exist.**  The plan
predicted that `input_new`'s documented *"first frame counts as a
transition"* would differ from the seam.  It does not: the seam starts
`prev = editor_input_empty()`, every flag false, which **is** "first frame
counts as a transition".  Both agree on all three semantics plan 08
pinned, so I1 does not change shape.  The prediction stays on the page,
because a probe that only confirms is a probe that was not worth running
— this one was worth running for the reason below.

⚠ **What DOES diverge: the seam forges its own `prev` mid-step.**  Four
sites write `s.prev.in_mouse_left = false` inside `editor_step`
(`:331` mode toggle, `:390` reload, `:406` clear-all, `:418` undo).
*(As I0 found them — all four are **deleted** as of 2026-08-12; see
§ The forge is deleted below.  The line numbers are I0's and have
since moved.)*
`input` cannot express that: `is_keys_prev` is opaque, and the only
caller-visible lever is `input_set_bindings`, which suppresses the whole
ACTION for as long as the rebind stands (probe A5) rather than one step's
read of one flag.

⚠ **And the forge is a BUG, so I1 deletes it rather than porting it.**
Measured — ground mode, button held and painting, then Tab with the button
still down:

```
B4  Tab with the button HELD  -> mode = 1, markers = 1
B4b Tab with the button UP    -> mode = 1, markers = 0
```

A marker is placed by the very frame that flips the mode.  Clearing
`prev.in_mouse_left` is what makes `input.in_mouse_left &&
!s.prev.in_mouse_left` true at the marker branch (`:510`) below it — so
the write **manufactures** the rising edge its own comment says it exists
to suppress.  `s.prev` is overwritten wholesale at `:525`, so the write
never reaches the next frame; its only reachable effect is on branches
below it in the same step.  The stroke it was meant to end is already
ended by `s.painting = false`, set beside each of the four sites.

The clear-all site is worse, because there is no mode guard on it:

```
B5  Ctrl+N, button HELD -> after clear = 1, at hover(7,0) = true
```

"Clear all" empties the layer and then puts a new marker on it.  Two of
the four sites are measured; the other two are reasoned about in @D001
and I1 tests all four rather than trusting that.

Filed as [`PROBLEMS.md` @D001](../../PROBLEMS.md).  **I0 changed no
`src/`** — it is a probe phase — so I1 inherits the deletion and one test
per site.  Plan 08's existing edge tests cannot see any of this: none of
them holds the button across another action, which is the whole gesture.

⚠ **A third finding, unasked for: `camera::InputState` works.**
`camera.loft` declares `pub struct InputState`; so does `input`.  Naming
the bare type with both loaded is a clean compile error that states its
own fix —

```
error: `InputState` is declared by more than one package here —
       write camera::InputState or input::InputState to say which
```

— and the qualified form compiles and runs beside `input`'s.  This is
**not** the `Double structure type …` panic that
[`QUESTIONS_FOR_LOFT.md`](../../QUESTIONS_FOR_LOFT.md) records as
blocking plan 07 W1; that entry was already moved to Resolved and this
independently confirms it.  So I1 needs no rename — but it must qualify,
and the ambiguity error has a nasty side effect worth knowing before it
bites someone: it dumps a **false** `warning[lost-write]` against
`src/spawn.loft`, filed as
[loft#883](https://github.com/loft-lang/loft/issues/883).  That warning
is wrong; the write persists on both backends.  Do not go fix
`move_order`.

⚠ **I0 puts I1's gate in tension with itself, and the order resolves it.**
I1's Verify is a parallel run: old path and new path compared field by
field on the same `EditorState`. @D001's fix deliberately makes them
DIFFER — on the one gesture where the old path places a spurious marker.
So the two land in that order, not together: **delete the forge first,
with its own four tests, and let the parallel run compare against the
corrected seam.** A parallel run against the buggy one either goes red
for the right reason and gets waved through, or gets "fixed" by porting
the bug — and the second is what happens when a gate goes red on a day
you are changing something else.

#### The forge is deleted — 2026-08-12, ahead of I1

All four writes are gone from `src/editor_step.loft`; nothing replaced
them, because the stroke each claimed to end is already ended by the
`s.painting = false` beside it. The gate is
[`tests/09_d001_the_forged_edge.loft`](../../tests/09_d001_the_forged_edge.loft),
written and run BEFORE the deletion so it could be seen failing:

```
site :331 toggle     RED → green      site :406 clear all  RED → green
site :390 reload     RED → green      site :418 undo       green → green
```

The two sites @D001 only *reasoned* about are now measured, and the
reasoning holds both times: **reload is harmful** (it does not touch
the mode, so the forged edge lands straight in the marker branch), and
**undo is DEAD** — its guard implies ground mode while the marker
branch needs marker mode, and no gesture holds both, because every
route to marker mode runs the toggle and empties the stroke on the way
past. So the deletion fixes three sites and removes one dead write.

Suite 497 → **505 green**; `scripts/validate.sh` unchanged at **233
measurements over 14 scripts**, which is the number I1's Verify column
tells you to read rather than trust. The invariant is now stated at
the chokepoint — `s.prev` is read-only for the whole of a step — so
I1 inherits a seam whose edge record is written in exactly one place,
which is the shape `input` can actually take over.

### I1 — and the plan-08 decision it revisits

Plan 08 V0 decided **"edge detection lives in the seam, not the caller"**,
because a caller that resolved edges would make the script and the editor
two different machines.  Adopting `input` moves edge detection back to the
input layer, so that decision is being revisited, not ignored.

It survives, for a better reason than it was made with.  `input` ships
`input_tick_from_state` — *"advance state from a caller-supplied snapshot
instead of polling graphics; used by tests (no GL context) and by
record-playback systems"* — so a script drives the SAME input object the GL
loop drives, and the two machines stay one.

But it inverts § One table, two readers.  Today there is no key table in the
runner because scripts name actions.  With `input`, a `Bindings` set maps
keys → actions and **both** readers use it: the GL loop polls it, and the
runner resolves an action name through the same bindings.  The table exists
— and there is exactly one of it, which is what that section actually
wanted.  A bonus falls out: rebinding becomes testable, because a script
that still passes under remapped keys proves the indirection is real.

⚠ **`do Tab` must still fail.**  Whatever I1 does, the script vocabulary
names actions.  If a key name starts working, a second table was built.

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
| **I0** ✅ | a key held five frames fires its action ONCE — **both do** | `input`'s edges mean what the seam's mean, on all three semantics | the predicted "first frame" divergence **is not real**; the real one is the seam forging `prev` mid-step, which is @D001 |
| **I1** | the five scenarios land on the same `EditorState` | swapping the input layer changes nothing a player could see | `do Tab` working means a second key table was built |

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
