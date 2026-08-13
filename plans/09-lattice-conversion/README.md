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

**Active — the I phases are DONE (I0 + @D001 + I1, 2026-08-12/13); the
lattice conversion C1–C6 is what remains, and it now happens on a
settled input seam, which is why I0–I1 went first.** This is plan 07's `W0c`,
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
| **I1** — the seam takes its input from `input` | S | parallel run | both paths run side by side and the resulting `EditorState` is compared field by field — the V1a gate, reused — then the old path is deleted. Second net: plan 08's edge tests unchanged and green, and `scripts/validate.sh` reports the SAME count it did before the swap (233 over 14 scripts as of 2026-08-12 — read it, do not trust this number) | **Shipped** — see § I1.  Compared at the INPUT level, which is stronger; 233 held |
| **C0** — probe: can `hex_grid` be the oracle? | XS | a probe first | its answers for a hand-checked cell set, AND that they **disagree** with dryopea's current axial math — an oracle that already agrees proves nothing | **Shipped** |
| **C1** — `lattice.loft` beside `world.loft` | S | parallel run | sweep ±16 cells: dryopea's neighbour / distance / corner answers equal `hex_grid`'s cell for cell. Negative control: run the sweep against the CURRENT axial functions — it must go RED, or the sweep cannot see the bug it exists to catch | **Shipped** — see § C1 shipped.  Negative control run: 8 of 17 red |
| **C2** — the relabel, and what it must preserve | S | parallel run | `axial_to_offset` ∘ `offset_to_axial` = identity over the sweep; and **adjacency is preserved** — every axial-adjacent pair maps to a `hex_grid`-adjacent pair. An off-by-one in the parity term goes red on odd rows only, which is exactly the shape that hides | **Shipped** — see § C2 shipped.  Strengthened to DISTANCE preservation, which implies adjacency AND injectivity |
| **C3** — the renderer draws pointy-top | M | one site at a time | a rendered hex is TALLER than wide (pointy-top, measured in pixels off a real canvas); the camera's hex lands at the canvas centre; neighbours sit one flat-to-flat away in the direction the compass names; `screen_to_hex ∘ world_to_canvas` = identity; plan 08's `frame` bands still hold | **Shipped** — see § C3 shipped.  ⚠ The goldens do NOT move once; the reviewed rebaseline is now C5's |
| **C4** — the marker arrows | S | one site at a time | each marker arrow points at the neighbour it names, measured against the actual step; the arrows are unit length and opposite pairs cancel. ⚠ Was "paint_line, marker arrows, spawn approach" — the other two turned out to be LABEL-space and moved to C5; see § C4 shipped | **Shipped** |
| **C5** — migrate the data AND the label-space code | M→H | parallel run | C2's adjacency check applied to every real map + `.keys` script; painted counts identical before and after; scripts converted BY the converter, never by hand; `a-wave-approaches` still shows `range` decreasing monotonically. ⚠ NO compensating y-flip — a map that comes back looking the same means the compass decision did not take. **⚠ Inherits from C3/C4**: the reviewed golden rebaseline, `paint_line` ×2 (which must move in ONE commit), `enemy_tick`'s step, the flow field, passability, occupancy, and every marker `direction` via `relabel_direction` | Open |
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

### I1 — shipped, and where the gate had to move

Landed 2026-08-13 in two commits: `src/bindings.loft` + the GL loop,
then the script runner.  `main.loft` went from 28 `gl_key_pressed`
calls to one (Escape, which is the loop's own exit and not an editor
action), and `script.loft`'s `script_set_action` — the second table —
is gone.

⚠ **The plan predicted the wrong gate, and the right one is cheaper.**
The Verify column says compare the two paths at the `EditorState`.
That only sees the key combinations some scenario happens to press,
and a binding wrong for a key no script uses is precisely what this
phase exists to catch.  `tests/09_i1_bindings.loft` compares at the
`EditorInput` instead: every key in the table × both modifier states
× both rate-limit states — 208 combinations — against a transcription
of the pre-I1 literal, all 22 fields each.  Seen RED before it was
trusted (rebinding `cycle_kind` from `k` to `h` reports `key 107
ctrl=0 shift=0 pan_due=0: in_cycle_kind differs`).

⚠ **The "edge detection moves to the input layer" framing was the
half that did not survive.**  § I1 above already said the V0 decision
*survives*, and it does — but that settles a fork the Verify column
leaves open, so it is worth stating plainly: `EditorInput` keeps HELD
semantics and `editor_step` still finds the edge against `s.prev`.
`input`'s `is_action_just_*` are unused, deliberately.  Flipping
`EditorInput` to carry resolved events would have made five held
frames fire five times and rewritten every plan-08 edge test — the
"unchanged and green" net is what rules it out.

**The ctrl rule had to be DATA, and that was not foreseen.**
`input::ActionBinding` is a name and a list of keys; it has no
modifier concept, and dryopea needs one for five combos.  Writing the
rule in the resolver would have meant writing it again in the runner
(`do undo` must know it holds Ctrl) — the second table wearing a
different hat.  So `ea_ctrl` rides on the record and both readers get
it from there.  Three names have no table row at all, because they
are not keys: `zoom_in` / `zoom_out` are the wheel, `rotate_ccw` is
rotate with Shift — each still resolved *through* the table.

**The predicted bonus is real and is now a test.**
`test_a_scenario_plays_the_same_on_a_remapped_keyboard` plays
`paint-a-base.keys` twice — default keys, then every action moved to
a different code — and compares the sessions.  Before I1 that test
could not have failed, because there was no keyboard to remap.  Its
negative control (`test_a_broken_remap_breaks_the_scenario`) kills a
palette binding and requires the run to FAIL, so "it still passes"
cannot mean "the script never pressed anything".

**Two behaviours changed on purpose**, both narrowings:
`palette <n>` for an entry with no hotkey is now an error rather than
a frame the seam silently range-checks away (a script asserting
nothing), and `do mod_ctrl` / `do palette_5` are refused so the
vocabulary cannot widen just because the table grew a row.  `do Tab`
still fails, as § I1 requires.

⚠ **`input` ships a PARKED banner and it is stale.**  The library
header says it is blocked on @P391 — `input_new`'s state allocated in
CONST_STORE under a cross-package call, so writes through
`&InputState` panic.  Probed before any of this was built, because
the whole phase rests on it: `input_new`, `input_tick_from_state` and
`input_set_bindings` all survive, a key held five ticks reads pressed
5/5 with one edge, and a rebind takes effect immediately with the old
key going dead.  Recorded in
[`QUESTIONS_FOR_LOFT.md`](../../QUESTIONS_FOR_LOFT.md).

Suite 505 → **518 green**; `scripts/validate.sh` unchanged at **233
measurements over 14 scripts**, which is the number this row told you
to read rather than trust.

### C1 — the new layer, and why it can go red alone

`src/lattice.loft` implements the target convention: `nb(q, r, d)`,
`distance`, `line`, `corner`, `to_px` / `from_px`. Nothing calls it yet.

That would normally be the classic un-cuttable phase — *something built and
called by nobody cannot fail*. It is a real phase here **only because of the
sweep**: `tests/09_c1_oracle.loft` compares every answer to `hex_grid`'s over
a bounded window, so a wrong direction table is red immediately, with no
consumer in sight. The negative control in the Verify column is what makes
that claim honest — the same sweep pointed at today's `world.loft` must fail.

### C1 shipped — and it DELEGATES, which changes what the gate can be

Landed 2026-08-13.  `src/lattice.loft` + `tests/09_c1_oracle.loft`;
nothing calls the new layer yet, exactly as planned.

⚠ **`lattice.loft` delegates to `hex_grid`; it does not reimplement.**
[`CLAUDE.md`](../../CLAUDE.md) § Loft consumer relationship makes that
the rule, and it is also the stronger position: delegation makes "every
coordinate dryopea owns is pointy-top odd-r" true by CONSTRUCTION,
where a second implementation would be true only for as long as two
copies stayed in step — which is the moros#10 failure exactly.  What
the file adds is the two things `hex_grid` cannot know: dryopea's `Hex`
type, and dryopea's metres.

**Which means this row's stated gate is partly tautological, and the
sweep had to be built out of things that can fail anyway.**  "dryopea's
answers equal `hex_grid`'s" compares the library to itself for a pure
pass-through.  It is still worth running — the test writes the argument
ORDER out independently, so a wrapper that fed `.r, .q` is caught — but
the load-bearing checks are the ones that hold regardless of
delegation: the direction round-trip, the metre scale, the relabel
round-trip, the line's on-line property, and the disc count `3N(N+1)+1`
(exact for any hex lattice, which is why `lat_disc` FILTERS a bounding
box rather than enumerating an axial range — the second would make the
count a tautology too).

**Seen RED twice before being trusted.**  Pointed at `world.loft` — the
row's own negative control — 8 of 17 go red, including "the lattices
disagree on more than a quarter of the window — **0** of 1089".  And
breaking only the parity term (`(r - (r&1))/2` → `r/2`) takes down 3,
naming cell **(-16, -15)**: an ODD row, which is the failure shape C2's
Verify says hides.

#### Open question 1 is answered: the metre survives, scaled by 0.75

`hex_grid` works at circumradius 1.0 with centres `√3` apart.  dryopea's
`HEX_DIAMETER` is 1.5 m vertex-to-vertex → circumradius 0.75 m →
centre-to-centre `√3 × 0.75 = 1.299038`, which is **exactly
`HEX_FLAT_TO_FLAT`, the constant `world.loft` already carried**.  So the
two agree already and the conversion is one multiply.  dryopea scales
`hex_grid`'s output and keeps its own metre; `hex_grid` needs no scale
parameter.

#### Two findings that will shear something if forgotten

⚠ **`hex_offset` has no counterpart — the operation is DELETED.**  In
odd-r offset the neighbour delta depends on ROW PARITY (four of the six
change), so a function answering a constant `(dq, dr)` per direction is
not translatable.  `lattice.loft` deliberately does not provide one, and
the sweep asserts both halves: four deltas shift in the new lattice,
zero shift in the axial one.  Today only `tests/03_m5_spawn.loft` calls
`hex_offset`; no `src/` module does — so C4/C5 inherit a test to retire,
not a call graph to unpick.  [`CLAUDE.md`](../../CLAUDE.md) § Hex
convention names `hex_offset` as one of the three places a coordinate
may be stepped; after C6 there are two.

⚠ **`hex_grid`'s compass names assume +y is UP and dryopea's is DOWN.**
It documents "r increases upward" and calls direction 5 `NE`, while
placing row `r+1` at LARGER y — and dryopea renders +y as SOUTH.  So
taken literally the two agree on the LATTICE and disagree on the compass
by a north↔south mirror.  It is not a bug in either side.

**Settled the same day (project owner): follow `hex_grid`'s compass, let
the maps flip.**  `lat_to_metres` / `lat_from_metres` /
`lat_corner_metres` negate y, so direction 5 really is north-east on
dryopea's screen — and existing maps render vertically mirrored, which
is accepted.  Landed in C1 rather than deferred: it is one negation,
nothing calls the file yet, and leaving it would have had C3 render
against a frame it then needed to flip.  See § Open questions 2.

⚠ The negation changes NO convention dryopea already had — world +y
still grows south, the render path still has no y-flip.  It sits in the
lattice→metres conversion beside the metre scale, the other thing
`hex_grid` cannot know.  Its gate is
`test_the_six_directions_point_where_hex_grid_names_them`: the SIGNS of
`(dx, dy)` for all six directions at every cell in the window.  Dropping
the negation fails it on all four diagonals — measured, "dir 1 is SE:
want signs (1, 1), got (1, -1)".  ⚠ The metre ROUND-TRIP stays green
through that mutation, because a consistent flip is invisible to it;
that is precisely why the compass needed its own gate.

⚠ **`hex_grid::hex_round` returns AXIAL, not offset** — its own
`px_to_hex` converts before handing back.  A caller that treats a
`hex_round` answer as an offset cell gets a sheared result with no
error.  `lat_from_axial` exists to stop that, and `lat_line` is the
first consumer.

#### The cut moved: the relabel functions are in C1, not C2

`lat_to_axial` / `lat_from_axial` were nominally C2's, but `lat_line`
cannot interpolate without them — offset coordinates are not linear, so
lerping them directly IS the shear — and `hex_grid` does not export the
pair.  They ship here with the round-trip gate.  **C2 keeps the harder
half**: applying the relabel to real DATA and proving adjacency is
preserved.

Suite 518 → **535 green**; `scripts/validate.sh` unchanged at **233
measurements over 14 scripts** (nothing calls the new layer yet, so it
had better be).

### C2 shipped — the invariant is DISTANCE, and the picture moves twice

Landed 2026-08-13.  `src/relabel.loft` + `tests/09_c2_relabel.loft`.
Nothing calls it yet; C5 does.

**The relabel is the STANDARD axial → odd-r conversion**, because
flat-top versus pointy-top is a RENDERING difference and not a graph
one — axial coordinates have the same six neighbour offsets either
way.  So dryopea's old labels and the new lattice's axial view are the
same graph, and the relabel is `lat_from_axial`.  ⚠ That is a claim,
and the test proves it rather than assuming it.

⚠ **This row asked for adjacency; adjacency is not sufficient.**  A
relabel can keep neighbours neighbours and still FOLD the plane, and
the damage is silent — two painted hexes landing on one, a ring that
closes early.  The gate is therefore **distance preservation** over
every pair in a ±6 window (28 561 comparisons), which implies both
adjacency and injectivity.  It is also what makes
[`CLAUDE.md`](../../CLAUDE.md) § the neighbour relation's promise true:
convert the table and plan 11's flow-field distances do not move.
Injectivity is additionally checked the way it would actually bite —
33×33 labels painted into the real sparse layer, and the count has to
come back 1089.

**The direction permutation was DERIVED before it was written down**,
at 25 cells × 6 directions, and the test re-derives it over the full
±16 window: `new = (old + 5) % 6`, uniform on both row parities.
Uniformity is the load-bearing part — a permutation that held only on
even rows is what a marker remap would trip over.

#### Two negative controls, both permanent

- **The identity relabel** — the mistake someone makes reasoning
  "axial and offset are both `(q, r)`, so the data is already fine" —
  must break adjacency, and does: exactly 289 breaks in a ±8 window.
- **The parity-free relabel** (`r/2` for `(r - (r&1))/2`) must move
  odd-row labels and no even-row ones.

⚠ **The second control was WRONG the first time and the fix is worth
keeping.**  Written as "count adjacency breaks and attribute them to
the source row", it reported 153 even-row breaks and looked like it
falsified this row's own "odd rows only" claim.  It did not: an
even-row cell is relabelled *correctly* by the broken version and
still fails an adjacency check, because four of its six neighbours sit
on odd rows and THOSE moved.  "Which cells get a wrong label" and
"which cells notice" are different questions.  Measure the labels.

Mutating `relabel_hex` itself to the parity-free version takes down 4
of the positive gates, the round-trip naming `(-16, -15)` — an odd
row, again.

#### ⚠ The picture moves by MORE than a mirror

§ Open questions 2 accepted "let the maps flip", and the flip is real.
But flat-top → pointy-top is ITSELF a re-orientation of the hexagon,
so the composite is a mirror **and one 60° hex rotation**.  Measured:
old direction 0 pointed due SOUTH on the old screen; its relabel is
new direction 5, which renders NORTH-EAST.

Nothing is wrong — it is the conversion doing what it says — but a
reviewer holding a converted map against an old screenshot will not
see "upside down", and needs to know that before hunting a bug that is
not there.  `test_the_picture_moves_by_a_mirror_and_a_rotation` pins
both halves so the fact stays measured rather than remembered.

Suite 539 → **551 green**; `scripts/validate.sh` unchanged at **233
measurements over 14 scripts**.

### C3 — shipped, and the goldens do NOT move once

Landed 2026-08-13.  `render.loft` + `marker_render.loft` draw from
`lattice.loft`; `camera.loft` and `script.loft`'s camera walk follow.

The load-bearing gate is `tests/09_c3_geometry.loft`, and it is measured
off a real rendered canvas rather than compared to a picture:

- **a rendered hex is TALLER than it is wide** — that is what pointy-top
  means in pixels, and it is the check the old renderer fails by having
  the ratio the other way up (measured: `37 x 33 px` before);
- the camera's own hex lands at the canvas centre, from any hex;
- neighbours sit one flat-to-flat away **in the compass direction
  `hex_grid` names**, measured in pixels;
- `screen_to_hex ∘ world_to_canvas` is the identity, on and off origin.

Run against the pre-C3 renderer, 5 of 8 go red.

⚠ **The round-trip check is green BEFORE the conversion too**, because a
round trip cannot see which lattice it is round-tripping.  It is in the
file to catch the forward path being converted without the inverse —
the mistake that makes a click land one hex from the cursor.

#### ⚠ This section used to say the goldens move ONCE, in C3.  Wrong.

A golden is a function of **two** things this plan moves separately:
geometry (C3) and labels (C5).  They are separable in the CODE — they
touch different files, and the relabel is a pure adjacency-preserving
bijection — but they are **not separable in a picture**.  Neither phase
alone leaves a reviewable image, and three of the sixteen additionally
pass through `paint_line`, which is C4's.

After C3 the ring golden is a lopsided blob, because axial labels are
being drawn with odd-r geometry.  Verified to be the LABELS and not the
geometry, twice:

- a disc built from the lattice itself (`lat_disc`) renders as a correct
  hexagonal flower — 19 hexes, properly centred;
- the same ring with its labels put through `relabel_hex` — exactly what
  C5 will do — closes perfectly.

So the goldens were promoted to the intermediate and **the reviewed
rebaseline moves to the end of the conversion**.  They still earn their
place meanwhile: they catch *unintended* change through C4 and C5.
`tests/golden/README.md` says all of this next to the files, because
that is where someone will be standing when they wonder why a ring is
not a ring.

⚠ **Do not "fix" the sheared goldens by relabelling the test fixtures
early.**  That pulls C5's work into C3 for three files and leaves the
other eight in the same state, which is worse than one honest
intermediate.

**11 of 16 moved; the 5 that did not are the sanity check** — two
all-sea canvases and three picker-only images, none of which contain a
world hex.  A UI golden that HAD moved would have meant the conversion
leaked into canvas-space drawing.

#### The camera had to move with it, and that is not cosmetic

`camera_update` panned north by `r -= 1`.  In the new lattice north is
**larger** r, so that walked the player backwards — and
`script_walk_camera`'s convergence test inverted with it, which would
have failed every `at` command as "target is more than 4096 camera steps
away" rather than as anything naming the cause.  Both flipped.
`tests/09_c3_geometry.loft` asserts the pan in METRES, not in `r`, so it
stays a statement about what the player sees.

Plan 08's measurements are the second net: `kind`, `marker`, `count` and
`range` are exact and label-only, so C3 must leave every one of them
untouched. A renderer change that moves a `kind` assertion has changed
something other than the drawing.  **Held: `scripts/validate.sh` is
unchanged at 233 measurements over 14 scripts, `frame` bands included.**

Suite 551 → **559 green**.

### C4 shipped — and it is ONE site, because two of the three were label-space

Landed 2026-08-13.  `marker_render.loft::direction_unit` now comes from
`lattice.loft::lat_direction_unit`, derived from `lat_neighbour` rather
than from an angle table — so a direction cannot mean one thing to the
enemy that walks it and another to the arrow drawn over it.

⚠ **The phase was written as three sites and only one belongs here.**
The distinction the conversion actually runs on, and the one worth
carrying into C5:

| | asks | depends on | when |
|---|---|---|---|
| **geometry** | *where on screen?* | the lattice only | C3 / C4 |
| **label space** | *which cell?* | how the DATA is labelled | C5 |

A direction index → a screen vector is geometry.  `paint_line` and
`enemy_tick` are label-space, and **dryopea's labels are mixed until
C5**: the editor's picking path emits new labels (C3 converted it)
while `.keys` files and saved maps still hold axial ones.

**`paint_line` was converted here and put back**, which is the
measurement worth keeping.  Its endpoints come from `screen_to_hex` in
the editor — already converted — but a `.keys` script feeds literal
coordinates straight through `drag`, bypassing picking entirely.  With
`lat_line` wired in, `paint-a-base` paints **20 hexes where it means
19**, so `scripts/validate.sh` goes red for a reason that is not a
defect.  A red gate blinds every phase after it, so it waits.

⚠ **Neither choice makes the intermediate correct** — with the axial
line the editor's own drag is sheared instead.  Keeping the gate green
is what decides it.

The duplicate lerp is the trap C5 inherits: `painted.loft::paint_line`
and `history.loft::paint_line_and_record` carry the SAME loop, and a
drag whose undo entry covers a different set from the paint is a
corrupt undo rather than a visible bug.
`tests/09_c4_arrows.loft::test_paint_and_record_cover_the_same_hexes`
is green now and is what fails if C5 moves only one of them.

**What C5 inherits, in one list:** `paint_line` ×2, `enemy_tick`'s
step, the flow field, passability, occupancy, the `.keys` literals,
the saved maps, every marker `direction` (through
`relabel_direction`), and the reviewed golden rebaseline.  They land
together because they are one change.

Suite 559 → **565 green**; `scripts/validate.sh` unchanged at **233
measurements over 14 scripts**.

### C5 — the data, and the one decision left open

Maps, `examples/*.json` and the five `.keys` scenarios all carry axial
coordinates in their text.

**Open:** whether converted maps bump `MapFile.version` and the loader
migrates on read, or the files are converted once and old files are refused.
dryopea has no released maps and one save slot, so refusing is defensible and
much simpler — but plan 04 (map library) is where saved content starts
mattering, and this is the cheapest moment to decide it. **Not decided here.**

⚠ **C5 has grown into the `H` this plan said it would not have, and it
must be re-cut before it is started.** § Phases opens with "No phase is
`H`" — an `H` step has no half-done state with anything exact to compare
against — and C5 now carries the data, the label-space code, and the
goldens. That is the shape the split was invented to avoid.

The reason it grew is not drift: C3 and C4 each found that a site they
were given is only meaningful relative to how the DATA is labelled, so
it cannot move before the data does. Everything label-space piled into
one phase because it genuinely is one atomic change of meaning.

⚠ **But atomic in MEANING is not atomic in VERIFICATION**, and that is
where the re-cut has to come from. Candidate seams, each of which keeps
`scripts/validate.sh` green on both sides:

- **C5a — the converter itself**, over the file formats, with no
  consumer switched. Gate: round-trip every real map and `.keys` script,
  painted counts and marker counts identical, adjacency preserved
  (C2's check applied to real data). Nothing in `src/` changes
  behaviour, so both gates stay green by construction.
- **C5b — the atomic flip**: run the converter over the files AND
  switch the label-space code (`paint_line` ×2, `enemy_tick`, flow,
  passability, occupancy, `relabel_direction` on markers) in ONE
  commit. Gate: the scenarios' own numbers — `count painted 19`,
  `range` decreasing — unchanged, because a relabel that preserves
  distance cannot move them. **That is the strongest gate in the whole
  plan** and it needs no goldens.
- **C5c — the reviewed golden rebaseline**, once, by eye, on a system
  that is finally self-consistent.

C5b is still the widest step here, but its gate is exact and its
negative control is free: if the scenario numbers move, the relabel did
not preserve distance, and C2 says that is impossible — so a change
means a site was missed.

## Invariant gate

| Phase | Concrete expected result | Invariant pinned | Negative control |
|---|---|---|---|
| **C0** | `hex_distance((0,0), (-1,-1)) == 1` | the oracle disagrees with axial, so it can surprise us | an oracle that already matched dryopea would be measuring nothing |
| **C1** ✅ | the ±16 sweep is green against `hex_grid`; the disc count is `3N(N+1)+1`; neighbouring centres are `HEX_FLAT_TO_FLAT` apart | dryopea's lattice IS `hex_grid`'s — by DELEGATION, so it cannot drift | pointed at `world.loft`: **8 of 17 red**, incl. "disagree on more than a quarter — 0 of 1089".  Parity term broken: 3 red at an ODD row |
| **C2** ✅ | every PAIR keeps its distance (28 561 comparisons), and 1089 labels paint 1089 hexes | the relabel is a bijection that preserves structure — distance, so adjacency AND injectivity | the identity relabel breaks adjacency 289 times; a parity-free one moves odd-row LABELS only.  ⚠ Attributing breaks to the source row instead reports even-row damage and looks like a falsification — it is not |
| **C3** ✅ | a rendered hex measures ~31 x 36 px at ppm 24 — TALLER than wide | the picture follows the lattice, not the other way round | a rebaselined golden agrees with a shear; the pixel measurements do not — 5 of 8 red against the old renderer.  ⚠ And the goldens DID come back sheared, because labels are C5's; that is why they are no longer this phase's evidence |
| **C4** ✅ | every arrow's forward vector equals the normalised step to the neighbour it names | the arrow and the mover read the same compass | the old angle table points arrow 0 at `(0, 1)` while its neighbour is at `(1, 0)` — measured.  ⚠ `a-wave-approaches` moved to C5 with the enemy step |
| **C5** | converted maps keep painted counts + adjacency | a relabel is not a content change | a map that gains or loses a hex was converted wrong |
| **I0** ✅ | a key held five frames fires its action ONCE — **both do** | `input`'s edges mean what the seam's mean, on all three semantics | the predicted "first frame" divergence **is not real**; the real one is the seam forging `prev` mid-step, which is @D001 |
| **I1** ✅ | 208 key/modifier combinations resolve to the SAME 22 `EditorInput` fields as the pre-I1 poll | swapping the input layer changes nothing a player could see | a scenario replayed on a keyboard with every key MOVED lands on the same session — and one with a palette key killed must FAIL, or "still passes" means "never pressed anything".  `do Tab` still refused |

## Open questions

1. ~~**Does `HEX_DIAMETER = 1.5 m` survive?**~~ **Answered in C1: yes,
   and dryopea SCALES `hex_grid`'s output.** One `hex_grid` unit is one
   dryopea circumradius = 0.75 m, so centre-to-centre is `√3 × 0.75 =
   1.299038` — which is `HEX_FLAT_TO_FLAT`, a constant `world.loft`
   already carried. The two conventions already agreed on the number;
   the conversion is one multiply and `hex_grid` needs no scale
   parameter.
2. ~~**Do the six direction NAMES survive?**~~ **Answered: no — dryopea
   follows `hex_grid`'s compass, and the maps flip.** (Project owner,
   2026-08-13.)

   C1 found the disagreement is not a reordering but a north↔south
   MIRROR: `hex_grid` documents "r increases upward" and names direction
   5 `NE` while placing row `r+1` at larger y, and dryopea's world +y is
   south. The decision takes the library's compass as authoritative and
   accepts that existing maps render vertically mirrored — the cheaper
   loss, because the alternative leaves every `hex_*` library and moros
   describing dryopea's world with the wrong words permanently, which is
   the thing this plan exists to end.

   ⚠ **Implemented in C1, not deferred to C4/C5**, because it is one
   negation in `lat_to_metres` / `lat_from_metres` / `lat_corner_metres`
   and nothing calls them yet — so it was free here and would have meant
   C3 rendering against a frame it then had to flip.

   ⚠ **It changes no convention dryopea already had.** World +y still
   grows south; the render path still has no y-flip in it. The negation
   sits in the lattice→metres conversion, which is where the other thing
   `hex_grid` cannot know (the metre scale) already lives.

   ⚠ **C5 must NOT add a compensating flip.** A converted map that comes
   back looking the same would mean the compass had not moved. The flip
   is the visible evidence the decision took effect.

   ⚠ **The winding reverses.** `hex_grid` walks corners counter-clockwise
   in its own frame, so in dryopea's they run clockwise. Consecutive
   corners are still adjacent and one side apart — all a convex fill
   needs — but C3 must check nothing depends on signed area.
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
