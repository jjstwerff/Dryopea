<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# plans/ — dryopea's plan structure

dryopea organises multi-phase work the way **moros** and **loft** do, so one
convention spans every repo. This file is the **binding** — the conventions,
and where dryopea differs.

- A **reference doc** ([`docs/DESIGN.md`](../docs/DESIGN.md),
  [`docs/GROUND_TYPES.md`](../docs/GROUND_TYPES.md), …) describes **how the
  thing works** — the durable truth, updated in place as the code changes.
- A **plan** describes **a change we intend to make** — phases, ordering,
  verification. It is temporary: when a phase ships, its reference content
  **moves out** to the doc that owns it, and the plan keeps only the closure
  record.

If you cannot say what *changes* when the plan is done, it is a doc, not a
plan.

## Pick the lightest workflow that fits

| Work shape | Path |
|---|---|
| **Bug fix** (one root cause, one commit) | Fix + a test in `tests/` + commit. No plan. |
| **Upstream defect** (loft, or a library) | File it in [`QUESTIONS_FOR_LOFT.md`](../QUESTIONS_FOR_LOFT.md) and fix it in the owning repo. **Never a dryopea plan, never a local workaround.** |
| **dryopea-internal bug** | A `@D<NNN>` row in [`PROBLEMS.md`](../PROBLEMS.md). |
| **Content work** (a ground type, a palette entry, a map) | Nothing, or one line in the doc that owns it. |
| **Light TODO** *(the default)* | An `## Open work` row in the reference doc that owns the area. |
| **Plan** | A directory here. Earns it only when the work is genuinely **multi-phase**. Cap active plans at **2–3**. |

Most work is not a plan. A row in the doc that owns the area beats a plan
directory that only points back at that doc.

## Identity — the plan number

A plan's identity is its **zero-padded integer**, and the directory is
**flat**: `plans/<NN>-<slug>/README.md`.

- **Never renumber an existing plan.** New plans take the next unused
  integer. Numbers appear in commits and prose, so a collision is expensive
  to unwind.
- **Numbering carries no priority.** [`ROADMAP.md`](ROADMAP.md) carries the
  logical ordering.
- **No `future/` · `finished/` · `deferred/` subdirectories.** Lifecycle
  state is a **field in the plan's own `## Status` section**, not a path — a
  plan that ships should not move on disk and invalidate every link to it.

> **Where dryopea differs from moros.** moros keys plan identity to its
> GitHub **issue number** and derives the overview from `gh issue list`.
> dryopea has no issues filed and no `plan` label, so identity stays a local
> integer and the index below is hand-maintained. If dryopea starts using
> issues, switching to moros's scheme is the better end state — that is an
> open decision, not a settled difference.

## Index

Each plan's own `## Status` section is the source of truth; this table is a
pointer, not a second copy.

| Plan | Value | Effort | Lifecycle | One line |
|---|---|---|---|---|
| [`01-ground-editor`](01-ground-editor/README.md) | G | M | Active | In-game ground-type editor; E1–E4 + smoke + E1-live shipped |
| [`02-solver-validation-viewer`](02-solver-validation-viewer/README.md) | G | MH | Future | 3D solver-output viewer; painted layer + height mesh |
| [`03-marker-layer-and-spawns`](03-marker-layer-and-spawns/README.md) | G | M | Shipped (M1–M5) | Second sparse layer; multi-direction spawn points |
| [`04-map-library`](04-map-library/README.md) | G | M | Future | MapFile schema + map index + browser + content |
| [`05-validation-scenario`](05-validation-scenario/README.md) | G | M | Future | Minimum playable thing; integration spec |
| [`06-editor-stencil-pipeline`](06-editor-stencil-pipeline/README.md) | F | MH | Future | Editor-as-content-pipeline; stencil mode + mesh baker |
| [`07-shared-world-substrate`](07-shared-world-substrate/README.md) | F | H–VH | **Blocked** (needs `hex_voxel` published; W0c cut out to plan 09) | Go 3D; adopt the shared hex substrate |
| [`08-game-validation`](08-game-validation/README.md) | S | MH | Complete (V0-V4) | Scripted play, measured effects, PNGs for inspection |
| [`09-lattice-conversion`](09-lattice-conversion/README.md) | F | MH | **Complete** (C0–C6 + I0–I1) | dryopea moved to pointy-top odd-r offset (+ adopted `input`), checked against `hex_grid`; the axial layer is deleted |
| [`10-extract-local-libraries`](10-extract-local-libraries/README.md) | C | MH | **Gated** (extract what survives 07 + 09) | The code dryopea owns becomes published libraries — descriptive names, never a brand |
| [`11-flow-field`](11-flow-field/README.md) | G | MH | **Complete** (F0-F8, plus F7b 2026-08-13) | Enemies route round walls to the core, per class, spread rather than stack, and besiege a sealed perimeter.  ⚠ F7b reopened it: an enemy blocked by a COMPANION steps beside it, a rule the spec always had and three phases judged latent — it turned out to set the whole balance |
| [`12-combat-resolution`](12-combat-resolution/README.md) | G | MH | **Complete** (2026-08-13) | Damage resolves on both sides, a wallet ends the run, and an unattended base falls on a measured clock — which a sealed wall nearly doubles and a TOWER cuts, because its bodies ramp over the wall it defends |

| [`13-the-vehicle`](13-the-vehicle/README.md) | G | MH | **Complete** (2026-08-14) | A player in the world — the crew plan 12 measured a need for.  It drives, boosts over its own wall, clears the bodies that beat the tower and gets PAID for them, taking the towered base from 95 ticks to 145 |

| [`14-helpers`](14-helpers/README.md) | G | MH | **Complete** (H0-H3; H4 shipped as plan 15 C2) | The crew becomes a cooperative — the first mover whose speed does not fit the tick, and a roster that buys COVERAGE rather than throughput (77 → 214 → 242 ticks on a base with two fronts), and a crew member who can be LOST for good |

| [`15-the-carry-model`](15-the-carry-model/README.md) | F | M | **Complete** (C0-C3, 2026-08-14) | One slot, one context-resolved action, and nothing is ever lost — the shared blocker helper retrieval, tower-tops and beacons all wait on.  ⚠ Conservation is STRUCTURAL (one record, one owner), so the wrong states cannot be written down; and it refuses the hash-keyed-by-hex shape every other runtime layer uses, because two carry objects on one hex is reachable and a hash deletes one.  C2 closed plan 14 H4: a lost crew member is carried home and rejoins the roster after exactly 90 ticks.  ⚠ C3 measured what that is WORTH and the answer is nothing yet — 85/79/79 ticks — because a 60 s recovery is priced against a SEVEN-wave base and dryopea plays one wave at a time |

| [`16-the-wave-system`](16-the-wave-system/README.md) | G | M | **Complete** | Waves arrive on their own, in the authored order, with a lull, and a run starts itself when the player pokes a far spawn marker — so a base is more than one wave long.  ⚠ **W4 measured it at its real length and found the ceiling**: the authored seven-wave list plays FOUR and falls at 321 with every tower black, because a tower is 300 HP of ammunition for the whole run against a 6150 HP list.  ⚠ And a retrieval is worth ONE tick even where the crew member does come back — the job is gone by the time they return, so **tower repair, not the wave system, is what unblocks it** |

| [`17-tower-hot-swap`](17-tower-hot-swap/README.md) | G | M | **Complete** | The upkeep loop, so a base can RECOVER between waves — a crew member rebuilds a black tower in the lull, and a tower-top becomes a carry object that can be transplanted onto a spent one.  ⚠ Opened because [plan 16](16-the-wave-system/README.md) W4 named it twice: the seven-wave list plays FOUR because a tower is 300 HP of ammunition for the whole run, and a retrieval is worth ONE tick because the crew member comes back to no job.  ⚠⚠ **The authored seven-wave list is now PLAYABLE**: seven towers and two SHUTTLING helpers clear all 205 robots, where plan 16 W4 measured four waves and a fall.  ⚠ POSITION is the whole of it — the same two parked on their towers reach 5/7 and the base falls.  ⚠ A retrieval finally pays, **+76 points** over the errand control, which plans 15 C3 and 16 W4 both failed to produce.  ⚠ The TRANSPLANT does not pay yet (+3 ticks at best, −50 if the donor was firing): a tower close enough to donate is close enough to be shooting, so its payoff waits on swap pits and strain |

| [`18-scenario-capture`](18-scenario-capture/README.md) | S | MH | **Complete** | Play until something interesting happens, then emit the smallest `.keys` scenario that reproduces it — so a state you can REACH but could not have thought of becomes a test.  ⚠ It is `S` because [plan 15](15-the-carry-model/README.md) C1's two-objects-on-one-hex case was reachable in the shipped game with *"no fault raised anywhere"* until somebody happened to imagine it.  ⚠ Emits `.keys` and never a state blob: a saved `WaveState` is a golden of the SIMULATION and inherits *a golden agrees with a shear*, where an authored starting position has nothing derived in it to go stale.  ⚠ The work is making the script vocabulary TOTAL over `WaveState` — enemies, towers, wallet and cargo have no setters today |

| [`19-the-interactive-loop`](19-the-interactive-loop/README.md) | G | H | **Active** (P0-P3 done) | The game runs in the WINDOW.  ⚠⚠ **P3 opened it**: `make play`, press P, and waves arrive because time passed — every clock plans 12-17 measured used to describe a game nobody had played.  ⚠ Nothing of the game is DRAWN yet (P4), so the console is the only way to see it.  ⚠ Its rule is plan 08 V0's one level up — and P1 sharpened it to *ONE caller of `wave_tick`* rather than one entry point, because a script asks for a COUNT and a frame for a DURATION and folding those together corrupts the count.  ⚠⚠ P1 FALSIFIED P0: `n × TICK_SECONDS` through the accumulator is one tick SHORT for 602 of the first 1000 `n`, and P0 had measured n = 30 — one of the lucky 398.  What survives is that the frame SIZE does not reach the simulation, because a REPEATING accumulator carries its remainder where a one-shot timer loses it |

| [`20-entity-art`](20-entity-art/README.md) | G | H | **Active** (designed, A1 next) | Every entity is a **part-tree** and its GEOMETRY is derived from it — the moros way ([`../moros/doc/claude/PARTS.md`](../../moros/doc/claude/PARTS.md) § P9.0: limbs on joints, three limb kinds, scale derived, hitbox a subset of the skin).  The hover unit is a big quadcopter with a fixed base, bigger REAR rotors and a canopy on a rear hinge.  ⚠⚠ **The tower already HAS a socket**: plan 17 T2's detachable top is moros's socket model, built in the simulation before anything drew it — so the art follows the mechanic instead of duplicating it.  ⚠ Two deviations from moros, each with its reason in [`docs/PARTS.md`](../docs/PARTS.md): a dryopea-native model (`hex_part` is unpublished and needs a second world model), and primitives rather than hex cells (a hex is 1.5 m; a rotor is 0.5 m).  ⚠ **Re-scoped before any code was written**: the first design baked SPRITES, and the dynamic camera deleted them — a sprite sheet does not degrade under a free camera, it lies.  Emits triangles and never a pixel |

| [`21-the-renderer`](21-the-renderer/README.md) | G | VH | **Active** (R0-R1 done, R2 next) | The camera comes to the vehicle — moros's `RenderCamera`, FOLLOW behind the facing, the boom eased.  ⚠⚠ **`camera_overview` pins elevation at 89°, which IS dryopea's editor view** — so this is one camera with two presets, not a second renderer.  ⚠⚠ **R0 measured the thing that could have killed it**: a GL frame captured under `xvfb`, decoded and classified with **zero** colour drift over 76 800 px — so the headless measurement culture (1094 tests / 520 measurements / 32 shots) survives going 3-D.  ⚠ The lump is not the camera, it is the TERRAIN: a ground-level camera makes the hex painting three-dimensional, and R3 is probably its own plan.  ⚠ Retires `DESIGN.md` § 12's *"locked in pose — no mouse orbit"* — and **R1 did the retiring**, rewriting § 11's mouse row and § 12's camera passage in the commit that landed the camera.  ⚠⚠ **R1 corrected the design's own frame note**: `+y` south is a CANVAS convention, and carried into 3-D it is left-handed and MIRRORS the world — measured at eight azimuths, of which the north frame reproduces the editor at exactly one and the south frame at none (`@M021`).  So `lat_to_world` negates y and the camera's world is `hex_grid`'s own.  ⚠ `camera_overview` at 89° now reproduces the software rasteriser to **0.0014 rad of bearing and 0.56% of scale** (`@M022`), so § R2's two-rasteriser plan holds.  ⚠ moros's follow formula ported verbatim puts the camera **ABEAM** — along-track exactly zero at all four cardinal headings, tracking and easing and looking like a working camera.  ⚠⚠ **The overview gate read a PERFECT 0.0 rad twice while measuring nothing** — once on an empty `const vector` ([loft#955](https://github.com/loft-lang/loft/issues/955), filed) and once comparing bearings in NDC where the aspect ratio is baked in; *an exact zero out of an integer-versus-float comparison is a tell, not a result*, and the missing control was the generic one: **can this gate produce a non-trivial reading at all?** |
| [`22-the-field-cache`](22-the-field-cache/README.md) | C | MH | **Active** (designed, S0 next) | Stop rebuilding the whole world's distance field every tick.  ⚠ **`flow_sweep` is ~75% of the suite's interpreted time** (re-profiled 2026-08-15), it is UNBOUNDED (`while len(frontier) > 0`, capped at 1M cells), and it is only READ inside the 25-hex bubble — so **~60% of every sweep is computed and never looked at**, and under exploration that goes to nearly 100%.  ⚠⚠ Every phase is EXACT: the field is a pure function of `(pal, pw, hl, climb, core)`, the invalidation surface is **two functions wide** (`height_raise` / `height_clear`, plus `paint`), and the equality gate was **written in advance** — `11_f8::test_the_field_a_tick_uses_equals_a_fresh_build` is green today *"because there is nothing to go stale"*.  ⚠ S0 changes nothing and decides the order: the cache wins when ticks are clean, the roster bound when the sweep is large.  ⚠ Not LOD — that attacks the ROSTER, this attacks the FIELD |

| [`23-the-small-robots`](23-the-small-robots/README.md) | G | M | **Complete** (K0-K3, 2026-08-15) | Scout, harvester, builder and miner — four roles, one AI, and the widest-but-cheapest gap in the design ([`ROADMAP.md`](ROADMAP.md) § The critical path, item 1).  ⚠ **K0 shipped the wall-damage axis and it was exactly as cheap as advertised**: every per-class question in the engine was already a lookup whose own comment said *"when a class gets a section it gets a row here"*.  Four of a class into one sealed band breach at **23 / 35 / 50 / 96 / 454** ticks (`@M011`) — 20x, from a mover that cannot tell them apart — with **not one of the 520 existing measurements moved**, because `robot` keeps its rate and the four APPEND.  ⚠ **K1 then gave a wave COMPOSITION** — `schedule 4 12` arms the list, `compose 1 4 miner 8 scout` fills one wave of it — and the design decision was to **delete this plan's own negative control**: a wave's size is SUMMED from its parts, so *the parts sum to the count* is the definition rather than something to refuse (`@X055`).  ⚠⚠ **Its real cost was a loft heap-corruption bug** ([loft#935](https://github.com/loft-lang/loft/issues/935)): a `vector<Struct>` local in `script_command` aborts an unrelated test file at compile time, and two plausible attributions were measured and discarded before the bisection landed.  ⚠⚠ **The roadmap's estimate was HALF right**: the design commits a second axis, SPEED, and honouring it broke `TICK_SECONDS = 1 / ENEMY_SPEED_HEX_PER_SECOND` (`@X058`) — two phases, not a row.  **K2a** rebuilt the mover on banked progress with every number held, so the gate was the corpus refusing to move (`@M015`); **K2b** then moved two of them, and nine hexes of one corridor now take **6 / 9 / 14** ticks (`@M016`).  ⚠⚠ **K2b's number was chosen for TESTABILITY as well as design**: 2.25 and 3.0 hex/s read as *"quite a bit faster"* too and both hide `ENEMY_PROGRESS_EPSILON` exactly as 1.5 does, so shipping one would have left the roster unable to see its own rounding — at 2.5, zeroing the guard turns both gates red (`@X063`, `@M017`).  ⚠ K0's own gate caught its first number: the scout at 0.2 HP/s breached at 231 ticks, INSIDE the 321 a real base lasts, because the sum was priced against the 100 HP braced hex while the siege chews the 30 HP end (`@M012`).  ⚠⚠ **K3 closed it with a finding the plan did not predict**: three waves of twelve fall at **94 / 126 / never**, so composition is legible — but every MIX lands within four ticks of a PURE wave of its FASTEST class, because only **three** hexes of a wall are ever attacked and the quickest four robots own all of them (`@M018`).  A wave two thirds miner performs like a pure harvester wave, which no rate arithmetic can produce; it needs the miners doing nothing, and the target list says they are.  ⚠ The ROSTER order is now worth nothing — the fast class overtakes — so K0's *order is worth 20x* is a statement about POSITION.  ⚠ The fix is the equal-distance sidestep `ENEMY_MOVEMENT.md` has carried since plan 11 F7, refused here on purpose (`@X064`): a measurement phase that changed the thing it measured would have measured nothing |

| [`24-the-siege-front`](24-the-siege-front/README.md) | G | MH | **Complete** (W0-W2, 2026-08-17) | A besieger attacks the wall it is TOUCHING instead of walking sideways to join a queue — [`ROADMAP.md`](ROADMAP.md) § The critical path item **1b**, opened because [plan 23](23-the-small-robots/README.md) K3 priced it (`@M018`: only three hexes of a wall are ever attacked, so a mixed wave collapses to its fastest class).  ⚠⚠ **W0 refuted the plan's own premise and five documents with it** (`@M019`): the missing rule is NOT *the equal-distance sidestep* — dryopea has had one since plan 11 F7b, and at the face hex `(7,-1)` half of what it offers steps BACK off the wall.  The three-hex front was the DESIRE FIELD's shape: a ring around the core has one minimum on a straight face, so exactly three hexes have no legal closer step, **for any wall length** — and all five face hexes TOUCH the wall while two walk away from it, because an enemy attacked only when it could not WALK.  ⚠⚠ **W1 fixed it with a PRECEDENCE rather than the second steering rule everyone had budgeted for** — *arriving beats queueing*, one pre-pass in `enemy_walk_desire` plus a DELETION in `enemy_target`, both asking the identical question.  ⚠ `enemy_target` takes no `Occupancy`, which forced the phrasing *"a wall is between me and the core"* — and that turned out to be the better rule: it needs no memory and cannot jitter, because an enemy that stops never moves again.  ⚠⚠ **`@M020`: the front went 3 → 4 hexes on a five-row wall and 3 → 6 on a seven-row one, and `@M018` is retired** — *4 scout + 8 miner* went from *never* to **126**, and a wave is now worth its front class PLUS what the front cannot COVER (four screens against a five-hex face leak exactly one miner; the leak is worth nothing to a builder screen and 39 ticks to a harvester one).  **The screen is arithmetic — bodies against face width.**  ⚠ A wider front makes most bases last LONGER (`a-base-on-two-fronts` 123 → 132): a besieger that stops at the wall is not walking on to drain the wallet.  ⚠⚠ **The tripwire written for this day did NOT fire** — plan 12 B3's fence test stayed green, because its six robots already touch the fence where their routes meet it, so *a tripwire aimed at the RULE you expect to build is not one aimed at the BEHAVIOUR you want*.  ⚠ W2 re-priced 16 assertions and 8 gate scripts, and TWO gates were saturated: `11_f7b`'s bracing test now measures which hex breaks FIRST instead of photographing the wreckage, which is strictly stronger |

Parked plans: [`DEFERRED.md`](DEFERRED.md). Roadmap entries without a plan
slot get one when their trigger fires.

## Value categories — what KIND of value

Same letters as moros and loft, so the convention reads the same across
repos. Read top-down and pick from the highest category with open work.

| Tag | Meaning | dryopea examples |
|---|---|---|
| **S** | **Silent failure / content corruption** — it "works" but the result is wrong, with no error | a renderer that draws the wrong shape and no test can see it; a map that round-trips to different bytes |
| **R** | **Regression / gate-blocker** — `scripts/test.sh` red, or a toolchain bump that breaks the build | a loft release that breaks the parse; a library migration that strands the deps |
| **G** | **Goal-enabling** — directly advances the playable game | the editor, the wave engine, the scramble loop |
| **F** | **Foundation** — unblocks 2+ downstream plans | the shared hex substrate, the map file format |
| **U** | **Player experience** — feel, readability, controls, art coherence | editor ergonomics, HUD legibility, proxy art |
| **C** | **Clean features** — removes special cases; keeps the game↔library seam honest | moving hex math out to the shared library |
| **Q** | **Internal quality** — perf, refactor, cleanup with a clear payoff | warning cleanups, test-suite speed |
| **N** | **Niche / opportunistic** — small, low-priority | one-off tools, conveniences |

**Effort letters, never calendar time** — `XS / S / M / MH / H / VH`.
"Two weeks" ships in two days and "quick" takes weeks; effort buckets stay
stable, projections don't.

## The verification rule

Every phase names a **gate** — how you see it works. dryopea has three, in
increasing order of what they can catch:

1. **Unit + round-trip tests** (`scripts/test.sh`) — exact invariants.
2. **Golden images** (`golden.loft::assert_golden`) — byte-equal renders.
   Exact, and brittle by design: any renderer change invalidates every
   golden at once.
3. **Measured frames** (plan 08) — scripted play, thresholds over classified
   pixel shares, PNGs kept for human inspection. Survives re-lighting and
   re-styling, and still catches "the thing is not drawn".

⚠ **A gate that cannot separate the things it measures is not a gate.** A
threshold over a bucket that mixes two subjects moves for reasons that have
nothing to do with the change under test — fix the instrument *before* you
trust the number over it. See plan 08 § The instrument comes first.

## What makes a step SAFE — and it is not how few lines it is

Adopted from moros, which paid for it: two steps of equal effort an hour
apart, one green at every moment, the other reverted whole.

> **A step should be as small as possible while STILL BEING VALIDATED — and
> those are two bounds, not one.**
>
> **Upper bound (safety).** A step is safe when the OLD path and the NEW one
> can both run at once and be COMPARED exactly. If the only way to see
> whether it worked is to swap and look, it is too big.
>
> **Lower bound (validity).** A step must be able to **go red on its own,
> for a real reason.** If the only way to test it is to also do the next
> step, they are ONE step and dividing them buys nothing but a green tick on
> an empty claim.

Two questions when cutting a phase, and a step has to pass both:

1. *At the moment this step is half done, what exactly am I comparing
   against?* If the answer is "nothing, I look at it afterwards", the step is
   **too big** — one big step wearing a small step's effort letter, whose
   failure mode is `git revert`.
2. *What test would go red if I did this step wrong?* If the honest answer is
   "none until the next step lands", the step is **too small** — merge it
   forward.

⚠ **A step that ends with something built and called by nobody cannot fail.**
Splitting "add the function" from "call it" manufactures that state on
purpose. If the first half cannot go red, it was never a step.

⚠ **A self-test is not validation.** "The key table exists and every key maps
to one action" is a claim about the table, checked against the table — it
cannot be surprised. The discriminator is not *is there an assert*, it is
*could this assert ever be surprised*.

**Three shapes that pass:**

- **Parallel run.** Build the new thing beside the old, compare exactly
  (bytes, a count, a histogram), *then* delete the old.
- **A probe first.** An `XS` step whose only job is to try to falsify the
  design before anything is built on it. The `fill_triangle` diagnosis was
  exactly this: two triangles side by side, one library call and one
  reordered, for the cost of a compile.
- **One site at a time, each with its own comparison.** "Wire four callers"
  is four steps, and each wants the same gate: *the old call and the new call
  leave the same world.*

⚠ **The comparison is the step; the edit is the easy part.**

### The two mechanical checks, when a plan STARTS

A phase is mis-cut in two ways a reader can see without judgement:

| it fails on | because |
|---|---|
| an open phase with an **empty Verify** | nothing about that step could go red — the lower bound |
| an open phase at **`H`/`VH`** | too big to have a half-done state with anything exact to compare against — the upper bound |

⚠ **One moment, not every run.** A design may be anything until it becomes
work — a sketch, a paragraph, half-formed rows. Demanding cut steps of every
idea is how a rule becomes something people route around. And these two
checks are the *mechanical* half only: whether a `Verify` cell names a **real
comparison** is judgement, and no checklist has it.

## See also

- [`_TEMPLATE.md`](_TEMPLATE.md) — copy this for a new plan
- [`ROADMAP.md`](ROADMAP.md) — logical-order feature list across all tiers
- [`../docs/DESIGN.md`](../docs/DESIGN.md) — master design
- [`../docs/DESIGN_HISTORY.md`](../docs/DESIGN_HISTORY.md) — 2023 seed material
