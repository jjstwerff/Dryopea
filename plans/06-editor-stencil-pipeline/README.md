<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Plan 06 — Editor-to-stencil pipeline

**Status:** Future (drafted 2026-05-27; no code).

## Goal

**One coherent editor that builds stencils for two purposes.**
The same authoring vocabulary that paints terrain (plan 01)
also paints stencils.  A stencil can then be used either as
**world-dressing prefab** (placed in a map like a building)
or as **a movable unit** (baked into a mesh and treated as a
game entity).

The deeper goal is to **bring the loft / dryopea / moros
suite into a rapid-prototyping posture**.  Every new piece
of content — a robot kind, a factory, an abandoned habitat,
a new tower variant, a sap-tree species — is authored in the
same tool with the same skills.  No separate art pipeline,
no per-entity asset shop, no "wait for the modeller."  When
a designer wants to test a new shape in play, they paint it,
bake it, drop it into a map, run.  The iteration loop closes
in minutes, not weeks.

This unlocks **two viable shipping paths**:

- **Polish path** (big-studio / well-funded teams).  A final
  art push lands near the end of development — high-fidelity
  textures, polished silhouettes, PBR materials, lighting
  response.  Plan 06's stencils are the *solid foundation*
  that push refines.  Every shape is correct, proportioned,
  composed, and placed *before* the polish artist arrives;
  they refine what already exists rather than modelling
  from a written brief.  Development is unblocked from "we
  need someone to draw this" through the entire arc; polish
  joins as a separate later workstream on a foundation
  that's already shipped and playable.
- **Strike path** (indie / starting devs).  The stencil
  pipeline's *output is the shipped game*.  No polish layer.
  No final art push.  The clean geometric look that
  stencils naturally produce — angular silhouettes, layered
  forms, distinctive proportions — **is the aesthetic**.
  Plenty of indie successes ship in this territory:
  art-deco geometric, low-poly, block-layout, faceted
  voxel-adjacent.  A starting dev can take dryopea + the
  stencil pipeline and ship a full game with **striking
  visuals** without ever hiring an artist — the art lives
  in *authoring discipline* (clean shapes, intentional
  colour palette, composition awareness) rather than in
  per-asset polish.

The strategic implication for the loft / dryopea / moros
suite: it's not just dryopea's content pipeline.  **It's a
viable game-engine offering for indies who don't have or
need an art team.**  The same pipeline serves both shipping
paths; the difference is whether the team chooses to add
the polish push at the end.

## The two purposes (the design hinge)

### 1. World-dressing prefabs

Stencils placed into the world like a prefab structure.  They
sit at hex coordinates, occupy multiple hexes, and persist as
authored scenery.  The painted hex grid remains the world; the
stencil is just a region of it that came from a saved file
instead of being painted in place.

Examples of what world-dressing prefabs cover:

- **Old human habitats** — the surface-era mountain-cave
  remnants (per SETTING.md § History) that the underground
  humans left behind when they moved deep.  Players scouting
  far from base find these and the lore lands diegetically.
- **Huge trees** — the sap-tree clusters of tier 2 (per
  SETTING.md § The other enemy — insects + sap).  Multi-hex
  organic shapes whose presence shapes insect behaviour.
- **Bridges for the robot economy** — supply lines between
  factories and mines, the visible infrastructure the AIs run.
  Robots crossing these bridges is a visible economic signal.
- **Robot factories** — the production sites the AIs operate
  (per SETTING.md § Robot diversity).  Multi-hex industrial
  structures that produce the swarm.  Disrupting them is
  the lore foundation for the future-expansion economy
  layer.
- **Abandoned bases** — leftover sites from previous
  expeditions (per SETTING.md § How mechanics fit, persistent-
  abandoned-bases row).  Walls + buildings + ruined cores.
- **Faction-territory signatures** — distinctive structural
  motifs that mark a region as belonging to one AI faction or
  another (per SETTING.md § Faction territory awareness).
  A player who's seen the silhouette before recognises whose
  domain they're in.

### 2. Unit creation

Stencils baked into a static mesh, then used as a movable
game entity that travels independently of the hex grid.
Possibly with **a second mesh on top** for animated parts —
tower rotating turret, vehicle swivel weapon, robot sensor
head.

Examples of what unit-creation stencils cover:

- **Robots** — every robot kind (workers / haulers / scouts /
  coordinators / defense bots / boss repair platforms — per
  SETTING.md § Robot diversity).  Each kind is one stencil
  in the editor; bake produces the unit; faction-flavoured
  variants are sibling stencils.
- **Player vehicle** — same pipeline as robots.  A
  "noncombatant manager" chassis (per DESIGN.md § 8) is
  drawn in the editor and baked.  Helper vehicles are
  variants — sibling stencils sharing primitives but
  composed differently.
- **Towers** — base + rotating top.  The base is one
  stencil; the top is another stencil mounted at a pivot.
  Tower variants (anti-insect / area-splash / etc. — per
  DESIGN.md § 7 Future tower types) are just different top
  stencils on the same base, swapped via the hot-swap
  mechanic.
- **Eventually: organic-feeling units (insects).**  Insects
  (tier 2) are an open question — they may use the same
  stencil mechanism with the extensions in phase S4 below
  (leg movement), or they may need a parallel pipeline.  See
  § Open questions.

## Two load-bearing design choices

### 1. One editor, one vocabulary

The stencil editor is **not a separate tool**.  It's a mode
of the dryopea editor (plan 01 E1-live), reachable via Tab
(reserving the existing Tab toggle for editor-vs-marker
modes — see DESIGN.md § 11 input handful).  Same palette,
same picker, same camera, same paint-line / paint-hover /
save-load verbs.

The vocabulary the editor speaks is the same as the world's:
- **Walls** (`wall` / `wall_high`) — the solid surfaces of
  the painted-hex palette.
- **Surfaces** — the ground tiles + the floor / ceiling /
  roof layers added when multi-layer painting ships in
  phase S1.
- **Bridges** — the multi-layer connecting primitives
  borrowed from moros (also phase S1).
- **Multi-layer** — moros-house-style stacked layers
  (ground, lower walls, upper walls, roof, …), so vertical
  variation is authorable without leaving the hex paint
  workflow.

Why one editor:

- **Skill transfer is free.**  Anyone who learned to paint
  terrain can author stencils without a second learning
  curve.
- **No tool drift.**  A separate stencil-editor would
  diverge from the world editor over time, just as moros
  historically suffered from editor-game render-pipeline
  drift.  Sharing the editor by construction eliminates the
  class of problem.
- **Modder-first by default.**  The content pipeline a
  modder needs is exactly the one the player learned to
  paint walls with.  No second SDK.

### 2. Bake to mesh — stencil as both data and asset

A stencil starts as **the same kind of data the world
holds** — painted hex cells with multi-layer + walls +
bridges + surfaces.  Two output paths:

- **Place as prefab** — the stencil's hex data is *stamped*
  into the world's painted-hash at a chosen offset, possibly
  with rotation.  After placement, it's indistinguishable
  from hand-painted world content.  The world's save file
  (MapFile) records the stencil reference + offset +
  rotation, not the stamped hexes (so a stencil edit
  re-applies cleanly).
- **Bake to unit** — the stencil's hex data goes through a
  **mesh baker** that produces a static 3D mesh, sized down
  to entity scale (a robot drawn at editor scale becomes a
  ~1-hex-tall unit when baked).  The baked mesh is what the
  runtime renders; the original hex data is the *source*,
  retained for re-bake.

Stencils used as units optionally compose: a parent mesh +
one or more child meshes attached at named pivots.  The
parent mesh is the base; the child meshes are rotating /
swiveling parts.  Same mechanism handles tower tops,
turrets, sensor heads, weapon mounts.

## Who this serves

Three audiences benefit from the same pipeline, with
different end-states:

### 1. The dryopea team (during dryopea's own development)

Rapid prototyping of dryopea's own content.  Every robot
kind, every world prefab, every tower variant is painted
in the editor, baked, dropped into a map, and play-tested.
The dogfood loop (per CLAUDE.md dev cadence) closes in
minutes per iteration.  At the end of dryopea's own
development, the team decides between the polish path
and the strike path for the shipped version.

### 2. Professional / well-funded studios using the suite

A team that wants the engine + the toolchain to ship a
high-fidelity game.  They use plan 06 to get **shape-
correct, proportioned, placed** content fast — far faster
than per-asset 3D modelling — and then run a **final art
push** (the polish path) over the existing shapes.  The
polish artist refines silhouettes, adds textures, lighting
response, materials.  The shipped game has the
professional polish a well-funded title needs, without
the upfront blocker of "wait three months for the
modeller before we can prototype the mechanic."

### 3. Indie / starting devs using the suite

The strategic unlock.  A solo developer or small indie
team takes the loft / dryopea / moros suite and **ships a
full game on the stencil output alone** — no polish push,
no hired artist.  The clean geometric look that stencils
naturally produce **is the aesthetic**.

Plenty of indie successes ship in this territory:
art-deco geometric, low-poly, block-layout, faceted
voxel-adjacent, isometric clean-line.  What makes them
look *intentional* rather than *unfinished* is:

- **Authoring discipline** — clean shapes, consistent
  proportions, deliberate silhouettes.  Stencil authoring
  rewards this; the bake faithfully renders what was
  painted.
- **Colour palette discipline** — a carefully-curated
  palette (dryopea's 11-entry palette is already this) +
  per-stencil colour overrides reads as *style*, not
  *placeholder*.
- **Composition** — how stencils are placed in a scene
  matters (foreground / background, scale relationships,
  spacing).  This is editor-time work, not asset-time
  work.
- **Engine-level atmospherics** — lighting angle, fog,
  particle accents, post-processing.  Engine effort,
  applied across the board; no per-asset cost.

For an indie, "striking visuals" comes from those four
levers, not from polish art.  The stencil pipeline
delivers shapes that *can* be striking when the four
levers are pulled with care — and it ships at indie
budget.

This makes the suite **a viable game-engine offering for
indies who don't have or need an art team**.  Not a
prototype tool that needs replacement; a *complete pipeline
that ships full games* at the indie tier.

## Scope

In scope:

- Multi-layer painting + bridges added to the existing
  editor (phase S1).
- Stencil authoring mode (bounded region, separate save
  format) (phase S2).
- Mesh baker producing static meshes from stencil data
  (phase S2).
- Mesh composition (mount + pivot) for rotating parts
  (phase S3).
- Entity runtime treating baked meshes as movable units
  (phase S3).
- Texturing / colour / surface materials of baked meshes
  (light scope; per stencil) (phase S3).
- Catalogue authoring discipline — how stencils get named,
  versioned, and indexed (phase S3).

Out of scope (deferred):

- **Jointed / leg movement** for organic-feeling units
  (insects, multi-legged variants, walking robot
  animations).  Sketched as phase S4; lands when tier-2
  insects (a future plan) trigger the need.
- **Procedural / generative stencil composition** — auto-
  combining parts into novel stencils.  Never in scope:
  dryopea's authoring philosophy is curation (per plan 04
  § No auto-generation), not procedural variety.
- **The final art push** — high-fidelity textures, normal
  maps, PBR materials, polished silhouettes, lighting-
  response surfaces, the works.  This is the *polish path*
  (see Goal § Two shipping paths); it lands near the end of
  development on solid shapes that plan 06 already shipped.
  Plan 06 is **not a substitute** for that push when a team
  chooses it — but it is also **not a prerequisite**:
  the *strike path* (indie / starting dev) ships the
  stencil output AS the aesthetic and never invokes a
  polish push at all.  See § Who this serves below.

## Phases

| Phase | What ships | Trigger | Effort |
|---|---|---|---|
| **S1** | Multi-layer + bridges in the editor | Plan 01 playtest closes | M |
| **S2** | Stencil mode + mesh baker | S1 shipped | MH |
| **S3** | Mesh composition + entity runtime | S2 shipped + over-the-shoulder camera (DESIGN § 12) approaching | M |
| **S4 (deferred)** | Jointed / leg movement extensions | Plan 07 tier-2 insects starts | M |

### Phase S1 — Multi-layer + bridges in the editor

The moros-house borrowings.  Editor gains:

- **Multi-layer painting.**  Each hex can carry content on
  multiple stacked layers (ground, lower walls, upper
  walls, roof, …).  Tab / hotkey cycles the active layer in
  the editor.  Save format extends MapFile (post the
  loft-JSON-cast fix that the schema is gated on) to carry
  per-hex per-layer entries.
- **Bridges as a primitive.**  Bridges connect two hexes
  *between* layers — e.g. a deck spanning a wall gap.
  Same paint verb; the picker gains a bridge entry.

These are already deferred items inside plan 01 (DESIGN.md
§ 5 walls notes bridges as a second-phase feature; multi-
layer is referenced as a moros pattern dryopea inherits).
S1 promotes them to first-class implementation work, because
the stencil pipeline can't represent meaningful entities
without vertical / connecting structure.

Tests follow the established pattern (golden-image renders +
assert-based round-trip).

### Phase S2 — Stencil mode + mesh baker

Two distinct deliverables:

**Stencil authoring mode** — Tab in the editor switches
between world mode and stencil mode.  Stencil mode:

- Bounded region (no infinite scrolling — the stencil has
  a footprint).
- Separate save format: `stencils/<name>.json` analogous to
  the MapFile, smaller in scope.
- Same painting verbs (palette, picker, click/drag, paint-
  line, hover-outline).
- Output is a `Stencil` data structure — hex cells + layers
  + walls + bridges + surfaces, plus a footprint extent.

**Mesh baker** — a command (CLI + in-editor button) that
takes a `Stencil` and produces a static 3D mesh suitable
for runtime rendering:

- Reads the stencil's painted layers + walls + bridges.
- Generates triangle geometry per filled cell / layer / wall
  segment / bridge span.
- Scales the result down to entity scale (a configurable
  factor; default makes a 5×5-hex stencil end up ~1-hex
  tall at baked scale).
- Bakes per-vertex / per-face colour or material based on
  the palette + a per-stencil colour-override layer.
- Writes the mesh to `meshes/<name>.{glb,bin}` or similar.

Tests: a small handful of bake-and-render round-trips with
golden meshes (vertex-count + bounding-box assertions, not
byte-equal — meshes are float-y).

### Phase S3 — Mesh composition + entity runtime

**Mesh composition.**  A baked-mesh manifest can name
**pivot points** (positions + axis-of-rotation).  At runtime
a second baked mesh can be mounted at a pivot and rotate /
swivel.  Examples:

- Tower base + tower rotating-top + named pivot at the
  base's top centre, rotation axis = up.  The top rotates
  to aim at targets.
- Vehicle body + swivel weapon + pivot at the vehicle's
  spine.  Weapon rotates without the whole vehicle turning.
- Robot body + sensor head + pivot at the neck — head
  tracks the scrambler bubble during the pre-walk
  visibility interval.

The composition format is **declarative** (JSON, parallel
to MapFile): a unit manifest lists a base mesh + zero or
more child meshes with pivot bindings and behaviour hints
(track-target / fixed-rotation / oscillate-rate / …).

**Entity runtime.**  The game gains a runtime that:

- Loads baked meshes by name from the asset directory.
- Spawns them as game entities with position + orientation.
- Moves them per their AI / wave / player input.
- Renders the base + composed child meshes per frame.
- Handles damage state — at validation, just colour
  overrides; later, swap-in damage-variant meshes per
  region (covered by stencil re-bake, not a separate
  pipeline).

Tests: a small ECS-like spawn + tick + render harness.
Headless golden tests prove the renderer + composition pipe
works; live tests in the GL editor prove unit motion +
animation.

### Phase S4 — Jointed / leg movement extensions (deferred)

When tier 2 (plan 07 insects) starts in earnest, the
stencil pipeline needs to handle **multi-jointed animation**
— specifically:

- **Leg-cycle locomotion.**  Walking on multiple legs with
  ground contact, gait sequencing, stepping rhythm.
  Insects (6 legs), boss repair platforms (possibly 4),
  walking-robot variants.
- **Joint constraints + IK.**  A leg has hip + knee +
  foot.  Stepping on uneven terrain requires inverse
  kinematics so the foot lands on the surface, not
  through it.
- **Stencil-time joint declaration.**  The author marks
  which sub-meshes are joints (vs rigid surfaces) and what
  their rotation limits are.  This is an *extension* to
  the stencil format, not a separate one.

Phase S4 is **explicitly deferred** until the trigger
fires.  When it lands, the existing stencil pipeline
remains valid — insects and walkers add joint metadata to
their stencils without re-authoring.

This phase also opens the door to **organic-feeling
units** in the broader sense — multi-jointed bodies,
oscillating wings (flying insects), articulated tails.
The same "joint + animation hint at stencil time" pattern
extends to all of them.

## Implementation + testing

### Testability discipline

Same as plan 01: factories + pure tick + headless tests +
golden-image renders.

- **Multi-layer + bridges** (S1) are pure data extensions.
  Test by round-tripping a multi-layer painted region
  through save/load + asserting the layer state survives.
- **Stencil mode** (S2 authoring) is pure data — assert
  that switching modes preserves the unedited state in
  the other mode; assert stencil-region edits don't leak
  into the world.
- **Mesh baker** (S2) is a pure function from Stencil →
  Mesh.  Test by baking a known stencil and asserting
  mesh properties (vertex count in a range, bounding box
  within tolerance, expected materials present).
- **Mesh composition + entity runtime** (S3) gets a
  headless tick harness — spawn a unit, advance N frames,
  assert position + orientation + child-mesh pivot angle.
- **Live tests** in the GL editor — open a window, place
  a stencil prefab, bake a unit and spawn it, watch the
  tower-top rotate.  Manual playtest, not automated.

### File layout (additions to dryopea/)

```
src/
  stencil.loft           # Stencil data structure + save/load
  bake.loft              # Mesh baker
  composition.loft       # Mesh-on-mesh mount + pivot
  entity.loft            # Runtime entity (baked mesh as a unit)
  layer.loft             # Multi-layer hex painting (S1)
  bridge.loft            # Bridge primitive (S1)

stencils/                # Authored stencils (input to bake)
  examples/
    starter-robot.json
    laser-tower-base.json
    laser-tower-top.json
    sap-tree-cluster.json
    abandoned-habitat-A.json

meshes/                  # Baked meshes (output of bake)
  starter-robot.glb      # or whatever the format settles to
  laser-tower-base.glb
  …

manifests/               # Composition manifests
  laser-tower.json       # base + rotating-top + pivot binding
  starter-robot.json     # base + head + pivot binding
```

## Open questions

1. **Insect pipeline — same stencils or separate?**  Phase S4
   *attempts* to extend stencils to leg movement.  If that
   proves awkward (organic shapes don't fit the wall / bridge
   / surface vocabulary cleanly), a parallel pipeline for
   biological units may land.  Decision deferred to plan 07.
2. **Elemental rendering — particles, not stencils.**
   Tier-3 elementals are particle-based (fire / water /
   wind / earth — per SETTING.md § Elementals) and almost
   certainly need a different pipeline.  Out of scope for
   plan 06.
3. **Bake output format.**  `.glb` aligns with loft
   `lib/graphics` mesh tooling, but the baker may target a
   loft-native format if that's leaner.  Decided in S2
   implementation.
4. **Stencil editing while a map references it.**  Editing
   a stencil that's placed in 5 maps should re-apply
   everywhere on next load.  The MapFile carries a
   reference, not the stamped hexes — but the runtime
   needs to handle "stencil version N was placed; we now
   have version N+1; re-place automatically" gracefully.
   Probably trivial; flag for S2 implementation.
5. **Scaling factor authorship.**  A robot drawn 5×5 hexes
   becomes ~1 hex when baked.  Per-stencil scale override,
   or per-purpose default (prefab = 1.0; unit = author-chosen
   shrink)?  S2 design call.
6. **Faction-flavoured variants.**  Each AI faction (per
   SETTING.md § Robot diversity) might prefer different
   parts of the same robot kind.  Implemented by **multiple
   stencils sharing prefixes** (`scout-bot-faction-A.json`,
   `scout-bot-faction-B.json`)?  Or by a colour-override
   layer applied per-faction?  Defer; both are viable.

## Dependencies

- **Plan 01 (ground editor)** — playtest closed.  The
  stencil mode lives inside that editor.
- **loft `lib/graphics`** — the mesh baker depends on
  whatever mesh + texture API the library provides; the
  composition runtime depends on GL state management for
  multiple meshes per frame.
- **loft lib_plan 24 — Universal hex-world editor +
  library extraction.**  Phases L1-L6 of that loft-side
  plan extract the shared substrate (`hex_grid` / `hex_map`
  / `hex_render` / `hex_stencil` / `hex_editor` /
  `hex_entity`) out of moros's existing rough-but-tested
  code.  **Plan 06 phases consume those slices as they
  land** — plan 06 S1 multi-layer + bridges = lib_plan 24
  L2 + L3 + L6 (stair-as-bridge); plan 06 S2 stencil mode
  + mesh baker = lib_plan 24 L4 + part of L6; plan 06 S3
  mesh composition + entity runtime = lib_plan 24 L6.
  See [loft lib_plans/24-universal-editor/REFERENCE.md](https://github.com/jjstwerff/loft/tree/main/doc/claude/lib_plans/future/24-universal-editor)
  for the extraction architecture.  **Without lib_plan
  24 landing, plan 06 either reimplements (waste) or
  copy-pastes moros code into dryopea (worst case).**

This plan is **NOT** dependent on plan 02 (solver-validation
viewer); plans 02 and 06 are parallel — 02 is for height-
solver debugging, 06 is for content authoring.  Both share
the same mesh-rendering infrastructure but at different
fidelities.

## Why this plan matters

- **It's the editor-IS-the-content-pipeline philosophy.**
  Plan 01 made the editor the way to paint terrain.  Plan
  06 makes it the way to author *everything else* — units,
  structures, scenery, mods.  The whole game's content
  surface collapses into one tool.
- **It brings the suite into rapid prototyping.**  Loft +
  dryopea + moros become a tight iteration loop: think of
  a new robot kind → paint it → bake it → drop into a map
  → run.  Minutes, not weeks.  This is the strategic
  positioning the suite has been driving toward.
- **Two shipping paths from one pipeline.**  The polish
  path adds a final art push for big-studio teams;
  the strike path ships the stencil output as the final
  aesthetic for indies.  See § Who this serves for the
  full audience breakdown.  Same pipeline, two viable
  outcomes — the team picks at the end of development,
  not at the beginning.
- **The developer is never waiting on art to reach a solid
  state.**  Whether a team intends the polish path or the
  strike path, the game is *playable and shape-correct
  throughout development*.  No "we can't prototype until
  the artist ships."  No "we can't ship a vertical slice
  because the magenta cuboid breaks the immersion."  The
  stencil pipeline guarantees correct shapes in every
  position from the moment a stencil is authored.
- **It's a viable game-engine offering for indies.**  The
  strategic implication of the strike path: loft +
  dryopea + moros become a complete game-engine pipeline
  for indie / starting devs who don't have or need an
  art team.  They ship full games on the stencil output
  alone.  This expands the addressable users of the
  whole suite well beyond dryopea's own development.
- **It honours the lore.**  AI personalities surface through
  stencil authoring choices (light vs brutalist silhouettes,
  preferred structural motifs); faction territory becomes
  visually identifiable through stencil prefixes.  The
  lore in SETTING.md gets to live in the visual world
  rather than being a footnote.
- **Modders inherit the same toolchain.**  Anyone with the
  editor can author new content — robots, factories,
  habitats, ruins.  The "modding SDK" is "the editor."
  Community content scales naturally with player retention.

## See also

- [`../../../docs/DESIGN.md`](../../../docs/DESIGN.md) —
  master design (towers, walls, units, vehicles)
- [`../../../docs/SETTING.md`](../../../docs/SETTING.md) —
  fiction (robot diversity, faction territory, lore-driven
  visual signatures)
- [`../01-ground-editor/`](../01-ground-editor/README.md) —
  the editor this plan extends
- [`../02-solver-validation-viewer/`](../02-solver-validation-viewer/README.md) —
  parallel mesh-rendering work (3D height-solved terrain
  viewer); shares some rendering infrastructure but
  different fidelity targets
- [`../../ROADMAP.md`](../../ROADMAP.md) — where this plan
  sits in the broader tier ordering
