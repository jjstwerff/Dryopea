<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Worldgen — deriving a scenario from a 1.5 km map

⚠⚠ **DESIGN, not built, and dryopea has NO procedural generation of any
kind today**: the three maps in `maps/` are authored `.keys` files built
into committed `.json` pairs, and there is no seed anywhere in `src/`.

⚠ This document is the **world → scenario** half.
[`ERRANDS.md`](ERRANDS.md) is the **what a mob is doing** half and owns
the two-scale ruling (`@X298`); [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md)
owns the graph that hangs on the coarse map.

⚠⚠ **It is written against `../crawler`, which has built this twice** —
once as the game's world and once as a real-data reference — and the most
useful thing in it is which of those is which.

> ⚠⚠ **AND `../crawler` IS NOT A FOREIGN SOURCE** (owner, 2026-08-28):
> *"parts of it landed in crawler, and the way the ortler map is
> formed."*  These are **dryopea's own 2023 designs, developed further
> in a sibling repo and coming home with an implementation and
> measurements attached** — which is why several of them converge with
> rulings made here independently (`@X299`, `@X301`, `@X306`).  ⚠ Read
> the citations below as *this idea, tried* rather than as *somebody
> else's idea*.

## ⚠⚠ THE CORRECTION: the Ortler map is a CALIBRATION FIXTURE, not a world  `@X309`

⚠ `@X307` recorded the owner's *"a world map in 1.5 km eventually like
the ortler map in `../crawler`"* and read *like the Ortler map* as *made
of real-world data*.  ⚠⚠ **Inspection says crawler has TWO world-map
artefacts and only one of them feeds the game**, and the distinction is
the most transferable thing it knows:

| | **the game's world map** | **the Ortler map** |
|---|---|---|
| what it is | a **hand-authored 9×7 array of material codes** | EU-DEM 25 m + OSM for the Ortles massif, 80×80 hexes |
| where | `src/overland.loft:73` `ov_mat_rows()` | `data/regions/ortler.bin` (882 236 bytes) |
| hex | 1500 m | 1500 m — **the same number** |
| who reads it | `src/sim.loft`, i.e. **the game** | `src/viewer.loft` and three smoke tests, **only** |
| its job | be the world | ⚠⚠ **be the GROUND TRUTH the game's classifier is tuned against** |

⚠ `src/sim.loft`'s import list contains `use overland;` and **no ortler
module at all**.  `overland_new()` takes no arguments and hard-codes
`ov_seed: 7` — **the overland is a constant of the build.**

### ⚠⚠ What the fixture was FOR, and what it paid back

`plans/1-ortler-worldgen-fixture/README.md`:

> *"Using the greater Ortler massif as a real test region, answer:
> **Adequacy** — is crawler's current 14-kind terrain taxonomy enough …
> **Defaults** — tune the per-kind default numbers … **Transition values
> → realistic fantasy defaults** — *derive* (not guess) the
> elevations/slopes where terrain types change inside a real range."*

> *"**The end deliverable — two same-scale drawings**: (A) realistic,
> from the actual data … the ground truth.  (B) our model … **The gap
> between B and A is the adequacy verdict**; closing it is the default
> tuning."*

⚠⚠ **And the payback was FOUR NUMBERS.**  `tuned-defaults.md`:
*"`TREELINE = 2150`, `ALPINE_TOP = 2500`, `ROCK = 2800`,
`SNOWLINE = 3100` (vs the engine's compressed `TREEL=520`/`SNOWL=880`)"*
— and those four literals are in the engine today
(`src/overland.loft:59,65,66,71`).  ⚠ Measured effect of wiring them in:
*"meadow 0.3 ‰ → 5 ‰ (~17×)"*.

⚠ **The verdict was also a REFUSAL**, which is what makes it worth
having: *"the 14 `K_*` kinds SUFFICE for the Ortler — no missing kind"*,
and *"**at 1.5 km, terrain type is an ELEVATION function, not slope** …
`K_FACE` / cliffs are **not resolvable at 1.5 km**."*

> ⚠⚠ **So the pattern to copy is not *import a mountain range*.  It is
> *import one, measure your model against it, keep the numbers, throw
> the data away.*** ⚠ crawler's own README records the failure that
> nearly lost it: *"⚠ **And note where F1 was marked FIXED: in the
> Python render, not in the engine**"* — a fixture can be green while
> the game it was for is unchanged.

### ⚠ Which leaves dryopea a fork, and it is the owner's

| | **A — real data IS the world** | **B — real data CALIBRATES an authored world** |
|---|---|---|
| the coarse map | imported elevation, per planet | authored or generated material codes |
| `@X224` *other players are the seed* | ⚠⚠ met at t = 0 for free, because **a heightmap is not authored** (`@X308`) | ⚠ met only by the ACCUMULATED layer |
| cost | a data pipeline, a licence, and **loft#854** below | one array and a tuning exercise |
| what crawler did | built it, and **does not ship it in the game** | ⚠⚠ **this one** |

⚠ **Recommendation: B, with A as the instrument** — which is exactly what
crawler converged on after building A.  ⚠⚠ But `@X308`'s argument for A
is real and crawler never made it: *a heightmap is author-free, so it
buys the anti-optimisation property before a single player has done
anything*.  **The ruling is the owner's**, and it is worth taking
knowing that crawler tried A and kept it as a measuring stick.

## ⚠⚠ THE SECOND REASON: A BACKDROP OF REAL GEOGRAPHY  `@X312`

Owner, 2026-08-28:

> *"there is another reason for the world map: I want to have a backdrop
> world with real features too, so a far away view for mountains, seas,
> islands, rivers etc.  Yes there is fog in this world but that doesn't
> mean that there are no circumstances where a view is possible.  They
> should feel **earned** by players and feel **impactful**."*

⚠⚠ **This changes the fork above, and it changes it toward A.**  §
Which leaves dryopea a fork recommends an authored coarse array with
real data as the instrument — **on the strength of the ECONOMY's needs
alone.**  A backdrop has a different requirement:

> ⚠⚠ **A distant massif has to LOOK like one, and a 9×7 array of
> material codes will not produce a convincing horizon.**  Real
> geography is what makes a far view read as a place rather than as
> generated scenery — which is `@X308`'s *a heightmap is not authored*
> arriving for a second, independent reason.

⚠ So the two reasons want different things from the same artefact, and
that is worth stating rather than resolving by preference:

| | what it needs from the coarse map |
|---|---|
| **the economy** (`@X298`) | which routes cross a cell, and what is in it — **a graph**, and material codes carry it |
| **the backdrop** (`@X312`) | ⚠⚠ **a skyline** — elevation, at enough resolution to be silhouetted from 40 hexes away |

⚠⚠ **And the backdrop is the CHEAPEST believability in the whole
design** by `@X303`'s test: it simulates **nothing at all** and is pure
observation.  Nothing walks on it, nothing is tracked in it, and no rule
has to be boundable — it is a picture of ground the player will never
stand on.

### ⚠⚠ THE HAZARD, and it is MEASURED: the default frame has no sky  `@M064`

⚠ `DESIGN.md` § Atmosphere already bounds sight at **~40 hexes** —
*"the engine never draws the whole map, just the haze radius around the
player"* — so a backdrop is by construction a thing seen only when the
haze allows.  ⚠⚠ **But the harder constraint is the CAMERA, and it
already killed a design:**

> `@M064` / `@X287`: the follow camera sits at **30.96°** with a 60° fov,
> so the top of the frame is **0.96° BELOW the horizon** — fourteen
> pixels on a 720-high frame.  **The game's default frame contains no
> sky at all.**  The battleships-as-permit-clock proposal was falsified
> on exactly this, and the verdict was that *a signal you must tilt the
> camera to consult is the HUD number `DESIGN.md` § HUD refused, with a
> tax on top.*

⚠⚠ **A backdrop is the same object and must not inherit the same
refusal.**  ⚠ But the owner's own word is what saves it, and it is the
whole difference:

> ⚠⚠ **A permit clock must be CONSULTABLE — always, cheaply, at a
> glance.  A view is EARNED — rare, deliberate, and an event.**  The
> refusal was of a signal the frame does not contain; **an earned view is
> a moment that brings its own frame.**

### ⚠ So *earned* is a design constraint, not flavour

⚠ Three things follow, and each is a rule rather than a wish:

1. ⚠⚠ **The moment must come with the frame that shows it.**  Either the
   earning is a PLACE whose geometry puts the horizon in view — standing
   on a ridge, on a tree limb (`DESIGN.md` § Trees as terrain has stems
   ten hexes wide), on the core — or the moment moves the camera.  ⚠ The
   arithmetic is encouraging: `@M064` says the horizon enters at
   **exactly 30.0°** against the follow camera's 30.96°, so it is **one
   degree away**, not a redesign.
2. ⚠ **Earned means the player DID something.**  Climbed, waited out
   weather, cleared a ridge, got somewhere at the right hour — the same
   *position triggers, not key presses* rule § 11 keeps for everything
   else.  ⚠⚠ A view that simply happens is atmosphere; a view you
   reached is a **reward**, and `@X303`'s test is met by the reaching
   rather than by the picture.
3. ⚠⚠ **Impactful means it TELLS you something.**  A horizon that is only
   pretty is spent once.  ⚠ The design already has the channel:
   [`EXPLORATION.md`](EXPLORATION.md) makes scouting the first fifteen
   minutes, and *"players must scout to learn what is beyond the haze"* —
   so **a view is scouting done from a height**, and what it shows is
   where the next sortie could be.  ⚠ That also makes it the one place
   the coarse map is legible to a player at all.

⚠ **And it is the strongest argument yet for `@X310`'s literal 1.5 m**:
a compressed terrain scale would put the mountains **ten times closer
than they are**, and a backdrop whose distances lie is worse than none.

## ⚠⚠ THE TERRAIN SCALE IS NOT THE ARCHITECTURE SCALE  `@X310`

⚠⚠ **`@X298` says one coarse hex has to produce a MILLION fine ones, and
crawler simply refuses that.**  `src/scale.loft:26-33`:

```
M_PER_HEX_STEP  = 1.5      // a hex is 1.5 m of ARCHITECTURE
TERRAIN_COMPRESS = 10.0    // and 15 m of TERRAIN
```

So a hex is **1.5 m wide for a building and 15 m wide for a mountain**,
and the whole ladder shortens by ten:

| | crawler | ratio to its 1.5 km tile |
|---|---|---|
| overland hex | 1500 m | 1 |
| block | ~60 m (41 fine hexes) | 25 |
| fine hex | **15 natural m** | 100 |

⚠⚠ **And that is why a crawler LEVEL IS ONE OVERLAND TILE**: the surface
window is 101×101 hexes, `// the surface: a wilderness WINDOW (1.5 km
across)` (`src/sim.loft:5085`) — 101 × 15 m ≈ 1.5 km.  **The million
disappears.**

⚠ dryopea's 1.5 m is **literal** — `HEX_DIAMETER` is what a wall is
across, and `@M050`'s five-hex wall is a five-hex wall.  So dryopea has
the same fork one level down:

- **keep 1.5 m literal**, and a scenario stays ~1 % of a cell
  (`@X298`) — a base is a patch and the cell is the neighbourhood;
- **or compress terrain**, and a scenario becomes a whole cell, at the
  price of *a hill is ten times further away than it looks*.

⚠⚠ **`@X298`'s arithmetic already assumed the first**, and it is
probably right for this game — dryopea is a base-scale tower defence
where crawler is a walking game — **but it should be a decision rather
than an accident**, because it is the number that decides whether the
coarse map is a neighbourhood or a level.

## ⚠⚠ THE DERIVATION IS A SAMPLER CASCADE — never a tree, never stored

`OVERLAND.md:403-416`, and this is the sentence to keep:

> *"The gap from 1.5 km tiles to 1.5 m walked hexes is bridged by a
> LADDER of layers, each a pure sampler over the one above — **never a
> tree you descend, never data you store**. … Two channels everywhere:
> continuous FIELDS give the ground; discrete FEATURES give the forms …
> **the detailed map is a VIEW of a function — only player deltas
> persist.**"*

⚠⚠ **That is [`ERRANDS.md`](ERRANDS.md) `@X299`'s rule-not-state, arrived
at independently for TERRAIN** — and `@X306`'s *the result is the
snapshot changed* is *only player deltas persist* said the other way
round.  **Two systems, one shape, and neither was derived from the
other.**

### ⚠⚠ A HEX'S CONTENT IS A FUNCTION OF ITS SIX NEIGHBOURS  `@X313`

Owner, 2026-08-28, recalling the original design:

> *"in my initial designs for this game the content of a hex is
> dependent on the 6 hexes around it — their terrain, their elevation,
> their water flow."*

⚠ **It is not written down anywhere.**  The 2023 notes in
[`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) § 2 carry the ingredients —
*"general slopes (flats, gradual, hills, mountain)"* and *"water flow,
amount of water"* — but not the rule that ties them together, and it is
the rule that decides the whole shape of a derivation.

⚠⚠ **Three things follow from it, and each is load-bearing:**

1. ⚠⚠ **IT IS WHAT MAKES A 1000× INTERPOLATION POSSIBLE AT ALL.**  A
   single cell has a value and **no gradient** — nothing in it says which
   way is downhill.  Six neighbours give **slope, aspect and drainage**
   for free, and those three are what every terrain classifier actually
   reads.  ⚠ `crawler`'s `ov_blend` (`src/overland.loft:997`) is exactly
   this: a smooth kernel blend of the surrounding cells producing
   `(height, watery, lakew, dominant material)`, and `OVERLAND.md:92`
   states the rule — *"the dominant tile sets the table; **in blend bands
   the tables interpolate**."*
2. ⚠⚠ **THE COARSE MAP STORES FAR LESS THAN IT PRODUCES**, which is what
   makes it small enough to BE a server state (`@X306`: *compact is the
   requirement, not an optimisation*).  Six neighbours × three fields is
   **eighteen numbers**, and a whole cell's terrain comes out of them.
3. ⚠⚠ **IT MAKES THE DERIVATION WATERTIGHT BY CONSTRUCTION — if the
   shared thing is computed the same way from both sides.**  This is the
   one place a neighbour rule can silently go wrong, and `crawler` states
   the fix as a commutativity requirement
   (`src/realworld/trimesh.loft:17-24`):

   > *"Corner shared by 3 hexes: height = **mean of the 3 hex centres**.
   > Edge-third shared by 2 hexes: height = **⅔·home + ⅓·neighbour** for
   > the ⅓ slot, swapped for the ⅔ slot"* — **commutative, so both
   > owners compute the identical number.**

   ⚠ Same rule in its river code: `edge_cross` hashes the **sorted**
   pair so both hexes agree (`src/realworld/rivers.loft:13`).  ⚠⚠ And
   *watertight across seams* is one of `@X311`'s four talus invariants —
   so in that repo it is a GATE, not a hope.

#### ⚠⚠ AND THE ORTLER MAP IS THIS RULE, MADE CONCRETE AND THEN TESTED  `@X314`

Owner, 2026-08-28: *"parts of it landed in crawler, and **the way the
ortler map is formed**."*

⚠⚠ **The 18-triangle sub-hex mesh IS the six-neighbour rule.**  Every
hex is split into 18 triangles over **19 points** — the centre, its 6
corners and the 12 edge-thirds — and **every one of those points except
the centre belongs to more than one hex**:

| point | shared by | its value |
|---|---|---|
| centre | 1 hex | the cell's own sample |
| corner | **3 hexes** | ⚠ the **mean of the three centres** |
| edge-third | **2 hexes** | ⚠ **⅔·home + ⅓·neighbour**, swapped for the other slot |

⚠ So a hex's drawn surface is *literally* a function of itself and its
six neighbours, and the commutativity above is what makes the two owners
of a shared point compute the identical number.  ⚠⚠ **The rule and its
watertightness are the same property**, which is why it can be gated.

⚠⚠ **And then the fixture TESTED it, which is the part worth having.**
The Ortler `.bin` stores **real EU-DEM heights at those 19 points** for
the 40×40 subset (14 878 vertices) — so at 40×40 the rule's answer is
*measured* and at 80×80 it is *derived*, and the two can be compared.
`plans/2-chunked-lod-world/s6-subhex-finding.md` reports what the
comparison found:

> *"The triangle vertices are **real EU-DEM samples** at the 19 sub-hex
> points (hex centre + 6 corners + 12 edge-thirds, ~250–750 m spacing) —
> **not interpolation**.  So this is the finest *real* signal the plan #1
> fixture holds."*

⚠⚠ **The verdict: the six-neighbour rule gives you geometry and does NOT
give you land cover.**  At ~350 m spacing *"no simple geometric sub-hex
signal (slope, relief) separates cliffs from forest"* (`@X311`), and
`@X313`'s spurious lakes are the same limit on the water side.  ⚠ So the
rule is right and **its output is a shape, not a classification** — which
is exactly why `@X311`'s answer was to run a physical process over the
shape rather than to classify it harder.

#### ⚠ Water flow is a first-class input, and it is derived not stored

⚠ The owner's third field is the interesting one: **flow is not a
property of a cell, it is a relation between cells.**  `crawler` computes
it rather than authoring it — `hydro_compute` (`src/realworld/hydro.loft`)
is a **priority-flood pit-fill**, then **steepest descent** giving a
direction 0..5, then **accumulation** = how many cells drain through.
Width follows from accumulation: `35.0 + 18.0·√acc`.

⚠ dryopea has the two static halves of this already and reads them:
the palette's **`slope`** (read at last by `damage.loft` § Footing,
`@X284`) and its **`drop`** (read at last by `moat.loft`, `@X282`).
**What it has never had is the third — flow.**

#### ⚠⚠ THE MEASURED TRAP: a neighbour rule OVER-PRODUCES LAKES

⚠ `crawler`'s gap analysis found it and quantified it
(`plans/1-ortler-worldgen-fixture/gap-analysis.md`, F2):

> *"**Spurious lakes — quantified: model 72 lakes vs OSM 6 water cells
> (~12×).**  Pit-fill floods local 1.5 km basins."* — marked VERIFIED,
> and **still open**.

⚠⚠ **So the honest statement is that a neighbourhood rule at 1.5 km
gives you slope and drainage cheaply and gives you STANDING WATER
wrongly**, because a basin one cell across is 1.5 km wide and a real lake
is not.  ⚠ It is the same shape as its other refusal — *cliffs are not
resolvable at 1.5 km* (`@X309`) — and it wants the same answer: either a
finer signal, or `@X311`'s move of replacing the classifier with a
process.

#### ⚠ And it revives something `DESIGN_HISTORY.md` recorded as dead

⚠⚠ § 2's *"What did NOT survive (yet)"* lists **"sampling png onto hex
grid"** — the image-as-input terrain importer — as *"a back-door bulk
import tool but not core"*.  ⚠ `@X312`'s backdrop **needs exactly that**,
because a skyline has to come from somewhere.  So a 2023 idea written off
as a convenience comes back as the thing a required feature rests on, and
its own note — *"specific point & 4 pixels around it"* — is this section's
rule in its original form.

### The one entry point

`ov_sample(o, x, y) -> (height, kind)` (`src/overland.loft:1186`).  In
order: a **kernel blend** of surrounding coarse cells (`ov_blend`) plus
ridge/peak fields and a river carve; then **fbm noise**; then a **branch
cascade** on `(height, slope, watery, dominant material, distance to the
nearest road / river / town / swamp)` producing one of 14 kinds.

⚠ So it is **interpolation, then noise, then a lookup cascade** — all
three, in that order, and the coarse cell reaches the fine hex only as
`mdom` (its dominant material) and three per-material numbers.

### ⚠⚠ TWO CHANNELS, and the second one is already dryopea's POI

`OVERLAND.md:92`: *"the dominant tile sets the table; in blend bands the
tables interpolate."*

| channel | what it is | dryopea's name for it |
|---|---|---|
| **FIELDS** | continuous, sampled per fine hex | the ground |
| **FEATURES** | ⚠⚠ a list of **world-coordinate records with bounded influence** — towns, forts, ruins, wizard towers, road stops, swamps, rivers, roads | ⚠⚠ **points of interest** (`@X301`) |

⚠⚠ **`@X301` has a working precedent and did not know it.**  crawler's
features stamp into a level by exactly the test `@X300` describes:

```
(fq, fr) = px_to_hex((ft.of_x - ax + cwx) / sstep, …);
if fq >= 8 && fq < w - 8 && fr >= 7 && fr < h - 7 { …stamp… }
```

**A feature appears iff its world position falls inside this window with
a margin** — which is the bound, the cull and the materialisation in one
line, and it is per-feature exactly as `@X301` makes it per-POI.

## ⚠⚠ WHEN A CLASSIFIER CANNOT BE MADE ACCURATE, REPLACE IT WITH A PROCESS  `@X311`

**The best thing in crawler's plan set, and it is a falsification.**
`plans/2-chunked-lod-world/s6-subhex-finding.md` set out to find a
sub-hex geometric signal that separates cliff from forest, and found
none:

> *"**Forest sits on terrain just as steep and rugged as rock.**  In the
> Alps the rock/vegetation boundary is the **treeline (an elevation +
> exposure effect)** — already captured by the height bands — *not* a
> geometric one."*
>
> *"**`slope > θ → cliff` gives ZERO κ gain.**  Sweeping θ: best κ
> **+0.464 at θ=50°** — identical to the height-band baseline."*
>
> *"**no simple geometric sub-hex signal (slope, relief) separates cliffs
> from forest at the ~350 m real-data resolution.**"*

⚠⚠ **And the resolution was to stop classifying and start SIMULATING a
process:** the *talus model* — bedrock plus a rubble layer, rubble cannot
hold a slope steeper than the **angle of repose (~35°)** so it slides to
lower neighbours (a sandpile relaxation); where bedrock alone exceeds
repose the rubble strips away to **bare face**, and the shed rubble piles
at the foot as **scree**.

⚠⚠ **The gate changed with it, and that is the transferable half:**

> *"**Gate is the INVARIANTS, not κ**: rubble **conserved**, relaxed
> rubble surface **≤ repose everywhere**, **watertight** across seams,
> **deterministic**."*

⚠ dryopea already prefers exactly this shape — `plans/22` refuses LOD
because *"its gate is 'the outcome is unchanged', which is a much weaker
statement"*, and every `@M` row here is an invariant or an exact number.
⚠⚠ **So the rule to carry: a derivation whose accuracy cannot be gated
should be replaced by a process whose CONSERVATION can.**

## ⚠ Siting — three rules crawler paid for

⚠ Each is a one-line lesson with a defect behind it, and each transfers
directly to placing a POI (`@X301`).

- ⚠⚠ **NEAREST IS NOT REACHABLE.**  `walk_reach` (`src/sim.loft:394`) is
  a flood fill *"because siting happens before a `Sim` exists"*, and it
  exists because *"a worker was given a job it could not reach.  It
  walked 13 hexes, stopped, and stood there for the life of the world."*
- ⚠⚠ **REACHABLE IS NOT WORKABLE.**  `WORK_MAX_D = 22` bounds a day's
  round trip, so a site can be reachable and still useless.
- ⚠⚠ **FARTHEST-REACHABLE IS A CUL-DE-SAC BY CONSTRUCTION** — *"the
  deepest hex of a pocket has the fewest ways out"* — so the den siting
  adds `room >= 4` walkable neighbours on top of the distance.

⚠ **And the honest failure**: the mine's siting has four fallbacks and
then gives up — *"⚠ **SO A VALLEY TOWN SIMPLY HAS NO MINE, and that is
the honest answer**"* (`src/sim.loft:5992`).  ⚠⚠ Which is the answer
`@X305` needs too: **a cell that cannot host a POI does not get a
substitute.**

## ⚠ Density — crawler caps everything, in small integers

⚠⚠ **Directly supporting `@X305`'s two-to-four**, these are the shipped
numbers: ≤ **4** towns in the whole world (min spacing 2.2 tiles),
≤ **4** ruins, exactly **2** wizard towers, one road stop per 2.8 km of
road, **exactly 10** wild fauna per window, and **at most 2** cave
mouths.  Town population is `ot_size` 1..3 and *everything* downstream
reads it — houses `3 + size*2`, guards `1 + size`, the wall radius
derived from the house count.

⚠ So a world has about a dozen discrete features across 13.5 × 9.1 km,
and a level window sees the few that fall in it.

## ⚠⚠ Persistence — and it is `@X306`'s shape, shipped

> *"Window state is **not stored** — a revisited window regenerates fresh"*
> (`src/sim.loft:6815`).

⚠ Nothing is written back to the overland at all; `overland_new()`
returns a fresh immutable world every call.  ⚠⚠ **The only persistence
is a DELTA, and only for dungeon depths:**

> *"A level is a pure function of its seed …  The ONLY thing persisted is
> what diverged: **which spawns died**. … **Round-trip = identity.**"*
> (`src/sim.loft:334`)

⚠⚠ **That is `@X306` — *the result is the snapshot, changed* — as
working code**, and its gate is the one dryopea would write: round-trip
identity.  ⚠ What crawler does NOT do is write the delta back into the
world map, and `DESIGN.md:606` marks the persistent version as designed
and unbuilt.  ⚠ So dryopea's `@X306` is a step past what crawler ships,
and the step is small.

## ⚠⚠ Two traps to carry over verbatim

- ⚠⚠ **DO NOT COMMIT GENERATED DATA AS A LOFT LITERAL.**
  `crawler/CLAUDE.md:235`: *"`src/regions/ortler.loft` is generated data
  whose largest line is an **86 400-element vector literal**, and loft
  parses a literal in **O(n²)** — **~18 min for that one line**, at 99 %
  CPU with no output, **so it presents as a hang**"* ([loft#854]).  ⚠ The
  whole family of six entry points that import it is *"not broken,
  **unusable**"*.  **dryopea's F8 must land as a binary file with a
  reader, never as a `.loft` array.**
- ⚠ **A seed that ignores the window makes every window draw the same
  numbers.**  crawler's `gseed = seed ^ world_key_seed(1, 1, depth)`
  passes **literal `1, 1`** rather than the window coordinates
  (`src/sim.loft:5088`), so the surface RNG stream is identical
  everywhere and only the terrain tests differ.  Undocumented, and found
  by reading.

## Open questions — the owner's

1. ⚠⚠ **A or B?** — is dryopea's coarse map real data, or an authored
   array that real data was used to TUNE?  § Which leaves dryopea a fork.
   ⚠⚠ **The recommendation has MOVED to A** on `@X312`: the economy is
   served by either, but **a backdrop needs a skyline** and a 9×7 array
   of material codes will not silhouette convincingly at 40 hexes.
   ⚠ crawler ended at B because it only ever needed A as a measuring
   stick; dryopea needs the data on screen, which is a requirement
   crawler never had.
2. ⚠⚠ **Is dryopea's hex 1.5 m of terrain, or 1.5 m of architecture?**
   (`@X310`).  ⚠ `@X298`'s arithmetic assumed terrain; crawler compresses
   10× and thereby makes a level one whole tile.  *Recommendation: keep
   1.5 m literal* — dryopea is base-scale where crawler is a walking
   game, and ⚠⚠ **`@X312` settles it independently**: a compressed scale
   puts the mountains ten times closer than they are, and **a backdrop
   whose distances lie is worse than no backdrop.**
4. ⚠⚠ **What EARNS a view, and does the frame contain it?** (`@X312`).
   `@M064` measured the default frame at **0.96° short of the horizon**,
   which killed the battleship clock — so the earning has to bring its
   own frame.  ⚠ *Recommendation: a PLACE — height — because it is the
   one that needs no camera rule and is already what scouting is.*
3. **How much of a cell does a sortie see?**  Follows from 2, and it is
   what `ERRANDS.md` § The two scales priced at ~1 %.

## See also

- [`ERRANDS.md`](ERRANDS.md) — what a mob is doing on the ground this
  produces, and `@X298`'s two scales.
- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) — the graph laid onto the coarse
  map.
- `../crawler/OVERLAND.md`, `../crawler/src/overland.loft`,
  `../crawler/plans/1-ortler-worldgen-fixture/`,
  `../crawler/plans/2-chunked-lod-world/s6-subhex-finding.md`.
