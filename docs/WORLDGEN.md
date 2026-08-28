<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Worldgen — deriving a scenario from a 1.5 km map

⚠⚠ **DESIGN, not built, and dryopea has NO procedural generation of any
kind today**: the three maps in `maps/` are authored `.keys` files built
into committed `.json` pairs, and there is no seed anywhere in `src/`.

> ⚠⚠ **THE THESIS, in the owner's words** (`@X323`): *"it all comes down
> to a way to **express detail based on a very compact base set of
> data**."*  ⚠ Every ruling below is an answer to that, and § THE THESIS
> reads them as one family — plus the refusal test it implies: **does
> this add data in proportion to the detail it produces?**
>
> ⚠⚠ **AND IT IS AN OLD DESIGN, AIMED AT A GAP** (`@X324`) — *"I am
> already designing this for a few years, because it hides a gap found in
> all modern games I observe."*  ⚠ Which is why **a piece dropped for
> convenience is a regression even when everything still works.**

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

## ⚠⚠ THE THESIS — express DETAIL from a very compact BASE SET  `@X323`

Owner, 2026-08-28:

> *"it all comes down to a way to **express detail based on a very
> compact base set of data**."*

⚠⚠ **That is what every ruling in this document is an answer to**, and
reading them as one family is worth more than reading them one at a
time:

| ruling | the compression | the rule that expands it |
|---|---|---|
| `@X313` six neighbours | **eighteen numbers** | slope, aspect and drainage — a cell alone has no gradient |
| `@X316` the block | a **row**, and a seed | the coarse map picks the table, the block picks the cell |
| `@X318` conditions | a **predicate**, not an index | *what I need*, so placement is never stored |
| `@X321` the residual | **one row** | ⚠⚠ the same block on a different slope is a different landform |
| `@X319` variants | ⚠ 6 rotations × 2 reflections **free** | detail with **no extra data at all** |
| `@X320` the edge contract | ⚠ nothing — it is a **restriction** | substitutability, bought by giving something up |
| `@X299` a mob's rule | **five anchors** | its whole life, at any time `t` |
| `@X302` `slip` | ⚠⚠ **one integer** | the closed form survives deviation |
| `@X300` the bound | one **region per POI** | a query where there was a scan |
| `@X306` the result | the **snapshot, changed** | a campaign, out of a few rows a sortie |
| `@X308` the given layer | ⚠ a **heightmap nobody authored** | a world the author does not know, at t = 0 |

> ⚠⚠ **Every one of them is a COMPRESSION with a rule for decompressing
> it — and every rule is LOCAL, DETERMINISTIC and COMMUTATIVE.**

⚠ Those three properties are not a coincidence either.  **Local** is what
lets a piece be produced without its neighbours being produced first;
**deterministic** is what lets it be thrown away and recomputed;
**commutative** is what lets two owners of a shared thing agree without
talking.  ⚠⚠ **Take any one away and the compression stops being usable**
— that is why `@X316`'s anchors, `@X320`'s edge ownership and `@X319`'s
position hash keep turning out to be the same requirement wearing
different hats.

### ⚠⚠ WHY IT IS AN OLD DESIGN — it is aimed at a GAP  `@X324`

Owner, 2026-08-28:

> *"and that is why it is an old design — I am already designing this for
> a few years, because it **hides a gap found in all modern games I
> observe**."*

⚠⚠ **This is the motivation, and recording it matters for a practical
reason: it is what makes the pieces non-optional.**  A design assembled
to solve today's problem can be simplified when today's problem changes;
a design aimed at a gap **loses its point the moment a piece is dropped
for convenience.**

⚠ **The repo carries the evidence of the age.**
[`DESIGN_HISTORY.md`](DESIGN_HISTORY.md) § 2 is the 2023 `todo` file, and
`@X313`'s six-neighbour rule is already in it in its original form —
*"sampling png onto hex grid: **specific point & 4 pixels around it**"* —
beside *"general slopes"* and *"water flow, amount of water"*.  ⚠⚠ **So
this is at minimum a three-year-old design, in writing, in this
repository**, and the coherence noted throughout these two documents is
because the parts were fitted rather than collected.

#### ⚠ What the gap is, as the design itself states it

⚠ The owner named the gap by its symptom rather than its cause, so what
follows is **this document's reading** of it, assembled from rulings they
did make:

| the design says | `@code` |
|---|---|
| believable behaviour, **without simulating a world** | `@X303` |
| detail, from **a very compact base set** | `@X323` |
| ⚠ **patterns must not become visible** | `@X319` |
| the same row must not read the same twice | `@X321` |
| a world **the author does not know** | `@X308` |
| ⚠⚠ *"I do not want to know what to find before I boot up the game"* | `@X224` |

⚠⚠ **Read together they describe a gap between two ways a modern game
can go wrong**, and the claim is that you need not choose:

| | it buys | it costs |
|---|---|---|
| **authored** content | specific, believable, meant | ⚠ finite, and **known** — by the author first and the player soon after |
| **generated** content | endless, cheap, surprising to the author | ⚠ shallow, **patterned**, and meaning nothing in particular |

> ⚠⚠ **The gap is that nobody gets BELIEVABLE DETAIL AT SCALE** — and
> this architecture is the claim that a compact base set plus local
> decompression rules gets you a world that is neither known nor shallow.

⚠ `@X224` is the sharpest existing statement of it, and it is the author
complaining about their **own** game: *if the seed is a number the author
chose then the author knows the world.*  ⚠⚠ **That is the gap seen from
inside**, and `@X308`'s author-free heightmap is one answer to it.

#### ⚠⚠ What it means for how this design is TREATED

⚠ Three practical consequences, and they are the reason this section is
here rather than in a preface:

1. ⚠⚠ **A piece dropped for convenience is a regression even when
   everything still works.**  `@X320`'s edge restriction buys nothing
   visible on its own; drop it and `@X319`'s dither becomes impossible.
   `@X321`'s residual looks like an implementation detail; drop it and
   the variant budget goes from a handful to thousands.
2. ⚠ **The design is not finished when it RUNS.**  `@X303` and `@X323`
   are both tests, not features, and a version that passes neither while
   producing terrain has produced the thing the gap is about.
3. ⚠⚠ **And it is why `@X322`'s library split matters more than usual**:
   a mechanism aimed at a gap is worth more than one game, and the
   several years are what make it plausible that the seams are in the
   right places.

⚠ It also explains an observation this document keeps making — that
`crawler`'s independently-built machinery keeps agreeing with rulings
made here (`@X299`, `@X301`, `@X306`, `@X316`).  ⚠⚠ **`@X314` already
gave the reason: it is the same design, in two places.**

### ⚠⚠ So it is also a REFUSAL TEST

> ⚠⚠ **DOES THIS ADD DATA IN PROPORTION TO THE DETAIL IT PRODUCES?**  If
> it does, it is the wrong mechanism.

⚠ Worked refusals, and each is already written somewhere in these two
documents:

| proposal | verdict |
|---|---|
| store the fine terrain a scenario generated | ❌ data ∝ output — `@X306` keeps the **delta** instead |
| a block as a **stamp** rather than a rule over anchors | ❌ — and `@X319` priced it: **2 908 variants** for a 10-hex block |
| node inventories ticking per cell | ❌ `@X298` puts the economy on the server; `@X303` refuses it as simulation nobody can see |
| a per-mob stored PATH | ❌ `@X302` — five anchors and an integer instead |
| a bespoke result format | ❌ `@X306` — *the result is the snapshot, changed* |
| a bigger palette, more `K_*` kinds | ⚠ `@X309` measured that one and answered **no**: *the 14 kinds SUFFICE* |

### ⚠⚠ And dryopea has been doing this since plan 01 without naming it

⚠ The thesis is not new to this repo — **it is the habit every shipped
system already has**, which is the strongest reason to trust it at a new
scale:

| system | the compact base | what it expresses |
|---|---|---|
| `painted.loft` | ⚠ sparse, **sea-default** — absence IS the sea | a whole world from what was painted |
| `height.loft` | metres of rubble | ⚠⚠ *a LAYER, never a repaint* — clearing restores exactly what was authored |
| `part.loft` + `catalogue.loft` | a part-**tree** | every entity's geometry, DERIVED |
| `pose.loft` | ⚠ nothing | *read the sim, never a second flag* |
| `entity_view.loft` | ⚠⚠ **nothing is STATE** | the roster walked, the triangles derived |
| `carry.loft` | one record with an **owner** | conservation, STRUCTURALLY |
| `mesh_chunks.loft` | the painted set **plus a ring** | the drawn region |
| `scramble.loft` | ⚠ three fields | what a sortie was worth (`@M068`) |

⚠⚠ **So `@X321`'s *rubble is a LAYER, never a repaint* at 1.5 km is not
an analogy — it is the same rule, and the repo has seven other instances
of it.**  ⚠ What is new here is only the scale, and `@X323` is the name
the habit never had.

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

## ⚠⚠ THE INHERITED SHAPE: THE BLOCK HOLDS THE DETAIL, THE BIG MAP SAYS WHETHER IT APPLIES  `@X316`

Owner, 2026-08-28:

> *"crawler inherits the biomes code from ZAngband, where **details are
> stored inside smaller-scale blocks** where **the bigger map gives the
> input if they apply locally**."*

⚠⚠ **This corrects the direction the rest of this document was reading
in.**  § The derivation is a sampler cascade describes `ov_sample`
pushing coarse values DOWN through interpolation, noise and a branch
cascade — and that is real, and `@X309` measured its ceiling: **62 %
agreement, *"the ceiling for elevation-only at 1.5 km"***.  ⚠ The
inherited model is the other half, and it is the half that does not
degrade with the scale ratio.

`crawler/OVERLAND.md:418` § The ZAngband graft, verbatim:

> *"ZAngband's wilderness gen already has this shape: **per-block seeded
> plasma fractal anchored at SHARED CORNER values** + **per-terrain
> LOOKUP TABLES** (fractal band → grass/tree/bush/rock/water) +
> **overlays**.  We adopt it as the micro layer, upgraded: **the anchors
> sample OUR field stack (the rough structure is followed by
> construction)**, the plasma residual is the per-type micro-roughness
> number, the tables are the inclusion tables … and our features
> rasterize over the result in **authority order**."*

### ⚠⚠ Three layers, and each answers a different question

| layer | question it answers | where it lives |
|---|---|---|
| **the coarse map** | ⚠⚠ **WHICH TABLE** — what kind of country is this? | 1.5 km cells |
| **the block's own fractal** | ⚠⚠ **WHICH CELL** — where within this block does each thing land? | the block, at its own resolution |
| **features / overlays** | ⚠ **WHAT OVERRIDES** — a river, a road, a site | world coordinates, in **authority order** |

> ⚠⚠ **THE COARSE MAP CHOOSES THE TABLE.  THE BLOCK CHOOSES THE CELL.
> FEATURES OVERWRITE, IN ORDER.**

⚠ So a mountain is not interpolated into existence.  **The coarse map
says *this is mountain country*, and the block then places
mountain-country detail at its own scale** — which is why the model does
not care that one cell is a million fine ones.  ⚠⚠ **The ratio never has
to be crossed by interpolation at all.**

### ⚠⚠ THE WORKED EXAMPLE: a steep mountain side  `@X318`

Owner, 2026-08-28:

> *"for example on a steep mountain side we have a **limited set of
> blocks that are possible** — with rock faces on them and with steep
> terrain — **some of these blocks hold a river or a waterfall**."*

⚠⚠ **This is *the bigger map gives the input if they apply locally* made
operational**, and it is the shape to build against.  The coarse cell
does not describe the ground; **it supplies predicates that ADMIT
blocks**:

| coarse input | where it comes from | what it admits |
|---|---|---|
| elevation band | the cell, and `@X309`'s four tuned numbers | the alpine / rock set |
| **slope** | ⚠ **the six neighbours** (`@X313`) — a cell alone has none | blocks carrying **rock faces** |
| **flow present?** | ⚠ `@X313`'s third field, still missing in dryopea | the subset that **holds a river** |
| **flow + a big drop along it** | flow, plus the elevation difference to the downstream neighbour | ⚠⚠ the subset that holds a **WATERFALL** |

⚠ So *steep mountain side* is not one answer.  It is **a small
admissible set**, and which member lands here is decided by the block's
own seed — `@X316`'s *the coarse map chooses the table, the block chooses
the cell*.

#### ⚠⚠ A BLOCK DECLARES WHAT IT NEEDS, NEVER WHERE IT GOES

⚠ That is the trait-seam pattern this ecosystem already uses twice.
`crawler/src/monsters.loft:26-32`:

> *"**The engine never asks for a monster BY KEY — it asks the merged
> catalog for one that FITS A PLACE.**"*

⚠ And [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The governing rule says
the same of installations: *an installation costs one ROW*.  ⚠⚠ **A
block is a row too**: conditions on one side, contents on the other, and
**no placement code anywhere**.

#### ⚠⚠ And dryopea's PALETTE IS ALREADY THE OUTPUT VOCABULARY

⚠ The example needs four kinds and **the palette has had all four since
plan 01**, with the numbers already priced:

| kind | `slope` | `drop` | what it does today |
|---|---|---|---|
| `rock` | **20** | — | walkable, and `@X284`'s sturdiest footing |
| `steep_rock` | **40** | — | ⚠⚠ `walk_ground: false` — **the cliff**, and `@X286` corrected it to refuse vehicles too |
| `rapids` | — | **3** | a trench a 3.0 m boost can just leave |
| `waterfall` | — | **8** | ⚠⚠ *"a hole nothing gets out of"* (`@X286`) |

⚠⚠ **So a waterfall block is terrain with real teeth and nothing has
ever placed one.**  `@X286` priced the palette's 0-1-3-8 **against the
boost** — 3.0 m clears `rapids` and never clears a `waterfall` — and
`@X282` gave the drop its job at the waterline.  ⚠ Both were reasoning
about a *player-dug trench*; the same numbers make an authored waterfall
a hazard the player cannot undo, which is the strongest thing in the
palette and is currently unused.

#### ⚠⚠ A BLOCK CONTRIBUTES A RESIDUAL, NEVER AN ABSOLUTE  `@X321`

Owner, 2026-08-28:

> *"combine the inner block details like rock-faces with the heights
> provided by the 1.5 km map and we get **more interesting and realistic
> shapes**."*

⚠⚠ **This is the composition rule, and it is what makes the whole
catalogue work rather than look pasted on.**  A block's detail is not
laid down flat — it is **added to the coarse field**:

```
height(fine hex) = coarse_field(from the six neighbours)   # the large form
                 + block_residual(this block, its seed)     # the local structure
```

⚠ `@X316` already states half of it — *"the anchors sample OUR field
stack, **the rough structure is followed by construction**"* — and
`crawler` names the other half in one word: *"**the plasma residual** is
the per-type micro-roughness number."*  ⚠⚠ **Residual.  Not height.**

##### ⚠⚠ It is why the shapes read as real

⚠ Real ground **is** a large form with local structure on it: a massif
leans one way over kilometres, and within that lean there are faces,
ledges and gullies.  ⚠⚠ Neither layer alone produces that —

| | alone | composed |
|---|---|---|
| the coarse field | a smooth ramp; `@X309` measured the ceiling at **62 %** | ⚠⚠ the ramp **with structure on it** |
| the block detail | a face floating on nothing, identical everywhere | a face **lying along the slope it belongs to** |

##### ⚠⚠ AND IT IS THE BEST ANSWER TO `@X319`'s PATTERN PROBLEM

> ⚠⚠ **The same block on a different slope is a different landform.**  A
> rock-face block on a gentle grade is a **low outcrop**; the identical
> row on a 40° flank is a **cliff band**.

⚠ So **one row has many appearances, for free**, and the variant budget
§ EVERY COMMON CASE computes is multiplied by however much the coarse
field varies underneath — which is continuous.  ⚠⚠ **That, not the
variant count, is what actually defeats visible tiling**, and it is why
`@X319`'s birthday arithmetic is the pessimistic bound rather than the
requirement.

##### ⚠ And it satisfies `@X320`'s edge contract by CONSTRUCTION

⚠⚠ **A residual that tapers to zero at the block's edges leaves the
coarse field untouched there** — so two neighbouring blocks agree at
their shared band without either knowing about the other, and § AND THE
EDGES ARE NOT THE BLOCK'S TO VARY is met by arithmetic instead of by a
rule.  ⚠ Same trick as `@X316`'s anchors, one layer down: **the property
is built in, so there is nothing to check.**

##### ⚠ The honest limit: a residual may not FIGHT the field

⚠ A block that adds thirty metres of face onto ground the coarse map
calls valley floor produces a mountain in a meadow.  ⚠⚠ **So the
admission predicate and the residual must agree** — which is exactly why
`@X318` has a block **declare what it needs**: the conditions are not a
filing system, they are **the guarantee that the residual lands on ground
it makes sense on.**

⚠ `crawler`'s frozen intent doc states the failure to avoid, and it is
the same one: *"rivers run the low ground … **no water on
ridgelines**"* (`plans/1-ortler-worldgen-fixture/intent.md`, P4).

##### ⚠⚠ dryopea already composes exactly this way, one scale down

⚠ `height.loft` is a **runtime layer of metres over an authored ground**,
and `hex_height` is the sum — which is the same equation:

| | base | residual | who reads the sum |
|---|---|---|---|
| **today**, at 1.5 m | the painted kind's height | the rubble layer's metres | `@X284`'s footing, `@X282`'s waterline, every passability check |
| **this**, at 1.5 km | the coarse field | the block's detail | the same functions, unchanged |

⚠⚠ **So the composition is not a new concept for dryopea — it is the
rubble layer's rule at a different scale**, and `CLAUDE.md`'s *rubble is
a LAYER, never a repaint* is the same sentence.  ⚠ It also means
`@X311`'s talus is the **process form** of this rule: bedrock plus
rubble, relaxed until the rubble holds — *coarse field plus residual*,
gated on the residual being conserved.

#### ⚠⚠ EVERY COMMON CASE NEEDS SEVERAL BLOCKS — the budget goes to the BORING one  `@X319`

Owner, 2026-08-28:

> *"I want to have a choice of multiple blocks for **all common cases**
> so we can **dither** the terrain with that even when the world-map is
> not very interesting, so we prevent the common problem where
> **patterns get visible**."*

⚠⚠ **This inverts the instinct, and that is why it needs writing down.**
The tempting way to spend an authoring budget is on the dramatic cases —
the waterfall, the cliff band, the gorge.  ⚠ But those appear **rarely**,
and **repetition is only visible in what appears OFTEN**:

> ⚠⚠ **The block budget belongs to the DULL cases.  A gorge seen once is
> a gorge; a grass shelf seen forty times is a pattern.**

##### ⚠ How many — the honest arithmetic, and then why it is not that bad

⚠ Treat a block as a **stamp** and it is a birthday problem: with `k`
blocks co-visible, an exact repeat becomes likely at
`n ≈ k(k−1) / 2 ln 2` variants.  At dryopea's haze radius of 40 hexes
(**104 m** across the visible disc):

| block | across | co-visible | variants for a coin-flip chance of no repeat | ⚠ ÷12 orientations |
|---|---|---|---|---|
| 10 hexes | 13.0 m | ~64 | **2 908** | 242 |
| 20 hexes | 26.0 m | ~16 | **173** | 14 |
| 41 hexes | 53.3 m | ~4 | **9** | ⚠ **1** |

⚠⚠ **So block SIZE is the lever and it trades quadratically** — halving
the block quadruples the co-visible count and multiplies the variants
needed by about sixteen.  ⚠ A hex lattice gives 6 rotations × 2
reflections **free**, which is the ÷12 column.

##### ⚠⚠ But a block is NOT a stamp, and that is the whole reason this works

⚠ The table above is the pessimistic reading, and `@X316` already
refuses it: *"per-block seeded plasma fractal **anchored at shared corner
values**"* means **a block is a RULE APPLIED TO ANCHORS, not a picture
laid down.**

> ⚠⚠ **Two instances of the same block on different corner heights are
> already different ground.**  What can repeat is the *arrangement*, not
> the geometry — so a handful of variants per common case, times twelve
> orientations, over continuously varying anchors, is the real budget.

⚠ It is the same property `@X299` relies on for mobs: **the thing is
generated from its context, so context does the varying for free.**

##### ⚠ And the dither must be a POSITION HASH, never a stream

⚠⚠ dryopea gates drawn output — 16 goldens, `mesh_crc`, and 920 scenario
measurements — so **terrain has to be reproducible or the gates die**.
⚠ A variant chosen from an RNG *stream* depends on how many draws came
before it; one chosen by hashing the block's coordinate does not.
⚠⚠ **crawler has the bug this prevents, and it is undocumented there**:
its `gseed` passes literal `1, 1` where the window coordinates belong,
so **every window draws the identical number sequence** and only the
terrain tests differ.

#### ⚠⚠ AND THE EDGES ARE NOT THE BLOCK'S TO VARY — which is what makes variants substitutable  `@X320`

Owner, 2026-08-28:

> *"and the side of blocks get features from multiple blocks to prevent
> visible seams."*

⚠⚠ **This is `@X316`'s ownership law extended from HEIGHTS to
FEATURES**, and it is the property that makes § EVERY COMMON CASE
possible at all:

> *"every feature is owned by the lattice element **all its observers
> share** — maps CONSUME contracts, never regenerate them."*

| zone | owned by | who contributes |
|---|---|---|
| a block's **interior** | ⚠ the block | itself alone — **and this is the only part a variant may vary** |
| a block's **edge band** | ⚠⚠ **the EDGE**, shared by two blocks | ⚠ **both**, and both read the same answer |
| a **corner** | the corner, shared by three | all three |

⚠⚠ **Read it as the reason rather than the technique.**  If an edge band
belonged to one block, swapping that block for a variant would move the
seam and **break its neighbour** — so variants could not be substituted
at all, and the dither above would be impossible.  ⚠ Because the edge
owns its own band, **every member of an admissible set is interchangeable
by construction.**

> ⚠⚠ **A VARIANT MAY DIFFER ONLY IN ITS INTERIOR.  ITS EDGES ARE NOT ITS
> TO VARY** — and that single restriction buys both the seamlessness and
> the substitutability.

⚠ It is not *blend the two afterwards*, which would be a second
computation of a shared thing and is exactly what the law forbids — and
what `@X285` made `VIEW_PPM` private to prevent one system over.  ⚠⚠ It
is **one owner, both neighbours reading**, which is why it is
commutative for free and needs no check.

⚠ And it compounds with the dither: a seam that carries features from
both sides is a seam that does not announce where one block ended, so
**the arrangement blurs as well as the geometry.**

#### ⚠⚠ THE STITCHING RULE: a block declares its EDGES, and flow picks them

⚠ A river-bearing block has one more obligation, and it is `@X316`'s
anchors applied to features rather than to heights:

> ⚠⚠ **A river must ENTER and LEAVE where its neighbours' rivers do.**

⚠ So a block that holds a river is indexed not only by its conditions but
by **which edge the water comes in and which it goes out** — and the
coarse **flow direction is exactly what picks that pair**.  ⚠⚠ Which
makes the catalogue an **edge-matching set**: elaborate content that
still stitches, because the matching is done by a field both neighbours
read rather than by a check after the fact.

⚠ And a waterfall is then the special case the flow already describes —
**the block where the outgoing edge drops further than a block can
absorb**, so the catalogue needs one row rather than a rule.

⚠⚠ **This is also where `@X313`'s measured trap bites hardest and is
answered**: a neighbourhood rule *over-produces lakes 12×* because a
1.5 km basin is not a lake.  ⚠ A block catalogue does not have that
failure mode — **it never invents standing water; it only places the
member whose edges the flow selected.**

### ⚠⚠ And the anchors are `@X313`, which is what makes it CORRECT

*"per-block seeded plasma fractal **anchored at SHARED CORNER values**"*
and *"**the rough structure is followed by construction**"* are the two
halves of one property:

- ⚠ a block's fractal is pinned at the values its neighbours also see, so
  **adjacent blocks cannot disagree** — the commutativity of `@X313`
  and `@X314`, one layer down;
- ⚠⚠ and because the anchors come from the coarse field stack, **the
  block physically cannot contradict the big map**.  Not *checked* —
  **constructed**.

⚠ That is the same reason `@X299`'s rule-not-state works for mobs and
`@X311`'s talus is gated on conservation rather than accuracy: **the
property is built in, so there is nothing to verify at runtime.**

### ⚠⚠ THE LAW: a feature is owned by the lattice element all its observers share

`crawler/OVERLAND.md:452` § THE OWNERSHIP CONTRACTS, and it is stated as
a law rather than a technique:

> *"**No feature ever sits ON exact lattice geometry, and every feature
> is owned by the lattice element all its observers share** — maps
> CONSUME contracts, never regenerate them."*

⚠⚠ **This is the generalisation of `@X314`'s corner rule**, and it says
why that rule takes the form it does: a corner is shared by three hexes,
so *the corner* owns its height and all three consume it.  ⚠ An edge is
shared by two, so the edge owns the river crossing.  A hex centre is
shared by nobody, so it owns the valley floor.

⚠ **And *maps CONSUME contracts, never regenerate them* is the operative
half**: two observers must never each compute the same shared thing —
one owner, everybody else reads.  ⚠⚠ dryopea has been bitten by exactly
this class already: `@X285` made `VIEW_PPM` **private** because *a test
cannot stop the next caller reaching for a base scale that looks like the
answer*, and `compare.loft`'s hand-maintained field list is the same
hazard unfixed.

### ⚠ What it changes about § WHERE SCENARIO-SCALE DETAIL COMES FROM

⚠ The three sources below stand, and this sharpens the first one:

| | as written below | as `@X316` corrects it |
|---|---|---|
| **FIELDS** | *"continuous, sampled per fine hex from the coarse neighbourhood"* | ⚠⚠ **a PREDICATE that selects a table**, not a value drawn down to a hex |
| **FEATURES** | world-coordinate records | unchanged — and *"rasterize in **authority order**"* is the missing detail |
| **PROCESSES** | run at fine scale over the shape | unchanged, and `@X311` is one |

⚠⚠ **And it adds a fourth thing this document did not have: the BLOCK
itself** — a unit of authored-or-parametric content, at its own scale,
that the coarse map merely *admits*.  ⚠ `crawler` sizes it at the dual
lattice's triangle (three mutually adjacent hex centres, *"at 1.5 km /
C=4 ≈ 27k walked cells ≈ two Angband levels"*), and dryopea's equivalent
is the scenario itself.

## ⚠⚠ WHERE SCENARIO-SCALE DETAIL COMES FROM — trees, mines, roads, rivers, coast, cliffs  `@X315`

Owner, 2026-08-28:

> *"but the ortler map is in a big scale, we want to use that on a
> smaller scenario scale too — are there trees, mines, roads, rivers,
> coast, cliffs etc?"*

### ⚠ First, what the Ortler data actually CONTAINS

⚠ Read off the fetcher's own Overpass query
(`plans/1-ortler-worldgen-fixture/ortler_import.py:294-298`) and the
stored legend (`src/realworld/region.loft:25-38`):

| | in the data? | how |
|---|---|---|
| **trees** | ✅ | `natural=wood`, `landuse=forest` → `OSM_WOOD` |
| **rivers** | ✅ | `waterway=river\|stream` → a per-hex hit mask, with the DIRECTION taken from steepest descent on the pit-filled DEM rather than from the OSM geometry |
| **cliffs** | ⚠ **fetched and then LOST** | `natural=cliff\|bare_rock` are queried, but the legend folds them into `OSM_ROCK` — and *"cliffs are **not resolvable at 1.5 km**"* is the fixture's own verdict |
| **coast** | ❌ | the Ortler is **inland** — 568 to 3835 m, no sea in the bbox.  `OSM_WATER` is lakes and reservoirs |
| **mines** | ❌ | not queried at all |
| **roads** | ❌ | ⚠ `highway` is **absent from the query** |

> ⚠⚠ **So the fixture answers TERRAIN questions and cannot answer
> INFRASTRUCTURE ones** — which is not an oversight, it is the
> FIELDS/FEATURES split showing through: landcover is a field, and roads
> and mines are features that `crawler` **places** rather than imports.

### ⚠⚠ And the general answer: THREE sources, and the size decides which

⚠ Everything the owner listed comes from one of three places, and
knowing which is the whole of this section:

| source | what it is | what it gives | scale it works at |
|---|---|---|---|
| **1. FIELDS** | continuous, sampled per fine hex from the coarse neighbourhood (`@X313`) | elevation, slope, wetness, a ground KIND | ⚠ **coarse only** |
| **2. FEATURES** | discrete **world-coordinate records** on the coarse map, stamped into whichever window holds them | rivers, roads, and dryopea's **POIs** (`@X301`) | ⚠ exact, at any scale |
| **3. PROCESSES** | run at FINE scale over the shape the fields gave | ⚠⚠ what neither of the others can — **cliffs and scree** (`@X311`) | ⚠ fine only |

⚠⚠ **THE TEST, and it is a measured one: anything smaller than ~350 m
cannot be read off the coarse data.**  That is the Ortler mesh's own
sub-hex spacing (`@X314`), and below it the fixture found no signal.  So:

| | its size | its source |
|---|---|---|
| **coast** | a shoreline, kilometres long | ⚠ **1** — a threshold on height, plus a beach band on `(height, wetness, slope)` |
| **rivers** | metres wide, kilometres long | ⚠ **2** — a curved course with a **width from flow ACCUMULATION**, not a painted line |
| **roads** | metres wide, kilometres long | ⚠ **2** — routed over the coarse cells, then *"within 9 m of the polyline"* at fine scale |
| **trees** | ⚠⚠ **a dryopea stem is TEN HEXES — 13 m** | ⚠ **2**, and the field only says HOW MANY |
| **mines** | one hex, at a rock face | ⚠ **2** — a POI, sited at scenario scale by a scan (`@X301`) |
| **cliffs** | 15 m | ⚠⚠ **3** — the fixture proved 1 cannot, and `@X311` is what replaced it |

> ⚠⚠ **THE COARSE MAP DECIDES *WHERE*.  IT NEVER DECIDES *WHAT* AT THE
> SCALE YOU PLAY AT.**

### ⚠ FIELD → DENSITY, FEATURE → INSTANCE

⚠⚠ **The relationship between the two channels, and it resolves the tree
question exactly.**  `DESIGN.md` § Trees as terrain makes a dryopea tree
a **piece of terrain ten hexes wide**, with a position and a spread —
so a tree is unambiguously a **feature**, an instance with coordinates.

⚠ What the coarse map contributes is not the tree.  `OSM_WOOD` on a
1.5 km cell says **there are trees here, this many** — a *density* — and
the placer turns a density into instances at scenario scale, sited by the
same rules `@X301` needs for a POI (§ Siting).

⚠ Same shape everywhere: `OSM_SCREE` is *how much loose rock*, and
`@X311`'s talus decides where each patch of it lands; the coarse
`material` is *what kind of country*, and the fine cascade decides which
of the palette's kinds each hex is.

### ⚠ What this means for dryopea, concretely

- ⚠ **Nothing in the palette needs to change.**  The eleven painted kinds
  plus `rubble` are the FIELD's vocabulary, and `GROUND_TYPES.md` already
  has `slope` and `drop` columns that `@X284` and `@X282` read.
- ⚠⚠ **The FEATURES channel does not exist at all**, and it is the same
  gap `@X301` names from the other end: dryopea has MARKERS (four kinds,
  authored) and no world-coordinate feature list.  ⚠ A POI is that list.
- ⚠⚠ **And FLOW is still the missing field** (`@X313`).  Rivers get their
  width from accumulation and roads ford where accumulation is low, so
  **two of the six things the owner listed are downstream of the one
  number dryopea has never computed.**
- ⚠ **Cliffs are a process, and dryopea half-has it**: `height.loft` is
  already a runtime layer of metres over an authored ground, and
  `@X311`'s talus is that layer with an angle of repose and a
  conservation gate.

## ⚠⚠ LAND IN THE OVERLAP — the placement rule, and it is where the choice comes from  `@X317`

Owner, 2026-08-28:

> *"so we can work with a limited set of interesting areas that get used
> in the scenario, **preferably with putting the player in the overlap
> region of several of them to give them a choice**."*

⚠⚠ **This is the rule the whole design was missing, and it closes
`@X305` from the other end.**  `@X305` says *two to four POIs, each one
load-bearing, and the count should exceed what one sortie can act on*.
This says **where the base goes relative to them**:

> ⚠⚠ **A base lands where several areas' influence OVERLAPS — because
> the overlap is what makes a choice exist.**

⚠ One area in reach is not a decision, it is a task list.  Several in
reach, with time for fewer than all of them, is `@X197`'s assignment
pillar **at the sortie scale** — *always more tasks than there are
helpers* read one level up.

### ⚠ Each area has an INFLUENCE, and the influence is `@X300`'s bound

⚠⚠ **No new geometry is needed**, which is the sign it is the right
rule.  `@X300` already gives every POI a **static bound** — the region
its cycles can reach — indexed once for the sortie.  ⚠ Landing in the
overlap is exactly:

> **the base's window intersects the bounds of several POIs at once.**

⚠ So the same number that culls a population also decides whether a
landing site is any good, and *"which edges the base happens to sit
on"* (`ROBOT_ECONOMY.md` § The graph) becomes *how many bounds contain
me*.

⚠⚠ **And the unit of influence is a TRIP, not a radius.**  The scrambler
bubble is 25 hexes — **32.5 m** — and a sortie drives much further than
that, so *in play* means *the player can get there and back inside the
sortie*, which is the same currency `@X305` prices a POI in and
[`plans/17`](../plans/17-tower-hot-swap/README.md) § T3 measured the crew
in.

### ⚠⚠ It gives `DESIGN.md` § 15's landing pick a MEANING it does not have

⚠ § 15 step 2 already lets the player *"click ANY hex on the selected
map"*, and today that decides only what ground they get.  ⚠⚠ Under this
rule **the pick is choosing which overlap to sit in** — and that is the
sortie-scale version of `@X019`'s *the base layout is the exam*, one
level up: **the layout exam starts before you land.**

⚠ It also gives `@X312`'s earned view something to be FOR: a horizon
from a height is how a player reads the overlap structure of a
neighbourhood they have not landed in yet.  ⚠⚠ **Scouting, a view, and
the landing pick are one loop**, and none of them needed inventing.

### ⚠⚠ The anti-pattern is MEASURED, which is what makes this a rule

⚠ A base that overlaps **nothing** is not merely quiet — dryopea has
already measured what it is worth.  `@M058`: a trench that sealed a base
so completely that it *"still stands at 378"* with **thirteen robots
alive and zero targets**, on exactly the opening 200 points:

> *"A wave that cannot reach you cannot die, and salvage is the only
> income."*

⚠⚠ **A landing site with no overlap is that outcome by geography instead
of by masonry** — nothing arrives, nothing is worth doing, and the run
is a stalemate the player cannot even lose.  ⚠ So the overlap
requirement is not a nicety; **it is what stops a legal landing site
from being an unplayable one.**

### ⚠ The gate, in dryopea's own currency

⚠⚠ `@X305` gates a POI by *removing it moves the clock*.  The placement
version is a scenario pair one token apart, and the token is the landing
hex:

> ⚠⚠ **A landing site earns its place if MOVING THE BASE OUT OF THE
> OVERLAP changes which POI the player deals with.**

⚠ If the same thing happens wherever you land, the overlap was
decoration.  ⚠ And the reading is the one this repo already knows how to
take — `@M050`'s 130 / 174, `@M070`'s 140 / 174 — a pair of scenarios
differing in one authored value.

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

## ⚠⚠ THIS IS LIBRARY WORK — three layers, and only the middle one is dryopea's  `@X322`

Owner, 2026-08-28:

> *"be aware that most of the routines we write here should be **generic
> enough to be implemented by multiple games**, so I want **libraries
> with their implementations** and **possible scripts for edge cases that
> can be provided by individual games**."*

⚠ It is the rule `CLAUDE.md` § Loft consumer relationship already keeps
in one direction — *reuse is the rule; do not write a local version of
what a library provides* — pointed the other way, which is what
[`plans/10`](../plans/10-extract-local-libraries/README.md) exists for:
*"the code dryopea wrote that is **not** game-specific becomes a
published library, so the next project does not write it a third time."*

### ⚠⚠ The split, and almost everything in this document is on the library side

| layer | who owns it | what it is | from this document |
|---|---|---|---|
| **1. the LIBRARY** | ⚠⚠ nobody's game | the mechanism, with no policy in it | the six-neighbour derivation (`@X313`), the ownership law (`@X316`), residual composition (`@X321`), the bound/cull tiers (`@X300`), edge-matching (`@X318`), the closed form and `slip` (`@X302`) |
| **2. the CATALOGUE** | ⚠ **the game**, declaratively | rows: conditions → contents | dryopea's palette, its block rows, its POI kinds, its errand roles |
| **3. the SCRIPT** | ⚠ **the game**, imperatively | the edge cases a table cannot say | dryopea's `.keys`; `crawler`'s bundles |

> ⚠⚠ **The library never asks for a thing BY NAME.  It asks the
> catalogue for one that FITS.**

⚠ That sentence is not new here — it is `crawler/src/monsters.loft:26`
verbatim (*"the engine never asks for a monster BY KEY — it asks the
merged catalog for one that FITS A PLACE"*), it is
[`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) § The governing rule
(*an installation costs one ROW*), and it is `@X318`'s *a block declares
what it needs, never where it goes*.  ⚠⚠ **Three systems, one seam,
arrived at independently — which is the evidence it is the right one.**

### ⚠ Layer 3 exists already, in both repos, and it is what the owner means

⚠ `crawler` runs the imperative layer as an **overlay the engine
executes**: `RoomStencil` / `Placement` in `src/worldtypes.loft`, an
authored instance in `bundles/desert_surprise/`, and the engine's own
comment — *"The active overlay … **decides**; we **stamp**. …
Deterministic, so re-entry regenerates identically."*  ⚠ dryopea's
equivalent is `script.loft` and the 50 `.keys` files, which are
simultaneously the gate and the worked example
([`plans/08`](../plans/08-game-validation/README.md)).

⚠⚠ **So the seam to design for is: the library reads a catalogue, and a
script may OVERRIDE any of it in authority order.**  ⚠ Authority order is
`@X316`'s word and it belongs to layer 1 — the library decides *how* an
override composes, the game decides *what* overrides.

### ⚠⚠ But extract on the SECOND consumer, not the first

⚠ [`plans/10`](../plans/10-extract-local-libraries/README.md) already sets
the bar and it is the right one — *"extract what SURVIVES"* — and this
document must not be read as licence to publish a library shaped by one
game.  ⚠⚠ **A library with one consumer is a refactor with a version
number.**

⚠ The good news is that the second consumer is **real and present**:
`crawler` has the ZAngband micro layer, the Ortler pipeline, the trimesh
and the hydrology **already built**, and `@X314` says parts of it are
dryopea's design to begin with.  ⚠⚠ **So the honest sequence is: build
the seams as library seams from day one, land the first implementation
wherever it is cheapest to gate, and publish when the second consumer has
read the API and disagreed with it.**

### ⚠ Naming, which `plans/10` already settled

⚠⚠ **Descriptive, never a brand** — *no `moros_*`, and equally no
`dryopea_*`* — and the family is already established in the registry:
`hex_grid`, `hex_field`, `hex_body`, `hex_world`, `hex_terrain`,
`hex_way`, `hex_edge`, `hex_fit`.  ⚠ What this document describes reads
naturally as more of the same shape, and `hex_terrain` and `hex_world`
are the two an implementer should read **before** proposing a new one:
the rule is `CLAUDE.md`'s — *do not write a local version of a routine a
library already provides*, and **that applies to this design more than
to anything else in the repo**, because none of it is written yet.

⚠ **And one thing here is emphatically NOT library work**: `@X303`'s
test, `@X305`'s two-to-four, `@X317`'s land-in-the-overlap and `@X312`'s
earned view are **dryopea's design decisions**.  ⚠⚠ A library that
enforced *two to four POIs* would be a game wearing a library's name —
**the library supplies the bound and the cull; the number is the game's.**

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
5. ⚠⚠ **Is the landing pick the PLAYER's or the game's?** (`@X317`).
   `DESIGN.md` § 15 has the player clicking any hex, and this rule gives
   that click a meaning — but a player cannot see the overlap structure
   before they land unless something shows it.  ⚠ *Recommendation: the
   player picks, and `@X312`'s earned view plus `EXPLORATION.md`'s
   scouting are how they learn what they are picking between* — which
   makes the FIRST sortie into a neighbourhood a blind one, deliberately.

## See also

- [`ERRANDS.md`](ERRANDS.md) — what a mob is doing on the ground this
  produces, and `@X298`'s two scales.
- [`ROBOT_ECONOMY.md`](ROBOT_ECONOMY.md) — the graph laid onto the coarse
  map.
- `../crawler/OVERLAND.md`, `../crawler/src/overland.loft`,
  `../crawler/plans/1-ortler-worldgen-fixture/`,
  `../crawler/plans/2-chunked-lod-world/s6-subhex-finding.md`.
