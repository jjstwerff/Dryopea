<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `33` — Exploration finds: something out there worth the trip

**Value:** `G` · **Effort:** `MH`

## Status

**OPEN — E0 and E1 COMPLETE 2026-08-30 (`@M093`, `@M094`, `@X349`), E2 next.**

⚠⚠ **E0 ANSWERED ITS FALSIFIER AND FOUND A SHARPER RULE THAN THE ONE IT
WENT LOOKING FOR.**  Four `.keys` files, one base, defences held equal,
and the only thing that moves is how far from the core the defence sits:

| | ticks |
|---|---|
| `a-base-drawn-in-tight` — front ±5, towers ±4 | **170** |
| `a-base-drawn-out-wide` — front ±14, towers ±13 | **207** |
| `a-tight-base-with-no-towers` | **102** |
| `a-wide-base-with-no-towers` | **102** |

⚠⚠ **The racing line is REAL — +37 ticks, +22 %** — so § X2b's *the
sortie's product is a BUILD DECISION* has a number under it at last and
`PROGRESSION.md` § P7's *no scenario varies the layout while holding the
defences equal* is answered.

⚠⚠ **But the NULL is the finding**: the same nine hexes of outward
displacement — eight wall hexes and both crew with them — is worth
**0.0, 102 against 102 to the tick**, once nothing on the map shoots.
***What a layout is worth is not where the WALL is; it is how far from
the core the FIGHT happens, and a base with nothing that shoots has no
such distance to sell.***

⚠⚠ **And the towers were never the variable**: all four cells burn
**all 30 shots each**.  The sixty shots are identical and only their
geometry moved — read off the instrument rather than assumed, which is
`@M025`'s hazard sidestepped.  ⚠ The swept 2×2 separates the halves and
they are **additive**: front ±5 / towers ±13 and front ±14 / towers ±4
**both read 187**, exactly halfway.

⚠ **What E0 does NOT say** is which of the two layouts a player
should want: 207 > 170 on THIS base, with the spawn markers at ±24 and a
list that ramps.  A base whose approach is short, or whose crew must
shuttle between fronts on a repair clock, is the other half of § P7's
trade and it is **not measured here**.

### ⚠⚠ And the swept factorial found something bigger than the layout  `@M094`

Deleting the walls and the towers each in turn, over the same two bases:

| | tight | wide | layout worth |
|---|---|---|---|
| neither | **95** | **95** | **0** |
| wall only | **102** | **102** | **0** |
| towers only | **250** | **324** | **+74** |
| wall **and** towers | **170** | **207** | **+37** |

⚠⚠ **A WALL STANDING IN FRONT OF A TOWER COSTS MORE THAN IT BUYS**:
eight wall hexes with a gate are worth **+7** alone, two towers are
worth **+155 and +229** alone, and **adding the wall to a towered base
takes 80 and 117 ticks back off it**.

⚠⚠ **`plans/12` B7 already saw the shape** — *adding a tower cuts it
back to 95, its own dead ramp over the wall it defends* — ⚠ **and the
ramp is FALSIFIED here**: a `pile` sweep of thirteen hexes from (2, 0)
to (24, 0), the wall's own outside face included, reads **0.0 rubble at
every one of them**.

⚠ **The leading hypothesis is SIGHT and it is NOT gated.**  `tower_sees`
walks a line and a wall blocks it, so a tower one hex inside its own wall
can see almost nothing — and the 2×2 orders exactly by *how much open
ground a tower can see*.  ⚠⚠ **The decisive probe is named rather than
run**: `range` / `target` / `tower_sight_fault` are already in the
`.keys` vocabulary.  ⚠ It also does not contradict `@M050`'s **+44** for
a wall: **the wall's SIGN depends on whether something that shoots stands
behind it**, which is a claim about the PAIR.

⚠ **And the layout null survives it**: **0** in BOTH tower-free rows —
102 / 102 and 95 / 95.

### ⚠⚠ E1 — the find is a cargo row, and it can be SEEN  `@X349`

`object <q> <r> find <points> <owner>` is a `.keys` verb, delivering one at
the core credits the wallet by exactly `<points>`, and **anything lying on
the ground is drawn**.

⚠⚠ **Three open questions closed at once, and the third one decided the
other two.**

1. **What does a find buy?**  **Points** — `plans/31`'s wallet already turns
   them into towers and the compounding is measured (`@M087`), so a find pays
   in the one currency whose downstream value the repo has priced.  ⚠ The
   wallet had no income outside salvage until this (`plans/13` V3), so the
   credit goes through `wallet_earn` rather than a door of its own.
2. **One kind, or a kind with a payload?**  **One**, and `subj` is the points.
   ⚠⚠ **The TRIPWIRE is written into the constant** rather than left to be
   found: the day a find yields something that is not points — the DISH
   (`@X348`) — `subj` holds two facts and must SAY so, as a `find_pack` beside
   `errand_load_pack`.  ⚠ What a find is worth is AUTHORED, so **E2 sweeps a
   number rather than rebuilding a mechanism**.
3. **Does a find need a MARKER kind?**  **No** — and § The open problem is
   what actually answered it: `entity_view.loft` is *the ROSTER, as triangles*
   and a `CarryObject` was never in it, so **a beacon set down and a tower top
   on the ground were invisible**.  One catalogue row and one walk fixes all
   three at once, against ~96 files for a marker kind.

⚠⚠ **The walk filters on `CARGO_GONE` and nothing else, and that is the
invariant rather than a shortcut.**  `carry.loft`'s conservation is
structural — *on the ground* is a VALUE of `owner` rather than a different
place to be — so a renderer asking `owner == BLOCKER_NONE` would be a SECOND
rule about where cargo lives, in step with the ledger only while somebody
maintained it.  **Drawing every record that is not consumed makes the frame's
count and `cargo_count` the same number by construction.**

⚠ It also finishes `DESIGN.md` § Carry visibility, which was half built: the
canopy already said LOADED (`entity_emit_hover` reads `cargo_carrying`), the
object itself was not there.

⚠⚠ **ONE drawn class for all five cargo kinds.**  Nothing the player can DO
about a thing on the ground differs by kind — one key, one slot,
context-resolved — so a colour per kind would be four rows nothing reads.  ⚠
The split is earned by the first scenario in which telling a beacon from a
wreck at a glance CHANGES what the player drives to.

#### ⚠ Four mutations, four caught — and the colour was the near miss

| mutation | caught by |
|---|---|
| the arrival branch never fires | the wallet reads 200 where 350 was asked |
| the walk filters `owner == BLOCKER_NONE` | the ledger holds 2 and the frame draws 1 — at the **held** state and nowhere else |
| the READER cannot name `find` | six assertions, and the corpus round trip |
| the WRITER cannot name `find` | *the capture holds no such line* — ⚠ `@D007`'s exact shape, and `tests/scripts/a-find-worth-fetching.keys` is what makes `tests/18_s2` able to see it at all |

⚠⚠ **And the colour nearly repeated `@M043`.**  `PROXY_ART.md` gives a loot
drop `#ffd000` — *"off-palette, signals pick me up"* — and the wallet ramp
runs `#ffc000` amber to `#ff0000` red, so **gold sits 256 squared-RGB from the
HUD's own full-wallet colour where the lane's floor is 3000**.  ⚠ The entity
colour is PEACH `#ff9060` instead, and `tests/19_p7`'s 201-value sweep is what
would have caught it — the same instrument that moved the scout.
***The warm lane belongs to the wallet.***

⚠ **Gates**: `scripts/test.sh` **1843 green / 151 files** (12 new in
`tests/33_e1_the_find.loft`, and three pinned corpus counts re-baselined
71 → 72 scenarios and 69 → 70 footing files — ⚠ the 182 wall hexes did NOT
move, because the new fixture paints grass and nothing else, and two counts
that move independently are two counts worth reading).  `scripts/validate.sh`
**72 scripts / 1237 measurements** (1 new, 10 new; the other 1227 UNMOVED).
`scripts/rules.sh` **49 defined, 30 enforced in 156 sites**.

### Where the plan sits

[`ROADMAP.md`](../ROADMAP.md) § The recommended order
item **2** — *exploration finds (item 9)*, **9 before 8**, and the thing
§ THE SESSION IS THE GAP NOW says is what makes a session long: the game has
every verb in `DESIGN.md` § 2's pitch and **nowhere to go**.

⚠⚠ **The first phase of `EXPLORATION.md` § The order of work is already
DONE and it inverted the page** (`@M092`, 2026-08-30, no code written): a
stranded crew member is worth **+74 ticks taken in passing and +116 fetched
at wave three** against a base that lives **248** — so a sortie PAYS, that
page's own falsifier does not fire, and `@X024`'s *a find's value collapses
once you are busy* runs the OTHER WAY for a find that is a **BODY**.  The
sharper rule it left behind is what this plan is built against:

> **A find decays with lateness only to the extent that what it buys is
> PERMANENT.**

⚠ That rule has **one measured half**.  This plan owes the other one, and
E2 is where it is paid.

## Goal

A find is a thing on the map worth driving to: **one cargo row**, a
destination, a consequence for taking it, and the intel that survives the
base.  When this plan is complete a `.keys` scenario can author a find, the
player can see it, fetch it, and be attacked for having done so — and the
run carries what was learned.

## Anchors

- [`docs/EXPLORATION.md`](../../docs/EXPLORATION.md) §§ X2b, X2c, X4, X5,
  X7, X8 and § The order of work — **the design; this plan restates none of
  it**.  ⚠ §§ X2c and X2d were rewritten by `@M092` and are what the
  phases below are written against.
- [`docs/PROGRESSION.md`](../../docs/PROGRESSION.md) § P7 — the racing
  line, and *"no scenario varies the layout while holding the defences
  equal"*.  E0 is that scenario.
- [`plans/BACKLOG.md`](../BACKLOG.md) **E2** — the same measurement, owed
  from the other direction.
- `src/carry.loft` — the carry model.  ⚠ `plans/15` § C0.4's contract: a
  kind is **a constant, a destination rule and what arriving does**, and
  **nothing in the carrying path**.
- `src/emit.loft` + `src/script.loft` — ⚠⚠ **the writer and the reader are
  a PAIR** (`@D007`); a cargo kind added to one is invisible until a
  scenario drops one.

## ⚠⚠ The site count, taken BEFORE any code — and it moves the design

`plans/31` § N1 earned its plan by counting the re-assertion sites first
and finding a fourth nobody had named.  The same count here answers
§ X4's *"a find is ONE marker row and ONE cargo row"* with a **no** on the
first half:

| the row | sites | measured |
|---|---|---|
| **a CARGO kind** | **~5** | the constant, a `cargo_destination_ok` row (or a deliberate absence, as `CARGO_BEACON` and `CARGO_SALVAGE` both take), what arriving does at its call site, `emit.loft`'s name and `script.loft`'s `object` reader |
| **a MARKER kind** | **~96 files** | `markers.loft` (6), `editor_view`, `marker_render`, and the `do cycle_kind` press in **70 `.keys` files (266 presses)** and **23 `.loft` test files** |

⚠⚠ **`CLAUDE.md` SAID the marker cycle "grows in 47 places", and taking the
count is what found that stale** — the corpus grew from 33 fixtures to 70
under a number nobody re-read.  `@M044`'s rule with a new instrument:
**re-measure before quoting a delta.**  ⚠ Both routers now carry the
corrected figure.

⚠ **So this plan buys the cargo row and defers the marker row**, and the
reason is not the price: `@M092`'s three fixtures authored a find with
`object <q> <r> wreck …` and needed no marker at all.  A marker kind is
what an authored **`maps/*.json`** would need — `MapFile` + the marker
sidecar are map CONTENT and a `CarryObject` is runtime state — so it is
owed by [`plans/04`](../04-map-library/README.md)'s content half and not by
this one.  **Written down here because deferring it silently is how a
*the map cannot hold a find* surprise is bought later.**

## ⚠ The open problem E1 had to answer: a find nobody can SEE is not a find — ANSWERED

`entity_view.loft` is *the ROSTER, as triangles* and a `CarryObject` on the
ground is **not in it** — `@M092`'s wreck draws because the downed crew
member is still a roster body, not because the cargo is.  ⚠⚠ So a beacon
set down and a tower top on the ground are **invisible today**, and a find
would inherit that.  Two doors, priced in E1:

- **a marker kind** — `marker_render` already draws markers; ⚠ the ~96
  files above, and the deferral just argued;
- **ground cargo draws as an entity** — one catalogue row and one walk in
  `entity_view.loft`, ⚠ and it fixes the beacon and the top at the same
  time.

⚠⚠ **The second is what E1 took** (`@X349`), and it cost `cat_cargo` plus
`entity_bake_cargo` — one row and one walk, as priced.  § E1 above carries
the argument and the invariant the walk is written against.

## Invariant gate

⚠ **E0 and E2 have no exact-invariant surface** — they are measurements,
and their gate is a band in a `.keys` file.

| phase | concrete expected result | invariant | negative control |
|---|---|---|---|
| **E1** | `object 9 0 find 0 ground` round-trips to itself through `emit_keys` | the writer/reader PAIR is total (`@D007`) | ⚠ `object 9 0 fnid …` is **refused by name**, not read as a wreck |
| **E3** | taking a find at `h` makes `h` a spawn source; leaving it does not | the consequence is caused by the TAKE and nothing else | ⚠ a find driven PAST changes no spawn count |
| **E4** | a find delivered, then a scramble, opens the next base with the intel | `manifest_of` converts once (`@FR-R-Carry-Once`) | ⚠ a find **carried but not delivered** carries nothing |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **E0** — ⚠⚠ **the LAYOUT quartet**: same defences, same list, tight vs wide, ± towers | S | four `.keys` in `tests/scripts/` + `scripts/validate.sh` | ✅ **COMPLETE** 2026-08-30 (`@M093`) |
| **E1** — the find, as ONE cargo row (§ X4) | M | `tests/33_e1` + a round-trip fixture | ✅ **COMPLETE** 2026-08-30 (`@X349`) |
| **E2** — ⚠⚠ **what a PERMANENT find is worth**: `@M092`'s three-file design, re-run with a reward that does not die.  ⚠ E1 made it a **number in a `.keys` file**, so this is a SWEEP: `object <q> <r> find <points> ground` at three fetch ticks.  ⚠ The DISH (`@X348`) stays the richer column and is not built | S–M | three `.keys` + `scripts/validate.sh` | **Next** |
| **E3** — the consequence (§ X5): taking a find opens a fight | M | `tests/33_e3` + a scenario pair | Unblocked |
| **E4** — intel that persists (§ X8 layer 2) | M | `tests/33_e4` over `manifest_of` | Blocked on E3 |

⚠ **§ X8 layer 3 — wave composition as a readout — is deliberately NOT a
phase.**  It is free only once a wave's contents are a function of where it
came from, and `waves.json`'s composition is authored (`@M090`).  It is the
economy's (item 10), not this plan's.

## ⚠⚠ E0 — what it is for, and why it is first

⚠ **It is this plan's falsifier, and it is aimed at a DIFFERENT section
from phase 1's.**  `@M092` tested § X2c (*a find is a build accelerant*).
E0 tests § X2b — ***the sortie is RECONNAISSANCE and its product is a BUILD
DECISION***.  If two bases with **the same defences** arranged two ways play
the same, then intel converts into **nothing** and the whole of exploration
is a fetch quest.

⚠⚠ **Four confounds have to be held, and three of them are already
measured mistakes:**

1. ⚠⚠ **`@M020` — the siege front is the WALL's WIDTH.**  A spread base
   with a wider wall gets a wider front, which is a *wall* result wearing a
   *layout* costume.  **Hold the wall's face width equal.**
2. ⚠⚠ **Saturation** (`@M020`, `@M085`, `@M087`) — past a certain wave
   size every column reads the same.  **The list must RAMP**, exactly as
   `@M092`'s did.
3. ⚠ **Bracing** (`plans/14` H2) — a 99-tick artefact that read exactly
   like a finding.  `q -> -q` is not a symmetry of an odd-r lattice.
4. ⚠ **Footing** (`@M061`) — `footing_of` takes the sturdiest ground *in
   reach*, so brittleness is a property of a REGION.  **Paint one kind.**

⚠ And the generic control `CLAUDE.md` § Testing something that moves asks
for: **can this pair produce a non-trivial reading at all?**

⚠⚠ **All four were held, the control answered yes at 37 ticks — and the
NULL is what turned a confirmation into a rule.**  § Status carries the
numbers; `@M093` and `@M094` carry them for grepping.

## Open questions

1. **What does a find BUY?**  ⚠ `@M092` measured a body.  E2 needs a
   PERMANENT reward and the cheapest is **points**, because
   `plans/31`'s wallet already converts points into towers and the
   compounding is measured (`@M087`).  ⚠⚠ **The richer candidate arrived
   2026-08-30**: a **TOWER TYPE unlocked** — `DESIGN.md` § 7 § THE DISH
   (`@X348`), which § Future tower types already says is *found on the map
   through scouting*, and which `EXPLORATION.md` § X2c names as exactly the
   permanent reward `@M092` could not price.  ⚠ **Points are the cheap
   probe and the dish is the real one**, so E2 may want both columns.
   ⚠⚠ **RESOLVED 2026-08-30 (`@X349`): POINTS, with the dish's tripwire
   written into the constant.**  *E2 prices it, and it is a SWEEP.*
2. **Does a find need a MARKER kind?**  ⚠ Argued **no** above, for a
   `.keys` corpus.  ⚠⚠ **RESOLVED 2026-08-30 (`@X349`): NO — and the
   visibility question is exactly what decided it.**  Ground cargo draws as
   an ENTITY, which fixes the beacon and the tower top at the same time.
3. **Is `CARGO_FIND` one kind or a kind with a payload?**  ⚠
   `CARGO_SALVAGE` packs two facts into `subj` and NAMES the packing
   rather than overloading in silence — the precedent to follow if a find
   has a size.  ⚠⚠ **RESOLVED 2026-08-30 (`@X349`): ONE kind**, `subj` is
   the POINTS, and the packing is named the day a second yield exists.
