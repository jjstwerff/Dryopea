<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 24 — The siege front: a besieger spreads ALONG the wall face

**Value:** `G` · **Effort:** `MH`

## Status

**W0 + W1 + W2 done** (2026-08-17) — the plan is **complete**.  Suite
**1162** green, gate **33 scripts / 654 measurements**.

⚠⚠ **`@M019` — the rule five documents asked for was one dryopea
already had.**  An enemy attacked only when it could not WALK, and a
desire ring has ONE minimum on a straight face, so exactly three hexes
could never walk — **for any wall length**.  All five face hexes touch
the wall and two of them walked away from it.  § W0.

⚠⚠ **`@M020` — one precedence, and `@M018` is retired.**  *Arriving
beats queueing*: a besieger attacks the wall hex it is TOUCHING.  The
front went **3 → 4** hexes on a five-row wall and **3 → 6** on a
seven-row one, and the wave that could never take the base now takes it
at 126:

| wave of twelve | was | now |
|---|---|---|
| 12 miner | 94 | 94 |
| 4 builder + 8 miner | 104 | 101 |
| 4 robot + 8 miner | 119 | 116 |
| 4 harvester + 8 miner | 164 | 122 |
| 4 scout + 8 miner | **never** | **126** |

⚠⚠ **The replacement headline: a wave is worth its front class PLUS
whatever the front class cannot COVER.**  Four screens against a
five-hex face leak exactly one miner, and the leak is worth nothing
against a hard-biting screen (builder 101 vs pure 100) and thirty-nine
ticks against a soft one (harvester 122 vs pure 161).  **The screen is
arithmetic — bodies against face width** — where it used to be
positional immunity.

⚠ **A wider front makes most bases last LONGER**, which is the half
nobody would predict: a besieger that stops at the wall is one that is
not walking on to stand on the core, and the wallet is drained by
nibblers rather than by wall damage.  `a-base-on-two-fronts` went
**123 → 132**; `@M005` went 321 → **320**.

[`ROADMAP.md`](../ROADMAP.md) § The critical path item **1b**, opened
because [plan 23](../23-the-small-robots/README.md) K3 put a price on
it (`@M018`): **only three hexes of a wall are ever attacked**, whatever
the wall's length, so a wave's usable width is three and the four
quickest robots in it own the lot.  That is what collapses a `compose`
line back to one symbol — *a wave is as dangerous as its fastest class
and no more* — and it is what makes [plan 12](../12-combat-resolution/README.md)
B3's bracing rule exact but latent.

⚠ **This is the fourth phase to bill the same missing rule**, after
plan 11 F7, plan 12 B3 and plan 23 K3.  Each judged it latent; K3 is
the one that measured what it costs.

⚠⚠ **Read [plan 11](../11-flow-field/README.md) F7b before sizing this.**
The last steering rule three phases judged latent moved **every clock in
the game** when it landed (161/311/180 → 61/104/95) and turned a
falsified B3 claim true.  This plan should be expected to move the
corpus, not to be additive — which is why W2 is a phase rather than a
paragraph in W1.

### ⚠ The name in the docs may be WRONG, and W0 is what settles it

`ENEMY_MOVEMENT.md`, `CLAUDE.md` and plan 23 all call the missing rule
**the equal-distance sidestep**, and dryopea already has one:
`flow_sidesteps` offers every neighbour at the SAME field distance, and
`enemy_walk_desire` reaches for it whenever a companion is what blocked
the step (plan 11 F7b).  So the rule as named is BUILT, and the front
is three hexes wide anyway.

The reason to doubt the name is geometric, and it is the first thing
W0 must measure rather than assume: **an equal-DESIRE contour runs
around the CORE, not along the WALL.**  In K3's fixture the wall is the
column `q = 6` and the core is `(0, 0)`, so the hexes a besieger can
stand on are the column `q = 7`, whose desire distances *rise* with
`|r|` — `(7, 0)` is the single nearest, and spreading along the face
means moving to a hex that is strictly FURTHER from the core.  An
equal-distance sidestep cannot express that; it can only offer hexes on
the same ring, which near a straight face means stepping back off it.

⚠ **If that is right, the fix is not the rule the docs name** — it is
*prefer a hex from which a wall can still be attacked, even at a cost
in distance*, which is a different and larger claim.  W0 measures the
contour before W1 commits to a rule, because a phase that built the
named rule and found the front still three wide would have spent its
budget proving the name wrong.

## Goal

A wave arriving down one approach attacks a wall across a front as wide
as the wall gives it, rather than across the three hexes its route
happens to cross — so a `compose` line's slower classes reach the wall,
and B3's bracing consequence becomes something a player can play
against.

## Anchors

Implements, and does not restate:

- [`docs/ENEMY_MOVEMENT.md`](../../docs/ENEMY_MOVEMENT.md) § A wall's HP
  is structural (§ The fix, and why it is not built), and § Sealing the
  perimeter — the spec has carried this rule since before any of the
  mover existed.
- [`plan 11`](../11-flow-field/README.md) F7 (the desire field), F7b
  (the companion sidestep — the half that IS built), and its own record
  of what a latent steering rule costs.
- [`plan 12`](../12-combat-resolution/README.md) B3 — the bracing rule
  this unlocks, and the tripwire test written to go red the day it
  lands.
- [`plan 23`](../23-the-small-robots/README.md) K3 — `@M018`, the price.
- `src/spawn.loft` (`enemy_walk_desire`, `enemy_step`, `enemy_target`),
  `src/flow.loft` (`flow_sidesteps`, `flow_steps`, `flow_desire`).

### The instruments already exist

Nothing here needs a new measuring tool, which is unusual and is worth
saying out loud:

| what it answers | where it lives |
|---|---|
| how many DISTINCT wall hexes the roster besieges (**3** today) | `tests/23_k3_what_composition_is_worth.loft::siege_shape` |
| how many of the besiegers are of a given class (**0** miners today) | the same function's second return |
| does the fan reach a wall's weak END | `tests/12_b3_bracing.loft::test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak` |
| does a wave's composition still read as one symbol | K3's five-row table (94 / 104 / 119 / 164 / never) |
| did anything cost more | `tests/11_f8_the_tick_budget.loft` — a RATIO, and this phase adds per-enemy work |

## W0 — the probe: what the contour offers at a wall face (2026-08-17)

K3's fixture exactly — a band `r = -2..2`, walled across `q = 6`, core
at `(0, 0)`, besiegers standing in the column `q = 7`.

**The desire distances.**  The field is a ring around the CORE, so the
face column has exactly **one minimum** and rises away from it:

| | `r=-2` | `r=-1` | `r=0` | `r=1` | `r=2` |
|---|---|---|---|---|---|
| `q=6` (the wall) | 7 | 7 | **6** | 7 | 7 |
| `q=7` (the face) | 8 | 8 | **7** | 8 | 8 |
| `q=8` | 9 | 9 | **8** | 9 | 9 |

**What each face hex is offered**, with the ground's answer to each:

| standing at | strictly-closer steps | equal-distance steps | so it |
|---|---|---|---|
| `(7,-2)` | `(6,-2)` ✗wall, `(6,-1)` ✗wall | `(7,-1)` ✓ | **stands and attacks** |
| `(7,-1)` | `(6,-1)` ✗wall, **`(7,0)` ✓** | `(7,-2)` ✓, `(8,0)` ✓ | walks to `(7,0)`, or queues and SIDESTEPS |
| `(7,0)` | `(6,0)` ✗wall | `(6,-1)` ✗, `(6,1)` ✗ | **stands and attacks** |
| `(7,1)` | **`(7,0)` ✓**, `(6,1)` ✗wall | `(8,0)` ✓, `(7,2)` ✓ | walks to `(7,0)`, or queues and SIDESTEPS |
| `(7,2)` | `(6,1)` ✗wall, `(6,2)` ✗wall | `(7,1)` ✓ | **stands and attacks** |

⚠ Every one of those five hexes touches the wall — `(7,0)` touches
three wall hexes, `(7,±1)` one each, `(7,±2)` two each.  **Two of the
five are beside the thing they came to break and walk away from it.**

### ⚠⚠ Three, and why a longer wall does not make it four

An enemy stands and attacks exactly when **every** strictly-closer
desire step is refused by the ground.  On a straight face that is true
only at the gradient's minimum and at the two hexes where the lateral
step has run out — which is **three, for any wall length**, because the
ring has one minimum on the face and every other face hex always has a
lateral step toward it.  On the seven-row band `(7,±3)` steps to
`(7,±2)` for the same reason `(7,±1)` steps to `(7,0)`.

⚠ **That is `@M018` explained**, and it is a property of the desire
field's shape rather than of the fan's width.  Plan 12 B3's *"the fan
is not that wide"* and this are the same fact from two sides.

### ⚠⚠ So the docs' name is WRONG, and the correction is the finding

`ENEMY_MOVEMENT.md`, `CLAUDE.md`, `plans/11`, `plans/12` and `plans/23`
all call the missing rule **the equal-distance sidestep**.  dryopea has
had one since plan 11 F7b, and the table above shows it is not merely
insufficient — at `(7,-1)` its two offers are `(7,-2)` (along the face)
and `(8,0)` (**back off it**), so the named rule is as likely to empty
the front as to widen it.

⚠ **The missing rule is a PRECEDENCE, not a steering mode:** *arriving
beats queueing* — a besieger beside a wall hex it could attack attacks
it, rather than walking on down a gradient that only leads to a hex a
companion is already standing on.

⚠ **It has to land in TWO functions or they disagree**, and that is the
phase's real cost:

- `enemy_walk_desire` must stop the walk, or the enemy attacks a
  different hex every tick and finishes none (F7b's jitter, exactly).
- `enemy_target`'s siege branch returns `ent_here` *"the first blocked
  step … only when NOTHING down it is legal"*.  ⚠ **It is
  occupancy-blind by SIGNATURE** — it takes no `Occupancy` — so a rule
  phrased as *"my closer steps are all held"* cannot be written there
  without changing the signature and every caller of `wave_targets`.
  ⚠ A rule phrased as *"I am beside an attackable wall"* needs no
  occupancy at all, which is why W1 should reach for that one first.

## W1 — the rule: arriving beats queueing (2026-08-17)

Two edits, and the second is a DELETION:

```
enemy_walk_desire   + a pre-pass:  any strictly-closer desire step the
                                   ground refuses  ->  stand and attack
enemy_target        - the early return that answered "attacking nothing"
                      the moment any closer step was legal
```

Both now ask the identical question — *is a wall between me and the
core?* — which is the whole of the fix.  ⚠ The mover's old `can_step`
check inside its walk loop became redundant and went with it: past the
pre-pass every closer step is one the ground allows, so `ewd_queued` no
longer has two meanings to tell apart.

### ⚠⚠ Why the rule is phrased about the WALL and not about the CROWD

The natural phrasing is *"my closer steps are all held by companions"*.
It is unbuildable and the reason is a signature: **`enemy_target` takes
no `Occupancy`**, and giving it one means changing `wave_targets` and
every caller.

⚠ **The phrasing forced on us is the better one**, which is worth
recording because it was luck rather than judgement:

- **It needs no memory.**  An enemy that stops never moves again, so
  `flow_steps`' `lat_neighbours` order names the same hex every tick.
  Open question 2 asked whether a spreading besieger needs a remembered
  target; the answer is no, and a `.keys` setter for it is unneeded.
- **It cannot jitter.**  Plan 11 F7b's whole reason for `ewd_queued`
  was that an unconditional sidestep shuffles a besieger along a face
  attacking a different hex each tick.  A rule that STOPS cannot do
  that.
- **A target that moved with the crowd would change during the move
  loop**, and `wave_damage` reads it afterwards.

### What the gate said

* `tests/24_w1_the_front.loft` — 7 functions, green: the front widens
  with the wall (4 on five rows, 6 on seven), the screened miners reach
  it, the screen is a ramp across classes, a screened wave takes the
  base where `@M018` measured it never taking it, and three negative
  controls.
* ⚠ **The controls all fired as intended and none of them is
  cosmetic**: a lone besieger bites ONE hex for twelve ticks (the
  jitter case, which this rule could plausibly have broken); a routed
  enemy walking past a wall through a gap attacks nothing (the rule
  must not leak into the engage branch, where 569 measurements live);
  and open ground with no wall on it is besieged by nobody.

### ⚠⚠ The tripwire written for this day did NOT fire

`tests/12_b3_bracing.loft::test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak`
was written in plan 12 B3 to go red the day somebody built this
steering, and `ENEMY_MOVEMENT.md` pointed at it.  **It stayed green.**

⚠ The reason is exact rather than lucky: B3's six robots come from six
directions and each already touches the fence where its own route meets
it, so a precedence about touching changes nothing for them.  ⚠ **A
tripwire aimed at the RULE you expect to build is not the same as one
aimed at the BEHAVIOUR you want** — and a different tripwire fired
instead (`11_f7b`'s bracing test, below), which is the one that was
written about behaviour.

## W2 — what it is WORTH: the corpus re-priced (2026-08-17)

**16 test assertions and 8 gate scripts moved.**  Every one was a
measurement of the old rule; nothing was a defect.

| what moved | from | to |
|---|---|---|
| the front, five-row wall | 3 hexes | **4** |
| the front, seven-row wall | 3 hexes | **6** |
| miners on the wall behind four scouts | 0 | **1** |
| `4 scout + 8 miner` | never | **126** |
| `4 harvester + 8 miner` | 164 | **122** |
| `a-base-on-two-fronts` | 123 | **132** |
| `@M005`, the longest base | 321 | **320** |
| `a-defended-base` braced middle | stands | **breaks** |

### ⚠⚠ Two gates were SATURATED, and the fix made them stronger

Three scenarios and one test asserted *the braced middle is still
standing at the end of the run*.  With a front half again as wide, a
five-hex wall does not survive that long — so the reading saturated,
and `CLAUDE.md` § A gate whose reading is already saturated cannot see
the thing you built applies to the gate that had been passing.

⚠ **`11_f7b`'s bracing test now measures the ORDER**: it records the
tick each hex breaks and asserts the 30 HP end goes before the 100 HP
middle.  That is what the bracing rule actually claims, it stays true
however long the run is and however wide the front gets, and it is
strictly stronger than the photograph it replaced.

⚠ **And the unzip had to be read the tick it happens.**  At tick 120 the
whole wall is down and `structure_max_hp` answers 0.0 — which is the
same 0.0 it uses for *"nothing there"*, so a late reading cannot tell an
unzip from rubble (`damage.loft` § structure_hp).

### ⚠ The scenarios that carried a FINDING got rewritten, not renumbered

`a-wave-screened-by-four.keys` narrated four scouts holding every hex
the wall could be reached from, with all eight miners in a field.  It
now narrates **enemy 6, a miner, on `(7, 2)`** — and it gained a `fall`
line it never had, because the base never used to fall.

⚠ `23_k3`'s own header is rewritten to record what it measured, what it
priced and what plan 24 retired — the file keeps its findings as history
rather than being edited into agreeing with today.

### ⚠ One duplicated assertion was deleted rather than inverted

K3's `test_only_three_hexes_are_ever_attacked` became a claim that
`24_w1` already gates, over the same two expensive `siege_shape` runs.
It is deleted with a pointer, per `docs/PROFILING.md` § a test that
RE-DERIVES an expensive value a sibling already computed.  What stayed
in K3 is the SCREEN — *four scouts leak exactly one miner* — which is
that plan's own subject.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **W0** ✓ | the two tables in § W0 — the face column reads 8 / 8 / **7** / 8 / 8, and two of its five hexes walk away from a wall they are touching | ⚠ **no invariant; this phase measured and built nothing** | ⚠ **the falsifier was live and it FIRED**: the phase could have come back *"the contour runs along the face"*, which would have made the docs' name right and W1 small.  It came back the other way, and the correction — *a precedence, not a steering mode* — is the phase's whole output |
| **W1** ✓ | `siege_shape` reports **4** besieged hexes on the five-row band and **6** on the seven-row one | a spread is along the FACE, so widening the wall widens the front — the property `@M018` measured the absence of | ✓ **all three controls fired and none is cosmetic**: the JITTER case (a lone besieger bites ONE hex for twelve ticks — the thing this rule could plausibly have broken); a ROUTED enemy walking past a wall through a gap attacks nothing, so the precedence cannot leak into the branch 569 measurements are pinned to; and open ground is besieged by nobody.  ✓ F5c's `test_a_blocked_enemy_never_moves_away_from_the_core` green |
| **W2** ✓ | 16 assertions and 8 gate scripts re-priced; `@M020` written; `@M005` 321 → 320 | ⚠ **a measurement is not a regression** — every clock that moved is re-recorded with its date, and five documents naming the wrong rule are corrected | ⚠⚠ **B3's tripwire did NOT fire, and it was the named control.**  `test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak` stayed green because its six robots come from six directions and each already touches the fence where its route meets it.  ✓ **A different tripwire fired instead** — `11_f7b`'s bracing test, the one written about BEHAVIOUR rather than about the rule — which is the finding: *aim a tripwire at the behaviour you want, never at the mechanism you predict* |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **W0** — the probe: what does the contour offer at a face? | XS | a throwaway test printing the desire distances and the `flow_sidesteps` offer along `q = 7` in K3's band.  ⚠ Built nothing and shipped nothing but the two tables in § W0 | ✅ **Done** |
| **W1** — the rule: arriving beats queueing | M | `tests/24_w1_the_front.loft` (7 fns) — `siege_shape` 4 on five rows and 6 on seven; the jitter control; a routed enemy attacking nothing; open ground besieged by nobody | ✅ **Done** |
| **W2** — what it is WORTH | S | the corpus re-measured: **1162 tests + 33 scripts / 654 measurements green**, 16 assertions and 8 scripts re-priced, `@M020` written and `@M018` retired | ✅ **Done** |

### Why the order is this order

**W0 first because it can kill the plan's premise for the price of a
probe.**  The rule every doc names is already built; if its contour
does run along a face then W1 is a small correction to
`enemy_walk_desire` and not a new steering rule at all.  Committing to
the larger claim before measuring is what plan 19 P0 did with a single
`n`, and P1 paid for it.

**W1 before W2** because a re-measurement of a corpus that has not
moved is a re-measurement of nothing.  ⚠ And they are separate phases
rather than one because W1's gate is *the front widened* while W2's is
*here is what that cost every base in the repo* — folding them together
is how a moved clock ends up with two possible causes (plan 23 K2's
seam, the same argument).

## What this plan does NOT build

- **Retaliation.**  Enemies attacking towers that hurt them is the
  other unbuilt steering rule in `ENEMY_MOVEMENT.md`, and it is a
  targeting question rather than a movement one.
- **A shorter tick, or the field cache.**  W1 adds per-enemy work to
  the siege branch, so `11_f8`'s ratio is a gate here — but if it goes
  red the answer is [`plan 22`](../22-the-field-cache/README.md), not a
  cheaper steering rule.
- **The boss's 2×2 footprint**, which changes what "a hex is occupied"
  means and would re-open every rule this plan touches.
- **Any change to the ENGAGE branch's preference order.**  The routed
  mover is what 569 measurements are pinned to; this plan is about the
  siege, and a change that moved both would be untraceable.

## Open questions

1. ✅ **Is the rule *equal-distance* or *stay-on-the-face*?**
   **NEITHER — it is a precedence: *arriving beats queueing*.**
   Answered by W0, against the hypothesis: the equal-distance sidestep
   is BUILT, and at `(7,-1)` half of what it offers steps back off the
   face.  ⚠ Five documents name the missing rule after a rule dryopea
   already has, and W2 corrects all of them.
2. ✅ **Does a spreading besieger need a memory?**  **NO** — and it is
   the phrasing that bought that.  A rule saying *"a wall is between me
   and the core"* makes an enemy STOP, and one that has stopped never
   moves, so `flow_steps`' `lat_neighbours` order names the same hex for
   ever.  ⚠ `Enemy` gains no field and plan 18's vocabulary needs no
   setter.
3. ⚠⚠ **Does the front widen without bound?**  **YES, and the answer is
   a design change worth flagging for review.**  The front is now the
   wall FACE's width with no reach limit, which deletes the *"a
   perimeter longer than a wave can reach across still hides its weak
   hexes"* property `ENEMY_MOVEMENT.md` named as one of F7b's two
   playable conditions.
   ⚠ **The replacement is DILUTION and it is measured**: the same wave
   spread over more hexes takes each of them down more slowly (a 30 HP
   end kept 14 HP at the tick a three-hex front had it at 10).  So
   length still pays — continuously, rather than off a cliff — and a
   continuous reward is the better mechanic for a thing the player is
   choosing the shape of.
   ⚠ **A reach limit remains available** if the owner wants the cliff
   back: it would be a maximum lateral offset from the hex the route
   arrived at.  Reverting is one commit, and W2's re-pricing is what it
   would cost again.
