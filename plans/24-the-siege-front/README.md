<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 24 — The siege front: a besieger spreads ALONG the wall face

**Value:** `G` · **Effort:** `MH`

## Status

**W0 done** (2026-08-17) — **the rule every document names is not the
rule that is missing**, and the three-hex front now has a mechanical
explanation rather than a measurement.  W1 is next; nothing is built.

⚠⚠ **`@M019` — an enemy attacks only when it cannot WALK, and on a
straight face exactly three hexes can never walk.**  Both halves of the
siege are occupancy-blind about arriving: `enemy_walk_desire` sidesteps
whenever a companion is what stopped it, and `enemy_target` returns
*attacking nothing* the moment any strictly-closer desire step is
legal.  So a besieger standing beside the wall it came to break walks
sideways to join a queue instead.  § W0 has the table.

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

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **W0** ✓ | the two tables in § W0 — the face column reads 8 / 8 / **7** / 8 / 8, and two of its five hexes walk away from a wall they are touching | ⚠ **no invariant; this phase measured and built nothing** | ⚠ **the falsifier was live and it FIRED**: the phase could have come back *"the contour runs along the face"*, which would have made the docs' name right and W1 small.  It came back the other way, and the correction — *a precedence, not a steering mode* — is the phase's whole output |
| **W1** | `siege_shape` reports **more than three** besieged hexes on the five-row band, and **more again** on the seven-row one | a spread is along the FACE, so widening the wall widens the front — the property `@M018` measured the absence of | ⚠ **a sidestep must still close no distance in the ENGAGE branch**: F5c's `test_a_blocked_enemy_never_moves_away_from_the_core` must stay green, and a rule that let a routed enemy wander is refused.  ⚠ And the JITTER control — an enemy at a wall face with no companion anywhere must still stand and attack the same hex every tick, or `wave_damage` finishes nothing (plan 11 F7b's own reason for `ewd_queued`) |
| **W2** | K3's table re-measured, plan 12 B7's 69 / 112 / 128, plan 14 H2's 123 / 135 / 138 and plan 17 T3's seven-wave list — each moved or held, with the number of record updated | ⚠ **a measurement is not a regression**: every clock that moves is re-recorded with its date, and every doc quoting the old one is corrected | ⚠ **B3's tripwire MUST fire.**  `test_a_besieged_fence_is_bitten_where_the_route_meets_it_not_where_it_is_weak` was written to go red the day this ships; a green one means W1 did not change steering |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **W0** — the probe: what does the contour offer at a face? | XS | a throwaway test printing the desire distances and the `flow_sidesteps` offer along `q = 7` in K3's band.  ⚠ Built nothing and shipped nothing but the two tables in § W0 | ✅ **Done** |
| **W1** — the rule: arriving beats queueing | M | `tests/24_w1_the_front.loft` — `siege_shape` > 3 on five rows and wider still on seven; the jitter control; F5c's no-wander test still green; `11_f8`'s ratio inside band | **Next** |
| **W2** — what it is WORTH | S | the corpus re-measured: `scripts/test.sh` + `scripts/validate.sh` green with every moved reading re-recorded, `@M019` written, and `@M018`'s table restated | Blocked on W1 |

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
2. **Does a spreading besieger need a memory?**  A hex chosen for its
   face-adjacency this tick may not be the one chosen next tick, and an
   enemy that re-picks every tick attacks a different hex each time and
   finishes none — which is precisely the jitter F7b's `ewd_queued`
   exists to prevent, one level up.  ⚠ If W1 needs a remembered target
   then `Enemy` gains a field and plan 18's `.keys` vocabulary needs a
   setter for it (§ S1a's rule: a placement leaves nothing derived).
   Decided by W1.
3. **Does the front widen without bound?**  A rule that spreads along
   any reachable face turns a long perimeter into a long front, which
   would delete the *"a perimeter longer than a wave can reach across
   still hides its weak hexes"* property `ENEMY_MOVEMENT.md` names as
   one of F7b's two playable conditions.  ⚠ That property is worth
   keeping — it is what makes bracing reward length — so a spread with
   a reach limit may be the design rather than a compromise.  Decided
   by W1, measured by W2.
