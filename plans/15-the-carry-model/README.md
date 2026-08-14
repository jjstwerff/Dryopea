<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# 15 — The carry model: one slot, and nothing is ever lost

**Value:** `F` · **Effort:** `M`

## Status

**C0 + C1 shipped** (2026-08-14).  C2 is next.

**C1 built the model and its first producer.**  `src/carry.loft` holds
one record per carryable thing with an `owner` field, so an object is on
the ground, in exactly one carrier's slot, or spent — and the wrong
states cannot be written down rather than being prevented.  A helper
that wrecks now leaves something to fetch.  Suite **883 green**, gate
**22 scripts / 395 measurements**.

⚠ **The gate is a SUM, because every assertion about one pickup is
green under all three wrong implementations.**  Take an object and look
at the carrier: it is holding the thing — whether or not the ground
still has a copy, whether or not its hands were already full, and
whether or not a second object on that hex was overwritten.  So
`tests/15_c1_the_slot.loft` asserts over the whole layer after *every*
mutation (`assert_sound`), never once at the end: a layer that breaks
and then repairs itself passes a final look.

⚠ **The two-objects-on-one-hex case is now reachable in a test, and it
was reachable in the shipped game before this plan existed.**  A helper
carrying a downed colleague, destroyed while blocking, leaves its load
AND its own wreck on one hex.  Under the hash-keyed-by-hex shape every
other runtime layer uses, one of them is a crew member deleted from the
run with no fault raised anywhere.

⚠ **And the scenario for it failed first for a reason worth keeping**:
a one-hex corridor has no room beside, so a live crewmate behind the
blocker is the next thing the wave reaches and wrecks — three objects
on two hexes, which reads exactly like the defect the test hunts.  The
test was wrong and the code was right, which is the direction that
costs a debugging session if you assume the other one.

This plan is `F` rather than `G` because **three separate features wait
on it** and each would otherwise re-derive it: helper retrieval
([plan 14](../14-helpers/README.md) H4), the tower-top repair and
hot-swap arc (`DESIGN.md` § 7), and the beacon ferry that orders a new
tower.  `plans/ROADMAP.md` § Tier B names it as a shared blocker for
exactly that reason.

⚠ **It ships with ONE consumer, and that is a rule rather than a
scoping accident.**  `plans/README.md` § What makes a step SAFE: *"a
step that ends with something built and called by nobody cannot
fail"*.  So the model lands together with helper retrieval — which
closes plan 14 H4 — and tower-tops and beacons are later plans that
consume it without extending it.

## Goal

A thing can be picked up, carried, and put down — and **it is never
duplicated and never lost**.

`DESIGN.md` § 11 gives the whole interface in one line: **one slot per
vehicle, one context-resolved key** (empty hands = pickup, carrying =
deposit), and § Carry visibility gives one universal rendering rule for
everything it moves.  What the design does *not* say is where a carried
object lives while it is carried — and that is the entire engineering
problem, because it is the question conservation turns on.

## Anchors

Implements, and does not restate:

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 11 § The handful of keys
  (**E**, context-resolved), § Carry visibility (universal rule), § 9
  § Damage → wreck → retrieve → recover, § 7 § Tower-top salvage.
- [`examples/numbers.json`](../../examples/numbers.json)
  § `helper.carry_slot_count` (**1**, and its doc says *"Same as
  player"* — so the player's slot count is stated there and nowhere
  else) and § `helper.recovery_time_after_retrieval` (**60 s**).
  **This plan adds no new tunable without a row in that file** — the
  rule plan 14 § Anchors states.

Source files it touches: a new `src/carry.loft`, plus `src/helper.loft`
(the recovery state), `src/spawn.loft` (the tick) and
`src/script.loft` (the verbs).

## C0 — the probe (2026-08-14)

No code, three measurements, and **two of them changed the design**.

### ⚠ 1. The obvious representation silently DELETES a crew member

Every runtime layer dryopea has is a **hash keyed by hex** —
`height.loft`, `damage.loft`, `occupancy.loft`, and H3's `BlockerMap`.
Reaching for the same shape here is the natural move, and
`BlockerMap`'s own header even blesses it: *"A hex holds ONE of them…
if a future one lets them, the later `blocker_set` wins"*.

**A carry object cannot take that deal, because two of them on one hex
is reachable in the shipped simulation.**  A helper carrying a downed
crew member can itself be destroyed while blocking, and then two
objects are lying on that hex — the wreck it was carrying and its own.
A hash keyed by hex answers with one of them and the other is **gone**:
a crew member deleted from the run, with no error, no fault, and no
assertion in the game able to see it.

So the store is a **vector with stable slots**, never compacted — the
shape `state.crew` already uses, and for the same reason H3 gives:
helper 2 stays helper 2 for the whole run.

### ⚠ 2. Conservation has to be STRUCTURAL, not maintained

The second natural shape is "the carrier holds a thing" — a field on
`Vehicle` and on `Helper` — with the ground holding the rest.  Then a
pickup is **two writes** (add to the slot, remove from the ground) and
so is a drop, and every path that does one without the other either
duplicates the object or destroys it.  Conservation becomes a property
somebody has to maintain at every call site, which is the shape
`vehicle_on` was deleted for in H3.

The fix is the move `damage.loft` makes with *damage TAKEN, not HP
remaining*: **one record, one `owner` field**, where "on the ground" is
a value of that field rather than a different place to be.  A pickup is
then a single assignment, duplication is unrepresentable, and the
conservation gate is checking a sum that cannot drift rather than
auditing call sites.

⚠ And the owner ids are **`occupancy.loft`'s `BLOCKER_*` vocabulary**,
not a second numbering: `BLOCKER_NONE` (-1) is the ground,
`BLOCKER_PLAYER` is 0 and helper `i` is `BLOCKER_CREW + i`.  dryopea
already has one answer to "who on the player's side", and a carry model
with its own indices would be the drift H3 closed.

### ⚠ 3. The epsilon trap arrives a FOURTH time — at its least visible

Recovery is 60.0 s and a tick is `1 / 1.5` s, so recovery is **exactly
90 ticks**.  Measured, subtracting a tick at a time from 60.0:

| timer | exact | bare `> 0.0` | with an epsilon |
|---|---|---|---|
| recovery 60.0 s | 90.0 ticks | **91** | 90 |
| boost 2.0 s (plan 13 V4) | 3.0 ticks | 4 | 3 |
| cooldown 5.0 s | 7.5 ticks | 8 | 8 |

⚠ **The discriminator is exact divisibility, and it inverts the
intuition.**  The cooldown is 7.5 ticks — *not* a whole number — and it
takes 8 ticks either way, so the epsilon is invisible there.  The trap
fires **only** where the timer divides the tick exactly, which is the
case that looks safest and is the only dangerous one.

This is the least visible appearance yet: one tick in ninety, against a
tower losing a third of its shots (plan 12 B5a), a boost running 33%
long (plan 13 V4) and a helper losing 6.7% of its speed for ever (plan
14 H0).  ⚠ **No assertion that the crew member came back can see it** —
only counting the ticks can, which is what makes C2's gate a count.

### 4. One slot serves all four cargo kinds, so the KIND is data

`DESIGN.md` names four things a vehicle carries — loot cube, tower-top,
beacon, downed helper — and `numbers.json` § helper.carry_slot_count is
1 for all of them (*"Cannot carry beacon + loot simultaneously"*).  No
kind needs a second slot, a different reach or a different carrier.

So the kind is a **field, not a code path** — the same rule `CLAUDE.md`
§ Movement + passability states for enemies (*"ONE AI, per-class
DATA"*).  What varies per kind is only **what a valid deposit
destination is** and **what arriving there does**, which is one table
row apiece.  ⚠ That is also the invariant a later plan is measured
against: a tower-top that needs new carrying code has broken this.

### 5. What retrieval costs is already paid, and it is the design test

`CLAUDE.md` § What dryopea is asks of every mechanic: *does this put
something in the player's hands at a moment when using it costs them
something?*  Retrieval needs no invented cost to pass it.  A helper
wrecks where it was **blocking a wave with nowhere to go round** (plan
13 V5's rule) — a chokepoint — so fetching it means driving into the
one hex the wave is coming through, and the round trip to the core is
time nobody is spending on the body ramp that beats a tower.

⚠ So this plan adds **no carry speed penalty**, and its absence is a
decision: `numbers.json` has no row for one, and the slot plus the
drive are the cost the design already priced.

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **C0** — the probe: where a carried object lives | XS | the three measurements above, against the shipped sim | **Done** |
| **C1** — an object exists, is taken, is put down, and is CONSERVED | S | `tests/15_c1_the_slot.loft` — a sum that cannot drift over a sequence of pickups and drops, and all three wrong implementations are red | **Done** |
| **C2** — deposit at the core recovers the crew member | S | `tests/15_c2_recovery.loft` — exactly 90 ticks, and a `.keys` scenario where a lost helper rejoins the roster.  Closes [plan 14](../14-helpers/README.md) H4 | Planned |
| **C3** — what a retrieval is WORTH | S | a `.keys` pair differing only in whether the player fetches the wreck, and the measured clock | Planned |

### Why the order is this order

C1 before C2 because conservation is the property every later phase
leans on, and it is gateable with **no destination rule at all** —
objects that are only ever picked up and put down still have to be
neither duplicated nor lost.  A phase that shipped conservation and
recovery together could not say which half a broken count came from.

C2 before C3 because C3 is a measurement and needs something to
measure.  ⚠ And C3 may well come back **zero**, the way plan 14 H2's
crewed base did: a base whose work is not its bottleneck cannot express
a returning helper any more than it could express a second one.  That
is a finding either way — but only if § C0.5's cost is real, which is
why C3 measures the clock rather than asserting the mechanic fired.

## Invariant gate

| Phase | Expected result | Invariant | Negative control |
|---|---|---|---|
| **C0** ✓ | the tables above | the representation is chosen by what can be LOST, not by what reads well | — (C0 measures; C1 is its gate) |
| **C1** | a carry object is on the ground **xor** in exactly one carrier's slot, always | conservation is structural — one record, one owner — so duplication is unrepresentable rather than prevented | a hash keyed by hex loses the second object on a shared hex; a two-write pickup duplicates on any path that does one write; a carrier that takes a second object has no slot rule |
| **C2** | a deposited wreck recovers in **exactly 90 ticks** and rejoins the roster | retrieval is the ONLY way back (§ 9), and the timer is a count rather than a "did it come back" | a bare `> 0.0` gives 91 ticks and every arrival assertion stays green; a helper that recovers with nobody carrying it has been given the player's respawn |
| **C3** | the measured clock, with and without the detour | a retrieval costs the base what the driver was not doing | a scenario whose fronts are not bracing-controlled reads a 99-tick artefact as a finding (plan 14 H2 § 3) |

## What this plan does NOT build

**Tower-tops and beacons.**  They are the two consumers that justify
the model being general, and building either here would make this the
tower plan.  C0 § 4 is the contract they arrive under: a kind row and a
destination rule, no new carrying code.

**Loot as a carry object.**  `DESIGN.md` § 13 says helpers *"carry to
the core"*, but dryopea's loot is earned the instant rubble is cleared
(plan 13 V3, `wallet_earn`), so making it cargo is a re-design of a
shipped mechanic rather than a use of this one.  ⚠ It is also the only
case where the single slot BITES (*"cannot carry beacon + loot
simultaneously"*), so the slot's stated cost stays latent until then.

**Carry rendering** (§ Carry visibility) — dryopea draws no vehicles
yet, so there is nothing to render above.

**No ordering, no landers, no stranded-helper persistence** — the same
exclusions plan 14 lists, for the same reasons.

**No helper dispatcher.**  A helper can be *ordered* to fetch a wreck
by a script verb, exactly as it is ordered to drive; nothing makes one
decide to.  Plan 14 § Open questions 1 recommends keeping helpers
passive and positional, and a crew member who fetched its own colleague
would be the dispatcher that recommendation refuses.

## Open questions

1. **Does a wreck obstruct?**  H3 decided a wrecked helper blocks
   nothing, on the grounds that an obstruction with no HP left is a
   free wall.  A carry object lying on the ground inherits that answer
   by default — but once tower-tops exist, a red disc on the floor of a
   corridor is a thing the player put there deliberately, and the
   argument does not obviously transfer.  *Recommendation: keep cargo
   non-obstructing until a plan wants the opposite, and record it as a
   decision rather than an oversight.*
2. **Can a carrier deposit onto another carrier?**  § 11's key is
   context-resolved on the CARRIER's state (empty hands = pickup), so a
   hand-off is two actions and a hex, and nothing needs building.
   *Recommendation: no hand-off verb; if a scenario ever wants one,
   the cost is that "deposit" stops being a property of the
   destination.*

## See also

- [`plans/14-helpers`](../14-helpers/README.md) — H3 built the wreck
  this plan carries, and H4 is what C2 closes.
- [`plans/13-the-vehicle`](../13-the-vehicle/README.md) — V5's blocker
  rule is why a wreck is somewhere inconvenient.
- [`plans/ROADMAP.md`](../ROADMAP.md) § Tier B — the three features
  that wait on this.
