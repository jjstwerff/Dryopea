<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `29` — The crew's own work

**Value:** `G` · **Effort:** `MH`

## Status

**O0 done 2026-08-28.  O1-O4 open.**

This is [`ROADMAP.md`](../ROADMAP.md) § Then the run becomes a RUN item
**5 — helper ORDERS**, and it is the first item past the four critical-path
gaps, all of which closed on 2026-08-27/28 ([27](../27-building/README.md)
building, [28](../28-the-scramble/README.md) the scramble).

⚠⚠ **It is also a PILLAR and not a convenience.**  `docs/DESIGN.md` § 9
§ ASSIGNMENT IS A PILLAR (`@X197`, owner 2026-08-26) is categorical —
*"decisions about what tasks to assign helpers to should be a big part of the
game"* — and today **there is no assignment at all**: `helper_drive`'s only
caller in the tree is the `.keys` script runner.

## Goal

A crew member with nobody telling them anything **finds work and goes to it**,
and a crew member the player has driven out to **hunts one kind of work across
the whole base** — so that `@X262`'s test (*does this system work with NO
input, and does its depth surface when the player reaches for it?*) has an
answer in code rather than in prose.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 9 § ASSIGNMENT IS A PILLAR
  (`@X197`), § SEMI-AUTOMATIC BY DEFAULT (`@X252`), § It is a RATCHET
  (`@X253`), § 11 (`@X261`, `@X262`).
- [`docs/PROGRESSION.md`](../../docs/PROGRESSION.md) § P2f (`@X255` — the
  debrief is how the pillar is DISCOVERED), § P2a (the skill lattice).
- `@X289` — a specialised helper **can** be widened, and it costs a trip.
- `src/helper.loft`, `src/skill.loft` (`detect_sees` — the radius that
  already exists), `src/build.loft`, `src/height.loft`, `src/tower.loft`,
  `src/trap.loft`, `src/spawn.loft` (`repair_target` / `rearm_target` — the
  nearest-wins rule already written twice).

## Invariant gate

⚠⚠ **THE REMIT TRADES BREADTH FOR REACH, and it is two exact halves.**

| half | claim | how it fails |
|---|---|---|
| **kind narrows** | a directed crew member **never** accepts a job outside their remit | a remit that also widened the kind set |
| **reach widens** | every job a GENERAL crew member would take, one directed to that job's kind takes too | a remit that narrowed the distance as well |

⚠ And the **cover**: the union over the four remits contains everything the
general rule accepts, so **no kind of work becomes unreachable by narrowing**.

⚠ The negative control is `@M025`'s shape — a gate aimed at the mechanism is
not one aimed at the hazard.  The hazard here is a default good enough that
directing buys nothing, so **O4's measurement is the falsifiable prediction**:
if a directed crew is not measurably better than the semi-automatic one, the
pillar is inert and the design has to move, not the code.

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **O0** — probe: what a crew member does when nobody tells them anything | XS | `tests/29_o0_probe.loft` | ✅ **done 2026-08-28** |
| **O1** — the semi-automatic DEFAULT: nearest job inside their own senses | M | `tests/29_o1_the_default.loft` | ⬜ |
| **O2** — what an unattended crew is WORTH | S | a scenario pair, one token apart | ⬜ |
| **O3** — the REMIT: one kind, base-wide, and a cycle that widens again | M | `tests/29_o3_the_remit.loft` | ⬜ |
| **O4** — what DIRECTING is worth | S | a scenario pair | ⬜ |

## ⚠⚠ O0 — what the probe found

Five readings, taken before a line was designed —
`tests/29_o0_probe.loft`, green against the tree at `10878a6`.

1. ⚠⚠ **AN UNATTENDED CREW MEMBER NEVER GOES TO THE WORK.**  One crew
   member four hexes from a wall order, left alone for **sixty ticks** —
   longer than two of the three authored maps take to fall — is still at
   (4, 0), the order is still an order, and **not one unit of work is in
   it.**
2. ⚠ **The CONTROL says everything works and nothing chooses**: the
   identical base with the crew member standing *on* the site finishes
   the wall inside the same sixty ticks, and `hex_ground_name` answers
   `wall`.
3. ⚠⚠ **The destination is never even SET.**  A ticked crew member
   answers `helper_arrived` **true** — because it was never sent.
   `helper_drive` is the verb, and its only caller in the whole tree is
   the `.keys` script runner.
4. ⚠⚠ **All four jobs reach exactly ONE hex** — `BUILD_REACH_HEXES`,
   `VEHICLE_SALVAGE_REACH_HEXES`, `TOWER_REPAIR_REACH_HEXES` and
   `TRAP_REACH_HEXES` are all 1.  So a crew member's entire usefulness is
   the six hexes around wherever a fixture left them, and one hex further
   out they contribute **nothing, for ever**.
5. ⚠ **The walk is CHEAP, so the DECISION is the whole cost**: four hexes
   is 1.6 s of a crew member's time, under three ticks of a base that
   runs three hundred.  ⚠⚠ Which is `@X252` stated as arithmetic — **the
   scarce thing is the player's attention, never the crew's legs** — and
   it stops a later phase quietly pricing the default by travel time.

### ⚠⚠ What O0 settles about the design

**The corpus has been paying for the missing decision by hand.**
`tests/scripts/a-base-the-player-builds.keys` says it out loud —
*"The crew, sitting on the line the player is about to draw"* — and
`tests/17_t3_what_upkeep_is_worth.loft` hand-rolls a **SHUTTLE** policy
(drive every crew member to the nearest black tower, every tick) inside
the test that measures what upkeep is worth.  ⚠ Seventeen of the 47 gate
scenarios name a helper.

⚠ So the semi-automatic default is not a new idea to be validated — it is
a rule three fixtures already implement privately, and O1 is where it
stops being three implementations.

## What this plan does NOT build

⚠ Named so a later reader does not think they were forgotten.

- **The crew SPEAKING.**  `@X142`'s idle helper who says so, and `@X255`'s
  debrief remark about a person being wasted, are how the pillar is
  *discovered*; `@X130` was lifted by BACKLOG B1 so text can be drawn, but a
  remark channel is its own piece of work.  This plan makes IDLE a state that
  is true and readable, and stops there.
- **The practice loop.**  `@X124`'s *a helper you keep directing to repair
  becomes a repairer* needs skill advancement, and `@M066` measured that nine
  of the twelve skills have no number in the tree at all.
- **Helpers boarding the rocket on their own** — `plans/28` parked it for the
  same reason.
- **Carrying as a job.**  Ferrying a beacon or a tower top is a `plans/15`
  errand with a destination rule; the four jobs here are the four a crew
  member already does by standing somewhere.
