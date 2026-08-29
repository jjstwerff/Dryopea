<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# `31` — Carryover: what crosses between two bases

**Value:** `G` · **Effort:** `M`

## Status

**COMPLETE — N1-N5 SHIPPED 2026-08-29.  A base opens with what the last one
carried, `manifest_opens` is the door, and the carry COMPOUNDS.**

⚠⚠ **`@M087` is the plan's number: 230.0 against 106.0.**  The same base,
crew, wave and tower sites, **and the same 100 points left when the ferry
ends** — a hundred points spent on two more towers came back as a hundred and
twenty-four.  That is what makes `DESIGN.md` § 14's *sequence of bases* a RUN
rather than a list: a good sortie does not merely hand the next base a bigger
number, it hands it a base that earns more.

⚠⚠ **And the sweep is the finding, `@M085`'s shape with a new subject**:
swept over the wave list the carry is worth **+124 points** at the authored
5 + 8, **the difference between standing and falling** from 26 robots to 50,
**+18 ticks** at 75 and **+11** past 115 — then nothing, because both columns
flatten (214-217 and 228) once the wave saturates the siege front.

⚠⚠ **THE FIRST VERSION OF THAT PAIR READ 198 AGAINST 198**, both "fallen" on
the tick the ferry ended, with twice the towers plainly on the map.  `fall`
plays until `wallet_broke`, and **the wallet is what BUYS towers as well as
what enemies drain** — so a base that spends its budget on defence is broke
before a robot arrives.  Holding the REMAINDER equal is what makes the
difference the towers; it is `@X292`'s *the wallet at zero is POVERTY and not
an ending* met in a fixture.

⚠⚠ **The measurement so far is `@M086`: a sortie's carry buys FOUR beacons
where the first base bought two.**  `@M065` measured *exactly two, and the
third press is refused* and read it as `@X288`'s landing exclusion arrived at
by arithmetic; that is about a FIRST base, and `beacon_buy` reads
`wallet_left` and nothing else — so the exclusion moves with the wallet and
the landing loadout is the same rule with a different first term.

⚠⚠ **What the design pass bought, before any code**: the invariant's
re-assertion sites were counted at **four**, and the fourth
(`hud.loft::hud_ink_for`) is one no gate in the corpus could have seen — see
§ Re-assertion sites.  ⚠ Both rules now have code: `rules.sh` moved
**19 → 21 enforced**.

⚠ **The prediction was written down and held**: 1777 → **1790** tests (the 13
new ones and nothing else), and **974 of 974** existing scenario measurements
unmoved, which is what says `carried` really is spelled the safe way round.
Gates **1790 green / 145 files**, **56 scripts / 985 measurements**.

⚠ Three corpus-size vacuity guards moved by one, as a new fixture is supposed
to make them: `09_c5a`, `18_s2` and `c6_the_footing` each pin how many
scripts the sweep found.

`plans/28` S3 built `manifest_of` — the points, the crew aboard and the crew
left behind — and `@M068` measured it: **200.0 cut short against 225.3 played
out**, the first number in dryopea of what a SORTIE was worth.  ⚠⚠ **And
nothing reads it.**  Outside `scramble.loft` and its own test, `manifest`
appears in `src/` exactly once, in a comment about an unrelated constraint.

This is [`ROADMAP.md`](../ROADMAP.md) § Then the run becomes a RUN item **7**,
and [`BACKLOG.md`](../BACKLOG.md) F7's own words for it: *"the thing plan 28
says it made possible and deliberately did not do."*

⚠⚠ **What this plan changes, in one line: the wallet a base opens with stops
being a constant.**  Everything else here follows from that one field.

## Goal

A sortie's manifest becomes the next sortie's opening wallet — applied
**exactly once**, by one door — and a two-sortie run is measured.

## Anchors

- [`docs/DESIGN.md`](../../docs/DESIGN.md) § 13 § Starting budget + 1:1
  carryover, § 14 Run structure, § 14 § Scramble exit + cargo manifest.
- `examples/numbers.json` § `economy` — `starting_budget_first_base` 200,
  `carryover_points_ratio` 1.0, `tower_top_carryover_effect` **`none`**.
- `src/wallet.loft` (the budget), `src/scramble.loft` (the producer),
  `src/hud.loft` (§ Re-assertion sites, the fourth one), `src/script.loft` +
  `src/emit.loft` + `src/compare.loft` (the `.keys` pair and the diff).

## The invariant

⚠⚠ **Defined in [`docs/DESIGN.md`](../../docs/DESIGN.md) § 14 § Formal
rules**, not here — a second copy of where a rule lives is the defect the
rule system exists to remove ([`docs/FORMAL.md`](../../docs/FORMAL.md)
§ F-Index-Generated).  Two rules, and the second is what keeps the first
from growing:

- **`@FR-R-Carry-Once`** — the carry lands **exactly once, at the opening,
  by one door**.  The producer converts nothing, the consumer converts once.
- **`@FR-R-Opening`** — a sortie's opening is a function of its **map** and
  its **carry**, and of nothing else.

⚠ `@FR-R-Opening` is the conservation half, and it is what makes the
refusals below **structural rather than a scope cut**: a thing that does not
cross is a thing the manifest does not carry, and the manifest is one record
with three columns.

## Re-assertion sites — counted before any code was written

⚠⚠ **The count is FOUR, and the fourth is the one a desk read misses.**

| site | what it re-states | silent if wrong? |
|---|---|---|
| `wallet.loft::wallet_left` | `left = budget − spent` | no — every gate in the corpus reads it |
| `wallet.loft::wallet_drain` | the ledger CLAMPS at the budget | ⚠ **yes** — a rich base stops draining at 200 and can never fall |
| `wallet.loft::wallet_broke` | broke is `spent >= budget` | ⚠ **yes** — `fall` fires early, and the scenario says *the base fell* |
| **`hud.loft::hud_ink_for`** | the ramp's **SPAN** is the budget | ⚠⚠ **YES, and nothing in the corpus can see it** |

⚠⚠ **The fourth one, written out, because it is the whole reason the sites
were counted first.**  `hud_ink_for` takes an integer and reads
`WALLET_STARTING_POINTS` for its span, clamping above it on purpose — *"a
wallet above full is not a colour past amber — it is amber"*.  Open a base
with 250 carried and the corner sits at **full green from 450 down to 200**:
the ramp dead for more than half the run, on the ONE number the game shows
(`DESIGN.md` § HUD).  ⚠ And `tests/19_p7` stays green through it, because its
exhaustive sweep of the 201 reachable colours sweeps **the span it was
given** — the class `@M034` named, *are the numbers I chose capable of
disagreeing?*, with a new subject.

⚠ **The cure collapses N to one door plus one argument**: `wallet_budget(w)`
is the only place the budget is computed and the three wallet sites call it;
`hud_ink_for` takes its span as a parameter, so the HUD cannot hold a second
opinion about what a full wallet is.

## The field is spelled as the CARRY, never as the BUDGET

⚠⚠ **The structural decision, and it is [loft#914] turned into a feature** —
the same move `wallet.loft` already made for `spent`, `play.loft` for
`digging` and `part.loft` for `top_removed`:

- store a **`budget`** → an omitted field defaults to `0.0` → `wallet_left`
  goes negative → **every `Wallet` literal opens a base that has already
  fallen**, and a test asserting *the base fell* passes with nobody walking;
- store a **`carried`** → an omitted field defaults to `0.0` → `wallet_left`
  is `WALLET_STARTING_POINTS − spent` → **exactly the game that exists
  today**.

So `wallet_budget(w) = WALLET_STARTING_POINTS + w.carried`, and the constant
keeps the name `numbers.json` gave it — `economy.starting_budget_first_base`
— which is what it has always said and what no reader has ever needed.

⚠ **The prediction this makes, and N1's gate: the 974 scenario measurements
do not move, and neither does the suite.**  A field that changes a reading is
a field spelled the other way round.

## What does NOT cross — and each one is blocked on a thing with a NAME

⚠⚠ **The manifest has three columns and exactly one of them crosses.**  Read
against `@X324` — *a piece dropped for convenience is a regression even when
everything still works* — none of these is dropped for convenience:

- **the crew.**  `DESIGN.md` § 9 § Roster: **2 starting, and the two are
  FIXED and the player does not choose them** (`@X258`); between missions the
  roster is MANAGED at the station ([`PROGRESSION.md`](../../docs/PROGRESSION.md)
  § P2i), and the station does not exist.  ⚠⚠ And `@X290` / `@M067`: **a crew
  member has no identity outside one run's roster** — so a crew COUNT that
  crossed would carry the number while losing the people, which is the half
  the design cares about.  *Blocked on: an identity.*
- **the stragglers.**  § 9 § Stranded helpers is explicit — *"For validation:
  stranded is a data state only; the rescue-quest UI is deferred"* — and
  `@X290` says the same thing about it.  The `left` column is a RECORD of
  what happened, not an input.  *Blocked on: the same identity.*
- **tower-tops.**  `numbers.json § economy.tower_top_carryover_effect` is
  literally `none`, *"Validation placeholder"*, and `scramble.loft` already
  records that there is no *deposit a top at the core* anywhere in the tree.
  *Blocked on: a deposit.*

## The ratio, and the only place it may ever live

`numbers.json § economy.carryover_points_ratio` is **1.0**, *"all unspent
points carry"*.  ⚠⚠ **`manifest_of` refuses to convert and says so in its own
header** — *"there is no conversion here and there must not be one: a ratio
applied twice is the class of defect `plans/26` spent four plans on."*  So
the ratio belongs to the CONSUMER, applied once at the opening; with the
producer structurally unable to apply it, `@FR-R-Carry-Once` is enforced by the
shape of the pair rather than by a comment asking nicely.

## Invariant gate

| phase | concrete expected result | invariant it pins | negative control |
|---|---|---|---|
| **N1** | `wallet_left(wallet_new()) == 200.0`; `wallet_left(wallet_carrying(225.3)) == 425.3`; drain 500 from a 425.3 wallet → `wallet_left == 0.0` and `wallet_broke` | the budget is `baseline + carried`, and the ledger clamps at **it** and not at the baseline | ⚠ a **negative** carry is REFUSED at the door — a carry is what you kept, and unlike `slip` (`@D009`) it has no branch that may go below zero |
| **N2** | a 425-point wallet at 425 points draws `HUD_INK_FULL`; at 0 draws `HUD_INK_BROKE`; the ramp is monotone across the whole 426 | the ramp's span is the RUN's budget | ⚠ the old behaviour reproduced — span pinned to 200 — must FAIL the new sweep.  ⚠⚠ A control that the plausible wrong version also satisfies is not one |
| **N3** | `carried 225.3` → emit → re-read → `state_diff` empty | the `.keys` pair round-trips (`@D007`, `@D009` — **the writer and the reader are a PAIR**) | ⚠ a `carried` line written by `emit` that `script` cannot read, and the reverse; `tests/18_s2` is what sees it |
| **N4** | `manifest_of` of a sortie carrying 225.3 → `wallet_carrying` → the next wallet's budget is `425.3` and its `spent` is `0.0` | `@FR-R-Carry-Once` — the carry lands once and the ledger starts clean | ⚠⚠ **the carry applied TWICE must be unreachable, not merely untested**: opening from a manifest twice gives two independent wallets, never one at 650.6 |
| **N5** | two `.keys` scenarios, same map and waves, opened with 0 carried and with 200 carried | the carry buys something the clock can see | ⚠ **price the supply against the capacity first** (`@M085`): a base whose front is already saturated cannot show what two more towers are worth |

## Phases

| Phase | Effort | Verify | Status |
|---|---|---|---|
| **N0** — count the sites, probe the default | XS | ✅ 4 sites, the HUD one silent; `Wallet` has 21 `wallet_new()` callers and one deliberate `Wallet { }` literal (`tests/12_b6`) | ✅ **Done** |
| **N1** — the budget is `baseline + carried` | S | `tests/31_n1_the_budget.loft` (8); ⚠ **974 measurements and the suite unmoved** | ✅ **Done** |
| **N2** — the HUD's span is the run's | XS | `tests/31_n1` § N2 — a CARRIED span swept whole, with the baseline-pinned ramp reproduced as the control | ✅ **Done** |
| **N3** — the `.keys` door and its round trip | S | `carried <points>` + the `emit` line + the `wallet_diff` row; `tests/18_s2`'s *fields no scenario exercises* control carries one | ✅ **Done** |
| **N4** — the chain: a manifest opens a wallet | S | `tests/31_n4_the_chain.loft` (5) — conservation, twice is unreachable, and two manifests differing only in CREW open the same base | ✅ **Done** |
| **N5** — what a sortie is worth to the NEXT one | M | `a-landing-a-sortie-paid-for.keys` prices the carry in BEACONS (`@M086`); `a-base-a-sortie-paid-for.keys` + `a-base-a-sortie-did-not-pay-for.keys` price it in what the base comes home with — **230.0 against 106.0**, and a SWEEP over the wave list (`@M087`) | ✅ **Done** |

## What this plan does NOT build

⚠ **The landing flow** (`ROADMAP.md` item **6**, `DESIGN.md` § 15) — map
selection, the planet view, the rocket's descent search, the starter tower's
lander, the helpers emerging.  ⚠⚠ Carryover does not need it: a sortie
boundary is a **fresh `WaveState`**, and a `.keys` scenario opened as a
starting position is already a second base.  Item 6 is what lets the PLAYER
pick one; this plan is what makes picking one mean anything.

⚠ **A `Run` record.**  There is nothing for one to hold: the points live on
the wallet, which is already authorable, already diffable and already
cropped whole.  A record whose only field is a number another record owns is
a second home for one fact.

## Open questions

1. ⚠ **Is the baseline for a second base the same 200?**  `DESIGN.md` § 13 is
   explicit — *"Every base begins with a points budget (default 200)"*, then
   *"the budget = baseline + the player's unspent wallet"* — so a perfect
   sortie opens the next base at **400**, and `@M065`'s *exactly two beacons*
   becomes four.  The doc rules it; N5 is where the arithmetic gets looked at
   against a clock, and if it reads badly that is a finding for the owner and
   not a licence to change the number quietly.
2. ⚠ **Does `wallet_earn` above the budget still clamp?**  `hud_ink_for`
   already says a wallet above full is amber; `wallet_drain` clamps `spent`
   at the budget and nothing clamps `left` from above.  N1 decides whether
   `wallet_budget` is a ceiling or only an opening, and the honest answer is
   **only an opening** — that is what the HUD's clamp already assumes.

## See also

- [`plans/28`](../28-the-scramble/README.md) — the producer, and `@M068`.
- [`BACKLOG.md`](../BACKLOG.md) F7 — the WORLD's half of the snapshot, which
  this plan does not build either.
