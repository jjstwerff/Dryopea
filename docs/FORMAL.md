<!--
Copyright (c) 2026 Jurjen Stellingwerff
SPDX-License-Identifier: LGPL-3.0-or-later
-->

# Formal rules — what a system IS, and what generates it

⚠⚠ **DESIGN, and one piece of it is built.**  `scripts/tags.sh` (`@X325`)
gates that every `@X###` / `@M###` citation resolves.  Everything below
is the system that grows around it.

> ⚠⚠ **The ask** (owner, 2026-08-28): *"for all the systems we make I
> want **formal rules that describe them including their invariants**"*
> and *"create a **formal system that generates** this world and the AI
> rules that fits the designs we made."*

⚠ Read against `../loft`, which has run this for months and whose
failures are the most valuable thing in it — every lesson quoted below is
loft's, attributed, and carried here because dryopea is about to make the
same mistakes.

## ⚠⚠ A RULE is not a DECISION and not a MEASUREMENT

⚠ dryopea already has three families and they are **all history**:

| family | what it records | tense |
|---|---|---|
| `@X###` | ⚠ **a decision**: we chose this, because that | ⚠ **past** — it happened once |
| `@M###` | a measurement: this read 174 | past — on a day, on a binary |
| `@D###` | a defect: this was broken | past — and then fixed |

⚠⚠ **A RULE is none of those.**  It is a claim about **every case,
including the ones nobody has run**, and it is *timeless*:

> ⚠⚠ **`@FR-<Name>` — a statement plus an INVARIANT, and the sites that
> enforce it CITE it.**

⚠ loft states the doctrine and it is the whole reason the family is
separate (`doc/claude/formal/README.md:20`):

> ⚠⚠ ***The rules do not change to match the code.  The code changes to
> match the rules.***

⚠ And `QUALITY.md:265`: *"A feature and a formal rule are meant to be
**timeless**; a bug is relevant in the moment and stops being so."*

### ⚠ Some `@X` rows are already rules wearing a decision's clothes

⚠⚠ **They should be promoted, not duplicated** — a rule and a decision
saying the same thing is the defect this whole document exists to
remove.  Clear cases from the recent design work:

⚠ **Done for the first six** — the rules exist and the decisions keep
their history half:

| today | promoted to |
|---|---|
| `@X296` an order is an order | `@FR-E-Order-Wins` |
| `@X313` a hex's content is a function of its six neighbours | `@FR-W-Neighbourhood` |
| `@X316` the coarse map chooses the table, the block the cell | `@FR-W-Table-Cell` |
| `@X320` a variant may differ only in its interior | `@FR-W-Edge-Owned` |
| `@X321` a block contributes a residual, never an absolute | `@FR-W-Residual` |
| `@X302` `position(t) = cycle(state, anchors, t − slip)` | `@FR-E-Rule-Not-State` + `@FR-E-Closed-Form` |

⚠⚠ **The last row is the one worth noticing**: one decision became TWO
rules, because *the position is a function* and *the function is
evaluable in O(legs)* are separate claims that a site can obey
separately.  ⚠ A decision may promote to several rules; a rule is never
several decisions.

⚠ The `@X` row stays as the **decision record** — *who decided, when,
and against what alternative* — and the `@FR-` rule carries the
statement the code obeys.  ⚠⚠ **Two families, two tenses, one
cross-reference each way.**

## ⚠⚠ THREE TIERS, and the third is what *generates*

⚠ This is where dryopea's ask goes further than loft's system, and the
difference is worth naming before anything is built.

| tier | the rule is … | who reads it | dryopea example |
|---|---|---|---|
| **1. PROSE** | a statement + invariant, cited from code | ⚠ a person; a tool checks only that the citation resolves | *blocked by a COMPANION → step beside* |
| **2. CHECKED** | plus a **predicate a tool evaluates against the tree** | a checker, at gate time | `@X325`'s *every citation resolves*; loft's `o_proxy_check.py` |
| **3. GENERATIVE** | plus a **machine form the ENGINE READS** | ⚠⚠ **the running game** | the palette; a block row; an errand role row |

> ⚠⚠ **At tier 3 the rule is not checked against the code — the rule
> IS the code's input.**  That is what *"a formal system that generates
> this world"* means, and it is `@X322`'s catalogue layer given a spine.

⚠ And it is the only tier where `@X323`'s thesis can be *enforced*
rather than believed: *express detail from a very compact base set* is a
property of a catalogue, and a catalogue is a thing a tool can measure.

### ⚠⚠ dryopea already has a tier-3 catalogue and NOTHING READS IT

⚠ Measured 2026-08-28: `examples/numbers.json` holds **353 leaf
numbers**, each already shaped `{value, units, doc}` — which is most of a
formal rule row — and **no code path loads it**.  ⚠ Twenty-three `.loft`
sites restate a value by hand under a `// numbers.json § path` comment,
and `CLAUDE.md` says so in its own router: *"⚠ nothing LOADS it; edit the
`.loft` constant too."*

⚠⚠ **A drift probe over those 23 found ZERO drift and FOUR false
positives — all of them the probe's fault — and that is the finding.**
The relation between a catalogue value and the constant implementing it
takes at least four shapes and **none of them is written down**:

| shape | example |
|---|---|
| same-line trailing citation | `pub const WALL_HP: float = 100.0;   // numbers.json § wall.wall_hp` |
| next-line citation | the 18 ordinary ones |
| ⚠ **a unit conversion** | `boost_duration` 2.0 s → `VEHICLE_BOOST_DURATION_UNITS = 6000000` |
| ⚠ **a derived unit** | `loot_value` 10 per body ÷ 0.5 m body → `LOOT_POINTS_PER_METRE = 20.0` |
| ⚠ **a wildcard family** | `// numbers.json § wall.brace_factor_*` over four constants |

> ⚠⚠ **A checker cannot verify what the comment does not say.**  So the
> first tier-3 job is not *assert the constant equals the value* — it is
> to make the **relation** machine-readable.

## The tag

⚠ `@FR-<Name>`, `[A-Z][A-Za-z0-9-]{1,40}` — **the same family loft
uses**, deliberately: the same person works in both trees, each repo's
checker reads only its own, and a second convention would be a second
thing to remember for no gain.

### ⚠⚠ BOUNDARY-EXACT, and loft learned this the expensive way

⚠ `doc/claude/formal/README.md:201`: **21 of loft's rules are a PREFIX of
another** (`@FR-B-View` ⊂ `@FR-B-View-Base`). <!--norule--> ⚠⚠ **`\b`
cannot help, because `-` is already a word boundary** — so a citation
matches only when the next character cannot continue a tag:

```
@FR-([A-Z][A-Za-z0-9-]{1,40})(?![-A-Za-z0-9])
```

⚠ And loft's ruling on the alternative, which dryopea should copy rather
than re-derive (`README.md:206`): *"**Renaming those 23 is deliberately
NOT the fix** — the sub-rule names are meaningful, and a matcher that is
right by construction beats 23 renames plus the churn."*

### Where a rule is DEFINED

⚠ In a **fenced block** in the area document that owns the subject, one
rule per line:

<!--norule:begin-->
```
  (Order-Wins)   `helper_drive` is an ORDER: the semi-automatic search
                 proposes a destination only to a crew member who has
                 none.  A verb that says GO HERE is honoured, or the
                 `.keys` vocabulary lies.
```
<!--norule:end-->

⚠⚠ **The example above is EXEMPTED, and it had to be** — it is a real
fenced line at exactly two spaces, so until 2026-08-29 this document
DEFINED a rule called `Order-Wins` just by teaching the shape of one.
The registry read **49 defined** where there were 48; the phantom
resolved, so nothing went red; and the real rule
(`@FR-E-Order-Wins`, [`ERRANDS.md`](ERRANDS.md)) read as UNCITED while
the citation example below pointed at the ghost.  ⚠ The exemption used
to govern citations only — *a document that describes the convention
names tags it does not cite* applies with equal force to the tags it
**shows how to define**.

⚠⚠ **Only a fenced line defines**, which is what stops a rule being
"defined" by prose that merely mentions it — loft's `_fenced_lines`
(`scripts/rule_tags.py:110`) exists for exactly that.

⚠ Area documents, mapped to what dryopea already has:

| area doc | rules about |
|---|---|
| [`ENEMY_MOVEMENT.md`](ENEMY_MOVEMENT.md) | passability, steering, the siege front |
| [`ERRANDS.md`](ERRANDS.md) | cycles, bounds, distraction, home |
| [`WORLDGEN.md`](WORLDGEN.md) | the neighbourhood, blocks, residuals, edges |
| [`HARD_WON_RULES.md`](HARD_WON_RULES.md) | ⚠⚠ **already the closest thing dryopea has to a rules file** — and it is prose, so its rules cannot be cited |
| [`DESIGN.md`](DESIGN.md) § 9, § 11 | the pillars |

## The citation

⚠ A comment in the implementing file, conventionally beginning
**"Enforces"**:

<!--norule:begin-->
```loft
// Enforces @FR-Order-Wins — the search proposes only to a crew member
// with no standing order, which is why `helper_seek` is a second door
// rather than a flag.
```
<!--norule:end-->

⚠ Exempted for the same reason as the block above: the tag in it is a
SHAPE, not a citation.  ⚠⚠ The rule it stands for is
**`@FR-E-Order-Wins`** — an example that drops the area letter teaches a
name that cannot resolve.

### ⚠⚠ Resolution GATES; coverage is REPORTED

⚠ loft's rule, and dryopea should adopt it verbatim
(`README.md:223`):

> ⚠⚠ ***Adopt honestly rather than completely.*** *A citation naming a
> rule that does not exist is worth failing on from the first day;
> "every rule has at least one citation" tightens as coverage grows.*

⚠⚠ **And the reason not to chase the coverage number**, which is the
single most important warning in loft's whole system
(`BUG_REVIEW.md:305`):

> ⚠⚠ *"**The remedy is NOT to add 179 citations.**  A citation added
> without reading the code records that somebody looked; it does not make
> the code adhere to the rule, and a tree at `76 cited → 255 cited` with
> the same duplication underneath would read as progress while nothing
> had changed. …  **evaluate the sites → de-duplicate onto one home →
> fix what the disagreement was already causing → then cite.  The
> citation is the RECEIPT, not the task.**"*

## ⚠⚠ Reading a citation COUNT — the rules the system applies to ITSELF  `@X329`

Owner, 2026-08-28:

> *"so if there are multiple implementations of the same rule they should
> be **different in a way**; if they are **completely different** the
> rule is probably **too broad**, but if they are **too similar** you
> probably want a **single implementation** (possibly with a flag to
> indicate differences)."*

⚠⚠ **That is what `rules.sh sites @FR-X` returning N > 1 is FOR.**  The
count alone says nothing; what it opens is a three-way reading, and the
middle band is the healthy one:

| the sites differ … | diagnosis | cure |
|---|---|---|
| ⚠ **completely** | **the RULE is too broad** — it is naming two rules that happen to share a sentence | **SPLIT the rule** |
| ⚠⚠ **in a NAMEABLE way** | ✅ healthy — the difference is the domain speaking | keep both, and **write the difference down** |
| ⚠ **not at all, or trivially** | one implementation, duplicated | **MERGE — with a FLAG if there is a real difference** |

> ⚠⚠ **THE TEST IS WHETHER THE DIFFERENCE CAN BE NAMED, AND WHETHER THE
> NAME IS A FACT ABOUT THE DOMAIN RATHER THAN ABOUT THE CODE.**  If it
> cannot be named, one of the two cures applies; which one depends on
> whether the sites are far apart or close together.

⚠ It is the design protocol's two failure modes read off a number
instead of off a feeling: *under-unify* is N mechanisms for one family,
*over-unify* is one mechanism forced over N families, and this is the
instrument that tells them apart.

### ⚠⚠ *With a flag* is dryopea's own established pattern, five times over

⚠ The merge is never *delete one and hope*.  It is **one implementation
that takes the difference as data** — and this repo already does it
everywhere, naming the flag each time:

| one implementation | the flag that carries the difference |
|---|---|
| `can_climb` | ⚠ takes a **CLIMB**, not a kind (`@FR-M-One-Rule`) |
| one mover for four robot roles | ⚠ the **class row** — speed, bite, climb (`@FR-M-One-AI`) |
| `salvage_at` | the **owner** — *"the shared chassis"* for player and crew alike |
| `crew_work_units` | the **level** — *"the ONE door for build and repair alike"* |
| `place_marker` | the **kind** — *"the ONE dispatch"* |

⚠⚠ **So a healthy merge REPLACES a difference in code with a difference
in DATA**, and the flag is the name the middle band asked for.

### ⚠ And the counter-force: similar CODE is not the same RULE

⚠⚠ loft measured this and it is the reason *too similar → merge* needs a
guard (`IMPLEMENTATIONS.md:20`):

> *"**A shared list is not automatically one rule.**  Before merging, ask
> what each site is asking.  Two sites that agree today because the
> language is small will silently constrain each other later."*

⚠ **Four of eight candidate families SPLIT rather than merged**, and
*"each split is a merge that would have coupled two rules that must stay
free to differ."*  ⚠⚠ Which is why the test above is about **naming the
question**, not about comparing the text: two sites with byte-identical
bodies asking different questions have a nameable difference — *the
question* — and stay apart.

> ⚠⚠ **Equality is EVIDENCE; sameness-of-rule is the CLAIM.**  Only the
> second licenses a merge, and a `@FR-` citation is that claim written
> down and reviewable.

### The rules, formally

```
  (F-Rule-Not-History)
                     a RULE claims something about every case including
                     the ones nobody has run, and is timeless.  A
                     decision, a measurement and a defect are all PAST
                     and belong to their own families; a rule that
                     records what happened is not a rule.

  (F-Fenced-Defines) a rule is DEFINED only by a fenced line whose name
                     sits at exactly two spaces of indent.  Naming one in
                     prose is a CITATION — otherwise a document defines
                     rules by discussing them.

  (F-Resolution-Gates)
                     every citation must RESOLVE and no rule may be
                     defined twice; coverage is only reported.  A tree
                     whose citation count rose while the duplication
                     underneath stayed would read as progress and be
                     none.

  (F-Nameable-Difference)
                     two sites may enforce one rule only if the
                     difference between them can be NAMED, and the name
                     must be a fact about the domain.  Un-nameable and
                     far apart means the rule is too broad and splits;
                     un-nameable and close together means one
                     implementation, with a FLAG carrying the difference
                     as data.

  (F-Index-Generated)
                     any rule-to-site index is DERIVED from the
                     citations and never kept beside them, because a
                     second copy of where the rules live is the defect
                     the convention exists to remove.
```

## Deviations — where the code does not obey

⚠ A rule is a target, so a doc may record where the code falls short.
loft's row shape (`README.md:241`), adopted:

```
### D<n> — one-line name
- **Violates:** <rule id(s)>
- **Where:** <file:symbol>
- **Effect:** <user-visible symptom>
- **Status:** OPEN | IN PROGRESS | CLOSED (then delete)
- **Removal:** the change that makes the code obey
```

⚠⚠ **Citing an OPEN deviation is ALLOWED and citing a CLOSED one is an
ERROR** (`rule_tags.py:82`, `:191`) — an open deviation is a live fact,
so *"the heap half is refused under `@FR-D-bind-11`"* <!--norule--> is
how you find every site that has to change when it closes; a closed one
*"is history, not law.  Cite the rule it was measured against."*

⚠ dryopea already has the closed half: [`PROBLEMS.md`](../PROBLEMS.md)
`@D###`, and *nothing is open*.

## ⚠⚠ The lessons carried from loft — every one is a failure it had

⚠ These are the reason to copy the system rather than invent one.

### 1. ⚠⚠ *OPEN: 0* is a claim, not a fact — and it failed FOUR ways

`README.md:47-96`.  Every failure was a **conformance corpus that held an
axis fixed**:

- `tuples.md` read zero while two `text`-element deviations were live,
  because its oracle *"is all-`(integer, integer)` and carries no `text`
  at all"*;
- `ownership.md` read zero for six weeks with a live bug, because its
  corpus swept four axes and *"holds the ARGUMENT SPELLING fixed at a
  variable in every cell"* — moving that one axis found **six more**;
- `closures.md`'s held-fixed axis *"was not the subject at all but the
  **moment**"*;
- `iteration.md` — *"the THIRD doc whose zero rested on an all-scalar
  corpus."*

> ⚠⚠ ***A corpus that varies the subject exhaustively still proves
> nothing about the axis it never varies — count what is held FIXED, not
> what is swept.***

⚠⚠ **dryopea has this exact hazard, already named** — `CLAUDE.md`
§ Testing something that moves: *a 1-hex corridor cannot tell a flow
field from a fixed heading*, *a world where every source hex is at 0 m
cannot tell a RISE from a destination height*, *the instrument is a
CROSS-PRODUCT*.  ⚠ Three instances of *count what is held fixed*, found
one at a time.  **The rules file is where that becomes one question
asked of every corpus.**

### 2. ⚠ Single-sourcing makes the answers agree; it does not make anyone ASK

`README.md:87` — a doc read `OPEN: 0` on the day it collapsed three
disagreeing lists into one, *"because the surviving list is only
consulted at one of the two sites that construct a `&(…)`."*

### 3. ⚠ A shared list is not automatically one rule

`IMPLEMENTATIONS.md:20` — *"Before merging, ask what each site is asking.
Two sites that agree today because the language is small will silently
constrain each other later."*  ⚠ Four of eight candidate families
**split rather than merged**, and *"each split is a merge that would have
coupled two rules that must stay free to differ."*

### 4. ⚠⚠ A number from an instrument that cannot represent what it counts is not a measurement

`IMPLEMENTATIONS.md:53` — loft's rule count moved **361 → 356 → 251 →
268 → 285** and its collision count **0 → 33 → 23 → 2 → 1 → 0**, and
*"every move was the instrument learning what it was counting."*

⚠⚠ **dryopea has already reproduced this twice in one day**:
`scripts/tags.sh` reported four tags dangling because it could not read a
RANGE row, and the `numbers.json` drift probe reported four drifts that
were all its own parsing.  ⚠ **Both instruments were wrong before the
tree was.**

### 5. ⚠ A diagnostic GATES iff ignoring it can produce a WRONG RESULT

`CLAUDE.md:429`, restated in `check_doc_drift.sh:57`.  ⚠ It is the whole
blocking/advisory split: *"A dangling doc citation is a broken link — it
cannot"* produce a wrong result, so it advises; a wrong store layout can,
so it gates.

### 6. ⚠⚠ A gate that examined NOTHING must not read as a pass

Repeated four times in loft's tree.  `check_doc_drift.sh:951` — *"A
check that examined nothing is not a pass — say which roots were
scanned, so a vacuous run reads as vacuous instead of green."*
⚠ dryopea's `scripts/examples.sh` already has this as `--self-test`, and
`scripts/tags.sh` prints its counts for the same reason.

### 7. ⚠ Prose does not fire at the moment you act

`.githooks/commit-msg:9` — a rule *"was already in CLAUDE.md,
ISSUE_TRACKING.md and two skills when a session shipped three fixes
under `Refs #N` anyway: **prose does not fire at the moment you type the
message.  This does.**"*

### 8. ⚠ The index is GENERATED from the citations, never kept beside them

`README.md:225` — *"a second copy of where the rules live is the defect
this convention exists to remove."*

## What dryopea builds, and in what order

⚠ Smallest first, and each is useful alone.

| # | piece | tier | gate |
|---|---|---|---|
| **1** | ✅ **`scripts/tags.sh`** — `@X`/`@M` citations resolve | 2 | shipped, inside `test.sh` |
| **2** | ✅ **43 rules defined** — `ENEMY_MOVEMENT.md` 12, `ERRANDS.md` 13, `WORLDGEN.md` 13, this file 5 | 1 | `scripts/rules.sh` |
| **3** | ✅ **`scripts/rules.sh`** — every `@FR-` citation resolves, no rule defined twice, coverage reported | 2 | shipped, inside `test.sh` |
| **4** | ⚠⚠ **the RELATION form for `numbers.json`** — a citation that says *how* a constant derives from a catalogue value | 3 | a drift gate that can finally be written |
| **5** | the catalogue as the generator's input — palette, blocks, roles | 3 | ⚠ [`plans/30`](../plans/30-the-mob-routine/README.md) and BACKLOG F |

⚠⚠ **Piece 4 is the one that turns the system from descriptive to
generative**, and it is small: the four relation shapes measured above
are `= value`, `= value × K`, `= value ÷ other`, and `family`.

## ⚠⚠ What `rules.sh` found in its OWN definition — three times

⚠ Three problems, every one in the checker rather than in the tree, and
all worth keeping because they are `@X328`'s *do not trust a count from a
new instrument until it has been wrong once* arriving on schedule and
then twice more.  ⚠⚠ **Two of the three were found by READING the shape
of the coverage rather than by a red gate** — a phantom rule resolves,
and a false enforcing site is still a site, so **neither could ever have
failed the gate that contains them**.  ⚠ The first two are from the
checker's first run; the third is from fixing the second, the same
afternoon.

1. ⚠⚠ **A rule read as DEFINED TWICE, and the second was a CONTINUATION
   LINE.**  `(W-Residual)`'s prose wrapped onto a line beginning
   `(W-Edge-Owned) by arithmetic…`, and a `^\s*\(Name\)` matcher cannot
   tell that from a definition.  ⚠ **loft's `DEF_INLINE` is exactly that
   loose form**, so it carries the same hazard unfound — dryopea's
   requires the name at **exactly two spaces** of indent, which makes the
   distinction structural.
2. ⚠ **A document that DESCRIBES the convention names tags it does not
   cite** — this file quotes loft's own rules as examples. <!--norule:begin-->
   `@FR-B-View` and `@FR-D-bind-11` are loft's, not dryopea's.
   <!--norule:end-->
   ⚠ A `<!--norule-->` on a line exempts that line, and a
   `<!--norule:begin-->` / `<!--norule:end-->` pair a block; it is loft's
   `<!--noindex-->` for the same reason.
   ⚠⚠ **The marker has to be on the line the tag is on**, which this
   section got wrong first: the block form exists because a prose
   paragraph wraps and the tag and the marker land on different lines.
   ⚠⚠ **AND IT EXEMPTS A DEFINITION AS WELL AS A CITATION** — it did not
   until 2026-08-29, and the half that was missing is the one that bites
   THIS file: a document teaching the convention has to SHOW a fenced
   definition, and a fenced definition is a definition wherever it sits.
   ⚠ So both worked examples above are wrapped in a `begin` / `end`
   pair.
3. ⚠⚠ **A file is classified by its EXTENSION, so a `.sh` or `.loft`
   comment merely DISCUSSING a rule counts as an enforcing site.**  The
   fix for point 2 introduced exactly that on its first run — the
   checker's own explanatory comment named the rule it had just stopped
   defining, and the rule jumped to ENFORCED with no implementing code
   anywhere.  ⚠ `<!--norule-->` works in any tracked file, and that is
   what it is for.

⚠⚠ **Coverage on 2026-08-29: 48 defined, 25 ENFORCED in 152 code
sites** — it read *43 defined, 5 ENFORCED in 5* when this section was
written, and the first printing of all said *13 cited*, because it
counted `DECISIONS.md` and `CLAUDE.md` cross-references as sites
(`@X330`).  ⚠ The original five were cited because **the code already
stated the rule in its own words** — `passable.loft` had *"the field
filters NODES by the surface and EDGES by the step"*, `lattice.loft` had
*"nothing may step a coordinate except `lat_neighbour`"* — so the
citation really is a receipt for an evaluation that already happened.
⚠⚠ **The rest are the work, and adding a citation to each without
reading its sites is exactly what `@X328` refuses.**

⚠⚠ **AND THE COUNT WAS 49 UNTIL THE INSTRUMENT WAS FIXED** (2026-08-29):
this document's own worked example DEFINED a rule called `Order-Wins`,
because a fenced line at exactly two spaces is a definition wherever it
appears — including in the document that teaches the shape.  ⚠ It is
`@X328`'s *do not trust a count from a new instrument until it has been
wrong once* landing a fourth time, and the tell was the SHAPE of the
coverage rather than the total: the real `@FR-E-Order-Wins` sat with
four citations and no code while a name nobody had written stood beside
it.  ⚠⚠ **The exemption now governs DEFINITION as well as citation** —
and fixing it immediately produced its own false positive, because the
checker's explanatory comment naming the rule is in a `.sh` file and so
counted as a CODE site.  ***A file is classified by its extension, so
prose about a rule that lives outside `docs/` reads as enforcement.***

⚠ **Where the uncovered rules actually are** (evaluated 2026-08-29): the
`W-*` family is 12 rules with almost no code and that is CORRECT — the
worldgen it describes is not built, and the rule is spec-first by
design.  The gap is the **`M-*` family**: six rules — `M-Besiege`,
`M-Desire-Removable`, `M-One-AI`, `M-Surface`, `M-Sweep-Inward`,
`M-Waterline` — whose subjects all shipped years of phases ago, plus
`@FR-E-Order-Wins` (`helper.loft`) and `@FR-E-Remit-Trades`
(`task.loft`), whose two implementing files cite **nothing at all**.
⚠⚠ **The routine system took the discipline and the mover under it did
not**: `errand.loft` and `poi.loft` carry **74 of the tree's 109 `src/`
citations** between them.

## Open questions

1. ⚠ **Do `@FR-` rules live in the area docs or in one file?**  loft uses
   the area docs and generates the cross-view.  *Recommendation: area
   docs* — dryopea's [`HARD_WON_RULES.md`](HARD_WON_RULES.md) is already
   the cross-view and would become the generated one.
2. ⚠⚠ **Which `@X` rows are promoted?**  Not all of them; a decision
   about a key binding is not a rule.  *Recommendation: promote only
   where a code site can CITE it* — that is the test, and it is the same
   one loft's coverage question asks.
3. ⚠ **Does a `.keys` scenario cite rules?**  A scenario is a gate, and a
   gate that names the rule it protects is a citation like any other.
   ⚠ loft's checker reads `src/**/*.rs` **only**, and its own report
   notes **24 citations under `tests/` that nothing validates** — a
   scope gap dryopea can avoid by reading the whole tree from day one.

## See also

- [`DECISIONS.md`](DECISIONS.md) — `@X` / `@M`, the history half.
- [`HARD_WON_RULES.md`](HARD_WON_RULES.md) — the rules dryopea already
  has, in prose, uncitable.
- [`EXAMPLES.md`](EXAMPLES.md) — `@DRY-###`, the worked-example family
  and the gate that keeps it honest.
- `../loft/doc/claude/formal/README.md` — the system this is read from.
