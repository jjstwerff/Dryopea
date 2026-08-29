#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# The FORMAL-RULE gate: every `@FR-<Name>` cited anywhere in the tree
# must resolve to a rule DEFINED in a fenced block of a docs/ file.
#
# ── What it gates, and what it only reports ─────────────────────
#
# ⚠⚠ **Resolution GATES; coverage is REPORTED** — `docs/FORMAL.md`
# § Resolution GATES, adopting loft's own rule: *"a citation naming a
# rule that does not exist is worth failing on from the first day;
# 'every rule has at least one citation' tightens as coverage grows."*
#
# ⚠ It also fails on a rule DEFINED TWICE, which is not tidiness: loft
# found `L-Ref` defined as two different rules in two docs — one for
# Lambda, one for Layout — and *"neither was reachable by reading"*.
#
# ── The two hard constraints ────────────────────────────────────
#
# ⚠⚠ **BOUNDARY-EXACT.**  Sub-rule names are prefixes of their parents,
# and `\b` cannot help because `-` is already a word boundary.  So a
# citation matches only when the next character cannot continue a tag:
# `(?![-A-Za-z0-9])`.  ⚠ loft has 21 such prefix pairs and ruled that
# *"renaming those is deliberately NOT the fix — a matcher that is right
# by construction beats 23 renames plus the churn."*
#
# ⚠⚠ **ONLY A FENCED LINE DEFINES.**  A rule named in prose is a
# CITATION, not a definition — otherwise a doc could define a rule by
# mentioning it, and the registry would grow by discussion.
#
# ── Scope ───────────────────────────────────────────────────────
#
# ⚠ It reads the WHOLE tracked tree, deliberately.  loft's own checker
# globs `src/**/*.rs` only and its report notes **24 citations under
# `tests/` that nothing validates** — a scope gap dryopea can simply not
# have.  A `.keys` scenario naming the rule it protects is a citation
# like any other.
#
# Usage:  scripts/rules.sh            # gate
#         scripts/rules.sh --list     # also list the uncited rules
#         scripts/rules.sh sites @FR-M-Sidestep

set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

MODE="${1:-check}"

python3 - "$MODE" "${2:-}" <<'PYEOF'
import re, subprocess, sys, os

mode = sys.argv[1]
want = sys.argv[2] if len(sys.argv) > 2 else ""

def tracked(*globs):
    out = subprocess.run(['git', 'ls-files', '--'] + list(globs),
                         capture_output=True, text=True).stdout
    return [f for f in out.split('\n') if f]

# ⚠ A definition is a line INSIDE a fence whose name sits at EXACTLY two
# spaces of indent, with prose after it.
#
# ⚠⚠ **The exact indent is the gate, not decoration.**  A loose `^\s*`
# also matches a CONTINUATION line that happens to begin with a
# parenthesised name — and it did, on this checker's first run: the prose
# of `(W-Residual)` wrapped onto a line starting `(W-Edge-Owned) by
# arithmetic…`, and the gate reported that rule DEFINED TWICE.  ⚠ loft's
# `DEF_INLINE` is the loose form and carries the same hazard unfound.
DEF = re.compile(r'^ {2}\(([A-Z][A-Za-z0-9-]{1,40})\)\s')
# ⚠ Anything parenthesised at the start of any indent is *a mention* —
# used only to keep such a line from being read as a citation either.
MENTION = re.compile(r'^\s*\(([A-Z][A-Za-z0-9-]{1,40})\)\s')
# ⚠⚠ Boundary-exact: the next character may not continue a tag.
CITE = re.compile(r'@FR-([A-Z][A-Za-z0-9-]{1,40})(?![-A-Za-z0-9])')

# ⚠⚠ **The exemption governs DEFINITION as well as citation, and it did
# not until 2026-08-29.**  `docs/FORMAL.md` TEACHES the convention, so it
# shows a fenced `  (Order-Wins)  …` line as a worked example — and a
# fenced line at exactly two spaces is a definition, so the document
# explaining the registry silently ADDED A RULE TO IT.  The phantom
# resolved, so the gate stayed green while `@FR-E-Order-Wins`, the real <!--norule-->
# rule at `ERRANDS.md:178`, read as uncited and its one example citation
# pointed at the ghost.
# ⚠ That is this checker's own stated hazard — *"otherwise a doc could
# define a rule by mentioning it, and the registry would grow by
# discussion"* — reaching it through the one door the guard did not
# cover: `<!--norule-->` was built for the CITATION half and the
# DEFINITION half never got it.
defined = {}          # name -> [file:line, ...]
for f in tracked('docs/*.md'):
    fenced = False
    skip = False
    for n, line in enumerate(open(f, encoding='utf-8'), 1):
        # ⚠ Block markers are read BEFORE the fence test so a pair may
        # wrap a whole fenced example, and the fence still toggles
        # inside a skipped region so the state is right after it.
        if '<!--norule:begin-->' in line:
            skip = True
            continue
        if '<!--norule:end-->' in line:
            skip = False
            continue
        if line.lstrip().startswith('```'):
            fenced = not fenced
            continue
        if not fenced or skip:
            continue
        if '<!--norule-->' in line:
            continue
        m = DEF.match(line)
        if m:
            defined.setdefault(m.group(1), []).append('%s:%d' % (f, n))

# ⚠⚠ **A DOC reference is not an ENFORCING SITE, and the difference is
# the whole point of the review below.**  A rule cited from `DECISIONS.md`
# or `CLAUDE.md` is being cross-referenced; a rule cited from `src/` or a
# test or a `.keys` scenario is being OBEYED.  ⚠ Counting them together
# made the first `--review` report four rules with "two sites" whose
# second site was a router row — the third time in one day an instrument
# could not represent what it was counting (`@X328`).
def is_code(path):
    return not path.endswith('.md')

cites = {}            # name -> [file:line, ...]  (everything)
for f in tracked('*.md', '*.loft', '*.keys', '*.sh', '*.json'):
    # ⚠ The registry's own defining lines are not citations of themselves.
    try:
        text = open(f, encoding='utf-8').read()
    except (UnicodeDecodeError, IsADirectoryError):
        continue
    skip = False
    for n, line in enumerate(text.split('\n'), 1):
        # ⚠⚠ A doc that DESCRIBES the convention has to name tags it does
        # not cite — `docs/FORMAL.md` quotes loft's own rules as examples.
        # `<!--norule-->` on a line exempts it, which is loft's
        # `<!--noindex-->` for the same reason.
        if '<!--norule-->' in line:
            continue
        if '<!--norule:begin-->' in line:
            skip = True
            continue
        if '<!--norule:end-->' in line:
            skip = False
            continue
        if skip or MENTION.match(line):
            continue
        for m in CITE.finditer(line):
            cites.setdefault(m.group(1), []).append('%s:%d' % (f, n))

if mode == 'sites':
    key = want[4:] if want.startswith('@FR-') else want
    if key not in defined:
        print('rules: %s is not a defined rule' % want, file=sys.stderr)
        sys.exit(1)
    for s in cites.get(key, []):
        print('  ' + s)
    print('rules: %s — %d site(s)' % (want, len(cites.get(key, []))))
    sys.exit(0)

fail = False

dups = {k: v for k, v in defined.items() if len(v) > 1}
if dups:
    fail = True
    print('rules: FAILED — a rule is defined more than once:', file=sys.stderr)
    for k, v in sorted(dups.items()):
        print('  @FR-%s' % k, file=sys.stderr)
        for s in v:
            print('      ' + s, file=sys.stderr)

dangling = {k: v for k, v in cites.items() if k not in defined}
if dangling:
    fail = True
    print('rules: FAILED — cited but not DEFINED in a fenced block:', file=sys.stderr)
    for k, v in sorted(dangling.items()):
        print('  @FR-%s' % k, file=sys.stderr)
        for s in v[:6]:
            print('      ' + s, file=sys.stderr)
    print('', file=sys.stderr)
    print('  A rule is defined by a fenced `  (Name)  prose` line in a docs/ file.',
          file=sys.stderr)
    print('  Naming one in prose is a CITATION, not a definition.', file=sys.stderr)

if fail:
    sys.exit(1)

code_cites = {k: [x for x in v if is_code(x.rsplit(':', 1)[0])]
              for k, v in cites.items()}
code_cites = {k: v for k, v in code_cites.items() if v}

nd = len(defined)
nc = len([k for k in defined if k in cites])
ncode = len([k for k in defined if k in code_cites])
ns = sum(len(v) for k, v in cites.items())
nsc = sum(len(v) for v in code_cites.values())
# ⚠⚠ A gate that examined NOTHING must not read as a pass — so the
# counts are printed, and a zero here is visible rather than green.
if nd == 0:
    print('rules: FAILED — no rules defined at all; the check is vacuous',
          file=sys.stderr)
    sys.exit(1)

if mode == '--list':
    un = sorted(k for k in defined if k not in cites)
    if un:
        print('rules: defined but not yet cited:')
        for k in un:
            print('  @FR-%-28s %s' % (k, defined[k][0]))

# ⚠⚠ **The REVIEW worklist — @FR-F-Nameable-Difference.**  A rule with
# more than one enforcing site is not a defect; it is a QUESTION, and the
# three answers are: the sites differ COMPLETELY (the rule is too broad —
# split it), differ in a NAMEABLE way (healthy — write the difference
# down), or barely differ at all (one implementation, with a FLAG
# carrying the difference as data).
#
# ⚠ A REPORT and never a gate, for loft's reason: *"some repeats are
# genuinely different questions that happen to share a list today, and
# merging those would couple two rules that must be free to differ."*
if mode == '--review':
    multi = sorted((k, v) for k, v in code_cites.items()
                   if k in defined and len(v) > 1)
    if not multi:
        print('rules: no rule has more than one ENFORCING (code) site yet')
    else:
        print('rules: rules with MORE THAN ONE site — is the difference NAMEABLE?')
        for k, v in multi:
            print('  @FR-%-28s %d sites' % (k, len(v)))
            for sfile in v:
                print('        ' + sfile)

print('rules: ok — %d defined, %d ENFORCED in %d code site(s), '
      '%d referenced in %d place(s) total'
      % (nd, ncode, nsc, nc, ns))
PYEOF
