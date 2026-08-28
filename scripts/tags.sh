#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# The DECISION-TAG gate: every `@X###` and `@M###` cited anywhere in the
# repo must resolve to a row in docs/DECISIONS.md.
#
# ── Why this exists ─────────────────────────────────────────────
#
# `docs/DECISIONS.md` is the greppable INDEX, and the whole point of a
# code like `@X295` is that it is a NAME you can look up — a rule written
# down once, cited from every site that obeys it.  ⚠⚠ **That only works
# if a citation is guaranteed to resolve.**  A dangling `@X` reads as
# authoritative and answers nothing, and nothing was checking:
# `@X043`, `@X045`, `@X046` and `@X047` were cited from `RENDERER.md`,
# `PARTS.md`, `ARCHITECTURE.md` and `READING_BY_GOAL.md` and defined
# nowhere, for months.
#
# ⚠ It is `scripts/examples.sh` for `@DRY-` tags, applied to the two
# families that had no gate — and it is what makes "count the sites that
# re-assert this rule" a QUERY rather than an audit.
#
# ⚠⚠ **Honest rather than complete**, deliberately: a citation naming a
# rule that does not exist FAILS, while *every rule is cited at least
# once* is only REPORTED.  A brand-new decision has no consumer yet and
# that is not an error.
#
# Usage:  scripts/tags.sh          # gate
#         scripts/tags.sh --list   # also list the uncited rows

set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

REG=docs/DECISIONS.md

[ -f "$REG" ] || { echo "tags: $REG is missing" >&2; exit 2; }

# ⚠ A row DEFINES a tag only in the FIRST COLUMN of the registry table.
# Anything else in that file is a CITATION, exactly like a citation
# anywhere else — which is what stops a decision citing a typo of itself
# and calling it defined.
#
# ⚠⚠ **AND A ROW MAY DEFINE A RANGE** — `| \`@X044\`-\`@X047\` | …` defines
# FOUR tags — which is the first thing this gate got wrong about its own
# registry: reading only single-tag rows reported `@X043`, `@X045`,
# `@X046` and `@X047` as dangling when all four are defined, and `@X046`
# is cited from ten files including `src/` and `tests/`.  ⚠ A false
# alarm from reading the registry's format too narrowly is exactly the
# way a citation count stops being trustworthy, so the expansion is the
# gate rather than a convenience.
defined() {
    grep -oE '^\| `@[XM][0-9]{3}`(-`@[XM][0-9]{3}`)?' "$REG" \
      >/dev/null
    # ⚠ Expanded in python rather than awk: a range is inclusive and its
    # two ends must share a family letter, and saying that in awk costs
    # more than it saves.
    python3 - "$REG" <<'PYEOF'
import re, sys
out = set()
for line in open(sys.argv[1]):
    m = re.match(r'^\| `@([XM])(\d{3})`(?:-`@([XM])(\d{3})`)?', line)
    if not m:
        continue
    fam, lo, fam2, hi = m.group(1), int(m.group(2)), m.group(3), m.group(4)
    if hi is None:
        out.add('@%s%03d' % (fam, lo))
        continue
    if fam2 != fam:
        sys.stderr.write('tags: range row mixes families: %s\n' % line[:40])
        sys.exit(2)
    for n in range(lo, int(hi) + 1):
        out.add('@%s%03d' % (fam, n))
for t in sorted(out):
    print(t)
PYEOF
}

# ⚠ Every tracked file, minus the registry's own definition column, minus
# this script.  ⚠⚠ `.gate/` and `shots/` are run output and must not be
# scanned — a stale log naming a tag is not a citation.
cited() {
    git ls-files -- '*.md' '*.loft' \
      | grep -v '^scripts/' \
      | xargs grep -hoE '@[XM][0-9]{3}' 2>/dev/null \
      | sort -u
}

DANGLING=$(comm -23 <(cited) <(defined))

if [ -n "$DANGLING" ]; then
    echo "tags: FAILED — cited but not defined in $REG:" >&2
    for t in $DANGLING; do
        echo "  $t" >&2
        git ls-files -- '*.md' '*.loft' | grep -v '^scripts/' \
          | xargs grep -ln "$t" 2>/dev/null | sed 's/^/      /' >&2
    done
    echo "" >&2
    echo "  A citation that resolves to nothing reads as authoritative and answers" >&2
    echo "  nothing.  Add the row, or fix the code." >&2
    exit 1
fi

NDEF=$(defined | wc -l | tr -d ' ')
NCIT=$(cited   | wc -l | tr -d ' ')
UNCITED=$(comm -13 <(cited) <(defined) | wc -l | tr -d ' ')

if [ "${1:-}" = "--list" ] && [ "$UNCITED" -gt 0 ]; then
    echo "tags: rows with no citation outside the registry:"
    comm -13 <(cited) <(defined) | sed 's/^/  /'
fi

echo "tags: ok — $NDEF defined, $NCIT cited, all resolve ($UNCITED not yet cited)"
