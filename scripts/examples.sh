#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# The worked-example gate.  `docs/EXAMPLES.md` is the convention; this
# is the half that keeps it from rotting.
#
# A public function is documented by the TESTS that show how to use
# it, cited by a stable marker:
#
#     src/…    // Example: fs001, fs004
#     tests/…  fn test_fs001_a_frame_loop_spends_its_backlog() {
#
# Three faults are always on, each a real way the convention rots:
#
#   dangling   a cited marker names no test
#   duplicate  two tests carry one marker
#   orphan     a marked test nobody cites
#
# and a fourth fires only for a file that opted in:
#
#   uncovered  a `pub fn` in an `#examples` file cites nothing
#
# ⚠ It does NOT require a public function to have an example unless
# the file opts in with `// #examples` in its header — the convention
# is for NEW work (project owner, 2026-08-17) and dryopea has 387
# public functions nobody is going to sweep.  The opt-in is a RATCHET:
# a gate red on 387 functions gets switched off, one that locks in each
# finished file can only go up.
#
# Usage:  scripts/examples.sh              # check the repo
#         scripts/examples.sh --self-test  # prove the checker can FAIL
#         scripts/examples.sh SRC TESTS    # check one pair of trees

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# A marker is a registered abbreviation plus three digits.  Kept as one
# pattern so the shape is stated once — `docs/EXAMPLES.md` § The
# abbreviations is where a new abbreviation is registered.
MARKER='[a-z][a-z0-9]{1,3}[0-9]{3}'

# ── the check ───────────────────────────────────────────────────────
#
# Answers 0 when the trees are consistent, 1 when they are not, and
# prints one line per fault.  Takes the two trees so `--self-test` can
# point it at fixtures.
check_trees() {
    ct_src="$1"
    ct_tests="$2"
    ct_faults=0

    # Every marker CITED by a source comment.
    #   src/play.loft:412:// Example: fs001, fs004
    #
    # ⚠ `Example: none — <reason>` is an EXEMPTION, not a citation, so
    # it is dropped here and kept by the coverage pass below.  Reading
    # it as a citation would make every exempted function look
    # documented by a test that does not exist.
    ct_cited=$(grep -r --exclude-dir='.*' -hE "^[[:space:]]*//[[:space:]]*Example:" "$ct_src" 2>/dev/null \
               | grep -vE "^[[:space:]]*//[[:space:]]*Example:[[:space:]]*none\b" \
               | grep -oE "$MARKER" | sort -u)

    # Every marker DEFINED by a test function name.
    ct_defined=$(grep -r --exclude-dir='.*' -hoE "^[[:space:]]*(pub )?fn test_${MARKER}_" \
                      "$ct_tests" 2>/dev/null \
                 | grep -oE "$MARKER" | sort)

    # 1. duplicate — two tests carry one marker, so a citation is
    # ambiguous and the reader follows it to whichever they find first.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        echo "FAULT duplicate: marker '$ct_m' names more than one test"
        grep -r --exclude-dir='.*' -nE "fn test_${ct_m}_" "$ct_tests" 2>/dev/null | sed 's/^/    /'
        ct_faults=$((ct_faults + 1))
    done < <(echo "$ct_defined" | uniq -d)

    ct_defined_u=$(echo "$ct_defined" | sort -u)

    # 2. dangling — a citation that resolves to nothing.  This is what
    # a rename leaves behind, and it is the failure the whole gate
    # exists for: the comment still reads correct.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        if ! echo "$ct_defined_u" | grep -qx "$ct_m"; then
            echo "FAULT dangling: '$ct_m' is cited but names no test"
            grep -r --exclude-dir='.*' -nE "Example:.*${ct_m}" "$ct_src" 2>/dev/null | sed 's/^/    /'
            ct_faults=$((ct_faults + 1))
        fi
    done < <(echo "$ct_cited")

    # 3. orphan — a marked test nobody cites.  The test is fine; what
    # is wrong is that it wears a contract nobody holds, so a later
    # reader treats it as load-bearing for a citation that is gone.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        if ! echo "$ct_cited" | grep -qx "$ct_m"; then
            echo "FAULT orphan: test marked '$ct_m' is cited by nothing"
            grep -r --exclude-dir='.*' -nE "fn test_${ct_m}_" "$ct_tests" 2>/dev/null | sed 's/^/    /'
            ct_faults=$((ct_faults + 1))
        fi
    done < <(echo "$ct_defined_u")

    # 4. coverage, and ONLY for a file that opted in with `#examples`.
    #
    # ⚠ A `pub fn` in an opted-in file must cite a marker or say why
    # not (`// Example: none — <reason>`).  The exemption is written
    # down rather than silent: an exemption a reader disagrees with is
    # an argument they can have.
    while read -r ct_f; do
        [ -z "$ct_f" ] && continue
        grep -qE '^[[:space:]]*//[[:space:]]*#examples' "$ct_f" || continue
        # Walk the file: remember the last `Example:` seen, and check
        # it was within the comment block immediately above the `pub
        # fn`.  A blank line or a `}` ends a block, so a citation
        # cannot drift down the file and cover an unrelated function.
        awk -v file="$ct_f" -v marker="$MARKER" '
            /^[[:space:]]*\/\/[[:space:]]*Example:/ { seen = 1; next }
            /^[[:space:]]*\/\// { next }
            /^[[:space:]]*$/ { seen = 0; next }
            /^pub fn / {
                if (!seen) {
                    name = $0
                    sub(/^pub fn /, "", name)
                    sub(/\(.*$/, "", name)
                    printf "FAULT uncovered: %s:%d %s() has no `Example:` line " \
                           "(file opted in with #examples)\n", file, NR, name
                    bad++
                }
                seen = 0
                next
            }
            { seen = 0 }
            END { exit(bad ? 1 : 0) }
        ' "$ct_f" || ct_faults=$((ct_faults + 1))
    done < <(find "$ct_src" -type f -name '*.loft' \
             -not -path '*/.*' 2>/dev/null | sort)

    return $((ct_faults > 0 ? 1 : 0))
}

# ── the self-test ───────────────────────────────────────────────────
#
# ⚠⚠ The gate would otherwise pass VACUOUSLY.  On the day it landed
# there was not one `Example:` line in the repo, so it was green over
# an empty set — which is `plans/21` § R1's trap exactly, where a
# camera gate reported perfect agreement twice while iterating over
# nothing.  The generic control is *can this gate produce a
# non-trivial reading at all?* and for a linter that is a fixture per
# failure mode.
self_test() {
    st_fix="$ROOT/scripts/examples_fixtures"
    st_fail=0

    # Each fixture is a (src, tests) pair whose name says which fault
    # it must produce.  `ok` must produce NONE — without it the whole
    # self-test is satisfied by a checker that always fails.
    for st_case in ok dangling duplicate orphan uncovered; do
        st_out=$(check_trees "$st_fix/$st_case/src" "$st_fix/$st_case/tests" 2>&1)
        st_rc=$?
        if [ "$st_case" = "ok" ]; then
            if [ $st_rc -ne 0 ]; then
                echo "SELF-TEST FAIL: the clean fixture reported a fault"
                echo "$st_out" | sed 's/^/    /'
                st_fail=1
            else
                echo "  ok         clean fixture is clean"
            fi
        else
            if [ $st_rc -eq 0 ]; then
                echo "SELF-TEST FAIL: the '$st_case' fixture was not caught —" \
                     "this gate cannot see the fault it names"
                st_fail=1
            elif ! echo "$st_out" | grep -q "FAULT $st_case"; then
                echo "SELF-TEST FAIL: the '$st_case' fixture failed for the" \
                     "WRONG reason:"
                echo "$st_out" | sed 's/^/    /'
                st_fail=1
            else
                echo "  ok         $st_case fires"
            fi
        fi
    done

    if [ $st_fail -ne 0 ]; then
        echo "examples: SELF-TEST FAILED" >&2
        return 1
    fi
    echo "examples: self-test green — all four faults fire, and the clean"\
         "fixture is clean"
    return 0
}

# ── entry ───────────────────────────────────────────────────────────

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit $?
fi

SRC="${1:-$ROOT/src}"
TESTS="${2:-$ROOT/tests}"

OUT=$(check_trees "$SRC" "$TESTS" 2>&1)
RC=$?
if [ $RC -ne 0 ]; then
    echo "$OUT" >&2
    echo "" >&2
    echo "examples: the worked-example gate FAILED — see docs/EXAMPLES.md" >&2
    exit 1
fi

# ⚠ The SAME two filters `check_trees` uses, and it matters: an
# exemption's reason may legitimately name a marker ("shown by xy001"),
# so a count that skipped the `none` filter would report more citations
# than the resolver actually holds.  The fixture does exactly that.
CITED=$(grep -r --exclude-dir='.*' -hE "^[[:space:]]*//[[:space:]]*Example:" "$SRC" 2>/dev/null \
        | grep -vE "^[[:space:]]*//[[:space:]]*Example:[[:space:]]*none\b" \
        | grep -oE "$MARKER" | sort -u | wc -l | tr -d ' ')
OPTED=$(grep -r --exclude-dir='.*' -lE '^[[:space:]]*//[[:space:]]*#examples' "$SRC"/*.loft 2>/dev/null \
        | wc -l | tr -d ' ')
echo "examples: ok — $CITED citation(s), $OPTED file(s) opted in"
