#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# The worked-example gate.  `docs/EXAMPLES.md` is the convention; this
# is the half that keeps it from rotting.
#
# A public function is documented by the TESTS that show how to use it,
# cited by an INDEX TAG in loft's `@XXX-###` form — the same family as
# `@P367` / `@X072`, scanned by the same indexer:
#
#     tests/…   // @FIX-001 — a frame loop spends its backlog
#               fn test_a_frame_loop_spends_its_backlog() {
#
#     src/…     // Example: @FIX-001, @FIX-004
#               pub fn clock_advance(…)
#
# Three faults are always on, each a real way the convention rots:
#
#   dangling   a cited tag names no test
#   duplicate  two tests carry one tag
#   orphan     a tagged test nobody cites
#
# and a fourth fires only for a file that opted in:
#
#   uncovered  a `pub fn` in an `#examples` file cites nothing
#
# ⚠ It does NOT require a public function to have an example unless the
# file opts in with `// #examples` in its header — the convention is for
# NEW work (project owner, 2026-08-17) and dryopea has 387 public
# functions nobody is going to sweep.  The opt-in is a RATCHET: a gate
# red on 387 functions gets switched off, one that locks in each
# finished file can only go up.
#
# ⚠⚠ THE TAG NAMESPACE IS THE ECOSYSTEM'S, NOT THIS REPO'S (project
# owner, 2026-08-17): the indexer covers the registered libraries too,
# so an abbreviation is claimed globally and `@FIX-001` must mean one
# test everywhere.  This script can only enforce that over the trees it
# is given — see `EXAMPLES_TEST_ROOTS`.
#
# Usage:  scripts/examples.sh              # check the repo
#         scripts/examples.sh --self-test  # prove the checker can FAIL
#         scripts/examples.sh SRC TESTS    # check one pair of trees
#
#         EXAMPLES_TEST_ROOTS="/a/tests:/b/tests" scripts/examples.sh
#             resolve citations against extra test trees as well — a
#             registered library's tests, so a consumer may cite the
#             library's own worked example instead of copying it.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# A tag is `@` + a THREE-LETTER acronym + `-` + three digits.
#
# ⚠ Exactly three letters (project owner, 2026-08-17): `@DRY-001`,
# `@FIX-001`.  Two-letter codes read as noise beside `@X072` and give a
# thin namespace for an ecosystem-wide registry; four start competing
# with the word they abbreviate.
#
# ⚠ The hyphen is what keeps it clear of `@X072` / `@M013` / `@D002` /
# `@P367`, which have none — so one indexer carries both families
# without either shadowing the other.  `docs/EXAMPLES.md` § The
# abbreviations is where an abbreviation is registered, and ⚠ that
# registry is ECOSYSTEM-WIDE: two libraries claiming `@FIX-` collide in
# the index even though neither repo can see the other.
TAG='@[A-Z]{3}-[0-9]{3}'

# ⚠⚠ `@XXX-###` is the SPECIMEN abbreviation and is ignored everywhere.
#
# A document that TEACHES this convention has to show the shape, and a
# shape shown in prose is not a claim that a test exists.  Without a
# reserved spelling the gate reads `docs/EXAMPLES.md`'s own examples as
# citations and reports them dangling — measured, the moment prose
# citations were admitted.
#
# ⚠ So the doc writes `@XXX-001`, which is never a real tag, and every
# other acronym means exactly what it says.
SPECIMEN='@XXX-[0-9]{3}'

# ── the file list ───────────────────────────────────────────────────
#
# Every `.loft` file under a tree, skipping hidden directories BELOW the
# root.
#
# ⚠⚠ **The root's own ancestors must not matter, and getting that wrong
# is how this gate silently scans NOTHING.**  `grep -r --exclude-dir='.*'`
# was the first spelling, and GNU grep applies `--exclude-dir` to the
# command-line directory too — so a checkout under any hidden path
# (`~/.local/src/…`, a CI workspace under `.cache/`, and every
# `~/.loft/registry/…` library) matched zero files while the gate
# reported a cheerful `ok — 0 citation(s)`.  That is the vacuous pass
# this whole script exists to prevent, arriving inside it.  Measured:
# one fixture read 5 lines under `/tmp` and 0 under `~/.cache`.
#
# ⚠ The registry case is why this is load-bearing rather than tidy —
# `~/.loft/` is hidden, so the ecosystem scope above would have been
# scanned as an empty set for ever.
#
# So the traversal runs FROM the root, where `./.*` can only mean a
# hidden directory inside the tree.  `examples_fixtures/.hidden/` is the
# fixture that keeps it that way, and it is asserted on its COUNT rather
# than on its cleanliness — a tree nobody opened is perfectly clean.
list_loft() {
    ll_root="${1%/}"
    ( cd "$ll_root" 2>/dev/null \
      && find . -type f -name '*.loft' -not -path './.*' -printf '%P\n' ) \
    | sed "s|^|$ll_root/|"
}

# Grep every `.loft` file under a tree.  ONE door, so no call site can
# reintroduce a recursive scan with its own exclusion rules.
scan() {
    sc_root="$1"; shift
    list_loft "$sc_root" | xargs -r -d '\n' grep "$@" 2>/dev/null
}

# ── what a tree cites, and what it defines ──────────────────────────

# Tags CITED by source comments, one per line, sorted unique.
#
# ⚠ `Example: none — <reason>` is an EXEMPTION, not a citation: dropped
# here and honoured by the coverage pass.  Reading it as a citation
# would make every exempted function look documented by a test that does
# not exist — and a reason may legitimately name a tag ("shown by
# @TST-001"), which the fixture does on purpose.
cited_tags() {
    {
        scan "$1" -hE "^[[:space:]]*//[[:space:]]*Example:" \
        | grep -vE "^[[:space:]]*//[[:space:]]*Example:[[:space:]]*none\b"
        cited_in_prose
    } | grep -oE "$TAG" | grep -vE "^$SPECIMEN$" | sort -u
}

# Tags referenced from DOCUMENTATION — `EXAMPLES_CITE_ROOTS`.
#
# ⚠⚠ **Not every worked example is a function's.**  A first-class
# program tags a test because the ALGORITHM is worth reading, not
# because it documents an API (project owner, 2026-08-17) — and such a
# tag has no `pub fn` to cite it.  Without this, the `orphan` rule
# would call every one of them dead and go red on exactly the use the
# tag family is being widened for.
#
# ⚠ So a citation is *any* mention in prose: `see @DRY-012` in a doc or
# a plan counts.  That keeps `orphan` meaning what it should — a tag
# nothing anywhere points at — rather than *a tag no function points
# at*.
cited_in_prose() {
    cip_roots="${EXAMPLES_CITE_ROOTS:-}"
    [ -z "$cip_roots" ] && return 0
    while IFS= read -r cip_r; do
        [ -z "$cip_r" ] && continue
        [ -d "$cip_r" ] || continue
        ( cd "$cip_r" && find . -type f -not -path './.*' -print0 ) \
        | ( cd "$cip_r" && xargs -r -0 grep -hoE "$TAG" 2>/dev/null )
    done < <(echo "$cip_roots" | tr ':' '\n')
}

# Tags DEFINED by a test, as `tag<TAB>file:line<TAB>fn-name`.
#
# ⚠ A tag binds to the `fn` that FOLLOWS it, within one comment block.
# A blank line breaks the block, so a tag in a file header cannot drift
# down and claim an unrelated test — the same rule the coverage pass
# uses for `pub fn`, and for the same reason.
defined_tags() {
    while read -r dt_f; do
        [ -z "$dt_f" ] && continue
        awk -v file="$dt_f" -v tg="$TAG" '
            match($0, "^[[:space:]]*//[[:space:]]*" tg) {
                m = substr($0, RSTART, RLENGTH)
                sub(/^[[:space:]]*\/\/[[:space:]]*/, "", m)
                pend = m; pline = NR; next
            }
            /^[[:space:]]*\/\// { next }
            /^[[:space:]]*$/ { pend = ""; next }
            /^[[:space:]]*(pub )?fn / {
                if (pend != "") {
                    name = $0
                    sub(/^[[:space:]]*(pub )?fn /, "", name)
                    sub(/\(.*$/, "", name)
                    printf "%s\t%s:%d\t%s\n", pend, file, pline, name
                }
                pend = ""; next
            }
            { pend = "" }
        ' "$dt_f"
    done < <(list_loft "$1")
}

# The same, over the primary test tree plus any `EXAMPLES_TEST_ROOTS`.
all_defined_tags() {
    defined_tags "$1"
    at_extra="${EXAMPLES_TEST_ROOTS:-}"
    [ -z "$at_extra" ] && return 0
    while IFS= read -r at_r; do
        [ -z "$at_r" ] && continue
        [ -d "$at_r" ] || continue
        defined_tags "$at_r"
    done < <(echo "$at_extra" | tr ':' '\n')
}

# ── the check ───────────────────────────────────────────────────────
#
# Answers 0 when the trees are consistent, 1 when they are not, and
# prints one line per fault.
check_trees() {
    ct_src="$1"
    ct_tests="$2"
    ct_faults=0

    ct_cited=$(cited_tags "$ct_src")
    ct_defs=$(all_defined_tags "$ct_tests")
    ct_defined=$(echo "$ct_defs" | cut -f1 | grep -v '^$' | sort)
    ct_defined_u=$(echo "$ct_defined" | sort -u)

    # 1. duplicate — two tests carry one tag, so a citation is ambiguous
    # and the reader follows it to whichever they find first.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        echo "FAULT duplicate: tag '$ct_m' names more than one test"
        echo "$ct_defs" | awk -F'\t' -v m="$ct_m" '$1 == m { print "    " $2 "  " $3 }'
        ct_faults=$((ct_faults + 1))
    done < <(echo "$ct_defined" | uniq -d)

    # 2. dangling — a citation that resolves to nothing.  What a DELETED
    # test leaves behind, and the failure the whole gate exists for: the
    # comment still reads correct.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        if ! echo "$ct_defined_u" | grep -qxF "$ct_m"; then
            echo "FAULT dangling: '$ct_m' is cited but names no test"
            scan "$ct_src" -HnE "Example:.*${ct_m}" | sed 's/^/    /'
            ct_faults=$((ct_faults + 1))
        fi
    done < <(echo "$ct_cited")

    # 3. orphan — a tagged test nobody cites.  The test is fine; what is
    # wrong is that it wears a contract nobody holds, so a later reader
    # treats it as load-bearing for a citation that is gone.
    #
    # ⚠ Asked only of the PRIMARY tree.  A registered library's tests
    # are cited by their own repo, and calling those orphans here would
    # make every consumer red for its dependencies' business.
    while read -r ct_m; do
        [ -z "$ct_m" ] && continue
        if ! echo "$ct_cited" | grep -qxF "$ct_m"; then
            echo "FAULT orphan: test tagged '$ct_m' is cited by nothing"
            echo "$ct_defs" | awk -F'\t' -v m="$ct_m" '$1 == m { print "    " $2 "  " $3 }'
            ct_faults=$((ct_faults + 1))
        fi
    done < <(defined_tags "$ct_tests" | cut -f1 | sort -u | grep -v '^$')

    # 4. coverage, and ONLY for a file that opted in with `#examples`.
    #
    # ⚠ A `pub fn` in an opted-in file must cite a tag or say why not
    # (`// Example: none — <reason>`).  The exemption is written down
    # rather than silent: one a reader disagrees with is an argument
    # they can have.
    while read -r ct_f; do
        [ -z "$ct_f" ] && continue
        grep -qE '^[[:space:]]*//[[:space:]]*#examples' "$ct_f" || continue
        awk -v file="$ct_f" '
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
                seen = 0; next
            }
            { seen = 0 }
            END { exit(bad ? 1 : 0) }
        ' "$ct_f" || ct_faults=$((ct_faults + 1))
    done < <(list_loft "$ct_src" | sort)

    return $((ct_faults > 0 ? 1 : 0))
}

# How many distinct tags a tree CITES — the positive reading the
# hidden-root control needs.
count_cited() {
    cited_tags "$1" | grep -c . || true
}

# ── the self-test ───────────────────────────────────────────────────
#
# ⚠⚠ The gate would otherwise pass VACUOUSLY.  On the day it landed
# there was not one `Example:` line in the repo, so it was green over an
# empty set — `plans/21` § R1's trap exactly, where a camera gate
# reported perfect agreement twice while iterating over nothing.  The
# generic control is *can this gate produce a non-trivial reading at
# all?*, and for a linter that is a fixture per failure mode.
self_test() {
    st_fix="$ROOT/scripts/examples_fixtures"
    st_fail=0

    # `ok` must produce NO fault — without it the whole self-test is
    # satisfied by a checker that always fails.
    for st_case in ok dangling duplicate orphan uncovered; do
        st_out=$(EXAMPLES_TEST_ROOTS="" EXAMPLES_CITE_ROOTS="" check_trees \
                     "$st_fix/$st_case/src" "$st_fix/$st_case/tests" 2>&1)
        st_rc=$?
        if [ "$st_case" = "ok" ]; then
            if [ $st_rc -ne 0 ]; then
                echo "SELF-TEST FAIL: the clean fixture reported a fault"
                echo "$st_out" | sed 's/^/    /'
                st_fail=1
            else
                echo "  ok         clean fixture is clean"
            fi
        elif [ $st_rc -eq 0 ]; then
            echo "SELF-TEST FAIL: the '$st_case' fixture was not caught —" \
                 "this gate cannot see the fault it names"
            st_fail=1
        elif ! echo "$st_out" | grep -q "FAULT $st_case"; then
            echo "SELF-TEST FAIL: the '$st_case' fixture failed for the WRONG reason:"
            echo "$st_out" | sed 's/^/    /'
            st_fail=1
        else
            echo "  ok         $st_case fires"
        fi
    done

    # ⚠⚠ Asserted on a COUNT rather than on cleanliness: a tree under a
    # hidden ancestor must read exactly as the same tree elsewhere.  "No
    # faults" cannot see this — a tree the scanner never opened has none
    # either.  ⚠ And every registry library lives under `~/.loft`, so
    # this is the ecosystem scope's gate, not a curiosity.
    st_a=$(EXAMPLES_CITE_ROOTS="" count_cited "$st_fix/ok/src")
    st_b=$(EXAMPLES_CITE_ROOTS="" count_cited "$st_fix/.hidden/ok/src")
    if [ "$st_a" -eq 0 ]; then
        echo "SELF-TEST FAIL: the clean fixture cites nothing, so the hidden-root"\
             "comparison proves nothing"
        st_fail=1
    elif [ "$st_a" != "$st_b" ]; then
        echo "SELF-TEST FAIL: a hidden ancestor changed the reading —" \
             "$st_a citation(s) visible, $st_b under a dot-directory." \
             "The scan is excluding its own root."
        st_fail=1
    else
        echo "  ok         a hidden ancestor does not hide the tree ($st_a cited)"
    fi

    # ⚠ And a citation resolving into ANOTHER tree — the registered
    # library case.  `crossref/src` cites a tag defined only in
    # `ok/tests`, so it must be dangling alone and clean with the extra
    # root.  Both halves: without the negative the feature is satisfied
    # by a checker that resolves everything.
    st_out=$(EXAMPLES_TEST_ROOTS="" EXAMPLES_CITE_ROOTS="" check_trees \
                 "$st_fix/crossref/src" "$st_fix/crossref/tests" 2>&1)
    if ! echo "$st_out" | grep -q "FAULT dangling"; then
        echo "SELF-TEST FAIL: a cross-tree citation resolved with no extra root," \
             "so EXAMPLES_TEST_ROOTS proves nothing"
        st_fail=1
    else
        st_out2=$(EXAMPLES_TEST_ROOTS="$st_fix/ok/tests" EXAMPLES_CITE_ROOTS="" check_trees \
                      "$st_fix/crossref/src" "$st_fix/crossref/tests" 2>&1)
        if [ $? -ne 0 ]; then
            echo "SELF-TEST FAIL: a cross-tree citation did not resolve against"\
                 "EXAMPLES_TEST_ROOTS:"
            echo "$st_out2" | sed 's/^/    /'
            st_fail=1
        else
            echo "  ok         a citation resolves into a registered library's tests"
        fi
    fi

    # ⚠⚠ And a tag cited only from PROSE — a first-class program's
    # algorithm example, which has no `pub fn` to cite it.  Without the
    # doc root it must read as an ORPHAN; with it, clean.  Both halves,
    # or the feature is satisfied by a checker that never calls anything
    # an orphan.
    st_out=$(EXAMPLES_TEST_ROOTS="" EXAMPLES_CITE_ROOTS="" check_trees \
                 "$st_fix/docsref/src" "$st_fix/docsref/tests" 2>&1)
    if ! echo "$st_out" | grep -q "FAULT orphan"; then
        echo "SELF-TEST FAIL: a prose-only tag was not an orphan without its doc" \
             "root, so EXAMPLES_CITE_ROOTS proves nothing"
        st_fail=1
    else
        st_out2=$(EXAMPLES_TEST_ROOTS="" EXAMPLES_CITE_ROOTS="$st_fix/docsref/docs" \
                      check_trees "$st_fix/docsref/src" "$st_fix/docsref/tests" 2>&1)
        if [ $? -ne 0 ]; then
            echo "SELF-TEST FAIL: an algorithm tag cited from prose still read as"\
                 "a fault:"
            echo "$st_out2" | sed 's/^/    /'
            st_fail=1
        else
            echo "  ok         an algorithm tag cited from prose is not an orphan"
        fi
    fi

    if [ $st_fail -ne 0 ]; then
        echo "examples: SELF-TEST FAILED" >&2
        return 1
    fi
    echo "examples: self-test green — four faults fire, the clean fixture is" \
         "clean, a hidden root reads the same as a visible one, and a" \
         "cross-tree citation resolves only when its tree is given"
    return 0
}

# ── entry ───────────────────────────────────────────────────────────

if [ "${1:-}" = "--self-test" ]; then
    self_test
    exit $?
fi

SRC="${1:-$ROOT/src}"
TESTS="${2:-$ROOT/tests}"

# ⚠ Default only — the self-test overrides it to "" so a fixture is
# never rescued by a tag that happens to appear in this repo's prose.
export EXAMPLES_CITE_ROOTS="${EXAMPLES_CITE_ROOTS-$ROOT/docs:$ROOT/plans}"

OUT=$(check_trees "$SRC" "$TESTS" 2>&1)
if [ $? -ne 0 ]; then
    echo "$OUT" >&2
    echo "" >&2
    echo "examples: the worked-example gate FAILED — see docs/EXAMPLES.md" >&2
    exit 1
fi

CITED=$(count_cited "$SRC")
OPTED=$(scan "$SRC" -lE '^[[:space:]]*//[[:space:]]*#examples' | wc -l | tr -d ' ')
echo "examples: ok — $CITED citation(s), $OPTED file(s) opted in"
