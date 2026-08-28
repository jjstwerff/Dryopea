#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Run dryopea's three gates detached, then report the VERDICT of the run
# that is (or was) going — and print the failures, not the log.
#
# ── Why this exists ─────────────────────────────────────────────
#
# `scripts/test.sh` takes minutes on a busy box (docs/TOOLCHAIN.md § The
# wall clock is not yours alone), and the obvious way to run it from an
# agent — start it in the background and keep grepping the log — costs a
# turn per poll and answers "still running" most of them.  It is also
# wrong three ways, all of which loft's `scripts/ci-run.sh` hit first and
# this is modelled on:
#
#   * a run that DIES — an OOM kill, a Ctrl-C, a `loft` hard-kill — never
#     writes a final line, so a waiter blocks for ever on a process that
#     is already gone;
#   * the PREVIOUS run's result is still in the log while the next one is
#     compiling, so grepping for it answers about the wrong run;
#   * a marker file left behind by a killed run means nothing.
#
# The fix is to record the run's own identity and check the PROCESS, not
# the log.  `.gate-verdict` holds one line — STATE PID EPOCH ELAPSED
# [detail] — and `status` re-reads the pid, so a run that vanished
# reports DIED rather than RUNNING.
#
# ── How to use it ───────────────────────────────────────────────
#
#   scripts/gate.sh start          # detach; returns at once
#   scripts/gate.sh wait           # exits ONCE, when a verdict exists
#   scripts/gate.sh status         # RUNNING / PASSED / FAILED / DIED
#   scripts/gate.sh report         # the failures from the last run
#
# ⚠⚠ **`wait` is the one to launch in the background from an agent.**
# The harness re-invokes on EXIT, so a `wait` that exits exactly once
# costs exactly one turn — where polling costs one per look.  A foreground
# wait blocks the session and the user with it.
#
# ⚠ `start` refuses a second run while one is going, which is CLAUDE.md
# § Do not run two `scripts/test.sh` at once enforced rather than
# documented: both pre-clean `tests/actual/`, so two runs clobber each
# other and fail for no reason.
#
# ── What it runs ────────────────────────────────────────────────
#
#   GATES=all        test + validate  (the default)
#   GATES=test       scripts/test.sh only
#   GATES=validate   scripts/validate.sh only
#   GATES=full       test + validate + validate-gl (needs xvfb)
#
# Logs land in .gate/<name>.log, which is gitignored.

set -uo pipefail
cd "$(dirname "$0")/.." || exit 1

# ⚠⚠ **The detached wrapper below is ONE single-quoted `bash -c` string,
# so an apostrophe anywhere inside it ENDS THE STRING** — including one
# in a comment.  It cost a `unexpected EOF while looking for matching`
# on the word *validate's*.  `bash -n scripts/gate.sh` catches it, and is
# worth running after any edit to the `start` branch.

V=.gate-verdict
D=.gate
GATES="${GATES:-all}"

# ⚠ 1500 s, not loft's 300 s default: dryopea's suite is close enough to
# the hard kill that a busy box trips it, and the message names a PARSE
# phase in an unrelated file and reads exactly like the cdylib fault
# (CLAUDE.md § The three gates).
export LOFT_TIMEOUT="${LOFT_TIMEOUT:-1500}"

mkdir -p "$D"

# ── The failure summary ─────────────────────────────────────────
#
# ⚠ The point of the whole script: a 15 000-line log is not a result.
# This prints the counts and the failing assertions and nothing else.
summarise() {
    if [ -f "$D/test.log" ]; then
        echo "── scripts/test.sh ───────────────────────────────"
        grep -E "^test result" "$D/test.log" || echo "  (no result line — the run did not finish)"
        grep -E "^  FAIL " "$D/test.log" | head -40
        grep -E "^FAULT |examples: " "$D/test.log" | head -10
    fi
    if [ -f "$D/validate.log" ]; then
        echo "── scripts/validate.sh ───────────────────────────"
        grep -E "^validate: " "$D/validate.log" || echo "  (no result line)"
        # ⚠ A failing scenario prints its name on the `── <name> ──`
        # banner and the reading several lines later, so pair them.
        awk '/^── /{n=$2} /FAILED at line/{print "  " n ": " $0}' \
            "$D/validate.log" | head -20
    fi
    if [ -f "$D/validate_gl.log" ]; then
        echo "── scripts/validate_gl.sh ────────────────────────"
        grep -E "^gl: |^validate" "$D/validate_gl.log" | tail -5
    fi
}

case "${1:-status}" in
  start)
    if [ -f $V ] && read -r st pid _ < $V && [ "$st" = RUNNING ] \
       && kill -0 "$pid" 2>/dev/null; then
        echo "already RUNNING (pid $pid) — refusing to start a second gate"
        echo "  (both runs pre-clean tests/actual/ and would clobber each other)"
        exit 1
    fi
    # ⚠⚠ **The verdict file is not the whole guard, and a killed run is
    # what showed it.**  Killing the wrapper writes KILLED, but the
    # `loft test` it launched keeps going — so a `start` that trusted the
    # verdict alone began a SECOND suite in this directory, which is the
    # clobber the guard exists to prevent (CLAUDE.md § Do not run two).
    # So look for the process too, and treat OUR cwd differently from
    # somebody else's.
    for p in $(pgrep -f "^loft test" 2>/dev/null); do
        cwd=$(readlink "/proc/$p/cwd" 2>/dev/null)
        [ -z "$cwd" ] && continue
        if [ "$cwd" = "$PWD" ]; then
            echo "a loft suite is already running HERE (pid $p) — refusing to start a second"
            echo "  it would clobber tests/actual/; kill it first:  kill $p"
            exit 1
        fi
        # ⚠ Another checkout's suite doubles the wall time and is not an
        # error — say so rather than refusing.
        echo "note: another loft suite is running in $cwd — expect ~2x wall time"
    done
    rm -f "$D"/*.log
    # Three ways this ends, and each writes a verdict so the waiter never
    # guesses: the gates exit 0 / non-zero, the run is killed by a signal
    # (rc > 128), or THIS wrapper is signalled (the trap writes KILLED).
    # Only SIGKILL escapes, which is why `status` re-reads the pid.
    setsid nohup bash -c '
      s=$(date +%s)
      note() { echo "$1 $$ $(date +%s) $(( $(date +%s) - s ))s $2" > '"$V"'; }
      for sg in TERM INT HUP QUIT; do
        trap "note KILLED \"the gate wrapper received SIG$sg\"; exit 1" $sg
      done
      rc=0
      case "'"$GATES"'" in
        all|test|full)
          scripts/test.sh > '"$D"'/test.log 2>&1 || rc=1 ;;
      esac
      case "'"$GATES"'" in
        all|validate|full)
          scripts/validate.sh > '"$D"'/validate.log 2>&1 || rc=1 ;;
      esac
      case "'"$GATES"'" in
        full)
          scripts/validate_gl.sh > '"$D"'/validate_gl.log 2>&1 || rc=1 ;;
      esac
      if   [ $rc -eq 0 ];   then note PASSED ""
      elif [ $rc -gt 128 ]; then note KILLED "a gate died on signal $((rc-128))"
      else
        # NOTE: no apostrophes anywhere in this wrapper body — it is one
        # single-quoted bash -c string, and one apostrophe ends it.
        # Only lines that ARE a failure.  The first version matched
        # "^examples: ", which is green far more often than not, so a
        # FAILED verdict carried "examples: ok" as its reason.  And
        # "  FAIL " comes FIRST because both gates embed DELIBERATELY
        # failing fixtures (validate has bad / b-bad as its own negative
        # controls), so a bare "FAILED at line" grep reports a passing
        # gate control as the verdict reason.
        det=$(grep -m1 -h "^  FAIL " '"$D"'/test.log 2>/dev/null | head -c 120)
        [ -z "$det" ] && det=$(grep -m1 -hE "^validate: FAILED|^examples: .*FAILED|^error" \
              '"$D"'/test.log '"$D"'/validate.log 2>/dev/null | head -c 120)
        note FAILED "$det"
      fi' >/dev/null 2>&1 &
    echo "RUNNING $! $(date +%s) 0s started GATES=$GATES" > $V
    echo "gate started (pid $!, GATES=$GATES) — \`scripts/gate.sh wait\` to be told once"
    ;;

  status)
    [ -f $V ] || { echo "NOT-STARTED"; exit 0; }
    read -r st pid epoch rest < $V
    if [ "$st" = RUNNING ] && ! kill -0 "$pid" 2>/dev/null; then
        # No verdict AND no process: every catchable end writes its own
        # verdict above, so reaching here really is the uncatchable case.
        echo "DIED after $(( $(date +%s) - epoch ))s — SIGKILL/uncatchable (OOM?); no verdict written"
        exit 2
    fi
    if [ "$st" = RUNNING ]; then
        echo "RUNNING for $(( $(date +%s) - epoch ))s"
        # ⚠ A progress line, because *how far in* is the one thing a
        # waiter legitimately wants and the verdict cannot carry.
        [ -f "$D/test.log" ] && echo "  test.sh: $(grep -c '^  ok    tests/' "$D/test.log") files done"
        exit 0
    fi
    echo "$st $rest"
    [ "$st" = PASSED ] && exit 0 || exit 1
    ;;

  wait|notify)
    # ⚠⚠ Launch THIS in the background from an agent: it exits exactly
    # once, the moment a verdict exists, so the harness re-invokes once.
    while :; do
        out=$("$0" status); rc=$?
        case "$out" in RUNNING*) sleep 15 ;; *) echo "$out"; summarise; exit $rc ;; esac
    done
    ;;

  report)
    summarise
    ;;

  *) echo "usage: scripts/gate.sh {start|status|wait|report}   [GATES=all|test|validate|full]"; exit 1 ;;
esac
