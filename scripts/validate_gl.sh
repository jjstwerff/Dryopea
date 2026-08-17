#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Draw every fixture in tests/gl/ through a real GL context and gate on
# what the frame CONTAINS — plan 25 phase M3.
#
# ⚠⚠ This is dryopea's THIRD gate and it is deliberately separate from
# scripts/validate.sh.  Plan 21 § R4 said the GL scenarios should join
# that sweep; taken literally that puts all 33 existing scripts behind an
# X server, and docs/RENDERER.md § R0's probe went out of its way to
# prove the readback does NOT need a display.  Keeping them apart is what
# lets a machine with no xvfb still run the gate it can.
#
# Usage:  scripts/validate_gl.sh                # every fixture
#         scripts/validate_gl.sh the-ground     # just that one
#         LOFT_BIN=/path/to/loft scripts/validate_gl.sh

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

if ! command -v "$LOFT" >/dev/null 2>&1; then
    echo "ERROR: loft binary not found: $LOFT" >&2
    echo "Install loft, or set LOFT_BIN to override." >&2
    exit 2
fi

# ⚠ A clear message rather than a GL failure twelve seconds later: with
# no X server `gl_create_window` answers false, and the gate's own
# precondition then says so — but only after paying the whole compile.
if ! command -v xvfb-run >/dev/null 2>&1; then
    echo "ERROR: xvfb-run not found — this gate needs a GL context." >&2
    echo "Install xvfb (Debian/Ubuntu: apt install xvfb)." >&2
    echo "The headless gate is unaffected: scripts/validate.sh" >&2
    exit 2
fi

mkdir -p "$ROOT"/shots

# Drop last run's captures, the way validate.sh does.  ⚠ It matters more
# here: this gate DECODES the PNG it just wrote, so a stale one from a
# previous run would be classified as if it were current the moment
# gl_screenshot silently failed.
rm -f "$ROOT"/shots/gl-*.png

cd "$ROOT"

# ⚠ `--interpret`, for validate.sh's reason: the native backend answers
# 0 entries from `load_palette` and would measure an empty world.
exec xvfb-run -a "$LOFT" --interpret src/gl_gate_main.loft "$@"
