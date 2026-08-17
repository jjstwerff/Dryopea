#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Plan 25 M3 — does a SHADER-drawn palette colour survive GL exactly?
#
# ⚠ R0 answered this for a canvas BLIT.  A fragment shader writing a
# float colour is a different path, and `measure.loft` classifies by an
# EXACT lookup — so one bit of drift makes M3's `other == 0` unwritable.
# Asked here, before the phase, on the real mesher and the real camera.
#
# Usage:  bash probe/m3/run.sh
#
# ⚠ bash, not sh — `set -o pipefail` is not POSIX and dash refuses it.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

command -v "$LOFT"  >/dev/null 2>&1 || { echo "ERROR: no loft binary"; exit 2; }
command -v xvfb-run >/dev/null 2>&1 || { echo "ERROR: no xvfb-run — install xvfb"; exit 2; }

cd "$ROOT/probe/m3"
rm -f capture_out.png

echo "── 1. mesh the ground, draw it through a shader, capture it ──"
xvfb-run -a "$LOFT" --interpret capture.loft 2>/dev/null | grep '^PROBE'

[ -s capture_out.png ] || { echo "FAIL: no PNG was written"; exit 1; }

echo
echo "── 2. classify it with dryopea's OWN instrument ──"
"$LOFT" --interpret classify.loft 2>/dev/null | grep '^PROBE2'

echo
echo "── 3. and is the drawn world the world the camera was POINTED at? ──"
# ⚠ The question that nearly produced a false alarm: the capture puts the
# rubble heap at (2, 2) ABOVE the wall_high hex at (0, 0), which looks
# like a lost y-negation until you remember `r` grows NORTH
# (`camera.loft`: pan north is `r += 1`).  Reasoning about it was wrong
# twice; measuring it settled it in one run — and this comparison is what
# plan 25 M3's LANDMARK gate grew out of.
"$LOFT" --interpret where.loft 2>/dev/null | grep '^WHERE'

echo
echo "⚠ The number that matters is 'drift' — unknown pixels that are NOT"
echo "  the clear colour.  ZERO means a shader-written palette colour"
echo "  comes back bit-exact, so plan 25 M3's gate can say other == 0"
echo "  over a frame with no background in it."
