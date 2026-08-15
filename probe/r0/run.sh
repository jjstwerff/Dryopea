#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Plan 21 R0 — can a GL frame be GATED with no display?
#
# ⚠ This is not a test and is deliberately not in `scripts/test.sh`: it
# spawns an X server.  It is the probe whose answer made plan 21
# affordable, kept runnable so the claim can be checked rather than
# believed.  See docs/RENDERER.md § R0.
#
# Usage:  bash probe/r0/run.sh
#
# ⚠ bash, not sh — `set -o pipefail` is not POSIX and dash refuses it.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

command -v "$LOFT"    >/dev/null 2>&1 || { echo "ERROR: no loft binary"; exit 2; }
command -v xvfb-run   >/dev/null 2>&1 || { echo "ERROR: no xvfb-run — install xvfb"; exit 2; }

cd "$ROOT/probe/r0"
rm -f capture_out.png

echo "── 1. a GL context and a captured frame, with NO display ──"
xvfb-run -a "$LOFT" --interpret capture.loft 2>/dev/null | grep '^PROBE'

[ -s capture_out.png ] || { echo "FAIL: no PNG was written"; exit 1; }

echo
echo "── 2. decode it and classify EXACTLY, the way measure.loft does ──"
# ⚠ No xvfb here on purpose: the readback must not need a display, or the
# gate it stands for could not run in CI either.
"$LOFT" --interpret classify.loft 2>/dev/null | grep '^PROBE2'

echo
echo "⚠ The number that matters is 'other'.  ZERO means the round trip"
echo "  introduced no colour drift, so classify_world's EXACT lookup"
echo "  survives GL.  Anything above zero and docs/RENDERER.md § R4 is"
echo "  a harder problem than it says."
