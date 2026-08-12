#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Play every `.keys` script in tests/scripts/ and gate on what they
# measure — plan 08 phase V4.
#
# This is dryopea's SECOND gate and it is deliberately not part of
# scripts/test.sh: the unit suite stays fast and hermetic, and this
# one plays the game.  Each script prints every measurement it takes
# beside the band it wanted; the first reading out of band ends that
# script's run, and any failed script makes this exit non-zero.
#
# The pictures land in shots/ (gitignored, rewritten every run), one
# per `snap` line, so a red gate can be looked at rather than only
# read about.
#
# Usage:  scripts/validate.sh                  # every script
#         scripts/validate.sh paint-a-base     # just that one
#         LOFT_BIN=/path/to/loft scripts/validate.sh

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

if ! command -v "$LOFT" >/dev/null 2>&1; then
    echo "ERROR: loft binary not found: $LOFT" >&2
    echo "Install loft, or set LOFT_BIN to override." >&2
    exit 2
fi

# shots/ is gitignored, so it is absent in a fresh checkout — and
# neither save_png nor the file writer creates parent directories.
# The scripts' own `snap` calls mkdir_all too; this is here so the
# pre-clean below has something to clean.
mkdir -p "$ROOT"/shots

# Drop last run's artefacts, the way scripts/test.sh drops
# tests/actual/.  A stale PNG from a script that has since been
# renamed reads as current evidence, and a stale save file makes
# round-trip's reload pass over a world it never wrote.
rm -f "$ROOT"/shots/*.png "$ROOT"/shots/*.json

cd "$ROOT"

# ⚠ `--interpret`, deliberately, for the same reason `make play`
# runs interpreted: the native backend miscompiles dryopea's JSON
# loaders today, and it does it SILENTLY — `load_palette` answers
# 0 entries natively and 11 interpreted.  A gate running on that
# backend measures an empty world and says so in eleven different
# ways.  See QUESTIONS_FOR_LOFT.md § "Native backend returns an
# EMPTY vector for a `text as vector<Struct>` cast".  Drop the flag
# when the upstream fix lands; the palette precondition inside
# validate_all is what stops this being silent either way.
exec "$LOFT" --interpret src/validate_main.loft "$@"
