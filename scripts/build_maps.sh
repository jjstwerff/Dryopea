#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Build every authored map in maps/ from its `.keys` source —
# BACKLOG A2.
#
# ⚠ `maps/<name>.json` + `maps/<name>_markers.json` are BUILT
# artefacts that are COMMITTED, because `make play MAP=<name>` loads
# them and a fresh checkout has no builder run in it.  The `.keys`
# beside them is the source: it is what a reviewer reads and what a
# pull request diffs.
#
# This is NOT a gate — it WRITES repo content, which is why it is not
# in scripts/test.sh.  The gate is `tests/a2_the_maps.loft`, which
# replays every source and asserts the shipped pair still matches it.
#
# Usage:  scripts/build_maps.sh                # every map
#         scripts/build_maps.sh starter_01     # just that one
#         LOFT_BIN=/path/to/loft scripts/build_maps.sh

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

if ! command -v "$LOFT" >/dev/null 2>&1; then
    echo "ERROR: loft binary not found: $LOFT" >&2
    echo "Install loft, or set LOFT_BIN to override." >&2
    exit 2
fi

# A map source may carry a `snap`, and neither save_png nor the file
# writer creates parent directories.
mkdir -p "$ROOT"/shots

cd "$ROOT"

# ⚠ `--interpret`, for the same reason `scripts/validate.sh` is:
# the native backend answers an EMPTY palette for dryopea's JSON
# loaders and does it SILENTLY, which here would write out maps
# painted in nothing.  The palette precondition inside
# `map_build_all` is what stops that being silent either way.
exec "$LOFT" --interpret src/mapbuild_main.loft "$@"
