#!/usr/bin/env bash
# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# Run dryopea's test suite.
#
# Golden-image tests assert via `assert_golden` in src/golden.loft;
# loft's test runner reports the assertion as FAILED (since @P367
# landed), so this wrapper is just a thin convenience over
# `loft test`.  Refreshes tests/actual/ before each run so a stale
# PNG from a removed test can't masquerade as current.
#
# Dependencies resolve from the loft package registry (see
# loft.toml), so no `--lib` path is needed.
#
# Usage:  scripts/test.sh          # uses `loft` from PATH
#         LOFT_BIN=/path/to/loft scripts/test.sh

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOFT="${LOFT_BIN:-loft}"

if ! command -v "$LOFT" >/dev/null 2>&1; then
    echo "ERROR: loft binary not found: $LOFT" >&2
    echo "Install loft, or set LOFT_BIN to override." >&2
    exit 2
fi

# tests/actual/ is gitignored, so it is absent in a fresh checkout.
# Neither save_png nor the file writer creates parent directories —
# without this the writes go nowhere and every golden test fails as
# a "mismatch" against a file that was never written.
mkdir -p "$ROOT"/tests/actual

# Drop stale actuals so a vanished test can't leave a PNG or JSON
# behind from a previous run.
rm -f "$ROOT"/tests/actual/*.png "$ROOT"/tests/actual/*.json

cd "$ROOT"

# The worked-example gate — `docs/EXAMPLES.md`.  A text scan costing
# milliseconds against a suite costing ~177 s, so it runs FIRST: a
# dangling `Example:` citation is a two-second fix and finding it after
# three minutes of tests is three minutes wasted.
#
# ⚠ Its own `--self-test` is what stops it passing vacuously — with no
# citations in the repo it is green over an empty set, which is
# `plans/21` § R1's trap.
if ! bash "$ROOT"/scripts/examples.sh; then
    exit 1
fi

exec "$LOFT" test "$@"
