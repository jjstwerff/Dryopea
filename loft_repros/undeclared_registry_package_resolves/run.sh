#!/usr/bin/env bash
# An UNDECLARED registry package resolves, and `loft.lock` does not
# record what the build used.
#
# `loft.toml` beside this script has no `[dependencies]` section at all
# and there is no `loft.lock`, yet `use hex_grid;` resolves and runs.
set -u
cd "$(dirname "$0")"

echo "--- what loft thinks this project depends on ---"
loft api 2>&1 | sed -n '1,3p'

echo
echo "--- and what it compiles anyway ---"
loft src/consumer.loft 2>&1 | tail -3
