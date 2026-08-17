#!/usr/bin/env bash
# Declaring a package as a PATH dependency SUPPRESSES the `--lib` search
# that would otherwise resolve it.  Rows 1 and 3 differ only by the
# `[dependencies]` block, and row 1 is the one that works.
set -u
cd "$(dirname "$0")"

BASE='[package]
name    = "consumer"
version = "0.1.0"
loft    = ">=0.8"

[library]
entry = "src/consumer.loft"
'
DEP='
[dependencies]
mylib = { path = "lib/mylib" }
'

case_run() {
    label="$1"; manifest="$2"; shift 2
    printf '%s' "$manifest" > loft.toml
    printf '%-40s ' "$label"
    LOFT_TIMEOUT=120 loft test "$@" tests/t2.loft 2>&1 \
        | grep -oE 'test result: (ok|FAILED)' | head -1
}

case_run "1. no dep,   --lib lib/"  "$BASE"      --lib lib/
case_run "2. no dep,   no flag"     "$BASE"
case_run "3. path dep, --lib lib/"  "$BASE$DEP"  --lib lib/
case_run "4. path dep, no flag"     "$BASE$DEP"
printf '%s' "$BASE$DEP" > loft.toml
