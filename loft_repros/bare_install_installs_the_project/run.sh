#!/usr/bin/env bash
# Bare `loft install` installs THE PROJECT into ~/.loft/lib/, under the
# DIRECTORY's name rather than the manifest's `[package] name` — and it
# does not install the project's declared dependencies, which is what
# `loft api`'s own hint tells you to run it for.
#
# ⚠ Self-cleaning.
set -u
cd "$(dirname "$0")"
DIR=$(basename "$PWD")            # bare_install_installs_the_project
PKG=$(grep -m1 '^name' loft.toml | sed 's/.*"\(.*\)".*/\1/')   # instprobe

echo "package name in loft.toml : $PKG"
echo "directory name            : $DIR"
echo
echo "--- loft api names the fix for the missing dep ---"
loft api 2>&1 | grep -E "moros_map" | head -1
echo
echo "--- running exactly that command ---"
LOFT_TIMEOUT=180 loft install 2>&1 | tail -1
echo
echo "--- what it actually did ---"
[ -d ~/.loft/lib/"$DIR" ] && echo "  ~/.loft/lib/$DIR EXISTS   <- installed under the DIRECTORY name"
[ -d ~/.loft/lib/"$PKG" ] && echo "  ~/.loft/lib/$PKG EXISTS" || echo "  ~/.loft/lib/$PKG absent  <- the manifest name was NOT used"
echo -n "  dependency still: "; loft api 2>&1 | grep -E "moros_map" | head -1

rm -rf ~/.loft/lib/"$DIR" ~/.loft/lib/"$PKG"
echo "--- cleaned up ---"
