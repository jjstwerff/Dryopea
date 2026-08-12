# Copyright (c) 2026 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# ==== What can this Makefile do for you? ================================
#
# If you just want to try things:
#
#   make play       Launch the interactive editor (E1-live) in a 960x720
#                   GL window.  Loads dryopea_save.json from cwd if
#                   present; auto-saves on exit.  WASD pan, scroll zoom,
#                   1-9 0 - = palette select, Ctrl+S save, Esc exit.
#
#   make play MAP=starter_01
#                   Same, but edits the named map at
#                   maps/starter_01.json + maps/starter_01_markers.json
#                   instead of the default single-slot save.  The maps/
#                   directory is auto-created on first save.
#
#   make test       Run the dryopea test suite via scripts/test.sh.
#                   Refreshes tests/actual/ first so stale artefacts
#                   can't masquerade as current.  ~15-20 seconds —
#                   most of it plan 08's frame measurement, which
#                   classifies every pixel of a full 960x720 frame.
#
#   make validate   Play every tests/scripts/*.keys script and gate on
#                   what they measure.  The SECOND gate — it plays the
#                   game, where `make test` tests the pieces.  Every
#                   measurement prints beside the band it wanted, a
#                   reading out of band exits non-zero, and the
#                   pictures land in shots/ to look at.  ~11 seconds.
#
#   make validate SCRIPT=paint-a-base
#                   Just that one script, for iterating on a scenario.
#
#   make help       Print this overview again.
#
# If you are working on dryopea itself:
#
#   make check FILE=src/<file>.loft
#                   Parse-check a single .loft file without running it.
#                   Equivalent to `loft --native-emit /tmp/x.rs …`.
#                   Quick syntax/type sanity for an edit in progress.
#                   ⚠ Worth running on src/main.loft and
#                   src/validate_main.loft by hand: both sit outside
#                   the aggregator, so the test suite never compiles
#                   them.  Everything they could get wrong lives in
#                   src/editor_step.loft / src/validate.loft, which it
#                   does.
#
#   make clean      Wipe tests/actual/ and shots/ plus the cwd save file
#                   (dryopea_save.json), so the next launch starts cold.
#
# Tunables (env or `make VAR=…`):
#
#   LOFT_BIN        The loft binary.  Default: `loft` from PATH.
#                   Libraries resolve from the package registry, so
#                   there is no stdlib path to set.
#
# Every target above is defined as a real rule later in this file.
# Scroll down to any name to see exactly what it does.
# =========================================================================

# dryopea runs on the INSTALLED loft; its libraries resolve from the
# package registry via loft.toml + loft.lock, so no --lib path is passed.
LOFT_BIN  ?= loft

.PHONY: help play play-native test validate check clean

# ── Help ─────────────────────────────────────────────────────────

# Print the overview at the top of this file.  Useful when you land on
# a fresh checkout and want to know what buttons are available without
# reading the whole Makefile.
help:
	@sed -n '/^# ==== What can this Makefile do for you/,/^# ====/p' Makefile \
	  | sed 's/^# \{0,1\}//'

# ── Common-use targets ───────────────────────────────────────────

# Launch the interactive editor.  Fails fast with a clear message if
# the loft binary is missing — install loft, or set LOFT_BIN.  Pass
# `MAP=<name>` to edit a named map under maps/ instead of the default
# single-slot save.
#
# WORKAROUND: runs in `--interpret` mode.  The reason CHANGED on
# 2026-08-12 — the old one (native codegen panicking on a
# hash-bearing struct return) is fixed and verified.  What blocks
# native now is loft-lang/loft#866: `text as vector<Struct>` in
# tail-return position silently answers [], so `load_palette` reads
# 0 entries and the native editor opens with an empty palette and
# cannot paint at all.  It does not crash — it just does nothing,
# which is worse.  Drop `--interpret` when #866 ships fixed.
play:
	@command -v $(LOFT_BIN) >/dev/null 2>&1 || { \
	  echo "ERROR: loft binary not found: $(LOFT_BIN)"; \
	  echo "Install loft, or set LOFT_BIN."; exit 2; }
	$(LOFT_BIN) --interpret src/main.loft $(MAP)

# Native-compile play target — currently useless (empty palette,
# see #866 above), though it no longer crashes.  Kept for testing
# the fix; flip `play` back to native when it works.
play-native:
	@command -v $(LOFT_BIN) >/dev/null 2>&1 || { \
	  echo "ERROR: loft binary not found: $(LOFT_BIN)"; \
	  echo "Install loft, or set LOFT_BIN."; exit 2; }
	$(LOFT_BIN) src/main.loft $(MAP)

# Full test suite.  Delegates to scripts/test.sh (single source of
# truth for the invocation — that script also cleans tests/actual/
# and respects LOFT_BIN).
test:
	@LOFT_BIN=$(LOFT_BIN) scripts/test.sh

# The second gate (plan 08 V4): play every tests/scripts/*.keys and
# gate on what they measure.  Kept SEPARATE from `test` on purpose —
# the unit suite stays fast and hermetic, and this one plays the
# game and leaves pictures behind.  SCRIPT=<name> plays just one.
validate:
	@LOFT_BIN=$(LOFT_BIN) scripts/validate.sh $(SCRIPT)

# ── Development helpers ──────────────────────────────────────────

# Parse-check a single .loft file.  Pass FILE=src/whatever.loft.
# Emits Rust to /tmp (discarded) — we only care about whether the
# loft frontend accepts the file.  Surfaces syntax errors + type
# warnings without running the program.
check:
	@if [ -z "$(FILE)" ]; then \
	  echo "Usage: make check FILE=src/<file>.loft"; \
	  exit 2; \
	fi
	$(LOFT_BIN) --native-emit /tmp/dryopea_check.rs $(FILE)

# Drop runtime save state and stale test artefacts.  scripts/test.sh
# also wipes tests/actual/* between runs, so a forgotten `make clean`
# isn't fatal — this target exists for explicit "start cold" intent.
clean:
	rm -f dryopea_save.json dryopea_save_markers.json
	rm -f tests/actual/*.png tests/actual/*.json
	rm -rf shots
