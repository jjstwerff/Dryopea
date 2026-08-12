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
#   make help       Print this overview again.
#
# If you are working on dryopea itself:
#
#   make check FILE=src/<file>.loft
#                   Parse-check a single .loft file without running it.
#                   Equivalent to `loft --native-emit /tmp/x.rs …`.
#                   Quick syntax/type sanity for an edit in progress.
#                   ⚠ Worth running on src/main.loft by hand: it sits
#                   outside the aggregator, so the test suite never
#                   compiles it.  Plan 08 V0 closes that hole.
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

.PHONY: help play play-native test check clean

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
# WORKAROUND: runs in `--interpret` mode because loft's native
# codegen currently loses struct type information when a function
# returns a struct containing a `hash<…>` (filed in
# QUESTIONS_FOR_LOFT.md + loft_repros/struct_with_hash_native_return.loft).
# load_markers_or_empty hits this; native compile panics before the
# GL window opens.  When the upstream fix lands, drop `--interpret`
# to get native performance back.
play:
	@command -v $(LOFT_BIN) >/dev/null 2>&1 || { \
	  echo "ERROR: loft binary not found: $(LOFT_BIN)"; \
	  echo "Install loft, or set LOFT_BIN."; exit 2; }
	$(LOFT_BIN) --interpret src/main.loft $(MAP)

# Native-compile play target — currently broken by the upstream
# struct-with-hash-return bug above.  Kept for testing the
# eventual fix; flip `play` back to native when it works.
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
