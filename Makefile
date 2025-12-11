# Copyright (c) 2022 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
all:
	rustfmt src/*.rs --edition 2024
	RUSTFLAGS=-g cargo build --release

install: all
	sudo ln -f -s ${PWD}/target/release/lavition /usr/local/bin/lavition

test: clippy
	rm tests/generated/* -f
	rm tests/result/*.txt tests/result/*.svg tests/result/*.glb -f
	RUST_BACKTRACE=1 cargo test -- --nocapture --test-threads=1 >>result.txt 2>&1

quick:
	RUST_BACKTRACE=1 cargo test --release -- --nocapture --test-threads=1 > result.txt 2>&1

profile:
	RUSTFLAGS=-g cargo build --release >result.txt 2>&1
	flamegraph -o profiler.svg -- target/release/dryopea auto

clean:
	rm result.txt tests/code/* tests/result/* tests/generated/* pkg target/* perf.data perf.data.old profiler.svg -rf

clippy:
	cargo clippy -- -W clippy::all -W clippy::cognitive_complexity > result.txt 2>&1
	cargo clippy --tests -- -W clippy::all -W clippy::cognitive_complexity >> result.txt 2>&1
	rustfmt src/*.rs --edition 2024
	rustfmt tests/*.rs --edition 2024

memory:
	valgrind target/debug/deps/vectors-1ff9433be145872a

last:
	cargo test --package dryopea --test wrap last --release -- --nocapture

meld:
	rustfmt tests/generated/text.rs --edition 2024
	cmp -s tests/generated/text.rs src/text.rs; if [ $$? -eq 1 ]; then meld tests/generated/text.rs src/text.rs; fi
	rustfmt tests/generated/fill.rs --edition 2024
	cmp -s tests/generated/fill.rs src/fill.rs; if [ $$? -eq 1 ]; then meld tests/generated/fill.rs src/fill.rs; fi
