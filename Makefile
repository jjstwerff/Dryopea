# Copyright (c) 2022 Jurjen Stellingwerff
# SPDX-License-Identifier: LGPL-3.0-or-later
all:
	rustfmt src/*.rs
	RUSTFLAGS=-g cargo build --release >result.txt 2>&1 ; sed -i 's/; finished in [0-9]\+.[0-9]\+s//g' result.txt  ; sed -i 's/ in [0-9]\+.[0-9]\+s//g' result.txt
	RUST_BACKTRACE=1 time -v ./target/release/dryopea auto > auto.txt 2>&1

test: clippy
	rm tests/generated/*.rs tests/generated/*.txt -f
	rm tests/result/*.txt tests/result/*.svg tests/result/*.glb -f
	RUST_BACKTRACE=1 cargo test -- --nocapture --test-threads=1 >>result.txt 2>&1 ; sed -i 's/; finished in [0-9]\+.[0-9]\+s//g' result.txt  ; sed -i 's/ in [0-9]\+.[0-9]\+s//g' result.txt

run:
	rustfmt src/*.rs
	RUSTFLAGS=-g cargo build --release >result.txt 2>&1 ; sed -i 's/; finished in [0-9]\+.[0-9]\+s//g' result.txt  ; sed -i 's/ in [0-9]\+.[0-9]\+s//g' result.txt
	./target/release/dryopea

profile:
	RUSTFLAGS=-g cargo build --release >result.txt 2>&1 ; sed -i 's/; finished in [0-9]\+.[0-9]\+s//g' result.txt  ; sed -i 's/ in [0-9]\+.[0-9]\+s//g' result.txt
	flamegraph -o profiler.svg -- target/release/dryopea auto

clean:
	rm result.txt tests/result/* tests/generated/* pkg target/* perf.data perf.data.old profiler.svg -rf

clippy:
	cargo clippy -- -W clippy::all -W clippy::cognitive_complexity > result.txt 2>&1
	rustfmt src/*.rs
	rustfmt tests/*.rs
