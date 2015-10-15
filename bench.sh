#!/bin/sh
set -eu
mkdir -p dist

time luajit sphtrace.lua >dist/sphtrace-lua.ppm

ghc -Wall -O sphtrace.hs -o dist/sphtrace-hs
time dist/sphtrace-hs

( cd rs && cargo build --release )
time rs/target/release/sphtrace >dist/sphtrace-rs.ppm
