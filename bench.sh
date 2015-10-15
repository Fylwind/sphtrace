#!/bin/sh
set -eu
mkdir -p dist

time luajit sphtrace.lua >dist/sphtrace-lua.ppm

ghc -Wall -O sphtrace.hs -o dist/sphtrace-hs
time dist/sphtrace-hs

( cd rs && cargo rustc --release -- -C target-cpu=native )
time rs/target/release/sphtrace >dist/sphtrace-rs.ppm
