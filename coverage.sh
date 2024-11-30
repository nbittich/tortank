#!/bin/bash

# Doc for grcov
# https://github.com/mozilla/grcov
#

set -xe
rm -f *.profraw
cargo clean
export RUSTFLAGS="-Cinstrument-coverage" 
cargo build
export LLVM_PROFILE_FILE="tortank-%p-%m.profraw" 
cargo test
grcov . -s . --binary-path ./target/debug/ -t html --branch --ignore-not-existing -o ./target/debug/coverage/
xdg-open "target/debug/coverage/index.html"
