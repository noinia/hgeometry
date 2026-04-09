#!/bin/sh

wasm32-wasi-cabal --project-file=cabal.wasm.project build hgeometry-examples:$1

hs_wasm_path=$(find dist-newstyle -name "$1.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
    --input "$hs_wasm_path" \
    --output hgeometry-examples/pub/ghc_wasm_jsffi.js

cp "$hs_wasm_path" hgeometry-examples/pub/$1.wasm
