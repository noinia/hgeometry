#!/bin/sh

wasm32-wasi-cabal --project-file=cabal.wasm.project build hgeometry-debugger:viewer

hs_wasm_path=$(find ../dist-newstyle -name "viewer.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
    --input "$hs_wasm_path" \
    --output pub/ghc_wasm_jsffi.js

cp "$hs_wasm_path" pub/viewer.wasm
