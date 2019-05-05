#!/bin/sh
cabal configure && cabal build && ./dist/build/hweblib-test/hweblib-test && ./dist/build/hweblib-benchmark/hweblib-benchmark
