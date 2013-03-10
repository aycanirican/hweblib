#!/bin/sh

cabal configure --enable-tests --enable-benchmark \
  && cabal build \
  && cabal haddock --hyperlink-source \
  && sh runTestsAndCoverage.sh

