#!/usr/bin/env bash

builddir=".asterius-work/build-check-all"

docker-scripts/hpack.sh
rm -rf ${builddir}
ahc-cabal new-build exe:cli -O0 --builddir=${builddir}
ahc-cabal new-build exe:web -O0 --builddir=${builddir}
