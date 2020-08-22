#!/usr/bin/env bash

docker-scripts/hpack.sh

rm -rf .asterius-work/build-lint-all
ahc-cabal v1-build $1 --builddir=.asterius-work/build-lint-all
