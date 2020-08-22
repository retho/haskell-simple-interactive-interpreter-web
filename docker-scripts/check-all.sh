#!/usr/bin/env bash

docker-scripts/hpack.sh

rm -rf .asterius-work/build-check-all
ahc-cabal v1-build $1 --builddir=.asterius-work/build-check-all
