#!/usr/bin/env bash

docker-scripts/hpack.sh

ahc-cabal v1-build $1 --builddir=.asterius-work/build-check
