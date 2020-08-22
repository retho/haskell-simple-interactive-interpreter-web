#!/usr/bin/env bash

docker-scripts/hpack.sh

ahc-cabal v1-install -j --builddir=.asterius-work/build --bindir=.asterius-work/dist
ahc-dist --input-exe=.asterius-work/dist/cli
ahc-dist --input-exe=.asterius-work/dist/web --browser
