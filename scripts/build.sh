#!/usr/bin/env bash

builddir=".asterius-work/build"
distdir=".dist"

scripts/hpack.sh
ahc-cabal new-install -j --builddir=${builddir} --installdir=${distdir} --install-method=copy --overwrite-policy=always
ahc-dist --input-exe=${distdir}/cli
ahc-dist --input-exe=${distdir}/web --browser
