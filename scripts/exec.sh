#!/usr/bin/env bash

distdir=".dist"

if [[ $1 == "web" ]]
then
serve -c serve.json -l 5000
elif [[ $1 == "cli" ]]
then
(cd $distdir && node cli.mjs)
fi
