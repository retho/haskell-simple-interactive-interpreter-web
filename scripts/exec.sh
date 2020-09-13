#!/usr/bin/env bash

distdir=".dist"

if [[ $1 == "web" ]]
then
serve -c serve.json -l 5000
elif [[ $1 == "cli" ]]
then
(cd $distdir && node cli.mjs)
else
echo "Unknown executable: $1" 1>&2
exit 1
fi
