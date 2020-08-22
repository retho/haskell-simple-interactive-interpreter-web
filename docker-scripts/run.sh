#!/usr/bin/env bash

if [[ $1 == "web" ]]
then
serve -c serve.json -l 5000
elif [[ $1 == "cli" ]]
then
(cd .asterius-work/dist/ && node cli.mjs)
fi
