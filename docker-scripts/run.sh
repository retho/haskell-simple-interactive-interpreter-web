#!/usr/bin/env bash

if [[ $1 == "web" ]]
then
serve -c serve.json
elif [[ $1 == "cli" ]]
then
(cd .asterius-work/dist/ && node cli.mjs)
fi
