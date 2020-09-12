#!/usr/bin/env bash

if [[ $1 == "docker" ]]
then
docker/start.sh
elif [[ $1 == "update" ]]
then
docker/update.sh
elif [[ $1 == "watch" ]]
then
docker/start.sh nodemon --watch src --watch src-cli --watch src-web -e hs scripts/check.js
elif [[ $1 == "check" ]]
then
docker/start.sh node scripts/check.js
elif [[ $1 == "check:all" ]]
then
docker/start.sh scripts/check-all.sh
elif [[ $1 == "build" ]]
then
docker/start.sh scripts/build.sh
elif [[ $1 == "exec" ]]
then
  if [[ $2 == "web" ]]
  then
  docker/start-exposing-port.sh scripts/exec.sh $2
  else
  docker/start.sh scripts/exec.sh $2
  fi
else
echo "Unknown command: $1" 1>&2
exit 1
fi
