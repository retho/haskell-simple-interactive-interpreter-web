#!/usr/bin/env bash

docker build --build-arg force_update=$(date '+%s') -t haskell-interpreter-web-docker - < Dockerfile
