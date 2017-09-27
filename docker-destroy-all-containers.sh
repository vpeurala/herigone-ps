#!/usr/bin/env sh
docker ps -a | grep -v herigone-ps-latest | tail -n +2 | awk '{print $1}' | xargs docker rm -f
