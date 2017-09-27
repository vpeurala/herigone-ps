#!/usr/bin/env sh
docker images --all | grep -v ubuntu | grep -v vpeurala/herigone-ps | tail -n +2 | awk '{print $3}' | xargs docker rmi -f
