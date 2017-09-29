#!/usr/bin/env sh
docker images --all | grep -v ubuntu | grep -v -e 'vpeurala/herigone-ps.*latest' | tail -n +2 | awk '{print $3}' | xargs docker rmi -f
