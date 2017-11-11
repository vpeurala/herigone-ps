#!/usr/bin/env bash
docker images --all | tail -n +2 | awk '{print $3}' | xargs docker rmi -f
