#!/usr/bin/env bash
docker container stop herigone-ps-nginx 2>/dev/null
docker container rm herigone-ps-nginx 2>/dev/null
docker container create --interactive --tty --name=herigone-ps-nginx --network=herigone-network --publish 80:80 --user=root vpeurala/herigone-ps-nginx:latest
docker container start herigone-ps-nginx
