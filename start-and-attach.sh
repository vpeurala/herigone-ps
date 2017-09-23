#!/usr/bin/env sh
docker rm herigone-ps-latest
docker container create -it --name herigone-ps-latest --user node --publish-all vpeurala/herigone-ps:latest
docker container start -ia herigone-ps-latest
