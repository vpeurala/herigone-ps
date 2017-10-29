#!/usr/bin/env sh
docker rm herigone-ps-latest
# TODO Specify the server port here from a parameter or something
docker container create -it --name herigone-ps-latest --user root --publish 9771:9771 vpeurala/herigone-ps:latest
docker container start -ia herigone-ps-latest
