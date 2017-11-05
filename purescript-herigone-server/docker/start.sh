#!/usr/bin/env bash
docker container stop herigone-ps-server 2>/dev/null;
docker container rm herigone-ps-server 2>/dev/null;
docker container create --interactive --tty --name=herigone-ps-server --network=herigone-network --publish 9771:9771 --user=root vpeurala/herigone-ps-server:latest;
docker container start herigone-ps-server;