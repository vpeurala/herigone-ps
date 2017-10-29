#!/usr/bin/env bash
docker container stop herigone-ps-db 2>/dev/null
docker container rm herigone-ps-db 2>/dev/null
docker container create --interactive --tty --env POSTGRES_PASSWORD='a7vc0Foqv4KrqCmL' --name=herigone-ps-db --network=herigone-network --user=postgres --publish 5432:5432 vpeurala/herigone-ps-db:latest
docker container start herigone-ps-db
