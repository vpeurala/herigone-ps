#!/usr/bin/env bash
set -ex
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-server;
npm install;
bower install;
pulp build;
docker build --file=docker/Dockerfile --tag=vpeurala/herigone-ps-server:latest ${PROJECT_ROOT}
