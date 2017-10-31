#!/usr/bin/env bash
set -ex
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-server;
yarn install;
find . -name "*.purs" | xargs purs compile;
docker build --file=docker/Dockerfile --tag=vpeurala/herigone-ps-server:latest ${PROJECT_ROOT}
