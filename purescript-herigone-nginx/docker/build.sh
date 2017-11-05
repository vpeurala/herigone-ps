#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-nginx;
docker build --file=docker/Dockerfile --tag=vpeurala/herigone-ps-nginx:latest ${PROJECT_ROOT}/purescript-herigone-nginx;