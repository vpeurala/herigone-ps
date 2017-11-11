#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
pushd .;

${PROJECT_ROOT}/scripts/create-network.sh || true;

${PROJECT_ROOT}/scripts/docker-stop-all-containers.sh;
${PROJECT_ROOT}/scripts/docker-destroy-all-containers.sh;

cd ${PROJECT_ROOT}/purescript-herigone-db/docker/;
./build.sh;
./start.sh;

cd ${PROJECT_ROOT}/purescript-herigone-server/;
flyway migrate;

cd ${PROJECT_ROOT}/purescript-herigone-server/docker/;
./build.sh;
./start.sh;

cd ${PROJECT_ROOT}/purescript-herigone-client/;
./build.sh;

cd ${PROJECT_ROOT}/purescript-herigone-nginx/docker/;
./build.sh;
./start.sh;

popd;