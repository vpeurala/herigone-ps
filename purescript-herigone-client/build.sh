#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-client;
yarn install;
bower install;
pulp build --optimise --to herigone.js;