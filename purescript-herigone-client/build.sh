#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-client;
yarn install;
bower install;
# TODO: Enable --optimise later
# pulp build --optimise --to herigone.js;
pulp build --to herigone.js;