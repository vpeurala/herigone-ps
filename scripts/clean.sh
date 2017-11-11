#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
pushd .;

for MODULE in $(ls -d ${PROJECT_ROOT}/purescript-herigone-*);
  do cd $MODULE;
  rm -rf .psc-package;
  rm -rf .psci_modules;
  rm -rf .pulp-cache;
  rm -rf bower_components;
  rm -rf node_modules;
  rm -rf output;
  # purescript-herigone-client
  rm -f .purs-repl;
  rm -f herigone.js;
  # purescript-herigone-server
  rm -f .psc-ide-port;
  rm -f index.js;
done;

${PROJECT_ROOT}/scripts/docker-stop-all-containers.sh;
${PROJECT_ROOT}/scripts/docker-destroy-all-containers.sh;
${PROJECT_ROOT}/scripts/docker-destroy-custom-images.sh;
${PROJECT_ROOT}/scripts/destroy-network.sh || true;

popd;
