#!/usr/bin/env sh
PROJECT_ROOT=$(git rev-parse --show-toplevel);
pushd .;
for MODULE in $(ls -d $PROJECT_ROOT/purescript-herigone-*);
  do cd $MODULE;
  rm -rf .pulp-cache;
  rm -rf bower_components;
  rm -rf node_modules;
  rm -rf output;
done;
popd;

