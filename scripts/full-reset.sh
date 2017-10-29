#!/usr/bin/env bash
set -x
PROJECT_ROOT=$(git rev-parse --show-toplevel);
docker network rm herigone-network 2>/dev/null;
$PROJECT_ROOT/scripts/create-network.sh;
$PROJECT_ROOT/scripts/docker-destroy-all-containers.sh;
$PROJECT_ROOT/scripts/docker-destroy-all-images.sh;
for MODULE in $(ls -d $PROJECT_ROOT/purescript-herigone-*); \
  do if [ -d "$MODULE/docker" ]; then
    cd $MODULE/docker;
    ./build.sh;
    ./start.sh;
  fi
done;
