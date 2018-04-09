#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
pushd .;

cd ${PROJECT_ROOT};

rsync -avzhe ssh purescript-herigone-nginx/conf/etc/nginx root@95.85.25.96:/etc/
