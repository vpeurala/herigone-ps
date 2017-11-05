#!/usr/bin/env bash
set -ex;
PROJECT_ROOT=$(git rev-parse --show-toplevel);
cd ${PROJECT_ROOT}/purescript-herigone-nginx;

if [ -z "${WWW_HERIGONE_COM_KEY}" ]; then echo "Mandatory environment variable WWW_HERIGONE_COM_KEY not set."; exit 1; fi;
cp ${WWW_HERIGONE_COM_KEY} conf/etc/nginx/keys/

if [ -z "${WWW_HERIGONE_COM_PEM}" ]; then echo "Mandatory environment variable WWW_HERIGONE_COM_PEM not set."; exit 1; fi;
cp ${WWW_HERIGONE_COM_PEM} conf/etc/nginx/keys/

if [ ! -f ${PROJECT_ROOT}/purescript-herigone-client/herigone.js ]; then echo "Module purescript-herigone-client is not built."; exit 1; fi;
cp ${PROJECT_ROOT}/purescript-herigone-client/herigone.js static/

docker build --file=docker/Dockerfile --tag=vpeurala/herigone-ps-nginx:latest ${PROJECT_ROOT}/purescript-herigone-nginx;