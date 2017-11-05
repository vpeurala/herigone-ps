#!/usr/bin/env bash
PROJECT_ROOT=$(git rev-parse --show-toplevel);
$PROJECT_ROOT/scripts/docker-destroy-all-containers.sh;
$PROJECT_ROOT/scripts/docker-destroy-all-images.sh;
./build.sh;
./start.sh;
./tail_f_logs.sh;