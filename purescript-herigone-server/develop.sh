#!/usr/bin/env sh
killall node
killall node-gyp
pulp --watch run --jobs 1
