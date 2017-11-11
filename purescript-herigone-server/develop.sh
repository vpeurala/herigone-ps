#!/usr/bin/env bash
killall node
killall node-gyp
pulp --watch run --jobs 1
