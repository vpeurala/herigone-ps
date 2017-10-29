#!/usr/bin/env bash
npm install
bower install
pulp build --optimise --to herigone.js
cp herigone.js ../purescript-herigone-static/docker/static/
