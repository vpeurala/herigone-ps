#!/usr/bin/env bash
for MOD in $(find . -name "bower.json"); do cd $(dirname $MOD); bower install -DFS; bower list > "$(dirname $MOD).json"; mv "$(dirname $MOD).json" ..; cd ..; done

