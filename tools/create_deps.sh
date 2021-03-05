#!/bin/bash

for x in $@
do
  [[ -f $x ]] || continue # ignore if not existing
  considered_module=$(basename ${x/%\.[fFc]*/.o})
  echo -n "${considered_module}: ";
  dep_files=($(basename -a $(for dep in $(awk '{print $2;}' <(grep -e "^\s*use\s*[a-zA-Z_]\+" $x -o) | sort | uniq); do grep -ile "^\s*module\s*$dep" $@ 2>/dev/null; done | tr '\n' ' ') 2> /dev/null ))
  dep_modules=(${dep_files[@]/%\.[fFc]*/.o})
  echo ${dep_modules[@]#${considered_module}}
done
