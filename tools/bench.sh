#!/bin/bash
set -e
FILE="$1"
CXXFLAGS="${@:2}"
[ -z "$CXXFLAGS" ] && CXXFLAGS="-O3"
BASE="${FILE%.cpp}"
[[ "$BASE" == *_out ]] && OTHER="${BASE%_out}.cpp" || OTHER="${BASE}_out.cpp"
g++ "$FILE" -o "${BASE}" $CXXFLAGS
g++ "$OTHER" -o "${OTHER%.cpp}" $CXXFLAGS

CYCLES1=$(sudo perf stat -e cycles -x, ./${BASE} 2>&1 | grep cycles | cut -d, -f1 | tr -d ' ')
CYCLES2=$(sudo perf stat -e cycles -x, ./${OTHER%.cpp} 2>&1 | grep cycles | cut -d, -f1 | tr -d ' ')

echo "$FILE: $CYCLES1 cycles"
echo "$OTHER: $CYCLES2 cycles"
