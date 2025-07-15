#!/bin/bash
# Compare perf of file.cpp and file_out.cpp using perf. (wo ucan either give file.cpp or file_out.cpp as input, it will search for the other file)
# you need to have correctly set up perf before using this script
# Allows to checks if the transformed code runs faster.
set -e
FILE="$1"
CXXFLAGS="${@:2}"
[ -z "$CXXFLAGS" ] && CXXFLAGS="-O3"
BASE="${FILE%.cpp}"
[[ "$BASE" == *_out ]] && OTHER="${BASE%_out}.cpp" || OTHER="${BASE}_out.cpp"
g++ "$FILE" -o "${BASE}.exe" $CXXFLAGS
g++ "$OTHER" -o "${OTHER%.cpp}.exe" $CXXFLAGS

CYCLES1=$(perf stat -e cycles -x, ./${BASE} 2>&1 | grep cycles | cut -d, -f1 | tr -d ' ')
CYCLES2=$(perf stat -e cycles -x, ./${OTHER%.cpp} 2>&1 | grep cycles | cut -d, -f1 | tr -d ' ')

echo "$FILE: $CYCLES1 cycles"
echo "$OTHER: $CYCLES2 cycles"
