#!/bin/bash

[ -z "$1" ] && exit 1

name="$1"

mkdir -p "$name"
touch "$name/$name.ml" "$name/${name}_doc.ml" "$name/${name}_doc.cpp" "$name/$name.cpp"
