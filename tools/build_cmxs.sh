#!/bin/bash
set -euo pipefail

# builds the .cmxs file from file $1 (.ml)

FILEBASE=${1%.ml}
mv dune dune.bak && trap 'mv dune.bak dune' EXIT || true
echo "(executable
  (name ${FILEBASE})
  (modules ${FILEBASE})
  (modes (native plugin))
  (promote)
  (libraries optitrust))" > dune
dune build ${FILEBASE}.cmxs
rm dune

echo