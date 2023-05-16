#!/bin/bash
set -euo pipefail

# builds the .cmxs file from file $1 (.ml)

# Using only one dune command that builds the project is faster than using two.
# Therefore we also force the runner to be up to date at this step.
# FIXME: This will not work if the script is used outside an optitrust buildtree.
# We should find a way to detect if we are.

FILEBASE=${1%.ml}
mv dune dune.bak 2>/dev/null || true
echo "(executable
  (name ${FILEBASE})
  (modules ${FILEBASE})
  (modes (native plugin))
  (promote)
  (libraries optitrust))

(alias
  (name default)
  (deps ${FILEBASE}.cmxs %{project_root}/runner/optitrust_runner.exe))" > dune
trap 'mv dune.bak dune 2>/dev/null || rm dune' EXIT
dune build