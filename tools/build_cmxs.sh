#!/bin/bash
set -euo pipefail

# builds the .cmxs file from file $1 (.ml)
FILE=$1
FILEBASE=${1%.ml}

# Using only one dune command that builds the project is faster than using two.
# Therefore we also force the runner to be up to date at this step.
# FIXME: This will not work if the script is used outside an optitrust buildtree.
# We should find a way to detect if we are.

# Path to the tools and optitrust folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
OPTITRUST_FOLDER="${TOOLS_FOLDER}/.."

TMPDIR=${OPTITRUST_FOLDER}/tmp
mkdir -p ${TMPDIR}
ln -sf "$(readlink -f -- "${FILE}";)" ${TMPDIR}/main.ml
echo "(executable
  (name main)
  (modules main)
  (modes (native plugin))
  (promote)
  (preprocess (pps ppx_transfo))
  (libraries optitrust))" > ${TMPDIR}/dune
pushd ${OPTITRUST_FOLDER} > /dev/null
dune build tmp/main.cmxs runner/optitrust_runner.exe
popd > /dev/null
mv ${TMPDIR}/main.cmxs ${FILEBASE}.cmxs
rm -r ${TMPDIR}