#!/bin/bash
set -euo pipefail

# Usage: ./build_cmxs.sh foo.ml
# Produces foo.cmxs

FILE="$1"
SRCBASE="${1%.ml}"
TARGET="${SRCBASE}.cmxs"



# Path to the tools and optitrust folder
TOOLS_FOLDER=$(dirname -- "$(readlink -f -- "$0";)")
OPTITRUST_FOLDER="${TOOLS_FOLDER}/.."

TMP_FOLDER=${OPTITRUST_FOLDER}/tmp
mkdir -p ${TMP_FOLDER}

#==========================================================================
# Check the dependencies
# We need a rebuild of the cmx file if any lib/ file is more recent
# than the .ml script

# TIMER4=`date +%s%3N`

LAST_MODIF_LIB=`find ${OPTITRUST_FOLDER}/lib -name "*.ml*" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2-2 -d" "`

NEEDS_REBUILD="0"

if [ ${LAST_MODIF_LIB} -nt ${TARGET} ]; then
  echo "Compiled cmx is out of date compared with ${LAST_MODIF_LIB}"
  NEEDS_REBUILD="1"
elif [ ${SRCBASE}.ml -nt ${TARGET} ]; then
  echo "Compiled cmx is out of date compared with ${SRCBASE}.ml"
  NEEDS_REBUILD="1"
fi


#==========================================================================
# Compile the script

# TIMER5=`date +%s%3N`

# produces ${OPTITRUST_FOLDER}/tmp/with_lines.ml from file $1 (.ml)

if [ "${NEEDS_REBUILD}" = "1" ]; then
  # Generate the script instrumented with line numbers
  ${TOOLS_FOLDER}/add_lines.sh ${SRCBASE}.ml > ${TMP_FOLDER}/with_lines.ml

  echo "Building cmxs file"

  echo "(executable
  (name with_lines)
  (modules with_lines)
  (modes (native plugin))
  (promote)
  (preprocess (pps ppx_transfo))
  (libraries optitrust))" > ${TMP_FOLDER}/dune

  # LATER: Think about using dune to check that script is already up to date

  # Using only one dune command that builds the project is faster than using two.
  # Therefore we also force the runner to be up to date at this step.
  # FIXME: This will not work if the script is used outside an optitrust buildtree.
  # We should find a way to detect if we are.
  pushd ${OPTITRUST_FOLDER} > /dev/null
  dune build tmp/with_lines.cmxs tools/runner/optitrust_runner.exe
  popd > /dev/null

  mv ${TMP_FOLDER}/with_lines.cmxs ${TARGET}
  rm -r ${TMP_FOLDER}

else
   echo "Skipping build of cmxs file"
fi

