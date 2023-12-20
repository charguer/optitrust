#!/bin/bash

# Usage: ./open_doc_for.sh filepath
#
# This script opens in a browser the documentation page associated
# with a given file from the /src folder.
#
# Eg. if filepath is "src/transfo/function.ml", then the page
# "_doc/optitrust/Optitrust/Function/index.html"
# is loaded in a browser.

OPTITRUST_PATH=$(dirname $0)/..
TOOLS_FOLDER=${OPTITRUST_PATH}/tools
cd ${OPTITRUST_PATH}

FILEPATH=$1

if [ -z ${FILEPATH} ]; then
  echo "open_doc_for.sh: missing argument"
  exit 1
fi

BASENAME=$(basename ${FILEPATH})
MODULE_LOWERCASE="${BASENAME%.*}"

MODULE="${MODULE_LOWERCASE^}"

TARGET="_doc/optitrust/Optitrust/${MODULE}/index.html"

TITLESTR="${MODULE} (optitrust.Optitrust.${MODULE})"

if [ -f ${TARGET} ]; then
  echo "${TOOLS_FOLDER}/open_in_browser.sh ${TARGET}"
  ${TOOLS_FOLDER}/open_in_browser.sh ${TARGET}
  # LATER: "${TITLESTR}"
else
  echo "open_doc_for_sh: target file does not exist (try 'make doc' first)"
  echo ${TARGET}
fi
