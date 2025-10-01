#!/usr/bin/env bash

# Usage (from OptiTrust root folder):
#   doc/open_doc_for.sh tests/label/add/label_add_doc.ml
#
# This script opens in a browser the documentation page associated
# with a given file from the /src folder.
#
# TODO: fix support for /src/lib/transfo/
# Eg. if filepath is "src/transfo/function.ml", then the page
# "_doc/optitrust/Optitrust/Optitrust_transfo/Function/index.html"
# is loaded in a browser.

OPTITRUST_PATH=$(dirname $0)/..
TOOLS_FOLDER=${OPTITRUST_PATH}/tools
cd ${OPTITRUST_PATH}

FILEPATH=$1

if [ -z ${FILEPATH} ]; then
  echo "open_doc_for.sh: missing argument"
  exit 1
fi

# Get the grand-parent folder path
BASE=$(dirname ${FILEPATH})
BASE=$(dirname ${BASE})
BASE=$(basename ${BASE})

# Obtain the module name by capitalizing
MODULE="${BASE^}"

TARGET="_doc/optitrust/Optitrust_transfo/${MODULE}/index.html"

# LATER: TITLESTR="${MODULE} (optitrust.Optitrust.${MODULE})"

if [ -f ${TARGET} ]; then
  echo "${TOOLS_FOLDER}/open_in_browser.sh ${TARGET}"
  ${TOOLS_FOLDER}/open_in_browser.sh ${TARGET}
else
  echo "open_doc_for_sh: file not found ${TARGET}."
  echo "  Did you do 'make doc' first? or perhaps there is no documentation for '${BASE}'."
fi

