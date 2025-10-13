#!/usr/bin/env bash

# Usage (from OptiTrust root folder):
#   doc/open_doc_for.sh tests/label/add/label_add_doc.ml
#
# This script opens in a browser the documentation page associated
# with a given file from the /src folder or from a doc file the /tests folder.
#
# Eg. if filepath is "tests/label/add/label_add_doc.ml", then load the page:
#   _doc/optitrust/Optitrust/Optitrust_transfo/Label/index.html
# Eg. if filepath is "src/transfo/function.ml", then load the page
#   _doc/optitrust/Optitrust/Optitrust_transfo/Function/index.html

OPTITRUST_PATH=$(dirname $0)/..
TOOLS_FOLDER=${OPTITRUST_PATH}/tools
cd ${OPTITRUST_PATH}

FILEPATH=$1
if [ -z ${FILEPATH} ]; then
  echo "open_doc_for.sh: missing argument"
  exit 1
fi

# Compute the MODULE name. Distinguish between tests and implementation modules

RELATIVE_FILEPATH=$(realpath --relative-to ${OPTITRUST_PATH} ${FILEPATH})

if [[ "${RELATIVE_FILEPATH}" == "lib/transfo/"* ]]; then
  # Extract the filename
  BASE=$(basename ${FILEPATH} ".ml")

elif [[ "${RELATIVE_FILEPATH}" == "tests/"* ]]; then
  # Get the grand-parent folder path
  BASE=$(dirname ${FILEPATH})
  BASE=$(dirname ${BASE})
  BASE=$(basename ${BASE})

else
  echo "open_doc_for.sh: '${RELATIVE_FILEPATH}' is not a path in tests/ or lib/transfo/"
  exit 1
fi

echo "${BASE}"


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

