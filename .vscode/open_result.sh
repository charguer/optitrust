#!/bin/bash

# How to test this script:
# cd src/.vscode
# touch ../unit_tests/test_split_json.js
# ./open_result.sh ../unit_tests test_split

VSCODE=`pwd`
TOOLS_FOLDER="${VSCODE}/../tools"

DIRNAME=$1
FILEBASE=$2

# Work in the file directory
cd ${DIRNAME}

TARGET="${FILEBASE}_result.html"

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="OptiTrust_Result_${FILEBASE}" # TODO: investigate how to make spaces and slash be properly escaped

# Compute json file name
FILEJSONJS="./${FILEBASE}.js"

# Start from templace, then substitute:
# -  './' with '${TOOLS_FOLDER}/'
# -  {INSERT_TITLE} with ${TITLESTR}
# -  'ast_json_template.js' with ${FILEJSONJS}

TEMPLATE="${TOOLS_FOLDER}/ast_view_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g;s#\./#${TOOLS_FOLDER}/#g;s#ast_json_template\.js#${FILEJSONJS}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...

echo "Generated ${TARGET}"
# exit

# Open the browser with the target file
cd ${VSCODE}
./open_in_browser.sh ${DIRNAME}/${TARGET} "${TITLESTR}"
