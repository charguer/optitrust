#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
WEB_VIEW_FOLDER="${TOOLS_FOLDER}/web_view"

FILEBASE=$1

TARGET="${FILEBASE}_result.html"

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="${FILEBASE} - OptiTrust Result"

# Compute json file name
FILEJSONJS="./${FILEBASE}.js"

# Start from templace, then substitute:
# -  './' with '${WEB_VIEW_FOLDER}/'
# -  {INSERT_TITLE} with ${TITLESTR}
# -  'ast_json_template.js' with ${FILEJSONJS}

TEMPLATE="${WEB_VIEW_FOLDER}/ast_view_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g;s#\./#${WEB_VIEW_FOLDER}/#g;s#ast_json_template\.js#${FILEJSONJS}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...

echo "Generated ${TARGET}"
# exit

# Open the browser with the target file
${TOOLS_FOLDER}/open_in_browser.sh ${TARGET} "${TITLESTR}"
