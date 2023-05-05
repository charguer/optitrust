#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
WEB_VIEW_FOLDER="${TOOLS_FOLDER}/web_view"


FILEBASE=$1
FILEEXT=$2
DIFFFOR=$3   # this flag could be "" or "enc"


CONTEXTSIZE="10"

# Read options from optitrust_flags.sh
# options: e.g., CONTEXTSIZE="100", NODIFFDISPLAY="1"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi

if [ "${NODIFFDISPLAY}" = "1" ]; then
  echo "NODIFFDISPLAY = 1, skipping diff display"
  exit 0;
fi

TARGET="${FILEBASE}_diff.html"

#echo "Generating ${TARGET}"

# Compute title
TITLESTR="${FILEBASE} - OptiTrust Diff"


# Read options from optitrust_flags.sh
# options: e.g., DIFFFOR="enc"
if [ -f "optitrust_flags.sh" ]; then
  source optitrust_flags.sh
fi

# Treat the options DIFFFOR="enc"
if [ "${DIFFFOR}" = "enc" ]; then
  ENCOPT="_enc"
fi


# Compute diff
DIFFCODE=`git diff --ignore-all-space --no-index -U${CONTEXTSIZE} ${FILEBASE}_before${ENCOPT}.${FILEEXT} ${FILEBASE}_after${ENCOPT}.${FILEEXT} | base64 -w 0`


if [ "$DIFFCODE" == "" ]; then
  echo ">>>============EMPTY DIFF============<<<"
  exit 0
fi

DIFFSTR="var diffString = window.atob(\"${DIFFCODE}\");"


# Take templace and substitute ${WEB_VIEW_FOLDER}, ${INSERT_TITLE}, and ${INSERT_DIFF}
TEMPLATE="${WEB_VIEW_FOLDER}/diff_template.html"

cp ${TEMPLATE} ${TARGET}

sed -i "s#{INSERT_TITLE}#${TITLESTR}#g;s#{WEB_VIEW_FOLDER}#${WEB_VIEW_FOLDER}#g;s#{INSERT_DIFF}#${DIFFSTR}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...
#echo "Generated ${TARGET}"

# Open the browser with the target file
${TOOLS_FOLDER}/open_in_browser.sh ${TARGET} "${TITLESTR}"
