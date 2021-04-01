#!/bin/bash

# How to test this script:
# cd src/test_suite
# # assume test_split_before.cpp and test_split_after.cpp exist
# ../.vscode/view_diff.sh `pwd` test_split


DIRNAME=$1
FILEBASE=$2

TARGET=${FILEBASE}_diff.html

echo "------------Generating ${TARGET}"

cd ${DIRNAME}

# Relative path to vscode -- TODO: make more general
VSCODE_FOLDER=../.vscode

# Computer tools folder
TOOLS_FOLDER=../tools

# Compute title
TITLESTR="OptiTrust_Diff_${FILEBASE}" # TODO: investigate how to make spaces and slash be properly escaped

# Compute diff
DIFFCODE=`git diff --no-index -U10 ${FILEBASE}_before.cpp ${FILEBASE}_after.cpp | base64 -w 0`
DIFFSTR="var diffString = window.atob(\"${DIFFCODE}\");"

# Take templace and substitute ${TOOLS_FOLDER}, ${INSERT_TITLE}, and ${INSERT_DIFF}
TEMPLATE="${TOOLS_FOLDER}/diff_template.html"
cp ${TEMPLATE} ${TARGET}
sed -i "s#{INSERT_TITLE}#${TITLESTR}#g;s#{TOOLS_FOLDER}#${TOOLS_FOLDER}#g;s#{INSERT_DIFF}#${DIFFSTR}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...
echo "Generated ${TARGET}"

# Open the browser
${VSCODE_FOLDER}/open_in_browser ${TARGET} "${TITLESTR}"
