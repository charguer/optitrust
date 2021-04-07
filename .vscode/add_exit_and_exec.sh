#!/bin/bash
# Arguments:
#   0. file dirname
#   1. filename of the transformation script (without extension)
#   2. active line number (to add exit_script instruction)
#   3. option(s) for execution
#      currently: only -dump-trace

# first step: add exit_script instruction at the end of the active line

# NOTE: if vscode is invoked from the test_suite folder, then no need for test_suite

DIRNAME=$1
FILEBASE=$2
LINE=$3
OPTIONS=$4

cd ${DIRNAME}

VSCODE=../.vscode

ocaml ../.vscode/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}

# second step: build and execute the script
ocamlbuild -pkgs clangml,refl,pprint,str,optiTrust.scriptTools "${FILEBASE}_with_exit.byte" || (echo "Cannot compile $1_with_exit.ml"; exit 1)
./${FILEBASE}_with_exit.byte ${OPTIONS}
echo ""
#echo "===>Executing: ${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE}"
#echo "===>Current folder is: "
#pwd
#${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE} &
#echo "===>Done with view_diff"

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

WID=`xdotool search --name "OptiTrust_Diff_${TITLESTR}"`
if [[ ! -n "${WID}" ]];  then firefox http://127.0.0.1:5500/unit_tests/${TARGET}
fi


#WID=`xdotool search --name "${WINDOWTITLE}" |head -1`

#${VSCODE_FOLDER}/open_in_browser.sh ${TARGET} "${TITLESTR}"
# third step: clean up and show the diff of the two last states of the program
ocamlbuild -clean
rm "${FILEBASE}_with_exit.ml"
