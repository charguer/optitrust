#!/bin/bash
# Arguments:
#   0. file dirname
#   1. filename of the transformation script (without extension)
#   2. active line number (to add exit_script instruction)
#   3. option(s) for execution
#      currently: only -dump-trace

DIRNAME=$1
FILEBASE=$2
LINE=$3
UPDATE=$4 # should be update or noupdated
OPTIONS=$5

UPDATE=noupdate

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`

# Special treatment of src folder!
if [ "${DIRNAME}" = "${SRCSRCFOLDER}" ]; then
  make -C ${SRCFOLDER}
  echo "Recompiled the lib, done."
  exit 0
fi

# Run make update in working folder if requested
if [ "${UPDATE}" = "update" ]; then
  echo "recompile lib"
  make -C ${DIRNAME} update
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Could not compile lib"
    exit 1
  fi
fi

# Make sure we work in the directory that contains the file
cd ${DIRNAME}

# First we create the source code for the transformation program
ocaml ${VSCODE}/add_exit.ml -file "${FILEBASE}.ml" -line ${LINE}

# Second, we compile that transformation program
ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str,optiTrust.optitrust "${FILEBASE}_with_exit.byte" 

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Could not compile file"
  exit 1
fi

# Third, we execute the transformation program, obtain "${FILEBASE}_before.cpp" and "${FILEBASE}_after.cpp
./${FILEBASE}_with_exit.byte ${OPTIONS} 
# DEPREACTED | tee stdoutput.txt

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Could not execute script"
  exit 1
fi

# Fourth, we vizualize the diff between these two files 

cd ${VSCODE}
${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE} &


exit
#echo "===>Executing: ${VSCODE}/view_diff.sh ${DIRNAME} ${FILEBASE}"
#echo "===>Current folder is: "

 

#echo "===>Done with view_diff"

exit


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
WID=`xdotool search --name "OptiTrust_Diff_${WINDOWTITLE}"`
echo "Window id is: $WID"
if [ -n "${WID}" ]; then

  # Immediately bring the window to the front
  xdotool windowactivate $WID
  # Refresh the page
  xdotool key ctrl+r

else

  # Launch fresh browser
  chromium-browser --new-window ${TARGET} > /dev/null &

fi

# if [ `wmctrl -l | grep -c "$WINDOWTITLE"` != 0 ]
#   then wmctrl -a "$WINTITLE" #
#       #xdotool key ctrl+r
# else
#   brave --new-window ${TARGET}
# fi
# exit 0



#WID=`xdotool search --name "${WINDOWTITLE}" |head -1`

#${VSCODE_FOLDER}/open_in_browser.sh ${TARGET} "${TITLESTR}"
# third step: clean up and show the diff of the two last states of the program
ocamlbuild -clean
rm "${FILEBASE}_with_exit.ml"
