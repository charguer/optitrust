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

# SELECT BROWSER
if [ -z "${BROWSER}" ]; then
  BROWSER="chromium-browser"
fi

# Relative path to vscode -- TODO: make more general
VSCODE_FOLDER=../.vscode

# Computer tools folder
TOOLS_FOLDER=../tools

# Compute title
TITLESTR="OptiTrustDiff" # -${FILEBASE} TODO: adding the file base seems to cause issues

# Compute diff
DIFFCODE=`git diff --no-index -U10 ${FILEBASE}_before.cpp ${FILEBASE}_after.cpp | base64 -w 0`
DIFFSTR="var diffString = window.atob(\"${DIFFCODE}\");"

# Take templace and substitute ${TOOLS_FOLDER}, ${INSERT_TITLE}, and ${INSERT_DIFF}
TEMPLATE="${VSCODE_FOLDER}/../tools/diff_template.html"
cp ${TEMPLATE} ${TARGET}
sed -i "s#{INSERT_TITLE}#${TITLESTR}#g;s#{TOOLS_FOLDER}#${TOOLS_FOLDER}#g;s#{INSERT_DIFF}#${DIFFSTR}#g" ${TARGET}
# Note: there seems to be an issue if performing the sed one after the other...
echo "Generated ${TARGET}"

# Open the browser
WID=`xdotool search --name "${TITLESTR}" | head -1`
echo "------------WID is ${WID}"
if [ -n "${WID}" ]; then

  # Immediately bring the window to the front
  xdotool windowactivate $WID
  # Refresh the page
  xdotool key ctrl+r

else

  # Launch fresh browser
  chromium-browser --new-window ${TARGET} > /dev/null &

fi


# DEPRECATED MORE COMPLICATED WAY
#
# lineOfTitle=$(grep -n 'INSERT_TITLE' ${TEMPLATE} | cut -d ":" -f 1)
# lineOfDiff=$(grep -n 'INSERT_DIFF' ${TEMPLATE} | cut -d ":" -f 1)
# head -n $((${lineOfTitle}-1)) ${TEMPLATE} > ${TARGET}
# echo ${TITLESTR} >> ${TARGET}
# tail -n +$((${lineOfTitle}+1)) ${TEMPLATE} | head -n $((${lineOfDiff}-${lineOfTitle}-1)) >> ${TARGET}
# echo "var diffString = window.atob(\"${DIFFSTR}\");" >> ${TARGET}
# tail -n +$((${lineOfDiff}+1)) ${TEMPLATE} >> ${TARGET}
# sed -e "s#\${TOOLS_FOLDER}#${TOOLS_FOLDER}#g" -i ${TARGET}




