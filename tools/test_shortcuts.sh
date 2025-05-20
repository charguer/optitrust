#!/bin/bash
#
# This script is for testing calling a shortcut from VSCode
# and for testing the launch of a browser from VSCode
#
# Usage: after configurating keybindings.json as explained in INSTALL.md
# open any file form the OptiTrust repository in VSCode and type 'alt+shift+F11'.
# The output window should report the file name and the line number.
# Then, a browser will be opened with the test file.
# Which exact command is used depends on the line number.
# Lines 1,2,3,4,5 have specific behaviors, see below.
# Any other line will use the 'open_in_browser.sh' script,
# which is the one that OptiTrust tasks rely upon.


FILE=$1
LINE=$2

echo "Successfully executed shortcut"
echo "Argument FILE is ${FILE}"
echo "Argument LINE is ${LINE}"
echo "Test html file tools/test.html"
URL="tools/test.html"

if [ "${LINE}" = "1" ]; then

  echo "LINE=1, thus trying to open test page in chromium with nohup"
  echo "Command is 'nohup chromium ${URL} >/dev/null 2>&1'"
  nohup chromium ${URL} >/dev/null 2>&1

elif [ "${LINE}" = "2" ]; then

  echo "LINE=2, thus trying to open test page in chromium without nohup"
  echo "Command is 'chromium ${URL}'"
  chromium ${URL}

elif [ "${LINE}" = "3" ]; then

  echo "LINE=3, thus trying to open test page in firefox"
  echo "Command is 'firefox ${URL}'"
  firefox ${URL}

elif [ "${LINE}" = "4" ]; then

  echo "LINE=4, thus trying to open test page using xdg-open"
  echo "Command is 'xdg-open ${URL}'"
  xdg-open ${URL}

elif [ "${LINE}" = "5" ]; then

  echo "LINE=5, thus trying to open test page using open_in_browser.sh specifying chromium as browser"
  export OPTITRUST_BROWSER="chromium"
  echo "Command is 'OPTITRUST_BROWSER=\"chromium\" tools/open_in_browser.sh ${URL} test'"
  tools/open_in_browser.sh ${URL} test

else

  echo "LINE>5, thus trying to execute script open_in_browser.sh"
  echo "Command is 'tools/open_in_browser.sh ${URL} test'"
  tools/open_in_browser.sh ${URL} test

fi

echo "Test completed"
