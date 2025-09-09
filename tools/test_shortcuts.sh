#!/bin/bash
#
# This script is for testing calling a shortcut from VSCode
# and for testing the launch of a browser from VSCode.
# Make sure that `watcher.sh` is running, else the command
# will be ran in a sandbox and the result might be broken.
#
# Usage: after configurating keybindings.json as explained in INSTALL.md
# open any file form the OptiTrust repository in VSCode and type 'alt+shift+F11'.
#
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
  echo "==> Warning: if you had no chromium previously opened, you'll need to close the window to terminate the vscode task."
  echo "Command is 'chromium ${URL}'"
  chromium ${URL}

elif [ "${LINE}" = "3" ]; then

  echo "LINE=3, thus trying to open test page in firefox without nohup"
  echo "==> Warning: if you had no firefox previously opened, you'll need to close the window to terminate the vscode task."
  echo "Command is 'firefox ${URL}'"
  firefox ${URL}

elif [ "${LINE}" = "4" ]; then

  echo "LINE=4, thus trying to open test page using xdg-open without nohup"
  echo "==> Warning: if you had no browser previously opened, you'll need to close the window to terminate the vscode task."
  echo "Command is 'xdg-open ${URL}'"
  if grep -qi "ubuntu" /etc/os-release && ! pgrep -f "watch.sh" > /dev/null; then
    echo "==> Warning: on Ubuntu, xdg-open in a snap environment usually does not work, unless using the watcher.sh script."
  fi
  xdg-open ${URL}
  # sometimes works fine in VSCode terminal, but not when executed as a task

elif [ "${LINE}" = "5" ]; then

  echo "LINE=5, thus trying to open test page using open_in_browser.sh specifying chromium as browser"
  export OPTITRUST_BROWSER="chromium"
  echo "Command is 'OPTITRUST_BROWSER=\"chromium\" tools/open_in_browser.sh ${URL} __optitrust_test_page'"
  tools/open_in_browser.sh ${URL} __optitrust_test_page

elif [ "${LINE}" = "6" ]; then

  echo "LINE=6, thus trying to open test page using open_in_browser.sh specifying firefox as browser"
  export OPTITRUST_BROWSER="firefox"
  echo "Command is 'OPTITRUST_BROWSER=\"firefox\" tools/open_in_browser.sh ${URL} __optitrust_test_page'"
  tools/open_in_browser.sh ${URL} __optitrust_test_page

else

  echo "LINE>6, thus trying to execute script open_in_browser.sh"
  echo "Command is 'tools/open_in_browser.sh ${URL} __optitrust_test_page'"
  tools/open_in_browser.sh ${URL} __optitrust_test_page

fi

echo "Test completed"
