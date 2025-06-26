#!/bin/bash

# This script opens ${URL} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

# If you want to use a specific browser, include a line in your ~/.bashrc such as:
#    export OPTITRUST_BROWSER=firefox


URL=$1
WINDOWTITLE=$2

# Debug function
msg() {
  echo $1
}

# SELECT BROWSER
# The fastest browser is one already loaded:
# honor the user defaults unless explicitly asked otherwise.
  # Disclaimer: xdg-open does not always work from vscode tasks, hence we don't use it.
if [ -z "${OPTITRUST_BROWSER}" ]; then

  if command -v chromium >/dev/null 2>&1; then
      OPTITRUST_BROWSER="chromium"
  elif command -v google-chrome >/dev/null 2>&1; then
      OPTITRUST_BROWSER="google-chrome"
  elif command -v google-chrome-stable >/dev/null 2>&1; then
      OPTITRUST_BROWSER="google-chrome-stable"
  elif command -v firefox >/dev/null 2>&1; then
      OPTITRUST_BROWSER="firefox"
  elif [ -n "${BROWSER}" ]; then
      OPTITRUST_BROWSER="${BROWSER}"
  else
      echo "No browser found, please install chromium or firefox, or set an environment variable e.g. 'export OPTITRUST_BROWSER=firefox'"
      exit 1
  fi

fi

msg "Using OPTITRUST_BROWSER=${OPTITRUST_BROWSER}"

# Open the browser

# WINDOWTITLE=""

# TODO: refactor code to factorize the default action.

if [ -z "${WINDOWTITLE}" ]; then

    msg "No title provided. Launching a fresh browser."
    ## Launch fresh browser
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1

elif [ "$XDG_SESSION_TYPE" = "x11" ] && command -v xdotool >/dev/null; then

  msg "Searching for an existing brower session with name '${WINDOWTITLE}' using xdotool on x11."

  WID=`xdotool search --name "${WINDOWTITLE}" | head -1`
  # WID="" # skipping

  if [ -n "${WID}" ]; then
    msg "Existing brower session found (wid=${WID}), using xdotool to bring it up."

    # Immediately brings the window to the front
    # echo ${WID}
    # xdotool windowactivate $WID
    xdotool windowactivate --sync $WID
    # Refresh the page
    xdotool key ctrl+r

  else

    msg "No existing browser session found. Lauching fresh browser."
    ## Launch fresh browser
    msg "Command is: cd `pwd`; nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1"
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1

  fi

else

  msg "Launching a fresh browser. (xdotool is not available, or wayland is not deactivated)."
  ## Launch fresh browser
  nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1

fi
