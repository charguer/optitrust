#!/usr/bin/env bash

# This script opens ${URL} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

# If you want to use a specific browser, include a line in your ~/.bashrc such as:
# export OPTITRUST_BROWSER=firefox

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
OPTITRUST_FOLDER="${TOOLS_FOLDER}/.."

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

  # Apparently xdg-open does not work on Ubuntu due issues with snap,
  # Hence on Ubuntu we select a specific browser based on availability
  # If desired, add to your ~/.bashrc: export OPTITRUST_BROWSER="chromium"
  if grep -qi "ubuntu" /etc/os-release; then

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

  else
      # For other OS, we simply use xdg-open, which launches the default application for html pages
      OPTITRUST_BROWSER="xdg-open"
  fi

fi

msg "Executing open_in_browser.sh on: ${URL}"
msg "  Using OPTITRUST_BROWSER=${OPTITRUST_BROWSER}"

# Not sure if the 'setsid' is useful, but it helps detaching the subtask even more than just nohup
OPTITRUST_BROWSER="setsid ${OPTITRUST_BROWSER}"

# Open the browser
EXEC="${OPTITRUST_FOLDER}/.vscode/exec_with_nohup_and_runaction.sh"

# WINDOWTITLE=""

if [ -z "${WINDOWTITLE}" ]; then

    msg "  No title provided. Launching a fresh browser."
    ## Launch fresh browser
    msg "  Command is: ${EXEC} ${OPTITRUST_BROWSER} ${URL}"
    ${EXEC} ${OPTITRUST_BROWSER} ${URL}

elif [ "$XDG_SESSION_TYPE" = "x11" ] && command -v xdotool >/dev/null; then

  msg "  Searching for an existing brower session with name '${WINDOWTITLE}' using xdotool on x11."

  # deprecated: WID=`xdotool search --name "${WINDOWTITLE}" | head -1`

  # Note: we currently don't check that the existing browser window is a process named ${OPTITRUST_BROWSER}; it could be another browser; that's not a big deal.

  WID=`xdotool search --limit 1 --name "${WINDOWTITLE}"`
  echo "  xdotool search --limit 1 --name \"${WINDOWTITLE}\""

  if [ -n "${WID}" ]; then
    msg "  Existing browser session found (wid=${WID}), using xdotool to bring it up."

    # Immediately brings the window to the front
    # echo ${WID}
    # xdotool windowactivate $WID
    msg "  activating window"
    xdotool windowactivate --sync ${WID}
    # sleep 0.5
    # Refresh the page
    msg "  sending ctrl+r to the browser"
    xdotool key --window ${WID} --clearmodifiers ctrl+r
  else

    msg "  No existing browser session found. Lauching fresh browser."
    ## Launch fresh browser
    msg "  Command is: ${EXEC} ${OPTITRUST_BROWSER} ${URL}"
    ${EXEC} ${OPTITRUST_BROWSER} ${URL}

  fi

else

  msg "Launching a fresh browser. (xdotool is not available, or wayland is not deactivated)."
  ## Launch fresh browser
  msg "Command is: ${EXEC} ${OPTITRUST_BROWSER} ${URL}"
  ${EXEC} ${OPTITRUST_BROWSER} ${URL}

fi
