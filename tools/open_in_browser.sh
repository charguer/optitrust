# This script opens ${URL} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

URL=$1
WINDOWTITLE=$2

# Debug function
msg() {
  echo $1
}

# SELECT BROWSER
# The fastest browser is one already loaded:
# honor the user defaults unless explicitly asked otherwise
if [ -z "${OPTITRUST_BROWSER}" ]; then

  if [ -n "${BROWSER}" ]; then
    OPTITRUST_BROWSER="${BROWSER}"
  else
    # OPTITRUST_BROWSER="xdg-open" // does not work .. version `GLIBCXX_3.4.29' not found (required by /lib/x86_64-linux-gnu/libproxy.so.1)
    # OPTITRUST_BROWSER="chromium"
    OPTITRUST_BROWSER="firefox"
  fi
fi

msg "Using OPTITRUST_BROWSER=${OPTITRUST_BROWSER}"

# Open the browser

if [ -z "${WINDOWTITLE}" ]; then

    msg "No title provided. Launching a fresh browser."
    ## Launch fresh browser
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1

else
  msg "Title provided. Searching for an existing brower session."

  WID=`xdotool search --name "${WINDOWTITLE}" | head -1`

  if [ -n "${WID}" ]; then
    msg "Existing brower session found, using xdotool to bring it up."

    # Immediately brings the window to the front
    # echo ${WID}
    # xdotool windowactivate $WID
    xdotool windowactivate --sync $WID
    # Refresh the page
    xdotool key ctrl+r

  else

    msg "No session found. Lauching fresh browser"
    ## Launch fresh browser
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1

  fi

fi

msg "open_in_browser.sh script completes"
