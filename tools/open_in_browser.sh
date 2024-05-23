# This script opens ${URL} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

URL=$1
WINDOWTITLE=$2

# SELECT BROWSER
# The fastest browser is one already loaded:
# honor the user defaults unless explicitly asked otherwise
if [ -z "${OPTITRUST_BROWSER}" ]; then
  if [ -n "${BROWSER}" ]; then
    OPTITRUST_BROWSER="${BROWSER}"
  else
    OPTITRUST_BROWSER="xdg-open"
  fi
fi

# Open the browser

if [ -z "${WINDOWTITLE}" ]; then

    ## Launch fresh browser
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1 &
    # TODO factorize

else

  WID=`xdotool search --name "${WINDOWTITLE}" | head -1`

  if [ -n "${WID}" ]; then
    # Immediately brings the window to the front
    # echo ${WID}
    # xdotool windowactivate $WID
    xdotool windowactivate --sync $WID
    # Refresh the page
    xdotool key ctrl+r

  else
    ## Launch fresh browser
    nohup ${OPTITRUST_BROWSER} ${URL} >/dev/null 2>&1 &

  fi

fi
