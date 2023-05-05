
# This script opens ${FILEPATH} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

FILEPATH=$1
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

WID=`xdotool search --screen 0 --name "${WINDOWTITLE}" | head -1`

if [ -n "${WID}" ]; then
  #echo "found window to reuse"
  # Immediately brings the window to the front
  # echo ${WID}
  # xdotool windowactivate $WID
  xdotool windowactivate --sync $WID
  #echo "now refreshing"
  # Refresh the page
  xdotool key ctrl+r

else
  ## Launch fresh browser
  nohup ${OPTITRUST_BROWSER} ${FILEPATH} >/dev/null 2>&1 &

fi
