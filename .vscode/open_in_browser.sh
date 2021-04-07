
# This script opens ${FILEPATH} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

FILEPATH=$1
WINDOWTITLE=$2

# SELECT BROWSER
if [ -z "${BROWSER}" ]; then
  BROWSER="chromium-browser"
fi

# Open the browser

#WID=`xdotool search "${WINDOWTITLE}"`
if [ -n "${WID}" ]; then

  # Immediately bring the window to the front
  xdotool windowactivate $WID
  # Refresh the page
  xdotool selectwindow key ctrl+r

else

  # Launch fresh browser
  chromium-browser --new-window ${FILEPATH} #> /dev/null &

fi
