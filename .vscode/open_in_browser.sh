
# This script opens ${FILEPATH} in a fresh browser, unless a windows
# with title ${WINDOWTITLE} already exists, in which case this window
# in brought to the front and its contents is refreshed.

VSCODE=`pwd`
FILEPATH=$1
WINDOWTITLE=$2

# SELECT BROWSER
if [ -z "${OPTITRUST_BROWSER}" ]; then
  if command -v chromium &> /dev/null; then
    OPTITRUST_BROWSER="chromium"
  else
    OPTITRUST_BROWSER="chromium-browser"
  fi
fi

# Open the browser

WID=`xdotool search --screen 0 --name "${WINDOWTITLE}" | head -1`

# || xdotool search --desktop 3 --name "${WINDOWTITLE}
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
  #echo "launch fresh window"
  ## Launch fresh browser
  nohup ${OPTITRUST_BROWSER} --new-window ${FILEPATH} >/dev/null 2>&1
  
fi

exit
