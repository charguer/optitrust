#!/usr/bin/env bash

# This script  a '_trace.html' file for a given a '.ml' script.
# Unlike `open_standalone_trace.sh`, here the trace produced depends on an interactive server,
# which is launched by the present script.
#
# The script then opens the trace in a browser using `open_in_browser.sh`.
# If the script is called from VScode, you might need `watch.sh` to get the browser to show up.

# Usage: (This script must be executed with a `pwd` matching the folder that contains the script)
#   ${OPTITRUST_FOLDER}/tools/open_trace.sh ${FILEBASE}
# where ${FILEBASE} is the script name without extension, e.g., label_add.
# The output is named, e.g. `label_add_trace.html`, and the server depends
# on the serialized trace, e.g. `label_add.trace`.

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
OPTITRUST_FOLDER=$(dirname "${TOOLS_FOLDER}")

FILEBASE=$1

# Name of the trace output file
TRACE_FILENAME="${FILEBASE}_trace.html"
TARGET=$(realpath --relative-to=${OPTITRUST_FOLDER} ${TRACE_FILENAME})

# LATER: we may want to remove this step and directly make the file in the webserver
${TOOLS_FOLDER}/build_trace.sh ${FILEBASE} ${TRACE_FILENAME}

cd ${OPTITRUST_FOLDER}
# The next line tests if we need to rebuild the trace server, in that case, it kills any potentially running trace server.
dune build tools/trace_server/ensure_up_to_date
# Restart a trace server. Does noting if the port 6775 is already taken. In that case, we assume it is an up-to-date optitrust_trace_server and continue.
nohup dune exec optitrust_trace_server > /dev/null 2>&1 &
# Wait until the server is initialized
echo "Waiting for trace server initialization..."
while ! curl -s localhost:6775
do
  sleep 0.1
done

# Open the browser with the target file
URL="http://localhost:6775/${TARGET}"
${TOOLS_FOLDER}/open_in_browser.sh "${URL}" "${FILEBASE} - OptiTrust Trace"
