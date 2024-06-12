#!/bin/bash

TOOLS_FOLDER=$(dirname -- "$( readlink -f -- "$0"; )")
OPTITRUST_FOLDER=$(dirname "${TOOLS_FOLDER}")

FILEBASE=$1

# Build the html file
# TODO: Remove this step and directly make the file in the webserver
TARGET="${FILEBASE}_trace.html"
${TOOLS_FOLDER}/build_trace.sh ${FILEBASE}

TARGET=$(realpath --relative-to=${OPTITRUST_FOLDER} ${TARGET})

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
echo "Opening browser on ${URL}"
${TOOLS_FOLDER}/open_in_browser.sh "${URL}" "${FILEBASE} - OptiTrust Trace"
