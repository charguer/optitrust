#!/bin/sh

# Usage: add_lines.sh input.ml > output.ml

# Replace "bigstep \"foo\"" with "Trace.open_bigstep ~line:__LINE__ \"foo\""
# Replace "!!" with "open_smallstep ~line:__LINE__ ()"
# Replace "!!!" with "open_smallstep ~line:__LINE__ ~reparse:true ()"

# TODO: Replace this hack by a PPX rewriter

INPUT_FILE=$1

if [ -z ${DISABLE_LINESHIFT} ]; then
  echo "# 1 \"${INPUT_FILE}\""
fi

sed 's/^\([[:space:]]*\)bigstep/\1open_bigstep ~line:__LINE__ /
s/^\([[:space:]]*\)!!!/\1open_smallstep ~line:__LINE__ ~reparse:true ();/
s/^\([[:space:]]*\)!!/\1open_smallstep ~line:__LINE__ ();/
' ${INPUT_FILE}
