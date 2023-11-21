#!/bin/sh

# Usage: add_lines.sh input.ml > output.ml

# Replace "bigstep \"foo\"" with "Trace.open_bigstep ~line:__LINE__ \"foo\""
# Replace "show" with "show ~line:__LINE__"
# Replace "show_foo" with "show_foo ~line:__LINE__"
# Replace "!!" with "open_smallstep ~line:__LINE__ ()"
# Replace "!!!" with "open_smallstep ~line:__LINE__ ~reparse:true ()"

# TODO: Replace this hack by a PPX rewriter

INPUT_FILE=$1

if [ -z ${DISABLE_LINESHIFT} ]; then
  echo "# 1 \"${INPUT_FILE}\""
fi

sed 's/^\([[:space:]!]*\)show_\([^[:space:](]*\)/\1show_\2 ~line:__LINE__ /
s/^\([[:space:]!]*[^[:space:](]*\)show /\1show ~line:__LINE__ /
s/^\([[:space:]]*\)bigstep/\1open_bigstep ~line:__LINE__ /
s/^\([[:space:]]*\)!!!/\1open_smallstep ~line:__LINE__ ~reparse:true ();/
s/^\([[:space:]]*\)!!/\1open_smallstep ~line:__LINE__ ();/
' ${INPUT_FILE}
