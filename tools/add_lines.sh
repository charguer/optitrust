#!/bin/sh

# Usage: add_lines.sh input.ml output.ml

# Replace "bigstep \"foo\"" with "Trace.open_bigstep ~line:__LINE__ \"foo\""
# Replace "show" with "show ~line:__LINE__"
# Replace "show_foo" with "show_foo ~line:__LINE__"
# Replace "!!" with "Trace.open_smallstep ~line:__LINE__ ()"
# Replace "!!!" with "Trace.open_smallstep ~line:__LINE__ ~reparse:true ()"


INPUT_FILE=$1
OUTPUT_FILE=$2

sed 's/^\([[:space:]]*\)show_\([^[:space:](]*\)/\1show_\2 ~line:__LINE__ /
s/^\([[:space:]]*[^[:space:](]*\)show /\1show ~line:__LINE__ /
s/^\([[:space:]]*\)bigstep/\1Trace.open_bigstep ~line:__LINE__ /
s/^\([[:space:]]*\)!!!/\1Trace.open_smallstep ~line:__LINE__ ~reparse:true ();/
s/^\([[:space:]]*\)!!/\1Trace.open_smallstep ~line:__LINE__ ();/
s/^\([[:space:]]*\)\open Target/\1open Target let ____ = (!!)/' ${INPUT_FILE} > ${OUTPUT_FILE}

# TODO: (*use -w33 attribute on open Target*)
