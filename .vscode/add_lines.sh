#!/bin/sh

# Usage: add_lines.sh input.ml output.ml

# Replace "!!^" with "Trace.check_exit_and_step ~line:__LINE__ ~reparse:true ()"
# Replace "!!!" with "Trace.check_exit_and_step ~line:__LINE__ ~ris_small_step:false ()"
# Replace "!!" with "Trace.check_exit_and_step ~line:__LINE__ ()"

INPUT_FILE=$1
OUTPUT_FILE=$2

sed 's/^\([[:space:]]*\)show /\1show ~line:__LINE__ /;s/\!\!\^/Trace.check_exit_and_step ~line:__LINE__ ~reparse:true ();/;s/!!!/Trace.check_exit_and_step ~line:__LINE__ ~is_small_step:false ();/;s/!!/Trace.check_exit_and_step ~line:__LINE__ ();/' ${INPUT_FILE} > ${OUTPUT_FILE}
