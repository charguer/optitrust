#!/bin/bash

# Given a unit test file, extract the lines between the first '!!' and
# until the closing parenthesis at a head of a line.
# Write the output in a file "foo_doc.txt", where "foo" is the basename of the input.
# Usage:
#   ./extract_demo.sh ../tests/basic/label_add.ml
#   ./extract_demo.sh ../tests/basic/label_add.ml output_file.txt

FILE=$1
OUTPUT=$2

if [ -z ${OUTPUT} ]; then
  OUTPUT="${FILE%.*}"_doc.txt
fi

# Line containing the first '!!' symbol
START=`grep -n -m 1 "\!\! " ${FILE} | cut -d : -f 1`

STOP=`grep -n -m 1 "^[ ]*)" ${FILE} | cut -d : -f 1`
STOP=$((STOP-1))

# echo ${START} ${STOP}

if [[ -z "${START}" || -z "${STOP}" ]]; then
   echo "ERROR: could not extract the doc source from ${FILE}"
   exit 1
fi


# Extract the contents of the comment
sed -n "${START},${STOP}p" ${FILE} > ${OUTPUT}

# cat ${OUTPUT}

