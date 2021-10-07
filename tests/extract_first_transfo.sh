#!/bin/bash

FILENAME=$1
OUTPUT="${FILENAME%.*}"_one.ml

LINECUT=`grep -n "\!\!" ${FILENAME} | awk -F  ":" '{print $1}' | tail -n +2 | head -1`

if [ -z "${LINECUT}" ]; then

   cp ${FILENAME} ${OUTPUT}

else

  sed "${LINECUT},1000d" ${FILENAME} > ${OUTPUT}

  echo ")" >> ${OUTPUT}

fi


echo "Generated ${OUTPUT}"

# example usage: ./extract_first_transfo.sh basic/label_add.ml
# produces
