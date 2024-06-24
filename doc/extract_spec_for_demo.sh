#!/bin/bash

# Given a file containing a piece of script, e.g.  !! Foo.bar args
# extract the documentation of function [bar] from source file [Foo].
# If the input file is [foo_doc.txt], write the output in [foo_doc_spec.txt].
# example usage: ./extract_spec_for_demo.sh ../tests/basic/label_add_doc.txt ..

FILE=$1
OPTITRUST_PATH=$2
OUTPUT="${FILE%.*}"_spec.txt

if [ ! -f "${FILE}" ]; then
  echo "extract_spec_for_demo: file ${FILE} not found." >> /dev/stderr
  exit 1;
fi


REGEXP="\!\! ([^.]*)\.[(]?([^ (]*)"
CONTENTS=`cat ${FILE}`

# echo ${CONTENTS}

[[ ${CONTENTS} =~ ${REGEXP} ]]

MODULE=${BASH_REMATCH[1]}
FCT=${BASH_REMATCH[2]}

# echo ${MODULE} ${FCT}

SRC_FILE=`echo ${MODULE} | tr '[:upper:]' '[:lower:]'`
SRC_PATH=${OPTITRUST_PATH}/lib/framework/${SRC_FILE}.ml

# echo ${SRC_PATH}

if [ ! -f "${SRC_PATH}" ]; then
  echo "extract_spec_for_demo: file ${SRC_PATH} not found."  >> /dev/stderr
  exit 1;
fi

SRC_PATH_BASIC="${SRC_PATH%.*}_basic.ml"

# echo "${OPTITRUST_PATH}/doc/extract_spec.sh ${SRC_PATH} ${FCT} > ${OUTPUT}"

# Search first in the main source file, then in the _basic version of the file.
${OPTITRUST_PATH}/doc/extract_spec.sh ${SRC_PATH} ${FCT} > ${OUTPUT} 2>/dev/null
OUT=$?
if [ ${OUT} -ne 0 ]; then
  if [ ! -f "${SRC_PATH_BASIC}" ]; then
    echo "extract_spec_for_demo: could not find spec for ${FCT} in ${SRC_PATH}."  >> /dev/stderr
    exit 1;
  else
    ${OPTITRUST_PATH}/doc/extract_spec.sh ${SRC_PATH_BASIC} ${FCT} > ${OUTPUT} 2>/dev/null
    OUT2=$?
    if [ ${OUT2} -ne 0 ]; then
      echo "extract_spec_for_demo: could not find spec for ${FCT} in ${SRC_PATH} or in ${SRC_PATH_BASIC}." >> /dev/stderr
      exit 1;
    fi
  fi
fi



# cat ${OUTPUT}

