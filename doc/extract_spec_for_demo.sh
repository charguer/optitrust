#!/bin/bash

# Given a file containing a piece of script, e.g.  !! Foo.bar args
# extract the documentation of function [bar] from source file [Foo].
# If the input file is [foo_doc.txt], write the output in [foo_doc_spec.txt].
# example usage: ./extract_spec_for_demo.sh ../tests/basic/label_add_doc.txt ..

FILE=$1
OPTITRUST_PATH=$2
OUTPUT="${FILE%.*}"_spec.txt

REGEXP="\!\! ([^.]*)\.([^ ]*)"
CONTENTS=`cat ${FILE}`
# echo ${CONTENTS}

[[ ${CONTENTS} =~ ${REGEXP} ]]

MODULE=${BASH_REMATCH[1]}
FCT=${BASH_REMATCH[2]}

# echo ${MODULE} ${FCT}

SRC_FILE=`echo ${MODULE} | tr '[:upper:]' '[:lower:]'`
SRC_PATH=${OPTITRUST_PATH}/src/${SRC_FILE}.ml

# echo ${SRC_PATH}

${OPTITRUST_PATH}/doc/extract_spec.sh ${SRC_PATH} ${FCT} > ${OUTPUT}

# cat ${OUTPUT}

