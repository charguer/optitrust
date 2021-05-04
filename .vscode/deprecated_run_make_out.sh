#!/bin/bash


#move to the unit_tests directory


# Go to the directory
DIRNAME=$1
FOO=$2
RECOMPILE_OPTITRUST=$3
CHK=$4

cd ${DIRNAME}
if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ]; then
  echo "recompile lib"
  make optitrust
  OUT=$?
  if [ ${OUT} -ne 0 ];then
    echo "Could not compile lib"
    exit 1
  fi
fi


if [[ $CHK == "" ]]
then
    make ${FOO}.out
else
    make ${FOO}.chk
fi