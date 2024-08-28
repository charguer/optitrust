#!/bin/bash

DEMO=$1

if [ "${DEMO}" = "rowsum" ]; then

  cd case_studies/opencv
  ../../tools/open_trace.sh rowsum

elif [ "${DEMO}" = "matmul" ]; then

  cd case_studies/matmul
  ../../tools/open_trace.sh matmul_check

elif [ "${DEMO}" = "pic" ]; then

  cd case_studies/minipic
  ../../tools/open_trace.sh micropic

else

  ./demo.sh rowsum
  ./demo.sh matmul

fi

