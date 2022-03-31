#!/bin/bash

# usage: ./vectinfo.sh filename.c mode
#  - the filename.c is searched for in the ../simulations folder
#  - the mode is described in vectmerge.sh


TARGET=$1
MODE=$2

if [ -z "${TARGET}" ]; then
  TARGET="pic_optimized.c"
fi
if [ -z "${MODE}" ]; then
  MODE="light"
fi

BASENAME="${TARGET%%.*}"

OUTPUT="${BASENAME}_infos.txt"
export VECTINFOS=" -fopt-info-vec-all"
#export VECTINFOS=" -fopt-info-vec-missed"

./compile.sh ${COMP} ${TARGET} 2> ${OUTPUT}

# remove the long paths
sed -i 's/^.*case_studies\/pic\/[^\/]*\///' ${OUTPUT}

# keep only the lines concerning the main file
PATTERN='/^\('
PATTERN+="${TARGET}"
PATTERN+='\)/!d'
sed -i ${PATTERN} ${OUTPUT}

# sort the lines
sort ${OUTPUT} | uniq -u > __temp.txt
mv __temp.txt ${OUTPUT}

# prepare the output in a readable format
./vectmerge.sh ${TARGET} ${MODE}



#---------
# echo "Produced ${OUTPUT}"
# 2> vect_info.txt -fopt-info-vec-missed -ftree-vectorize
# -fopt-info-vec-missed -fopt-info-all -fopt-info-vec-all
# GENERATE ASSEMBLY -Wa,-adhln"
# VECTINFOS=" -fopt-info-vec-all"
