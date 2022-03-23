#!/bin/bash
TARGET=$1
if [ -z "$TARGET" ]; then
  TARGET="pic_optimized.c"
fi
BASENAME="${TARGET%%.*}"

OUTPUT="${BASENAME}_infos.txt"
export VECTINFOS=" -fopt-info-vec-all"
#export VECTINFOS=" -fopt-info-vec-missed"

./compile.sh ${TARGET} 2> ${OUTPUT}
sed -i 's/^.*case_studies\/pic\/[^\/]*\///' ${OUTPUT}
PATTERN='/^\('
PATTERN+="${TARGET}"
PATTERN+='\)/!d'
echo ${PATTERN};
sed -i ${PATTERN} ${OUTPUT}
sort -o ${OUTPUT} ${OUTPUT}
sort ${OUTPUT} | uniq -u > __temp.txt
mv __temp.txt ${OUTPUT}
echo "Produced ${OUTPUT}"
./vectmerge.sh ${TARGET}


# 2> vect_info.txt -fopt-info-vec-missed -ftree-vectorize
# -fopt-info-vec-missed -fopt-info-all -fopt-info-vec-all
# GENERATE ASSEMBLY -Wa,-adhln"
# VECTINFOS=" -fopt-info-vec-all"
