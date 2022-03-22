#!/bin/bash
TARGET=$1
if [ -z "$TARGET" ]; then
  TARGET="pic_optimized.c"
fi
BASENAME="${TARGET%%.*}"

OUTPUT="${BASENAME}_infos.txt"
export VECTINFOS=" -fopt-info-vec-all"
#export VECTINFOS=" -fopt-info-vec-missed"

./compile.sh ${TARGET} 2> ${BASENAME}_infos.txt
sed -i 's/^.*case_studies\/pic\/[^\/]*\///' ${OUTPUT}
sed -i '/^\(pic_optimized.c\)/!d' ${OUTPUT}
sort -o ${OUTPUT} ${OUTPUT}
sort pic_optimized_infos.txt | uniq -u > __temp.txt
mv __temp.txt ${OUTPUT}
# echo "Produced ${BASENAME}_infos.txt"
./vectmerge.sh ${TARGET}


# 2> vect_info.txt -fopt-info-vec-missed -ftree-vectorize
# -fopt-info-vec-missed -fopt-info-all -fopt-info-vec-all
# GENERATE ASSEMBLY -Wa,-adhln"
# VECTINFOS=" -fopt-info-vec-all"
