#!/bin/bash
TARGET=$1
if [ -z "$TARGET" ]; then
  TARGET="pic_optimized.c"
fi
BASENAME="${TARGET%%.*}"

OUTPUT="${BASENAME}_infos.txt"
./compile.sh ${TARGET} 2> ${BASENAME}_infos.txt
sed -i 's/^.*case_studies\/pic\/[^\/]*\///' ${OUTPUT}
sed -i '/^\(pic_optimized.c\)/!d' ${OUTPUT}
sort -o ${OUTPUT} ${OUTPUT}
sort pic_optimized_infos.txt | uniq -u > __temp.txt
mv __temp.txt ${OUTPUT}
echo "Produced ${BASENAME}_infos.txt"
