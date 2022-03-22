#!/bin/bash
LINE="pic_optimized.c:105:8: note: not vectorized: not enough data-refs in basic block."

PATTERN=':([0-9]*)'


readarray -t SRCLINES < ../simulations/pic_optimized.c

INPUT="pic_optimized_infos.txt"

OUTPUT="pic_optimized_infos.c"

echo "" > ${OUTPUT}

LASTLINE=0
while read p; do
  THELINE="$p"
  [[ $THELINE =~ $PATTERN ]] # $pat must be unquoted
  REFLINENB="${BASH_REMATCH[1]}"
  REFLINENB=$((REFLINENB-1))
  echo "${THELINE}" >> ${OUTPUT}

  if [ ${REFLINENB} != ${LASTLINE} ]; then
    echo "${SRCLINES[REFLINENB]}" >> ${OUTPUT}
    echo "" >> ${OUTPUT}
  fi
  LASTLINE=${REFLINENB}

done < ${INPUT}

echo "${OUTPUT} produced"
