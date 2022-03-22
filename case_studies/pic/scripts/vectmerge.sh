#!/bin/bash

TARGET=$1
BASENAME="${TARGET%%.*}"

readarray -t SRCLINES < ../simulations/${TARGET}

INPUT="${BASENAME}_infos.txt"
OUTPUT="${BASENAME}_infos.c"

echo "" > ${OUTPUT}

PATTERN=':([0-9]*)'
PATTERNVEC='loop vectorized'

LASTLINE=-1
while read p; do
  THELINE="$p"
  [[ $THELINE =~ $PATTERN ]]
  REFLINENB="${BASH_REMATCH[1]}"
  REFLINENB=$((REFLINENB-1))

  if [[ $THELINE =~ $PATTERNVEC ]]; then
    echo "CODE:   ${SRCLINES[REFLINENB]}" >> ${OUTPUT}
  fi

  if [ ${LASTLINE} != -1 ] && [ ${REFLINENB} != ${LASTLINE} ]; then
    echo "CODE:   ${SRCLINES[LASTLINE]}" >> ${OUTPUT}
    echo "" >> ${OUTPUT}
  fi
  LASTLINE=${REFLINENB}
  echo "${THELINE}" >> ${OUTPUT}

done < ${INPUT}

echo "CODE:   ${SRCLINES[LASTLINE]}" >> ${OUTPUT}
echo "Produced ${OUTPUT}"
