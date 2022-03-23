#!/bin/bash

TARGET=$1
BASENAME="${TARGET%%.*}"

readarray -t SRCLINES < ../simulations/${TARGET}

INPUT="${BASENAME}_infos.txt"
OUTPUT="${BASENAME}_infos.c"

# clear file
> ${OUTPUT}

PATTERN=':([0-9]*)'
PATTERNVEC='loop vectorized'

LASTLINE=0
while read p; do
  THELINE="$p"
  [[ $THELINE =~ $PATTERN ]]
  REFLINENB="${BASH_REMATCH[1]}"
  REFLINENB=$((REFLINENB-1))

  if [[ $THELINE =~ $PATTERNVEC ]]; then
    echo "CODE:   ${SRCLINES[REFLINENB]}" >> ${OUTPUT}
  fi

  if [ ${REFLINENB} != ${LASTLINE} ]; then
    while [[ ${LASTLINE} < ${REFLINENB} ]]; do
      echo "${SRCLINES[LASTLINE]}" >> ${OUTPUT}
      LASTLINE=$((LASTLINE+1))
    done;
    #echo "" >> ${OUTPUT}
  fi
  LASTLINE=${REFLINENB}
  echo "${THELINE}" >> ${OUTPUT}

done < ${INPUT}

echo "Produced ${OUTPUT}"
COMMAND="meld ../simulations/${TARGET} ${OUTPUT}"
echo "View gectorization information using:\n ${COMMAND}"
${COMMAND} &
