#!/bin/bash

# usage: ./vectmerge.sh filename.c mode
#  - the filename.c is searched for in the ../simulations folder
#  - the mode is one of
#    - "light": reports in command line, only about vectorized instructions
#    - "full": produces filename_infos.txt and filename_infos.c
#    - "view": like "full" but open the output using the "meld" diff tool


TARGET=$1
MODE=$2
BASENAME="${TARGET%%.*}"

if [ -z "${MODE}" ]; then
  MODE="light"
fi

readarray -t SRCLINES < ../simulations/${TARGET}

INPUT="${BASENAME}_infos.txt"
OUTPUT="${BASENAME}_infos.c"

# clear file
> ${OUTPUT}

PATTERN=':([0-9]*)'
PATTERNVEC='loop vectorized'
VECCOUNTER=0

LASTLINE=0
while read p; do
  THELINE="$p"
  [[ $THELINE =~ $PATTERN ]]
  REFLINENB="${BASH_REMATCH[1]}"
  REFLINENB=$((REFLINENB-1))

  if [[ $THELINE =~ $PATTERNVEC ]]; then

    if [ "${MODE}" = "light" ]; then
      echo "Vectorized loop:"
      VECCOUNTER=$((VECCOUNTER+1))
      for i in {0..5}; do
        OUTLINE=$((REFLINENB+i))
        echo "   ${SRCLINES[OUTLINE]}"
      done
    else
      echo "CODE:   ${SRCLINES[REFLINENB]}" >> ${OUTPUT}
    fi
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

if [ "${MODE}" = "view" ]; then
   meld ../simulations/${TARGET} ${OUTPUT}
elif [ "${MODE}" = "full" ]; then
  echo "Produced ${OUTPUT}"
elif [ "${MODE}" = "light" ]; then
  echo "Total vectorized: ${VECCOUNTER}"
fi
