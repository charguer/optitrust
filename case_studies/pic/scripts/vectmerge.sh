#!/bin/bash

# usage: ./vectmerge.sh filename.c mode
#  - the filename.c is searched for in the ../simulations folder
#  - the mode is one of
#    - "light": reports in command line, only about vectorized instructions
#       (currently, it also produces the file like "full" does)
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
PATTERNFUNVOID='^void'
PATTERNVEC='loop vectorized'
LASTFUNVOID=
VECCOUNTER=0

LASTLINE=0
while read p; do
  THELINE="$p"
  [[ $THELINE =~ $PATTERN ]]
  HUMANREFLINENB="${BASH_REMATCH[1]}"
  REFLINENB=$((HUMANREFLINENB-1))

  if [[ $THELINE =~ $PATTERNVEC ]]; then

    if [ "${MODE}" = "light" ]; then
      echo "$TARGET:${HUMANREFLINENB}: Vectorized loop:"
      VECCOUNTER=$((VECCOUNTER+1))
      echo "...in ${LASTFUNVOID}..."
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
      SRCELINE="${SRCLINES[LASTLINE]}"
      echo "${SRCELINE}" >> ${OUTPUT}
      LASTLINE=$((LASTLINE+1))
      if [ "${MODE}" = "light" ] && [[ $SRCELINE =~ $PATTERNFUNVOID ]]; then
        LASTFUNVOID="${SRCELINE}"
      fi
    done;
    #echo "" >> ${OUTPUT}
  fi
  LASTLINE=${REFLINENB}
  echo "${THELINE}" >> ${OUTPUT}

done < ${INPUT}

if [ "${MODE}" = "view" ]; then
   meld ../simulations/${TARGET} ${OUTPUT} || echo "Produced ${OUTPUT}"
elif [ "${MODE}" = "full" ]; then
  echo "Produced ${OUTPUT}"
elif [ "${MODE}" = "light" ]; then
  echo "Total vectorized: ${VECCOUNTER}"
fi
