#!/bin/bash

#TODO: factorize this script
CURDIR=`pwd`

cd ${CURDIR}/tests/basic

NBBASIC=`ls *.ml | wc -l`

echo "${NBBASIC} basic unit tests"

cd  ${CURDIR}/tests/combi
NBCOMBI=`ls *.ml | wc -l`
echo "${NBCOMBI} combi unit tests"

NBTESTS=$((NBBASIC+NBCOMBI))
echo "${NBTESTS} unit tests"


cd ${CURDIR}/lib
FILES=`ls *.ml`
NBFILES=`ls *.ml | wc -l`
echo "${NBFILES} files in ml"

NBLINES=`wc -l  ${FILES} | tail -1`
echo "${NBLINES} lines of code"



cd ${CURDIR}/tests/basic
FILES=`ls *.ml`
NBSTEPS_BASIC=`grep -o '!!' ${FILES} | wc -l`
echo "${NBSTEPS_BASIC} steps in basic unit tests"

cd ${CURDIR}/tests/combi
FILES=`ls *.ml`
NBSTEPS_COMBI=`grep -o '!!' ${FILES} | wc -l`
echo "${NBSTEPS_COMBI} steps in combi unit tests"

NBSTEPS=$((NBSTEPS_BASIC+NBSTEPS_COMBI))
echo "${NBSTEPS} steps in all unit tests"


cd ${CURDIR}/demo
FILE="pic_demo.ml"
NBSTEPS=`grep -o '!!' ${FILE} | wc -l`

NB_REPARSE=`grep -o 'reparse()' ${FILE} | wc -l`

echo "${NBSTEPS} steps in demo"

NB_BIGSTEPS=`grep -o 'bigstep' ${FILE} | wc -l`
echo "${NB_BIGSTEPS} big steps in demo"






