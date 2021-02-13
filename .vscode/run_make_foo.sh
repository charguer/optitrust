#!/bin/bash


#move to the unit_tests directory


# Go to the directory
DIRNAME=$1
FOO=$2
CHK=$3

cd ${DIRNAME}

if [[ $CHK == "" ]]
then
    make ${FOO}.out
else
    make ${FOO}.chk
fi