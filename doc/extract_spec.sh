#/bin/bash

# Extract the comment above the definition of function $2 in file $1
# For debugging: ./extract_function_doc.sh ../src/label_basic.ml add

# File where to search for the function
FILE=$1
# Name of the function to search for
FCT=$2

# Line containing the function definition
LINE=`grep -n "^let ${FCT} " ${FILE} | cut -d : -f 1`

# Number of lines of the comments above the function
STOP=$((LINE-1))
NB=`head -${STOP} ${FILE} | tac | grep -m 1 -n "^(\*" | cut -d : -f 1`

# Line of the start of the comment
START=$((LINE-NB))

#echo ${NB} ${STOP} ${START}

# Extract the contents of the comment
sed -n "${START},${STOP}p" ${FILE}
