#!/bin/bash

# Variant of view_result.sh that is able to:
# - if ${VIEW} is "save_intermediate_state" then save the state at a given line,
#    generating ${FILEBASE}_inter.cpp  for the intermediate state
#    and ${FILEBASE}_inter.ml for the prefix of the transformation script used
#    (the line number at which the script was cut can be recovered from the number of lines of that file)
# - if ${VIEW} is "view_diff" executes the transformation script from the saved line until the cursor line

DIRNAME=$1
FILEBASE=$2
LINE=$3 # should be the first line to be not executed
VIEW=$4 # should be view_diff or save_intermediate_state
RECOMPILE_OPTITRUST=$5 # should be recompile_optitrust_yes or recompile_optitrust_no
OPTIONS=$6
OPTIONS2=$7

# Path to .vscode folder and src folder and src/src folder
VSCODE=`pwd`
SRCFOLDER=`cd .. && pwd`
SRCSRCFOLDER=`cd ../src && pwd`

# This can help with opam switches
eval $(opam env)

# Make sure we work in the directory that contains the file
cd ${DIRNAME}


#----------------------------------------------------
# Prepare the transformation program

if [ "${VIEW}" = "view_diff" ]; then

  # Read options from optitrust_flags.sh
  # options: e.g., FLAGS="-dump-ast-details -analyse-time-details -debug-reparse"
  if [ -f "optitrust_flags.sh" ]; then
    source optitrust_flags.sh
  fi

  # Generate "${FILEBASE}_fast.ml" as a version of the script with lines
  # that were saved in "${FILEBASE}_inter.ml" replaced with blank lines.

  SOURCEBASE="${FILEBASE}_fast"

  if [ ! -f "${FILEBASE}_inter.ml" ]; then
    echo "Error: intermediate state ${FILEBASE}_inter.ml does not exist. Try saving a state first."  >> /dev/stderr
    exit 1
  fi

  # Compute the number of lines in the intermediate script
  LINE_INTER=`wc -l ${FILEBASE}_inter.ml | awk '{ print $1 }'`

  # Error message if requested line is prior to the saved point
  if [ "${LINE}" -lt "${LINE_INTER}" ]; then
    echo "Error: line ${LINE} is before the saved intermediate state at line ${LINE_INTER}"  >> /dev/stderr
    exit 1
  fi

  # Compute the line containing the call to "script_cpp"
  LINE_RUNSCRIPT=`grep -n "Run.script_cpp" ${FILEBASE}.ml | cut -d : -f 1`

  # Error message if lines are not found
  if [ -z "${LINE_RUNSCRIPT}" ] || [ -z "${LINE_INTER}" ]; then
    echo "Error: cannot use intermediate state"  >> /dev/stderr
    exit 1
  fi

  # Make the lines blank in the script on the prefix that produces the intermediate state
  LINE_START=$((LINE_RUNSCRIPT+1))
  LINE_STOP=$((LINE_INTER-1))

  echo "Using cache up to line ${LINE_STOP}" # from ${LINE_START}
  sed "${LINE_START}","${LINE_STOP}"'{s/^.*$/  /;}' ${FILEBASE}.ml > ${SOURCEBASE}.ml

  # Patch the call to "script_cpp" so that it loads the saved intermediate CPP file
  sed -i "s/script_cpp/script_cpp ~filename:\"${FILEBASE}_inter_before.cpp\" ~prefix:\"${FILEBASE}_fast\"/;" ${SOURCEBASE}.ml

  # echo "Produced ${SOURCEBASE}.ml"

elif [ "${VIEW}" = "save_intermediate_state" ]; then

  # Generate "${FILEBASE}_inter.ml" as a trimmed version of the "${FILEBASE}.ml" script,
  # by removing all the lines starting from ${LINE}.

  SOURCEBASE="${FILEBASE}_inter"

  LINESTOKEEP=$((LINE-1))
  # echo "LINESTOKEEP=${LINESTOKEEP}"
  head -${LINESTOKEEP} ${FILEBASE}.ml > ${SOURCEBASE}.ml
  echo "  !!(); )" >> ${SOURCEBASE}.ml

  # Patch the call to "script_cpp" so that it loads the right CPP file
  sed -i "s/script_cpp/script_cpp ~filename:\"${FILEBASE}.cpp\" ~prefix:\"${FILEBASE}_inter\"/;" ${SOURCEBASE}.ml

  echo "Produced ${SOURCEBASE}.ml"

  #TODO: FLAGS=-serialized-output only
  # in Flags.ml: -serialized-output  "choose between 'yes' or 'no' or 'only'"
  #   - yes means that the output file is serialized
  #   - no means that the output file is not serialized
  #   - only means that only the serialized output is created
  #TODO: we should trim the script at the LINE_STOP and run it without an exit line number to produce the intermediate file
else

  echo "invalid VIEW argument"  >> /dev/stderr
  exit 1

fi

#----------------------------------------------------
# Instrument line numbers in the transformation program

${VSCODE}/add_lines.sh ${SOURCEBASE}.ml ${SOURCEBASE}_with_lines.ml

#----------------------------------------------------
# Compile the transformation program

PROG="${SOURCEBASE}_with_lines.byte"

# TODO: what is the rule for needs rebuild? and what is the rule for declaring the inter files out of date?
# if [[ "${FILEBASE}.ml" -nt "${PROG}" ]] || [[ "${FILEBASE}.cpp" -nt "${PROG}" ]]; then
#   # echo FILE1 is newer than FILE2
#   PROGNEEDSREBUILD="needsrebuild"
# fi

# if [ "${RECOMPILE_OPTITRUST}" = "recompile_optitrust_yes" ] || [ "${PROGNEEDSREBUILD}" = "needsrebuild" ]; then
ocamlbuild -tag debug -quiet -r -pkgs clangml,refl,pprint,str,optitrust ${PROG}
#fi

OUT=$?
if [ ${OUT} -ne 0 ];then
  echo "Could not compile file"  >> /dev/stderr
  exit 1
fi

#----------------------------------------------------
# Execute the transformation program

# Execute with backtrace activated, and specifying the -exit-line value
OCAMLRUNPARAM=b ./${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2} ${FLAGS} -report-big-steps
# echo "./${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2}"

OUT=$?
if [ ${OUT} -ne 0 ];then
  #echo "Error executing the script:"
  #echo "  cd ${DIRNAME}; ./${PROG} -exit-line ${LINE} ${OPTIONS} ${OPTIONS2}"
  exit 1
fi

#----------------------------------------------------

if [ "${VIEW}" = "view_diff" ]; then

   # We need to cd to ${VSCODE} folder because that's how the scripts know the path to .vscode
   cd ${VSCODE}
  ./open_diff.sh ${DIRNAME} ${SOURCEBASE} &

elif [ "${VIEW}" = "save_intermediate_state" ]; then

  # TODO: we could clear ${SOURCEBASE}_after.cpp and rename ${SOURCEBASE}_before.cpp to a different name

  echo "Produced ${SOURCEBASE}_before.cpp as checkpoint for line ${LINE}"

fi

exit
