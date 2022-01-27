#!/bin/bash

# This script generates an html page containing a list of documentation items
# Usage: ./doc_create.sh $(path_to_optitrust) $(output_file) $(list_of_basename_of_unit_tests_to_include)
# e.g. one BASENAME can be "label_add"

OPTITRUST=$1
OUTFILE=$2
shift 2
BASENAMES=$*

cp ${OPTITRUST}/doc/doc_template.html ${OUTFILE}

NBBASENAMES=`echo "${BASENAMES}" | wc -w`

if [ ${NBBASENAMES} = 1 ]; then
  sed -i "s/<\/title>/ for ${BASENAMES}<\/title>/" ${OUTFILE}
fi

for BASENAME in ${BASENAMES}; do
  # echo "Including ${BASENAME}"

  JSFILE="${BASENAME}_doc.js"
  echo "<script src="${JSFILE}"></script>" >> ${OUTFILE}
  echo "<div class="test" id="${BASENAME}"></div>" >> ${OUTFILE}

done

echo "</body></html>" >> ${OUTFILE}


# DEPRECATED: id="${FOLDER}__${BASENAME}
# DEPRECATED  OUTFILE="${BASENAME}_doc.html"