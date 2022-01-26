#!/bin/bash

OPTITRUST=$1
FOLDER=$2
BASENAME=$3
OUTFILE=$4

cp ${OPTITRUST}/doc/doc_template.html ${OUTFILE}

echo "<script src="${BASENAME}_doc.js"></script>" >> ${OUTFILE}
echo "<div class="test" id="${FOLDER}__${BASENAME}"></div>" >> ${OUTFILE}
echo "</body></html>" >> ${OUTFILE}

