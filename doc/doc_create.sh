#!/bin/bash

OPTITRUST=$1
BASENAME=$2
# eg BASENAME="label_add"
JSFILE="${BASENAME}_doc.js"
OUTFILE="${BASENAME}_doc.html"

cp ${OPTITRUST}/doc/doc_template.html ${OUTFILE}

# DEPRECATED echo "<h2>${BASENAME}</h2>" >> ${OUTFILE}

echo "<script src="${JSFILE}"></script>" >> ${OUTFILE}
echo "<div class="test" id="${BASENAME}"></div>" >> ${OUTFILE}
echo "</body></html>" >> ${OUTFILE}

# FOLDER=$3
# DEPRECATED: id="${FOLDER}__${BASENAME}

