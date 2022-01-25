#!/bin/bash

echo "deprecated script"
exit 1

# git clone https://github.com/AbsInt/CompCert.git
# ./configure x86_64-linux && make -j4 all

SRC="/home/charguer/shared/CompCert"
DEST=`pwd`


FILES="\
  cparser/*.ml \
  cparser/*.mli \
  extraction/*.ml \
  extraction/*.mli \
  lib/*.ml \
  lib/*.mli \
  driver/*.ml \
  driver/*.mli \
  x86_64/*.ml \
  x86_64/*.mli \
  debug/*.ml \
  debug/*.mli \
  common/*.ml \
  common/*.mli \
  x86/CBuiltins.ml \
  compcert.ini \
  "

#echo "cd ${SRC}"
#echo "cp ${FILES} ${DEST}"

cd ${SRC}
cp ${FILES} ${DEST}

cd ${DEST}
git add *.ml *.mli compcert.ini
ocamlbuild -package str,unix,menhirLib Test.byte


