#!/bin/bash

# git clone https://github.com/AbsInt/CompCert.git
# ./configure x86_64-linux && make extraction

SRC="/home/charguer/shared/CompCertCharguer"
DEST=`pwd`

FILES="\
  cparser/Cprint.ml \
  cparser/Elab.ml \
  cparser/StructPassing.ml \
  cparser/Ceval.ml \
  cparser/Cutil.ml \
  cparser/Cutil.mli \
  cparser/C.mli \
  cparser/Env.ml \
  cparser/Env.mli \
  x86/CBuiltins.ml \
  extraction/Cabs.ml \
  "

cd ${SRC}
cp ${FILES} ${DEST}
echo "Copied files: ${FILES}"
