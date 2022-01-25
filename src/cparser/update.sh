#!/bin/bash

# git clone https://github.com/AbsInt/CompCert.git
# ./configure x86_64-linux && make extraction

SRC="/home/charguer/shared/CompCertCharguer"
DEST=`pwd`

FILES="\
  cparser/Cprint.ml \
  cparser/Elab.ml \
  cparser/Ceval.ml \
  cparser/Cutil.ml \
  cparser/Cutil.mli \
  cparser/C.mli \
  cparser/Env.ml \
  cparser/Env.mli \
  cparser/pre_parser.ml \
  cparser/pre_parser.mli \
  cparser/pre_parser_aux.ml \
  cparser/pre_parser_aux.mli \
  cparser/pre_parser_messages.mli \
  x86/CBuiltins.ml \
  extraction/Cabs.ml \
  extraction/Cabs.mli \
  extraction/Parser.ml \
  extraction/Parser.mli \
  "

cd ${SRC}
cp ${FILES} ${DEST}
echo "Copied files: ${FILES}"

cd ${DEST}
sed -i 's/Cleanup\.program p/p/;s/Checks\.unused_variables p;//;s/Checks\.unknown_attrs_program p;//;s/Checks\.non_linear_conditional p;//' Elab.ml
sed -i '' Elab.ml
echo "Patched Elab.ml"


#  cparser/StructPassing.ml \
#  cparser/PackedStructs.ml \
#  cparser/Rename.ml \
#  cparser/Unblock.ml \
#  cparser/Transform.ml \
#  cparser/Cleanup.ml \
#  cparser/Cleanup.mli \"