#!/bin/bash

# git clone https://github.com/AbsInt/CompCert.git
# ./configure x86_64-linux && make extraction

SRC="/home/charguer/shared/CompCertCharguer"
DEST=`pwd`


# TEMPORARY: were directly modified
# cparser/Elab.ml \
#  cparser/Elab.mli \
#  cparser/C.ml \
#  cparser/C.mli \
#  cparser/Cutil.ml \

FILES="\
  cparser/Cprint.ml \
  cparser/Ceval.ml \
  cparser/Cutil.mli \
  cparser/Env.ml \
  cparser/Env.mli \
  cparser/pre_parser.ml \
  cparser/pre_parser.mli \
  cparser/pre_parser_aux.ml \
  cparser/pre_parser_aux.mli \
  cparser/pre_parser_messages.ml \
  driver/Driveraux.ml \
  driver/Driveraux.mli \
  x86/CBuiltins.ml \
  extraction/Cabs.ml \
  extraction/Cabs.mli \
  cparser/Lexer.ml \
  extraction/Parser.ml \
  extraction/Parser.mli \
  "

cd ${SRC}
cp ${FILES} ${DEST}
echo "Copied files: ${FILES}"

cd ${DEST}
# TEMPORARY sed -i 's/Cleanup\.program p/List.rev p/;s/Checks\.unused_variables p;//;s/Checks\.unknown_attrs_program p;//;s/Checks\.non_linear_conditional p;//' Elab.ml
sed -i 's/ccomp:/OptiTrust-Menhir-parser:/' Diagnostics.ml

echo "Patched Elab.ml and Diagnostics.ml to remove cleanup and checks."

CONFIGPAT='s/Configuration\./Config./'
sed -i ${CONFIGPAT} Lexer.ml
sed -i ${CONFIGPAT} Archi.ml
sed -i ${CONFIGPAT} Driveraux.ml
sed -i ${CONFIGPAT} Clflags.ml

echo "Patched Lexer.ml, Archi.ml, Driveraux.ml and Clflags.ml to rename Configuration to Config."

echo "Warning: C2C.ml is not copied automatically, because the patch was done by hand."
# ARTHUR: should automate the patch at some point.


#  cparser/StructPassing.ml \
#  cparser/PackedStructs.ml \
#  cparser/Rename.ml \
#  cparser/Unblock.ml \
#  cparser/Transform.ml \
#  cparser/Cleanup.ml \
#  cparser/Cleanup.mli \"
