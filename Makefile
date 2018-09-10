#
# This Makefile is a wrapper for the generic Makefile provided by TLC
#


##############################################################################
# Certain custom settings can be defined in settings.sh.

# To specify the location of the Coq binaries, define COQBIN (with a
# trailing slash), e.g. COQBIN=/var/tmp/coq/bin/.
# If COQBIN is undefined, then "coqc" is used.

-include settings.sh

export COQBIN


############################################################################
# We assume that TLC has been installed.
# (Note: this definition can be overriden from outside.)

ifndef TLC
	TLC := $(shell $(COQBIN)coqc -where)/user-contrib/TLC
endif

ifeq ($(wildcard $(TLC)),)
  $(error $(TLC) does not exist. \
          Please install TLC first)
endif

export TLC


##############################################################################
# List of files

SRC := util/TLCbuffer util/MyLibVar util/Bind Language Wellformedness Typing SemanticsLL Semantics TypeSoundness TrLowLevel TrLowLevelFun TrGroup TrGroupFun TrTiling TrTilingFun TrSoA TrSoAFun


# using the variable SRC_CUSTOM, one can modify the compilation targets 
# and/or their order.

ifdef SRC_FORCE
	SRC := $(SRC_FORCE)
endif


##############################################################################
# Compilation settings

PWD := $(shell pwd)

V := $(addprefix $(PWD)/,$(SRC:=.v))

COQFLAGS:=-w -notation-overridden,-implicits-in-term,-redundant-canonical-projection,-several-object-files

COQINCLUDE := \
  -R $(TLC) TLC -R $(PWD)/util util

include $(TLC)/Makefile.coq
