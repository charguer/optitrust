
# Example usage for rebuilding just the demo:
#   make optitrust sequence_sub_out.cpp

TARGET_MAKE_ALL := transfo

OPTITRUST := ..

TESTS := pic_demo.ml

FLAGS_MAKEFILE := -report-big-steps -serialized-input none

include ../tests/Common.Makefile


# 'make import' imports the source files from ../case_studies/pic/ 
PICDIR := ../case_studies/pic/

import:
	cp $(PICDIR)/simulations/pic_demo.c .
	cp $(PICDIR)/include/pic_demo .
	cp $(PICDIR)/src/pic_demo .


