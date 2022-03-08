
# Example usage for rebuilding just the demo:
#   make optitrust sequence_sub_out.cpp

TARGET_MAKE_ALL := transfo

OPTITRUST := ..

TESTS := pic_demo.ml

FLAGS_MAKEFILE := -report-big-steps -serialized-input none

include ../tests/Common.Makefile


# 'make import' imports the source files from ../case_studies/pic/ 
PICDIR := ../case_studies/pic/

SOURCES := \
	$(PICDIR)/include/optitrust.h \
	$(PICDIR)/include/particle.h \
	$(PICDIR)/include/bag.h \
	$(PICDIR)/include/bag_atomics.h \
	$(PICDIR)/include/mymacros.h \
	$(PICDIR)/src/particle.c \
	$(PICDIR)/src/bag.c \
	$(PICDIR)/simulations/pic_demo.h \
	$(PICDIR)/simulations/pic_demo_aux.h \
	$(PICDIR)/simulations/pic_demo.c \

import:
	cp $(SOURCES) .
	mv pic_demo.c pic_demo.cpp

export:
	cp pic_optimized.cpp $(PICDIR)/simulations/pic_optimized.c

FILES := $(subst pic_demo.c,pic_demo.cpp,$(notdir $(SOURCES)))

cleanfiles:
	rm -f $(FILES)
