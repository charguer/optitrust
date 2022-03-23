
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

# 	$(PICDIR)/src/optitrust.c \

import:
	cp $(SOURCES) .
	mv pic_demo.c pic_demo.cpp

export:
	cp pic_demo_out.cpp $(PICDIR)/simulations/pic_optimized.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized.c

export_fast_before:
	cp pic_demo_fast_before.cpp $(PICDIR)/simulations/pic_optimized.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized.c

export_fast:
	cp pic_demo_fast_after.cpp $(PICDIR)/simulations/pic_optimized.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized.c

export_steps: optitrust import pic_demo.bigsteps export_bigsteps

export_bigsteps:
	cd bigsteps; sed "s/const int CHUNK_SIZE = 128;//" -i pic_demo_*_out.cpp
	for x in bigsteps/*; do mv $$x bigsteps/"`basename $$x .cpp`.c"; done
	rm -f $(PICDIR)/simulations/pic_demo_*_out.c
	cp bigsteps/*.c $(PICDIR)/simulations/

FILES := $(subst pic_demo.c,pic_demo.cpp,$(notdir $(SOURCES)))

clean::
	rm -rf bigsteps

cleanfiles:
	rm -f $(FILES)

perf: pic_demo_out.cpp 
	$(MAKE) export
	cd $(PICDIR)/scripts; ./perf.sh

%_checker_out.cpp: %_with_lines.$(PROGEXT) %.cpp %.ml %_with_lines.ml
	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS) -usechecker
	@echo "Produced $@"

chk: pic_demo_checker_out.cpp
	cp pic_demo_checker_out.cpp $(PICDIR)/simulations/pic_optimized_checker.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized_checker.c
	cd $(PICDIR)/scripts; ./check.sh pic_barsamian.c pic_optimized_checker.c

# LATER: build different output files depending on the usechecker flag
