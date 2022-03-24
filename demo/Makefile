
# Example usage for rebuilding just the demo:
#   make optitrust sequence_sub_out.cpp

TARGET_MAKE_ALL := transfo


OPTITRUST := ..

TESTS := pic_demo.ml

FLAGS_MAKEFILE := -report-big-steps -serialized-input none

include ../tests/Common.Makefile


# 'make import' imports the source files from ../case_studies/pic/
PICDIR := ../case_studies/pic/
SCRIPTDIR := $(PICDIR)/scripts

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
	echo "ONLY FOR USE WITH let usechecker = true (CHECKER=1)"
	cp pic_demo_out.cpp $(PICDIR)/simulations/pic_optimized_checker.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized_checker.c
	cd $(SCRIPTDIR); ./check.sh pic_barsamian.c pic_optimized_checker.c

export_fast:
	echo "ONLY FOR USE WITH let usechecker = true (CHECKER=1)"
	cp pic_demo_fast_after.cpp $(PICDIR)/simulations/pic_fast_checker.c
	sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_fast_checker.c

FILES := $(subst pic_demo.c,pic_demo.cpp,$(notdir $(SOURCES)))

clean::
	$(V)rm -rf bigsteps

cleanfiles:
	$(V)rm -f $(FILES)

optim: pic_demo_out.cpp 
	$(V)cp pic_demo_out.cpp $(PICDIR)/simulations/pic_optimized.c
	$(V)sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized.c

perf: optim
	$(V)cd $(SCRIPTDIR); ./perf.sh

vect: optim
	$(V)cd $(SCRIPTDIR); ./vectinfo.sh pic_optimized.c light

vectview: optim
	$(V)cd $(SCRIPTDIR); ./vectinfo.sh pic_optimized.c view

%_checker_out.cpp: %_with_lines.$(PROGEXT) %.cpp %.ml %_with_lines.ml
	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS) -usechecker
	$(V)mv $*_out.cpp $*_checker_out.cpp
	@echo "Produced $@"

chk: pic_demo_checker_out.cpp
	$(V)cp pic_demo_checker_out.cpp $(PICDIR)/simulations/pic_optimized_checker.c
	$(V)sed "s/const int CHUNK_SIZE = 128;//" -i $(PICDIR)/simulations/pic_optimized_checker.c
	$(V)cd $(SCRIPTDIR); ./check.sh pic_barsamian.c pic_optimized_checker.c

chk_fast: export_fast
	$(V)cd $(SCRIPTDIR); ./check.sh pic_barsamian.c pic_fast_checker.c

chksteps:
	$(V)FLAGS="-usechecker" $(MAKE) pic_demo.bigsteps
	$(V)cd bigsteps; sed "s/const int CHUNK_SIZE = 128;//" -i pic_demo_checker_*_out.cpp
	$(V)for x in bigsteps/*; do mv $$x bigsteps/"`basename $$x .cpp`.c"; done
	$(V)rm -f $(PICDIR)/simulations/pic_demo_checker_*_out.c
	$(V)cp bigsteps/*.c $(PICDIR)/simulations/
	$(V)cd ../case_studies/pic/scripts; ./check_steps.sh pic_demo_checker

init: optitrust import 

trace: pic_demo_trace.html

traceview: trace
	$(V)chromium-browser pic_demo_trace.html &


# LATER: build different output files depending on the usechecker flag




