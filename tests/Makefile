# DISCLAIMER: make -j does not work due to ocamlbuild, it will work when using dune --LATER

##############################################################################
# Folders

TEST_DIRS = \
	basic \
	combi \
	target \
	ast \

# Path to the folder containing optitrust main Makefile, on which to call make install
OPTITRUST ?= ..

TARGETS=transfo check clean batch batch_check

##############################################################################
# Targets

.PHONY: all $(TARGETS) seq

all: clean optitrust
	$(MAKE) -C basic batch_check
	$(MAKE) -C combi batch_check
	$(MAKE) -C target batch_check
	$(MAKE) -C ast batch_check




# "make all" and "make clean" can be run in parallel on all directories.

# "make optitrust" reinstalls the library once, and clean all directories
optitrust:
	$(MAKE) -C $(OPTITRUST) install
	$(MAKE) clean

batch_recheck: optitrust
	$(MAKE) batch_check

$(TARGETS): $(TEST_DIRS)

$(TEST_DIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

# "make seq" forces sequential compilation of the directories, and
# stops at the first error.

seq:
	for folder in $(EXAMPLE_DIRS); do make -C $${folder} || exit 1 ; done
