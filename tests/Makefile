# DISCLAIMER: make -j does not work due to ocamlbuild, it will work when using dune --LATER

##############################################################################
# Folders

TEST_DIRS = \
	basic \
	combi \

# Path to the folder containing optitrust main Makefile, on which to call make install
OPTITRUST ?= ..

TARGETS=all transfo check clean batch batch_check

##############################################################################
# Targets

.PHONY: $(TARGETS) seq

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
