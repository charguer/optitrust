# DISCLAIMER: make -j does not work due to ocamlbuild, it will work when using dune --LATER

##############################################################################
# Folders

TEST_DIRS = \
	basic \
	combi \

TARGETS=all optitrust transfo check clean

##############################################################################
# Targets

.PHONY: $(TARGETS) seq

# "make all" and "make clean" can be run in parallel on all directories.

$(TARGETS): $(TEST_DIRS)

$(TEST_DIRS)::
	$(MAKE) -C $@ $(MAKECMDGOALS)

# "make seq" forces sequential compilation of the directories, and
# stops at the first error.

seq:
	for folder in $(EXAMPLE_DIRS); do make -C $${folder} || exit 1 ; done
