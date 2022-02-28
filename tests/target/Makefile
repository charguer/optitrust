TARGET_MAKE_ALL := transfo

FLAGS := -dump-ast-details


# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	target_args.ml \
	target_combi.ml \
	target_multi.ml \
	target_between.ml \

NOT_A_TEST := \
	target_regexp.ml \
	target_debug.ml \

TESTS_TO_FIX := \
	target_one.ml \
	target_type.ml \

TESTS := $(TESTS_WORKING)

include ../Common.Makefile
