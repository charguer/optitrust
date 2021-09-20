
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	loop_fusion.ml \
	loop_hoist.ml \
	loop_invariant.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	variable_fold.ml \


TESTS_TOCHECK := \
	function_bind.ml \
	aos_to_soa.ml \
	delocalize.ml \
	loop_unroll.ml \
	sequence_intro.ml \
	function_elim_body.ml \
	function_inline_complex.ml \
	function_inline_simple.ml \

TESTS := $(TESTS_WORKING)

include ../Common.Makefile

