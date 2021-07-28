
TARGET_MAKE_ALL := transfo

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	functino_elim_body.ml \
	function_bind.ml \
	function_inline_complex.ml \
	function_inline_simple.ml \
	loop_hoist.ml \
  loop_fusion.ml \
  loop_invariant.ml \
	loop_move.ml \
	sequence_intro.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	variable_inline.ml \
	variable_fold.ml \

TESTS_TOCHECK := \
	aos_to_soa.ml \
	delocalize.ml \
  loop_unroll.ml \

TESTS := $(TESTS_WORKING) $(TESTS_TOCHECK)

COMPILE := \
	loop_grid_enumerate.ml \
	loop_color.ml \
	loop_to_unit_steps.cpp \
	loop_tile.ml \

include ../Common.Makefile

