
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	instr_inline_last_write.ml \
	function_bind_args.ml \
	function_bind.ml \
	function_elim_body.ml \
	function_inline_complex.ml \
	function_inline_simple.ml \
	function_inline_struct.ml \
	loop_fusion.ml \
	loop_fold.ml \
	loop_hoist.ml \
	loop_invariant.ml \
	loop_move.ml \
	loop_reorder.ml \
	loop_unroll.ml \
	sequence_intro.ml \
	sequence_split.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	variable_intro_pattern_array.ml \
	variable_delocalize.ml \
	variable_delocalize_in_vars.ml \
	variable_fold.ml \
	variable_reuse.ml \


TESTS_TOCHECK := \
	instr_accumulate.ml \
	instr_read_last_write.ml \
	instr_gather.ml \
	loop_fold.ml \
	loop_fusion_targets.ml \
	matrix_intro_mops.ml \
	matrix_delocalize.ml \
	variable_reverse_fold.ml \
	variable_elim_redundant.ml \
  variable_insert_list.ml \

# TODO:
# instr_moveout.ml \  -- currently move_invariant


TESTS_TO_FIX := \
	aos_to_soa.ml \
	function_beta.ml \



TESTS_WITH_DOC := \
	loop_fusion.ml \


TESTS := $(TESTS_WORKING) $(TESTS_TOCHECK)

include ../Common.Makefile

