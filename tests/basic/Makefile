
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp


TESTS_TOCHECK := \

TESTS_BROKEN := \
	accesses_scale.ml \
	accesses_shift.ml \
  flow_insert_if.ml \

TESTS_WORKING := \
	aos_to_soa_sized_array.ml \
	aos_to_soa_typedef.ml \
	array_set_explicit.ml \
	array_swap.ml \
	array_tile.ml \
	array_to_variables.ml \
	cast_insert.ml \
	function_bind_intro.ml \
	function_inline.ml \
	instr_move.ml \
	instr_read_last_write.ml \
	instr_replace.ml \
	instr_replace_fun.ml \
	label_add.ml \
	label_remove.ml \
	loop_color.ml \
	loop_fission.ml \
	loop_fusion.ml \
	loop_fold.ml \
	loop_grid_enumerate.ml \
	loop_hoist.ml \
	loop_interchange.ml \
	loop_invariant.ml \
	loop_to_unit_steps.ml \
	loop_unroll.ml \
	loop_unswitch.ml \
	pointer_fold.ml \
	reference_fold.ml \
	reference_inline.ml \
	rewrite_equiv_at.ml \
	rewrite_compute.ml \
	rewrite_compute_inside.ml \
	sequence_delete.ml \
	sequence_elim_around_instr.ml \
	sequence_elim.ml \
	sequence_intro.ml \
	sequence_intro_on_instr.ml \
	sequence_partition.ml \
	struct_fields_reorder.ml \
	struct_inline.ml \
	struct_rename_fields.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	struct_update_fields_type.ml \
	typedef_fold.ml \
	typedef_inline.ml \
	typedef_insert.ml \
	variable_change_type.ml \
	variable_fold.ml \
	variable_inline.ml \
	variable_insert.ml \
	variable_insert_and_fold.ml \
	variable_local_name.ml \
	variable_rename.ml \
	variable_replace_occurrences.ml \

TESTS_TOCHECK := \
	loop_tile.ml \
  variable_bind_intro.ml \
	variable_init_attach.ml \
	matrix_intro_mcalloc.ml \
	matrix_intro_mmalloc.ml \
	matrix_reorder_dims.ml \
	instr_accumulate.ml \
	struct_simpl_proj.ml \
	function_use_infix_ops.ml \
	sequence_insert.ml \



TESTS_WITH_DOC := \
	label_add.ml \
	label_remove.ml \


# arith_simpl.ml

#  change_type.ml \
# arith_shift.ml \
#	local_name.ml

# TODO: script for vscode opening

TESTS := $(TESTS_WORKING) $(TESTS_TOCHECK)


# TEMPORARY FOR TESTING MAKEFILE
# TESTS := loop_color.ml loop_fission.ml loop_fusion.ml

COMPILE := \
	loop_grid_enumerate.ml \
	loop_to_unit_steps.cpp \
	loop_tile.ml \

include ../Common.Makefile

# LATER: flattening:  for i (for j ...)   ->  for k    // reverse of gridify
# for i { for j = 0; j < nb[i]; j++)  ...  example: sparse matrix
#   compute the prefix-sum array first

# LATER: target to say "first for loop after instruction foo"