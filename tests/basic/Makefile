
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	accesses_scale.ml \
	accesses_shift.ml \
	aos_to_soa_sized_array.ml \
	aos_to_soa_typedef.ml \
	array_set_explicit.ml \
	array_swap.ml \
	array_tile.ml \
	array_to_variables.ml \
	cast_insert.ml \
  flow_insert_if.ml \
	function_bind_intro.ml \
	function_inline.ml \
	instr_move.ml \
	instr_replace.ml \
	instr_read_last_write.ml \
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
	loop_moveout.ml \
	loop_tile.ml \
	loop_to_unit_steps.ml \
	loop_unroll.ml \
	loop_unswitch.ml \
	pointer_fold.ml \
	reference_fold.ml \
	rewrite_equiv_at.ml \
	rewrite_compute.ml \
	rewrite_compute_inside.ml \
	sequence_delete.ml \
	sequence_elim_around_instr.ml \
	sequence_elim.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_intro_on_instr.ml \
	sequence_partition.ml \
	struct_fields_reorder.ml \
	struct_inline.ml \
	struct_rename_field.ml \
	struct_rename_fields.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	struct_update_fields_type.ml \
	typedef_fold.ml \
	typedef_inline.ml \
	typedef_insert.ml \
	typedef_insert_copy.ml \
	variable_change_type.ml \
	variable_fold.ml \
	variable_init_attach.ml \
	variable_init_detach.ml \
	variable_inline.ml \
	variable_insert.ml \
	variable_local_name.ml \
	variable_rename.ml \
	variable_replace_occurrences.ml \
  variable_unfold.ml \
	
TESTS_TOCHECK := \
	accesses_intro.ml \
	arith_simplify.ml \
	function_beta.ml \
	function_uninline.ml \
	function_use_infix_ops.ml \
	include_inline.ml \
	instr_accumulate.ml \
	reference_inline.ml \
	specialize_any.ml \
	specialize_choose.ml \
	struct_simpl_proj.ml \
	variable_bind.ml \
	variable_simpl_deref.ml \
	variable_to_const.ml \
	matrix_delocalize.ml \
	matrix_intro_mcalloc.ml \
	matrix_intro_mindex.ml \
	matrix_intro_mmalloc.ml \
	matrix_local_name.ml \
	matrix_reorder_dims.ml \

# missing expected
# matrix_insert_alloc_dim.ml
#	matrix_insert_access_dim_index.ml \



TESTS_TO_FIX := \
	arith_shift.ml \
	arith_to_fix.ml \
	


TESTS_WITH_DOC := \
	label_add.ml \
	label_remove.ml \


# TODO:
# arith_simpl.ml


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