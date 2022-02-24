
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

FLAGS := -dump-ast-details

TESTS_WITH_DOC := \
	accesses_scale.ml \
	accesses_shift.ml \
	arith_simplify.ml \
	array_set_explicit.ml \
	array_tile.ml \
	array_to_variables.ml  \
	cast_insert.ml \
	flow_insert_if.ml \
	function_inline.ml \
	function_beta.ml \
	function_bind_intro.ml \
	function_use_infix_ops.ml \
	include_inline.ml \
	instr_accumulate.ml \
	instr_read_last_write.ml \
	instr_move.ml \
	instr_replace.ml \
	instr_replace_fun.ml \
	label_add.ml \
	loop_color.ml \
	loop_fission.ml \
	loop_fold.ml \
	loop_fusion.ml \
	loop_hoist.ml \
	loop_moveout.ml \
	loop_swap.ml \
	loop_tile.ml \
	loop_to_unit_steps.ml \
	loop_unswitch.ml \
	loop_unroll.ml \
	pointer_fold.ml \
	reference_fold.ml \
	reference_unfold.ml \
	rewrite_compute.ml \
	sequence_delete.ml \
	sequence_elim_around_instr.ml \
	sequence_elim.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_intro_on_instr.ml \
	sequence_partition.ml \
	sequence_split.ml \
	struct_fields_reorder.ml \
	struct_rename_field.ml \
	struct_rename_fields.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	struct_update_fields_type.ml \
	typedef_inline.ml \
	typedef_insert.ml \
	typedef_insert_copy.ml \
	typedef_fold.ml \
	variable_fold.ml \
	variable_init_attach.ml \
	variable_init_detach.ml \
	variable_inline.ml \
	variable_local_name.ml \
	variable_rename.ml \
	variable_simpl_deref.ml \
	variable_subst.ml \
	variable_to_const.ml \
	variable_to_nonconst.ml \
	variable_unfold.ml \

TESTS_WORKING_NOTINDOC := \
	specialize_choose.ml \

# TODO: move include_inline to tests/interact/

TESTS_TOCHECK := \
	matrix_delocalize.ml \
	matrix_intro_mcalloc.ml \
	matrix_intro_mindex.ml \
	matrix_intro_mmalloc.ml \
	matrix_local_name.ml \
	function_rename_args.ml \





# missing expected
# matrix_insert_alloc_dim.ml
#	matrix_insert_access_dim_index.ml \

TESTS_TO_FIX := \
	struct_simpl_proj.ml \
	function_uninline.ml \
	label_remove.ml \
	loop_grid_enumerate.ml \
	rewrite_compute_inside.ml \
	rewrite_equiv_at.ml \
	specialize_any.ml \
	accesses_intro.ml \
	struct_inline.ml \
	variable_bind.ml \
	variable_insert.ml \
	matrix_reorder_dims.ml \


NOT_NEEEDED_TO_FIX := \
	arith_scale.ml \
	arith_shift.ml \
	array_swap.ml \
	aos_to_soa_typedef.ml \
	aos_to_soa_sized_array.ml \
	variable_change_type.ml \



  




# TODO:
# arith_simpl.ml


# TODO: script for vscode opening

TESTS := $(TESTS_WITH_DOC) $(TESTS_WORKING_NOTINDOC) $(TESTS_TOCHECK)


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