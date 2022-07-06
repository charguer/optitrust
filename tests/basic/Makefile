
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

FLAGS := -dump-ast-details


#	accesses_intro.ml \

TESTS_WITH_DOC := \
	accesses_scale.ml \
	accesses_shift.ml \
	align_def.ml \
	arith_simpl.ml \
	arith_scale.ml \
	arith_shift.ml \
	array_set_explicit.ml \
	array_swap.ml \
	array_tile.ml \
	array_to_variables.ml  \
	apac_use_goto_for_return.ml \
	cast_insert.ml \
	expr_replace.ml \
	expr_replace_fun.ml \
	flow_insert_if.ml \
	function_beta.ml \
	function_bind_intro.ml \
	function_dsp_call.ml \
	function_dsp_def.ml \
	function_inline.ml \
	function_rename_args.ml \
	function_uninline.ml \
	function_use_infix_ops.ml \
	instr_accumulate.ml \
	instr_move.ml \
	instr_read_last_write.ml \
	label_add.ml \
	label_remove.ml \
	loop_color.ml \
	loop_fission.ml \
	loop_fold.ml \
	loop_fusion.ml \
	loop_grid_enumerate.ml \
	loop_hoist.ml \
	loop_moveout.ml \
	loop_split_range.ml \
	loop_swap.ml \
	loop_tile.ml \
	loop_to_unit_steps.ml \
	loop_unroll.ml \
	loop_unswitch.ml \
	matrix_delocalize.ml \
	matrix_insert_alloc_dim.ml \
	matrix_intro_calloc.ml \
	matrix_intro_malloc.ml \
	matrix_intro_mindex.ml \
	matrix_local_name.ml \
	matrix_reorder_dims.ml \
	pointer_fold.ml \
	reference_fold.ml \
	reference_unfold.ml \
	rewrite_compute.ml \
	rewrite_compute_inside.ml \
	rewrite_equiv_at.ml \
	sequence_delete.ml \
	sequence_elim.ml \
	sequence_elim_on_instr.ml \
	sequence_intro_on_instr.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_partition.ml \
	sequence_split.ml \
	specialize_any.ml \
	specialize_choose.ml \
	struct_change_field_access_kind.ml \
	struct_rename_fields.ml \
	struct_reveal_field.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	struct_simpl_proj.ml \
	typedef_insert.ml \
	typedef_insert_copy.ml \
	typedef_fold.ml \
	typedef_unfold.ml \
	variable_bind.ml \
	variable_fold.ml \
	variable_unfold.ml \
	variable_inline.ml \
	variable_init_attach.ml \
	variable_init_detach.ml \
	variable_insert.ml \
	variable_local_name.ml \
	variable_ref_to_pointer.ml \
	variable_rename.ml \
	variable_subst.ml \
	variable_to_const.ml \
	variable_to_nonconst.ml \


TESTS_NODOC := \
	matrix_delocalize_obj.ml \
	matrix_local_name_obj.ml \


TESTS_TO_FIX := \
	struct_reorder_fields.ml \
	struct_update_fields_type.ml \
	matrix_insert_access_dim_index.ml \


NOT_NEEEDED_TO_FIX := \
	aos_to_soa_typedef.ml \
	aos_to_soa_sized_array.ml \
	variable_change_type.ml \


TESTS := $(TESTS_WITH_DOC) $(TESTS_NODOC) 


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
