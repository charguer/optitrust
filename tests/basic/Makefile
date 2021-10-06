
TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	aos_to_soa_sized_array.ml \
	aos_to_soa_typedef.ml \
	array_swap.ml \
	array_tile.ml \
	array_to_variables.ml \
	flow_insert_if.ml \
	function_bind_intro.ml \
	function_inline.ml \
	label_add.ml \
	label_remove.ml \
	local_other_name.ml \
	loop_color.ml \
	loop_fission.ml \
	loop_fusion.ml \
	loop_grid_enumerate.ml \
	loop_hoist.ml \
	loop_interchange.ml \
	loop_invariant.ml \
	loop_tile.ml \
	loop_to_unit_steps.ml \
	loop_unroll.ml \
	loop_unswitch.ml \
	pointer_fold.ml \
	reference_fold.ml \
	reference_inline.ml \
	instr_replace.ml \
	instr_replace_fun.ml \
	sequence_delete.ml \
	sequence_elim_around_instr.ml \
	sequence_elim.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_intro_on_instr.ml \
	sequence_partition.ml \
	struct_fields_reorder.ml \
	struct_inline.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
	typedef_fold.ml \
	typedef_inline.ml \
	typedef_insert.ml \
	variable_fold.ml \
	variable_inline.ml \
	variable_insert.ml \
	variable_insert_and_fold.ml \
	variable_rename.ml\

TESTS_TOCHECK := \
	change_type.ml \
	sequence_partition.ml \
	


ARTHUR_TO_SPEC := \
	from_one_to_many.ml \

TESTS_OTHERS := \


# TODO: script for vscode opening

TESTS := $(TESTS_WORKING)

COMPILE := \
	loop_grid_enumerate.ml \
	loop_to_unit_steps.cpp \
	loop_tile.ml \

include ../Common.Makefile

# LATER: flattening:  for i (for j ...)   ->  for k    // reverse of gridify
# for i { for j = 0; j < nb[i]; j++)  ...  example: sparse matrix
#   compute the prefix-sum array first

# LATER: target to say "first for loop after instruction foo"