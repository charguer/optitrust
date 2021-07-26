
TARGET_MAKE_ALL := transfo

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	arbitrary_if.ml \
	array_swap.ml \
  array_tile.ml \
	array_to_variables.ml \
  label_add.ml \
  label_remove.ml \
	loop_hoist.ml \
  loop_fusion.ml \
  loop_interchange.ml \
  loop_grid_enumerate.ml \
  loop_color.ml \
  loop_unroll.ml \
  loop_invariant.ml \
  loop_fission.ml \
	loop_tile.ml \
	loop_unswitch.ml \
	loop_to_unit_steps.ml \
	sequence_elim_around_instr.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_delete.ml \
  sequence_elim.ml \
  sequence_intro_on_instr.ml \
	struct_fields_reorder.ml \
	struct_set_explicit.ml \
	typedef_inline.ml \
	variable_inline.ml \
	variable_fold.ml \

TESTS_TOCHECK := \
	aos_to_soa.ml \
	struct_set_implicit.ml \
	typedef_fold.ml \
  change_occurrence.ml \
	reference_fold.ml \
	reference_inline.ml \

ARTHUR_TO_SPEC := \
	from_one_to_many.ml \

TESTS_OTHERS := \


# TODO: remove? typedef_insert.ml \
# TODO: script for vscode opening

TESTS := $(TESTS_WORKING) $(TESTS_TOCHECK) $(TESTS_OTHERS)

COMPILE := \
	loop_grid_enumerate.ml \
	loop_color.ml \
	loop_to_unit_steps.cpp \
	loop_tile.ml \

include ../Common.Makefile



# LATER: loop-invariant code motion:
# - basic level: move one instruction out of one loop
# - higher-level: figure out the list of constants to move, and move them out of one or more loops

# LATER: Unswitching – moves a conditional from inside a loop to outside
# for ( if (b) c1 else c2 )
# if (b) for (c1) else for (c2)

# LATER: flattening:  for i (for j ...)   ->  for k    // reverse of gridify
# for i { for j = 0; j < nb[i]; j++)  ...  example: sparse matrix
#   compute the prefix-sum array first


# LATER: target to say "first for loop after instruction foo"