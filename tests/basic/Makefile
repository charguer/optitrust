
TARGET_MAKE_ALL := transfo

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
  sequence_delete.ml \
	loop_tile.ml \
	sequence_insert.ml \
	sequence_sub.ml \
	array_swap.ml \
  label_add.ml \
  label_remove.ml \
	array_to_variables.ml \
	loop_hoist.ml \
  loop_fusion.ml \
  loop_interchange.ml \
  loop_grid_enumerate.ml \
  array_tile.ml \
  loop_color.ml \
  loop_unroll.ml \
  loop_invariant.ml \
  loop_fission.ml \
  sequence_delete.ml \

TESTS_TOCHECK := \
  aos_to_soa.ml \
  loop_tile.ml \
  loop_unswitch.ml \
  loop_to_unit_steps.ml \

TESTS_OTHERS := \
  sequence_inline.ml \
  sequence_sub.ml \
  sequence_wrap.ml \
  sequence_unwrap.ml \
  struct_reorder.ml \
	struct_set_explicit.ml \
	struct_set_implicit.ml \
  typedef_fold.ml \
	typedef_inline.ml \
	typedef_insert.ml \
	variable_fold.ml \
	variable_inline.ml \
	variable_insert.ml \
	sequence_insert.ml \
	arbitrary_if.ml \
	change_occurrence.ml \
	from_one_to_many.ml \
	reference_fold.ml \
	reference_inline.ml \


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