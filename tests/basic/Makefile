# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

# Validated: sequence_delete loop_tile

# Tolook at: aos_to_soa, sequence_insert  sequence_sub array_swap loop_hoist


# List of files to consider.
TESTS := \
  aos_to_soa.ml \
	array_swap \
  array_tile.ml \
	array_to_variables.ml \
  label_add.ml \
  label_remove.ml \
	loop_color.ml \
  loop_fusion.ml \
  loop_hoist.ml \
  loop_split \
  loop_swap.ml \
  loop_tile.ml \
	sequence_delete.ml \
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
  variable_insert_and_fold.ml \
	sequence_insert.ml \



# To consider all ml files except specific ones, use EXCLUDE_TESTS (see ../Common.Makefile)

include ../Common.Makefile
