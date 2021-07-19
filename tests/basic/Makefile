# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
  sequence_delete \
	loop_tile \
	sequence_insert \
	sequence_sub \
	array_swap \
  label_add \


TESTS_TOCHECK := \
  aos_to_soa.ml \
	loop_hoist \
	array_tile \
	array_to_variables \

TESTS_OTHERS := \
  label_remove.ml \
	loop_color.ml \
  loop_fusion.ml \
  loop_hoist.ml \
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

TESTS := $(TESTS_WORKING) $(TESTS_TOCHECK) $(TESTS_OTHERS)

include ../Common.Makefile
