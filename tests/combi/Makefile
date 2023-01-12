# FLAGS := -dump-ast-details
FLAGS := -dump-ast-details

TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

#	function_use_infix_ops.ml \

# TODO fix

TESTS_WITH_DOC := \
	accesses_scale.ml \
	accesses_shift.ml \
	align_alloc.ml \
	apac_parallel_task_group.ml \
	expr_replace_fun.ml \
	function_beta.ml \
	function_bind.ml \
	function_bind_args.ml \
	function_elim_body.ml \
	function_inline_complex.ml \
	function_inline_simple.ml \
	function_inline_struct.ml \
	function_uninline.ml \
	instr_accumulate.ml \
	instr_accumulate_targets.ml \
	instr_inline_last_write.ml \
	instr_gather.ml \
	instr_read_last_write.ml \
	instr_move.ml \
	instr_moveout.ml \
	instr_moveout_of_fun.ml \
	loop_fold.ml \
	loop_fold_instrs.ml \
	loop_fusion.ml \
	loop_fusion_targets.ml \
	loop_shift.ml \
	loop_grid_enumerate.ml \
	loop_isolate_first_iteration.ml \
	loop_move.ml \
	loop_moveout.ml \
	loop_reorder.ml \
	loop_unroll.ml \
	matrix_delocalize.ml \
	matrix_intro_calloc.ml \
	matrix_intro_malloc.ml \
	matrix_intro_mindex.ml \
	matrix_intro_mops.ml \
	sequence_intro.ml \
	sequence_intro_targets.ml \
   record_set_explicit.ml \
 	record_set_implicit.ml \
	record_rename_field.ml \
	variable_delocalize.ml \
	variable_delocalize_in_vars.ml \
	variable_elim_redundant.ml \
	variable_fold.ml \
	variable_inline_and_rename.ml \
	variable_insert.ml \
	variable_insert_list.ml \
	variable_insert_and_fold.ml \
  variable_intro_pattern_array.ml \
	variable_renames.ml \
	variable_reuse.ml \

# 	
TESTS_WITHOUT_DOC := \
	matrix_delocalize_obj.ml \
	apac_constify_functions_arguments.ml \
	apac_sync_with_taskwait.ml \
	apac_insert_tasks_naive.ml \

	
#variable_inline.ml \ TODO: Update variable_inline unit test

TESTS_TO_FIX := \
	record_align_field.ml \
	loop_hoist.ml \
	loop_fission.ml \
	record_align_field.ml \
	apac_heapify_nested_seq.ml \
	
NOT_NEEEDED_TO_FIX := \
	aos_to_soa.ml \



TESTS := $(TESTS_WITH_DOC) $(TESTS_WITHOUT_DOC) 


#---------------------------------------------------------------------
# Rule for generating documentation for tests from the basic folder
# and copying the files over here

TESTS_COMBI_WITH_DOC := $(TESTS_WITH_DOC)

# LATER: filter out the ones that are redundant with combi transformations
TESTS_BASIC_WITH_DOC := \
	accesses_scale.ml \
	accesses_shift.ml \
	arith_simpl.ml \
	arith_scale.ml \
	arith_shift.ml \
	array_tile.ml \
	array_to_variables.ml  \
	cast_insert.ml \
	expr_replace.ml \
	expr_replace_fun.ml \
	flow_insert_if.ml \
	function_inline.ml \
	function_uninline.ml \
	function_beta.ml \
	function_bind_intro.ml \
	function_use_infix_ops.ml \
	instr_accumulate.ml \
	instr_read_last_write.ml \
	instr_move.ml \
	label_add.ml \
	label_remove.ml \
	loop_color.ml \
	loop_fission.ml \
	loop_fold.ml \
	loop_fusion.ml \
	loop_grid_enumerate.ml \
	loop_hoist.ml \
	loop_moveout.ml \
	loop_swap.ml \
	loop_tile.ml \
	loop_to_unit_steps.ml \
	loop_unroll.ml \
	loop_unswitch.ml \
	pointer_fold.ml \
	reference_fold.ml \
	reference_unfold.ml \
	rewrite_compute.ml \
	rewrite_compute_inside.ml \
	sequence_delete.ml \
	sequence_elim_on_instr.ml \
	sequence_intro_on_instr.ml \
	sequence_elim.ml \
	sequence_insert.ml \
	sequence_intro.ml \
	sequence_partition.ml \
	sequence_split.ml \
	specialize_function_arg.ml \
	record_fields_reorder.ml \
	record_reveal_field.ml \
	record_rename_fields.ml \
	record_set_explicit.ml \
	record_set_implicit.ml \
	record_update_fields_type.ml \
	typedef_insert.ml \
	typedef_insert_copy.ml \
	variable_bind.ml \
	variable_fold.ml \
	variable_unfold.ml \
	variable_inline.ml \
	variable_init_attach.ml \
	variable_init_detach.ml \
	variable_insert.ml \
	variable_local_name.ml \
	variable_rename.ml \
	variable_subst.ml \
	variable_to_const.ml \
	variable_to_nonconst.ml \

TESTS_BASIC_WITH_DOC := $(TESTS_BASIC_WITH_DOC:.ml=__basic.ml)

TESTS_WITH_DOC := $(sort $(TESTS_COMBI_WITH_DOC) $(TESTS_BASIC_WITH_DOC))

BASICDIR:=../basic

.PRECIOUS: ../basic/%_doc.js

../basic/%_doc.js:
	$(V)$(MAKE) -C $(BASICDIR) $*_doc.js

.SECONDEXPANSION:
%__basic_doc.js: ../basic/$$*_doc.js
	$(V)cp $< $(*F)_doc.js
	$(V)touch $@
	@echo "Produced $(*F)_doc.js"

# $(V)cp $< $@

# LATER: if basic/*_doc.js files were not encoded, we could remove "Basic_" from it; for now we hack using a hook

TESTS_FOR_DOC := $(subst __basic, , $(TESTS_WITH_DOC:.ml=))

DOCJS := $(TESTS_WITH_DOC:.ml=_doc.js)

# LATER: dependency on docjs from basic
doc.html: $(DOCJS) 
	$(V)$(OPTITRUST)/doc/doc_create.sh $(OPTITRUST) $@ $(TESTS_FOR_DOC)
	sed -i "s#<\/body><\/html>#<script>var hide_basic = 1;<\/script><\/body><\/html>#" $@
	@echo Produced $@

cleandoc::
	@rm -f *__basic_doc.js


#---------------------------------------------------------------------
# Common makefile for test folders

# Flag to disable the default rule for target "doc.html"
SPECIAL_RULE_FOR_DOC_HTML := yes

include ../Common.Makefile
