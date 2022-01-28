# FLAGS := -dump-ast-details
FLAGS := -dump-ast-details

TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

# TESTS_WITH_DOC_INCLUDING_BASIC := \
#	 label_add__basic.ml \
#  LATER: define TESTS_WITH_DOC  as a filter of TESTS_WITH_DOC_INCLUDING_BASIC by removing *__basic.ml from the list

TESTS_WITH_DOC := \
	instr_gather.ml \
	instr_moveout.ml \
	instr_inline_last_write.ml \
	instr_read_last_write.ml \
	loop_fusion.ml \
	loop_fold.ml \
	loop_fold_instr.ml \
	loop_moveout.ml \
	loop_reorder.ml \
	sequence_intro.ml \
	sequence_intro_targets.ml \
	struct_set_implicit.ml \
  variable_insert.ml \
	variable_insert_list.ml \
  variable_intro_pattern_array.ml \
	variable_renames.ml \
	variable_reuse.ml \

TESTS_WITHOUT_DOC := \
	function_bind_args.ml \
	function_bind.ml \
	function_elim_body.ml \
	function_inline_complex.ml \
	function_inline_simple.ml \
	function_inline_struct.ml \
	variable_delocalize.ml \
	variable_delocalize_in_vars.ml \

TESTS_TOCHECK := \
	function_beta.ml \
	function_uninline.ml \
	function_use_infix_ops.ml \
	instr_accumulate.ml \
	instr_accumulate_targets.ml \
	matrix_intro_mops.ml \
	matrix_delocalize.ml \
	variable_inline_and_rename.ml \

TESTS_TO_FIX := \
	variable_fold.ml \
	variable_insert_and_fold.ml \
	variable_elim_redundant.ml \
	struct_set_explicit.ml \
	aos_to_soa.ml \
	loop_hoist.ml \
	loop_move.ml \
	loop_unroll.ml \
	loop_fusion_targets.ml \

# 	variable_renames.ml \


TESTS := $(TESTS_WITH_DOC) $(TESTS_WITHOUT_DOC) $(TESTS_TOCHECK)

#---------------------------------------------------------------------
# Rule for generating documentation for tests from the basic folder
# and copying the files over here

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

SPECIAL_RULE_FOR_DOC_HTML := yes
doc.html: $(DOCJS)
	$(V)$(OPTITRUST)/doc/doc_create.sh $(OPTITRUST) $@ $(TESTS_FOR_DOC)
	sed -i "s#<\/body><\/html>#<script>var hide_basic = 1;<\/script><\/body><\/html>#" $@
	@echo Produced $@

cleandoc::
	rm -f *__basic_doc.js


#---------------------------------------------------------------------
# Common makefile for test folders

include ../Common.Makefile

