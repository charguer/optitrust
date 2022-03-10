
# Usage: "make" alias "make doc"
# "make clean" or "make cleanall" to also clean in basic and combi folders

TARGET_MAKE_ALL := mydoc

TESTS_WITH_DOC := \
	label_add__basic.ml \
	variable_to_const__basic.ml \
	loop_fold__combi.ml \
	function_inline.ml \
	function_bind_intro__basic.ml \
	\
	instr_replace__basic.ml \
	accesses_scale__basic.ml \
	\
	variable_fold__combi.ml \
	instr_read_last_write__basic.ml \
	sequence_delete__basic.ml \
	variable_reuse__combi.ml \
	\
	instr_move.ml \
	loop_reorder__combi.ml \
	loop_unswitch__basic.ml \
	\
	loop_fission.ml \
	\
	struct_fields_reorder.ml \


TESTS := $(filter-out %__basic.ml %__combi.ml,$(TESTS_WITH_DOC))

redoc: optitrust cleanall mydoc

mydoc: 
	$(MAKE) -i BATCH=1 doc


#---------------------------------------------------------------------
# Common makefile for test folders

# Flag to disable the default rule for target "doc.html"
SPECIAL_RULE_FOR_DOC_HTML := yes

include ../Common.Makefile


#---------------------------------------------------------------------
# Rule for generating documentation for tests from the basic folder
# and copying the files over here

BASICDIR:=../basic
COMBIDIR:=../combi

.PRECIOUS: $(BASICDIR)/%_doc.js $(COMBIDIR)/%_doc.js

$(BASICDIR)/%_doc.js:
	$(V)$(MAKE) -C $(BASICDIR) $*_doc.js

$(COMBIDIR)/%_doc.js:
	$(V)$(MAKE) -C $(COMBIDIR) $*_doc.js

.SECONDEXPANSION:
%__basic_doc.js: ../basic/$$*_doc.js
	$(V)cp $< $(*F)_doc.js
	$(V)touch $@
	@echo "Produced $(*F)_doc.js"

.SECONDEXPANSION:
%__combi_doc.js: ../combi/$$*_doc.js
	$(V)cp $< $(*F)_doc.js
	$(V)touch $@
	@echo "Produced $(*F)_doc.js"

# LATER: if basic/*_doc.js files were not encoded, we could remove "Basic_" from it; for now we hack using a hook

TESTS_FOR_DOC :=  $(subst __combi, ,$(subst __basic, , $(TESTS_WITH_DOC:.ml=)))

doc.html: $(DOCJS)
	$(V)$(OPTITRUST)/doc/doc_create.sh $(OPTITRUST) $@ $(TESTS_FOR_DOC)
	sed -i "s#<\/body><\/html>#<script>var hide_basic = 0; var only_correctness = 1;<\/script><\/body><\/html>#" $@
	@echo Produced $@

cleandoc::
	rm -f *_doc.js doc.html

cleanall: clean
	$(MAKE) -C $(BASICDIR) clean
	$(MAKE) -C $(COMBIDIR) clean
