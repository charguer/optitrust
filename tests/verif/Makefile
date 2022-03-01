
# Usage: "make" alias "make doc"
# "make clean" or "make cleanall" to also clean in basic and combi folders

TARGET_MAKE_ALL := mydoc

TESTS_WITH_DOC := \
	label_add__basic.ml \
	variable_fold__combi.ml \

TESTS :=

mydoc: cleanall
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
	sed -i "s#<\/body><\/html>#<script>var hide_basic = 1; var only_correctness = 1;<\/script><\/body><\/html>#" $@
	@echo Produced $@

cleandoc::
	rm -f *__basic_doc.js *__combi_doc.js

cleanall: clean
	$(MAKE) -C $(BASICDIR) clean
	$(MAKE) -C $(COMBIDIR) clean
