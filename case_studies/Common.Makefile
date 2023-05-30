# Verbosity for commands
V ?= @

OPTITRUST := ../..
TOOLS_FOLDER := $(OPTITRUST)/tools

.FORCE:

%_with_lines.ml: %.ml
	$(V)$(TOOLS_FOLDER)/add_lines.sh $< $@

%_out.cpp: %_with_lines.ml .FORCE
	@rm -f $@
	$(V)$(TOOLS_FOLDER)/build_cmxs.sh $<
	$(V)OCAMLRUNPARAM=b dune exec optitrust_runner -- $*_with_lines.cmxs $(FLAGS)
	@echo "Produced $@"

  # $(patsubst %.ml,%.cmxs,$<)

# TEST FOR THESE RULES:
# - change foo.ml
# - change foo_with_lines.ml
# - change optitrust lib

clean::
	$(V)rm -f *.js *_out.cpp *.cmxs *.byte *.native *.chk *.log *.ast *.out *.cmi *.cmx *.prog *_enc.cpp *_diff.js *_before.cpp *_after.cpp *_trace.js *_trace.html *_diff.html *_with_exit.ml *_with_lines.ml *.html *_before_* tmp_*  *_fast.ml *_inter.ml batch.ml *.ser *.i *_inlined.cpp
	$(V)rm -Rf _build
	@echo "Clean successful"