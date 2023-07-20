# Verbosity for commands
V ?= @

OPTITRUST := ../..
TOOLS_FOLDER := $(OPTITRUST)/tools

.FORCE:

%.cmxs: .FORCE
	$(V)$(TOOLS_FOLDER)/build_cmxs.sh $*.ml

%_out.cpp: %.cmxs
	# Remove file to avoid confusion when execution fails
	@rm -f $@
	$(V)OCAMLRUNPARAM=b dune exec optitrust_runner -- $*.cmxs $(FLAGS)
	@echo "Produced $@"

%_trace.js: %.cmxs
	# Remove file to avoid confusion when execution fails
	@rm -f $@
	$(V)OCAMLRUNPARAM=b dune exec optitrust_runner -- $*.cmxs -dump-trace $(TRACEFLAGS)
	@echo "Produced $@"

%.trace: %_trace.js .FORCE
	$(TOOLS_FOLDER)/open_trace.sh $*

.PRECIOUS: %.cmxs %_out.cpp %_trace.js

clean::
	$(V)rm -f *.js *_out.cpp *.cmxs *.byte *.native *.chk *.log *.ast *.out *.cmi *.cmx *.prog *_enc.cpp *_diff.js *_before.cpp *_after.cpp *_trace.js *_trace.html *_diff.html *.html *_before_* tmp_*  *_fast.ml *_inter.ml batch.ml *.ser *.i *_inlined.cpp
	@echo "Clean successful"