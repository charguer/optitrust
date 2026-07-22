ifeq ($(MONOLITH_AFL),1)

SWITCH ?= 4.14.2+afl
AFL_COMPILER ?= ocaml-variants.4.14.2+options
AFL_OPTION ?= ocaml-option-afl
EXE := alist_monolith_test.exe

-include $(shell ocamlfind query monolith)/Makefile.monolith

DUNEBUILD := opam exec --switch $(SWITCH) -- dune build --profile monolith --no-print-directory --display quiet

.PHONY: setup
setup:
	@if opam switch list --short | grep -x '$(SWITCH)' >/dev/null ; then \
	  echo "The switch $(SWITCH) already exists." ; \
	else \
	  echo "Creating switch $(SWITCH) with $(AFL_COMPILER) and $(AFL_OPTION)..." ; \
	  opam switch create $(SWITCH) $(AFL_COMPILER) $(AFL_OPTION) --no-switch ; \
	fi
	@echo "Installing test dependencies in the switch $(SWITCH)..."
	@opam install --yes monolith base64 ppx_deriving.6.0.3 ppxlib.0.35.0 sexplib0.v0.16.0 --switch $(SWITCH)

else

TIMEOUT ?= 10
MAX_SCENARIOS ?= 1
EXE := _build/default/tests_infra/alist/alist_monolith_test.exe

.PHONY: all
all:
	dune build --profile monolith alist_monolith_test.exe

.PHONY: random
random: all
	dune exec --profile monolith ./alist_monolith_test.exe -- --timeout $(TIMEOUT) --max-scenarios $(MAX_SCENARIOS)

.PHONY: test min show summary setup
test min show summary setup:
	$(MAKE) -C . MONOLITH_AFL=1 $@

endif
