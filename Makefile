# The project's name.
THIS := optitrust

OPTITRUST_PREFIX := `opam config var prefix`
INSTALL_TARGET := $(OPTITRUST_PREFIX)/lib/$(THIS)
BROWSER ?= xdg-open

OPTITRUST_BROWSER ?= firefox

all:
	dune build

precompile:
	$(MAKE) -C include precompiled_stdlib.pch

keep_building:
	dune build --watch --terminal-persistence=clear-on-rebuild

install_git_hooks: .git/hooks/pre-commit

.git/hooks/pre-commit:
	ln -s ../../tools/pre-commit.sh $@

# requires root access -- can use another absolute path if needed
COMPCERT_STDLIB_DIR_DST=/usr/local/lib/compcert
COMPCERT_STDLIB_DIR_SRC=./src/c/compcert_parser/include

install_compcert_stdlib:
	install -d $(COMPCERT_STDLIB_DIR_DST)
	install -m 0644 $(COMPCERT_STDLIB_DIR_SRC)/*.h $(COMPCERT_STDLIB_DIR_DST)

install:
	dune build -p $(THIS) @install
	dune install -p $(THIS)

uninstall:
	@ ocamlfind remove $(THIS) || true
#  dune uninstall

reinstall: uninstall
	@ make install

show_install:
	@ echo "#require \"$(THIS)\";;\nopen Optitrust;;\n#show Run;;" | ocaml

BUILDOCDIR = _build/default/_doc/_html
FINALDOCDIR = _doc
DOC = $(FINALDOCDIR)/optitrust/Optitrust/index.html

.PHONY: cleandoc
cleandoc:
	@ rm -rf $(BUILDOCDIR)

# FIXME: should probably run './tester run tests/*/*_doc -dump-trace', but its expensive, let user do it?
.PHONY: doc
doc:
	@ dune build -p $(THIS) @doc
	@ rm -rf $(FINALDOCDIR)
	@ cp -r $(BUILDOCDIR) $(FINALDOCDIR)
	@ ./doc/add_tests_into_doc.sh
	@ echo "You can view the documentation by typing 'make viewdoc'".

test_into_doc:
	@ ./doc/add_tests_into_doc.sh

.PHONY: viewdoc
viewdoc:
	tools/open_in_browser.sh $(DOC)

# Also for testing:
#   in terminal, execute utop-full
#   type: #require "optitrust";;
#   type: open Optitrust;;
#   type: #show "Run";;
#   type: exit 0;;

tests:
	./tester run

style:
	find tests/ src/ -path tests/batch -prune -o -path src/c/compcert_parser -prune -o -name "*.ml" -print0 | xargs -0 grep --color -nE '.{101}'

clean: clean_cache
	dune clean
	rm include/*.pch

clean_cache:
	find . -type f -name '*.ser' -exec rm {} +
	find . -type f -name '*.trace' -exec rm {} +
	find . -type f -name '*_notfmt.cpp' -exec rm {} +
	find . -type f -name '*.cmxs' -exec rm {} +

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1

PDFS := $(patsubst %.md, %.pdf, $(wildcard *.md))

md: $(PDFS)

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

artifact: sc_artifact.pdf


.PHONY: all install install_git_hooks install_compcert_stdlib uninstall show_install tests clean watch md artifact
