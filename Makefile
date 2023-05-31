# The project's name.
THIS := optitrust

OPTITRUST_PREFIX := `opam config var prefix`
INSTALL_TARGET := $(OPTITRUST_PREFIX)/lib/$(THIS)

all:
	dune build

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

# Also for testing:
#   in terminal, execute utop-full
#   type: #require "optitrust";;
#   type: open Optitrust;;
#   type: #show "Run";;
#   type: exit 0;;

tests:
	# should be: ./tester
	./tester basic
	read -p "Enter to Continue" REPLY
	./tester combi
	read -p "Enter to Continue" REPLY
	./tester target
	read -p "Enter to Continue" REPLY
	./tester case_studies

clean:
	dune clean

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1


PDFS := $(patsubst %.md, %.pdf, $(wildcard *.md))

md: $(PDFS)

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

artifact: sc_artifact.pdf


.PHONY: all install install_compcert_stdlib uninstall show_install tests clean watch md artifact
