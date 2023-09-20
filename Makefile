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

DOCDIR = _build/default/_doc/_html
DOC    = $(DOCDIR)/index.html
CSS    = $(DOCDIR)/odoc.css

.PHONY: doc
doc:
	@ dune build -p $(THIS) @doc
	# @ sed -i.bak 's/font-weight: 500;/font-weight: bold;/' $(CSS) && rm -f $(CSS).bak
	@ echo "You can view the documentation by typing 'make viewdoc'".

.PHONY: viewdoc
viewdoc: doc
	@ echo Attempting to open $(DOC)...
	$(BROWSER) $(DOC)

# Also for testing:
#   in terminal, execute utop-full
#   type: #require "optitrust";;
#   type: open Optitrust;;
#   type: #show "Run";;
#   type: exit 0;;

tests:
	./tester

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
