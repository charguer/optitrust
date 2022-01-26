# The project's name.
THIS := optitrust

INSTALL_TARGET=`opam config var prefix`/lib/$(THIS)


all: install tests

library:
	dune build @install

install_lib: library
	dune install -p $(THIS)

# requires root access -- can use another absolute path if needed
COMPCERT_STDLIB_DIR_DST=/usr/local/lib/compcert
COMPCERT_STDLIB_DIR_SRC=./src/cparser/include

install_compcert_stdlib:
	install -d $(COMPCERT_STDLIB_DIR_DST)
	install -m 0644 $(COMPCERT_STDLIB_DIR_SRC)/*.h $(COMPCERT_STDLIB_DIR_DST)


install: install_lib
	mkdir -p $(INSTALL_TARGET)/tools
	install -m755 tools/*.* $(INSTALL_TARGET)/tools

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

tests: install
	$(MAKE) -C tests/ast debug
# temporary: we only aim for executing debug

clean:
	dune clean

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1
