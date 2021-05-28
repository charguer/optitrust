
INSTALL_TARGET=`opam config var prefix`/lib/optiTrust

all: install tests

library:
	dune build @install

install_lib: library
	dune install

install: install_lib
	mkdir -p $(INSTALL_TARGET)/tools
	install -m755 tools/*.* $(INSTALL_TARGET)/tools

uninstall:
	dune uninstall

#%.exe: all
#	dune build $@

tests: install
	$(MAKE) -C tests/ast debug
# temporary: we only aim for executing debug

clean:
	dune clean

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1
