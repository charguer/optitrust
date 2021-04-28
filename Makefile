
INSTALL_TARGET=`opam config var prefix`/lib/optiTrust

all: install tests

library: 
	dune build @install

install: library
	mkdir -p $(INSTALL_TARGET)/tools
	install -m755 tools/*.* $(INSTALL_TARGET)/tools

uninstall:
	dune uninstall

#%.exe: all
#	dune build $@
 
tests: install 
	$(MAKE) -C ../unit_tests

clean:
	dune clean

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1
