TESTS = test_parser test_transformations test_path test_aosoa test_switch

INSTALL_TARGET=`opam config var prefix`/lib/optiTrust

all:
	dune build @install

# TODO: find a way to get dune to do the install of the 'tools' folder automatically?

install: all
	dune install
	mkdir -p $(INSTALL_TARGET)/tools
	install -m755 tools/*.* $(INSTALL_TARGET)/tools

uninstall:
	dune uninstall

%.exe: all
	dune build $@

tests: % : test_suite/%.exe
		dune exec --no-build test_suite/$@.exe $(TESTS)

clean:
	dune clean

watch:
	nohup .vscode/watch.sh >/dev/null 2>&1
