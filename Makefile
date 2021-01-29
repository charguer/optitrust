TESTS = test_parser test_transformations test_path test_aosoa test_switch

all:
	dune build @install

install: all
	dune install

uninstall:
	dune uninstall

%.exe: all
	dune build $@

tests: % : test_suite/%.exe
	dune exec --no-build test_suite/$@.exe $(TESTS)

clean:
	dune clean
