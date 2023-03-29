TARGET_MAKE_ALL := build_timer
include ../Common.Makefile

# no effect? FLAGS := -disable-clang-format

CURRENT := build_perf.ml

build_timer:
	touch ../../src/ast.ml
	$(MAKE) V= FLAGS="-disable-clang-format" optitrust build_perf_out.cpp

