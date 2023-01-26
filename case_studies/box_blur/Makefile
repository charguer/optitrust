
# Usage: make optitrust box_blur_out.cpp

TARGET_MAKE_ALL := transfo

OPTITRUST := ../..

TESTS := box_blur.ml

# -fno-tree-vectorize to see difference
_build/box_blur.o: box_blur_after.cpp
	g++ -c -Ofast -march=native -fno-tree-vectorize -std=c++17 -I ../../include/ -Wall -Werror -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi -fopenmp -Wall box_blur_after.cpp -o _build/box_blur.o

include ../../tests/Common.Makefile

HALIDE_DIRECTORY ?= ~/Halide
HALIDE_BLUR_DIR := $(HALIDE_DIRECTORY)/apps/blur/

_build/box_blur_bench: box_blur_bench.cpp _build/box_blur.o
	$(V)cd $(HALIDE_BLUR_DIR) && make bin/host/halide_blur.a
	$(V)g++ -O2 -std=c++17 -I $(HALIDE_DIRECTORY)/distrib/include -I $(HALIDE_DIRECTORY)/distrib/tools -Wall -Werror -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi -fopenmp -Wall -I $(HALIDE_BLUR_DIR)/bin/host box_blur_bench.cpp _build/box_blur.o $(HALIDE_BLUR_DIR)/bin/host/halide_blur.a -o _build/box_blur_bench -ldl -lpthread -lz -ltinfo -ljpeg -lpng

bench_ref: _build/box_blur_bench
	$(V)_build/box_blur_bench reference noisy.png

bench: _build/box_blur_bench
	$(V)_build/box_blur_bench OptiTrust noisy.png