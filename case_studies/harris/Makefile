TESTS := harris.ml

include ../Common.Makefile

_build/%.o: %.c #_build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -std=c11 -I ../../include/ $(WARN_FLAGS) -c $< -o $@

# FIXME: avoid treating .cpp files as .c files, use .c files directly instead.
_build/%.o: %.cpp _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -std=c11 -I ../../include/ $(WARN_FLAGS) -c -x c $< -o $@

_build/%.bc: %.cpp _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -std=c11 -I ../../include/ $(WARN_FLAGS) -c -x c -emit-llvm $< -o $@

_build/%.s: %.cpp _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -std=c11 -I ../../include/ $(WARN_FLAGS) -S -x c $< -o $@

HALIDE_PATH=../../../../Halide
HALIDE_TARGET=host
#HALIDE_TARGET=x86-64-linux-avx-avx2-fma-sse41

_build/halide_runtime.a: #_build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/runtime.a
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/runtime.a $@

_build/harris_halide.a _build/harris_halide.h: #_build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/host/harris.generator && ./bin/host/harris.generator -g harris -f harris_halide -o bin/$(HALIDE_TARGET)/ -e c_header,static_library target=$(HALIDE_TARGET)-no_runtime
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris_halide.a _build/
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris_halide.h _build/

#_build/harris_halide_auto.a: _build
#	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/harris_auto_schedule.a
#	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris_auto_schedule.a $@

# FIXME? using $(CC) below fails

_build/bench_halide: bench_halide.cpp _build/harris_halide.a _build/halide_runtime.a bench_util.h _build/harris_halide.h bench_util.h
	$(V)gcc $(filter-out %.h,$^) -O2 -std=c++17 -lstdc++ -lm -lpthread -ldl -lpng -ljpeg -I$(HALIDE_PATH)/include -I$(HALIDE_PATH)/tools $(WARN_FLAGS) -o $@

_build/bench_%: bench_c.cpp _build/harris_halide.a _build/halide_runtime.a _build/%.o bench_util.h _build/harris_halide.h harris.h bench_util.h
	$(V)gcc $(filter-out %.h,$^) -O2 -std=c++17 -lstdc++ -lm -lpthread -ldl -lpng -ljpeg -liomp5 -I$(HALIDE_PATH)/include -I$(HALIDE_PATH)/tools $(WARN_FLAGS) -o $@

.PHONY: bench bench_%

HARRIS_CPP := $(wildcard harris*.cpp)
HARRIS_BENCHS := $(patsubst %.cpp,bench_%,$(HARRIS_CPP))
HARRIS_BENCHS := $(filter-out bench_harris_after,$(HARRIS_BENCHS))
HARRIS_BENCHS := $(filter-out bench_harris_before,$(HARRIS_BENCHS))
HARRIS_BENCHS := $(filter-out bench_harris_exp,$(HARRIS_BENCHS))

bench: bench_halide $(HARRIS_BENCHS)

bench_halide: _build/bench_halide
	@echo "---- Benchmarking Halide .."
	$(V)$< $(HALIDE_PATH)/apps/images/rgba.png output.png
	@echo "----"

bench_%: _build/bench_%
	@echo "---- Benchmarking $(patsubst bench_%,%.cpp,$<) .."
	$(V)$< $(HALIDE_PATH)/apps/images/rgba.png
	@echo "----"
