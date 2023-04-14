TESTS := harris.ml

include ../Common.Makefile

_build/%.o: %.c _build
	$(V)$(CC) $(OPT_FLAGS) -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

# FIXME: avoid treating .cpp files as .c files, use .c files directly instead.
_build/%.o: %.cpp _build
	$(V)$(CC) $(OPT_FLAGS) -std=c11 -I ../../include/ $(WARN_FLAGS) -x c $< -o $@

HALIDE_PATH=../../../../Halide
HALIDE_TARGET=host
#HALIDE_TARGET=x86-64-linux-avx-avx2-fma-sse41

_build/halide_runtime.a: _build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/runtime.a
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/runtime.a $@

_build/harris_halide.a: _build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/harris.a
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris.a $@

_build/harris_halide.h: _build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/harris.h
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris.h $@

_build/harris_halide_auto.a: _build
	$(V)cd $(HALIDE_PATH)/apps/harris && make bin/$(HALIDE_TARGET)/harris_auto_schedule.a
	$(V)cp $(HALIDE_PATH)/apps/harris/bin/$(HALIDE_TARGET)/harris_auto_schedule.a $@

# TODO? $(CC)
BENCH_HALIDE_OBJS := bench_halide.cpp _build/harris_halide.a _build/halide_runtime.a
_build/bench_halide: $(BENCH_HALIDE_OBJS) bench_util.cpp _build/harris_halide.h
	$(V)gcc $(BENCH_HALIDE_OBJS) -O2 -std=c++17 -lstdc++ -lm -lpthread -ldl -lpng -ljpeg -I$(HALIDE_PATH)/include -I$(HALIDE_PATH)/tools $(WARN_FLAGS) -o $@

.PHONY: bench bench_%

HARRIS_CPP := $(wildcard harris*.cpp)
HARRIS_BENCHS := $(patsubst %.cpp,bench_%,$(HARRIS_CPP))
HARRIS_BENCHS := $(filter-out bench_harris_after,$(HARRIS_BENCHS))
HARRIS_BENCHS := $(filter-out bench_harris_before,$(HARRIS_BENCHS))

bench: bench_ref $(HARRIS_BENCHS)

bench_halide: _build/bench_halide
	$(V)_build/bench_halide $(HALIDE_PATH)/apps/images/rgba.png output.png

bench_%: _build/%.o
	echo "TBD" $<