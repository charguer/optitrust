
DIR_ROOT=../..
DIR_SIM=$(DIR_ROOT)/simulations
DIR_PROGS=$(DIR_ROOT)/3d_runs/run1
BINARIES=$(DIR_PROGS)/pic_demo.out $(DIR_PROGS)/pic_barsamian.out
SCRIPTS=.

DEPENDENCIES=$(wildcard $(DIR_ROOT)/simulations/* $(DIR_ROOT)/src/* $(DIR_ROOT)/include/* $(SCRIPTS)/compile.sh $(ROOT)/your_configuration.sh)

all: checker.out progs

checker.out: checker.c
	gcc $< -o $@  -lm

progs: $(BINARIES)

$(DIR_PROGS)/%.out: $(DEPENDENCIES)
	./compile.sh $*.c 1

clean:
	rm -f checker.out *.res $(DIR_PROGS)/*.out

test: checker.out
	./$< pic_demo.res pic_barsamian.res
