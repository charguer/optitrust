
DIR_ROOT=..
DIR_SIM=$(DIR_ROOT)/simulations
DIR_PROGS=$(DIR_ROOT)/3d_runs/run1
BINARIES=$(DIR_PROGS)/pic_demo.out $(DIR_PROGS)/pic_barsamian.out
SCRIPTS=.


DEPENDENCIES=$(wildcard $(DIR_ROOT)/simulations/* $(DIR_ROOT)/src/* $(DIR_ROOT)/include/* $(SCRIPTS)/compile.sh $(ROOT)/your_configuration.sh)

all: checker.out progs params

checker.out: checker.c
	gcc -g $< -o $@  -lm

progs: $(BINARIES)

params: $(DIR_PROGS)/parameters_3d.txt

$(DIR_PROGS)/parameters_3d.txt: parameters_3d.txt
	cp $< $@

# The 1 below means the particles need to be dumped in "particle.res"
$(DIR_PROGS)/%_checker.out: $(DEPENDENCIES)
	./compile.sh $*.c 1
#$(DIR_PROGS)/checker_%.out: $(DEPENDENCIES)
#	./compile.sh $*.c 1


$(DIR_PROGS)/%.out: $(DEPENDENCIES)
	./compile.sh $*.c

clean:
	rm -f checker.out *.res $(DIR_PROGS)/*.out $(DIR_PROGS)/*.res

test: checker.out
	./$< 


#	./$< pic_demo.res pic_barsamian.res
# LATER  -- `basename $@ .out`.res