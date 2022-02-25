
all: checker.out

checker.out: checker.c
	gcc $< -o $@  -lm

clean:
	rm -f checker.out *.res

test: checker.out
	./$< pic_demo.res pic_barsamian.res
