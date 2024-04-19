all: sequential parallel

parallel: molecular_dyn_out.cpp tools
	clang++ -Wall -g -fopenmp -o molecular_dyn-parallel.bin $< tools.o

sequential: molecular_dyn.cpp tools
	clang++ -Wall -g -o molecular_dyn.bin $< tools.o

tools: tools.cpp tools.hpp
	clang++ -Wall -g -c -o tools.o $<

clean:
	rm -f *.bin
