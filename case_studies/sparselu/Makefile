all: sequential parallel

parallel: sparselu_out.cpp tools
	clang++ -Wall -g -fopenmp -o sparselu-parallel.bin $< tools.o

sequential: sparselu.cpp tools
	clang++ -Wall -g -o sparselu.bin $< tools.o

tools: tools.cpp tools.hpp
	clang++ -Wall -g -c -o tools.o $<

clean:
	rm -f *.bin
