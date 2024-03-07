all: sequential parallel

parallel: molecular_dyn_out.cpp
	g++ -Wall -g -fopenmp -o molecular_dyn-parallel.bin $<

sequential: molecular_dyn.cpp
	g++ -Wall -g -o molecular_dyn.bin $<

clean:
	rm -f *.bin
