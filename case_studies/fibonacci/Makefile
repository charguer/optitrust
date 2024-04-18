all: sequential parallel

parallel: fibonacci_out.cpp
	g++ -Wall -g -fopenmp -o fibonacci-parallel.bin $<

sequential: fibonacci.cpp
	g++ -Wall -g -o fibonacci.bin $<

clean:
	rm -f *.bin
