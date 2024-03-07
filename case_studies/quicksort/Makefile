all: sequential parallel

parallel: quicksort_out.cpp
	g++ -Wall -g -fopenmp -o quicksort-parallel.bin $<

sequential: quicksort.cpp
	g++ -Wall -g -o quicksort.bin $<

clean:
	rm -f *.bin
