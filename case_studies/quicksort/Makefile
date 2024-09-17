all: sequential profiling parallel

parallel: quicksort_parallel.cpp
	g++ -Wall -g -fopenmp -o quicksort_parallel.bin $<

profiling: quicksort_profiling.cpp
	g++ -Wall -Wno-unused-label -g -o quicksort_profiling.bin $<

sequential: quicksort.cpp
	g++ -Wall -g -o quicksort.bin $<

clean:
	rm -f *.bin
