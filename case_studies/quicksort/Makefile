all: sequential profiling parallel

parallel: quicksort_out.cpp
	g++ -Wall -g -fopenmp -o quicksort-parallel.bin $<

profiling: quicksort_profiling_out.cpp
	g++ -Wall -Wno-unused-label -g -o quicksort-profiling.bin $<

sequential: quicksort.cpp
	g++ -Wall -g -o quicksort.bin $<

clean:
	rm -f *.bin
