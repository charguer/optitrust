CC = g++
CFLAGS = -Wall -Wno-unused-label
LDFLAGS ?=

parallel: $(BENCHMARK)_parallel.cpp dependencies
	$(CC) $(CFLAGS) -c -fopenmp -o $(BENCHMARK)_parallel.o $<
	$(CC) $(CFLAGS) -fopenmp -o $(BENCHMARK)_parallel.exe *.o $(LDFLAGS)

sequential: $(BENCHMARK).cpp dependencies
	$(CC) $(CFLAGS) -c -o $(BENCHMARK).o $<
	$(CC) $(CFLAGS) -o $(BENCHMARK).exe *.o $(LDFLAGS)

purge: clean
	rm -f *_parallel.c* *_profiling.* *.profile *.model

clean:
	rm -f *.o *.exe *.log
