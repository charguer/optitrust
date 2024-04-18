#include <stdio.h>
#include <stdlib.h>

long long int fibonacci(int n) {
	long long int x, y;
	if (n < 2) return n;

	x = fibonacci(n - 1);
	y = fibonacci(n - 2);

	return x + y;
}

int main(int argc, char ** argv) {
  int n = 16;
  if(argc > 1) {
    n = atoi(argv[1]);
  }

  long long int result;
  result = fibonacci(n);

  printf("fibonacci(%d) = %lld\n", n, result);

  return 0;
}
