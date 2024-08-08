#include <stdlib.h>

int ** f(int ** v) {
	v = (int **) malloc(sizeof(int*));
	return v;
}

int * g(int * v) {
	v = (int *) malloc(sizeof(int));
	return v;
}

int k() {
	return 42;
}

int h(int ** v1, int * v2, int v3) {
  v1 = f(v1);
  int ** a1 = v1;
  a1[0] = g(a1[0]);
  int * a2 = a1[0];
  int * a2bis = NULL;
  a2[0] = k();
  a2bis = v2;
  a2 = v2 + 1;
  a2[0] = k();
  a2bis[0] = k();
  int a3 = v3;
  a3++;
}
