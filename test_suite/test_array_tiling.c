#include <stdlib.h>

void* my_alloc(int nb_elts, int size_elt) {
  return malloc(nb_elts * size_elt);
}

typedef int T[10];
typedef int* S;

int get_5 (int* t) {
  return t[5];
}

int get_6_5 (int* t, int* v) {
  return t[6] + get_5(v);
}

int main () {
  T t;
  S v = my_alloc(10, sizeof(int));

  for (int i = 0; i < 10; i++) {
    t[i] = 2 * i;
  }

  int n = t[5] + get_6_5(t, v) + get_6_5(v, t) + get_6_5(t, t) +
    get_6_5(v, v);

  free(v);

  return 0;
}
