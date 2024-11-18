typedef struct {
  int x;
  int y;
} vect;

int foo(vect v) {
  int __res = (v.x);
  __res;
}

void demo() {
  vect a = {0, 1};
  int* ax = ref int(/*@body*/ ({
    int __res = (a.x);
    __res;
  }) /*body@*/);
  vect* c = ref vect(a);
  int* cx = ref int(/*@body*/ ({
    int __res = *(c[.] x);
    __res;
  }) /*body@*/);
  vect* b = ref vect{0, 1};
  vect** p = ref vect * (b);
  int e = *((*p)[.] x);
  int f = *((*p)[.] x);
  int g = *(b[.] x);
}
