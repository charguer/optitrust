typedef struct {
  int x;
  int y;
} vect;

int foo(vect v) { return v.x; }

int demo() {
  const vect a = { 0, 1 };
  int ax = foo(a);
  vect c = a;
  int cx = foo(c);

  vect b = { 0, 1 };
  vect* p = &b;
  const int e = (*p).x;
  const int f = p->x;
  const int g = (*(&b)).x;

}
