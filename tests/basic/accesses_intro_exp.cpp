typedef struct {
  int x;
  int y;
} vect;

int foo(vect v) { return v.x; }

int demo() {
  const vect a = {0, 1};
  int ax;
  /*@body*/ { ax = a.x; } /*body@*/
  vect c = a;
  int cx;
  /*@body*/ { cx = c.x; } /*body@*/
  vect b = {0, 1};
  vect *p = (&b);
  int const e = (p.x);
  int const f = (p->x);
  int const g = (*(&b)).x;
}
