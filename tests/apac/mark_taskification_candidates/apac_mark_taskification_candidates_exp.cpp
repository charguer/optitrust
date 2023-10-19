int f(int a) { return a * 3; }

int g(int a) { return f(a) + 3; }

/*@taskify*/ int h(int a) { return f(a * 3) / f(a); } /*taskify@*/

/*@taskify*/ int i(int a) {
  int out = f(a);
  out *= g(a);
  return out;
} /*taskify@*/

/*@taskify*/ int j(int a) { return f(g(a * i(a))); } /*taskify@*/
