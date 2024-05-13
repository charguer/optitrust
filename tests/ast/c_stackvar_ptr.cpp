int f(int arg, int* argp) {
  int i;
  int a = 1;
  int b = i;
  int& c = i;
  int d[5] = {1, 2, 3, 4, 5};
  int* e = &i;
  int* f = &a;
  int* g = e;
  int* const h = &i;
  int* const j = &a;
  int* const k = e;
  const int l = 1;
  const int m = i;
  const int& n = 1;
  const int& o = i;
  const int p[5] = {1, 2, 3, 4, 5};
  const int* q = &i;
  const int* r = &l;
  const int* s = q;
  const int* const t = &i;
  const int* const u = &l;
  const int* const v = q;
  int w = arg;
  int* x = &arg;
  int* y = argp;
  int z = *argp;
}

int g(int arg, int * argp) {
  int i;
  int a = 1, b = i, &c = i, *e = &i,  *f = &a, *g = e,
      *const h = &i, *const j = &a, *const k = e;
  int const l = 1, m = i, &n = 1, &o = i, *q = &i,
      *r = &l, *s = q, *const t = &i, *const u = &l, *const v = q;
  int w = arg, *x = &arg, *y = argp, z = *argp;
}
