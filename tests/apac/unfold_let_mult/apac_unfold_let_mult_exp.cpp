void f(int arg, int* argp) {
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
  int& n = 1;
  int& o = i;
  int p[5] = {1, 2, 3, 4, 5};
  int* q = &i;
  int* r = &l;
  int* s = q;
  int* const t = &i;
  int* const u = &l;
  int* const v = q;
  int w = arg;
  int* x = &arg;
  int* y = argp;
  int z = *argp;
}
