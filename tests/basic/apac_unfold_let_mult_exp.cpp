int f(int arg, int* argp) {
  int i;
  int a = 1;
  int b = i;
  int& c = i;
  int d[5] = {1, 2, 3, 4, 5};
  int* e = &i;
  int* g = e;
  int* const h = &i;
  int* const k = e;
  const int l = 1;
  const int m = i;
  const int& n = 1;
  const int& o = i;
  const int p[5] = {1, 2, 3, 4, 5};
  const int* q = &i;
  const int* s = q;
  const int* const t = &i;
  const int* const v = q;
}
