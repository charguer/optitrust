int f(int arg, int* argp) {
  int i;
  int a = 1;
  int b = i;
  const int& c = i;
  const int d[5] = {1, 2, 3, 4, 5};
  const int* e = &i;
  const int* f = &a;
  const int* g = e;
  const int* const h = &i;
  const int* const j = &a;
  const int* const k = e;
}
