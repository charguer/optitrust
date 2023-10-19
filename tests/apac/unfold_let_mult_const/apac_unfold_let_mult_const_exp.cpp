int f(int arg, int* argp) {
  int i;
  const int a = 1;
  const int b = i;
  const int& c = i;
  const int d[5] = {1, 2, 3, 4, 5};
  const int* const e = &i;
  const int* const f = a;
  const int* const g = *e;
  const int* const h = &i;
  const int* const j = a;
  const int* const k = *e;
}
