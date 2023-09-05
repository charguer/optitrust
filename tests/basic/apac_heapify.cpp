int main() {
  int i;
  int x;
  int a = 1;
  int b = i;
  int const c = 1;
  int const d = i;
  int &e = i;
  int const &f = 1;
  int const &g = i;
  int const h[5] = {1, 2, 3, 4, 5};
  int j[5] = {1, 2, 3, 4, 5};
  int k = 1, l = i;
  int const m = 1, n = i;
  int &o = i, &p = x;
  int const &r = 1, &s = i;
  int const t[5] = {1, 2, 3, 4, 5}, u[2] = {1, 2}, v[1] = {0};
  int w[5] = {1, 2, 3, 4, 5}, y[2] = {1, 2}, z[1] = {0};
  int * aa = &i, * ab = &x, * ac = 0x0;
  int const * ad = &i, * ae = &i, * const af = &x;
  int const * const ag = af;
  int const * const ah = &x;
  int * po[10], *pl[2] = {aa, ab};
  int * pg[2] = {aa, ab};
  return 0;
}
