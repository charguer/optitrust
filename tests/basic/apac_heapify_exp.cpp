int main() {
  int i = 1;
  int x = 2;
  int* a = new int(1);
  int* b = new int(i);
  const int* const c = new const int(1);
  const int* const d = new const int(i);
  int& e = i;
  const int* const f = new const int(1);
  const int& g = i;
  const int* const h = new const int[5]{1, 2, 3, 4, 5};
  int* j = new int[5]{1, 2, 3, 4, 5};
  int *k = new int(1), *l = new int(i);
  const int *m = new const int(1), *n = new const int(i);
  int &o = i, &p = x;
  const int *r = new const int(1), &s = i;
  const int *const t = new const int[5]{1, 2, 3, 4, 5},
                   *const u = new const int[2]{1, 2},
                   *const v = new const int[1]{0};
  int *w = new int[5]{1, 2, 3, 4, 5}, *y = new int[2]{1, 2}, *z = new int[1]{0};
  int *aa = &i, *ab = &x, *ac = 0;
  const int *ad = &i, *ae = &i, *const af = &x;
  const int* const ag = af;
  const int* const ah = &x;
  int **po = new int *[10](), **pl = new int *[2] { aa, ab };
  int** pg = new int* [2] { aa, ab };
  int* ai = new int(1);
  delete c;
  delete d;
  delete f;
  delete[] h;
  delete k;
  delete l;
  delete m;
  delete n;
  delete r;
  delete[] t;
  delete[] u;
  delete[] v;
  delete[] w;
  delete[] y;
  delete[] z;
  delete[] po;
  delete[] pl;
  return 0;
}
