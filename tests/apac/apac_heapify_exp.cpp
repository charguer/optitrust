int i = 1;

int x = 2;

int main() {
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
#pragma omp task default(shared) depend(inout : a)
  delete a;
#pragma omp task default(shared) depend(inout : b)
  delete b;
#pragma omp task default(shared) depend(inout : c)
  delete c;
#pragma omp task default(shared) depend(inout : d)
  delete d;
#pragma omp task default(shared) depend(inout : f)
  delete f;
#pragma omp task default(shared) depend(inout : h)
  delete[] h;
#pragma omp task default(shared) depend(inout : j)
  delete[] j;
#pragma omp task default(shared) depend(inout : k)
  delete k;
#pragma omp task default(shared) depend(inout : l)
  delete l;
#pragma omp task default(shared) depend(inout : m)
  delete m;
#pragma omp task default(shared) depend(inout : n)
  delete n;
#pragma omp task default(shared) depend(inout : r)
  delete r;
#pragma omp task default(shared) depend(inout : t)
  delete[] t;
#pragma omp task default(shared) depend(inout : u)
  delete[] u;
#pragma omp task default(shared) depend(inout : v)
  delete[] v;
#pragma omp task default(shared) depend(inout : w)
  delete[] w;
#pragma omp task default(shared) depend(inout : y)
  delete[] y;
#pragma omp task default(shared) depend(inout : z)
  delete[] z;
#pragma omp task default(shared) depend(inout : po)
  delete[] po;
#pragma omp task default(shared) depend(inout : pl)
  delete[] pl;
#pragma omp task default(shared) depend(inout : pg)
  delete[] pg;
  return 0;
}
