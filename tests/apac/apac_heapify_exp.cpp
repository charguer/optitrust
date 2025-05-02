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
  int* k = new int(1);
  int* l = new int(i);
  const int* const m = new const int(1);
  const int* const n = new const int(i);
  int& o = i;
  int& p = x;
  const int* const r = new const int(1);
  const int& s = i;
  const int* const t = new const int[5]{1, 2, 3, 4, 5};
  const int* const u = new const int[2]{1, 2};
  const int* const v = new const int[1]{0};
  int* w = new int[5]{1, 2, 3, 4, 5};
  int* y = new int[2]{1, 2};
  int* z = new int[1]{0};
  int* aa = &i;
  int* ab = &x;
  int* ac = 0;
  const int* ad = &i;
  const int* ae = &i;
  const int* const af = &x;
  const int* const ag = af;
  const int* const ah = &x;
  int** po = new int*[10]();
  int** pl = new int* [2] { aa, ab };
  int** pg = new int* [2] { aa, ab };
  int* ai = new int(1);
#pragma omp task default(shared) depend(inout : a[0]) firstprivate(a)
  delete a;
#pragma omp task default(shared) depend(inout : b[0]) firstprivate(b)
  delete b;
#pragma omp task default(shared) depend(inout : c[0]) firstprivate(c)
  delete c;
#pragma omp task default(shared) depend(inout : d[0]) firstprivate(d)
  delete d;
#pragma omp task default(shared) depend(inout : f[0]) firstprivate(f)
  delete f;
#pragma omp task default(shared) depend(inout : h[0]) firstprivate(h)
  delete[] h;
#pragma omp task default(shared) depend(inout : j[0]) firstprivate(j)
  delete[] j;
#pragma omp task default(shared) depend(inout : k[0]) firstprivate(k)
  delete k;
#pragma omp task default(shared) depend(inout : l[0]) firstprivate(l)
  delete l;
#pragma omp task default(shared) depend(inout : m[0]) firstprivate(m)
  delete m;
#pragma omp task default(shared) depend(inout : n[0]) firstprivate(n)
  delete n;
#pragma omp task default(shared) depend(inout : r[0]) firstprivate(r)
  delete r;
#pragma omp task default(shared) depend(inout : t[0]) firstprivate(t)
  delete[] t;
#pragma omp task default(shared) depend(inout : u[0]) firstprivate(u)
  delete[] u;
#pragma omp task default(shared) depend(inout : v[0]) firstprivate(v)
  delete[] v;
#pragma omp task default(shared) depend(inout : w[0]) firstprivate(w)
  delete[] w;
#pragma omp task default(shared) depend(inout : y[0]) firstprivate(y)
  delete[] y;
#pragma omp task default(shared) depend(inout : z[0]) firstprivate(z)
  delete[] z;
#pragma omp task default(shared) depend(inout \
                                        : po[0], po[0][0]) firstprivate(po)
  delete[] po;
#pragma omp task default(shared) depend(inout \
                                        : pl[0], pl[0][0]) firstprivate(pl)
  delete[] pl;
#pragma omp task default(shared) depend(inout \
                                        : pg[0], pg[0][0]) firstprivate(pg)
  delete[] pg;
  return 0;
}
