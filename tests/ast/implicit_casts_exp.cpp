typedef long unsigned int size_t;

typedef int myint;

void* malloc(size_t n);

void free(void* p);

void f(int a);

void g(int* a);

void h(const char* c);

void fc(const int a);

void gc(int* const a);

void hc(const char* const c);

void demo_stack_var() {
  const int x = 3;
  f(x);
  fc(x);
  int z;
  z = 6;
  const int v = z;
  int y = 5;
  f(y);
  fc(y);
  y = y + 1;
  y += 4;
}

void demo_malloc_var() {
  int* const a = cast<void*, int*>(malloc(sizeof(int)));
  *a = *a + 2;
  g(a);
  free(a);
}

void demo_pointers() {
  int y = 1;
  int z = 2;
  int* const p = &y;
  *p = *p + 3;
  int* q = &y;
  q = &z;
  *q = *q + 3;
}

void demo_malloc_array() {
  int* const t =
      cast<void*, int*>(malloc(cast<int, long unsigned int>(9) * sizeof(int)));
  t[1] = t[1] + 2;
  free(t);
  int* u =
      cast<void*, int*>(malloc(cast<int, long unsigned int>(9) * sizeof(int)));
  u[1] = u[1] + 2;
  free(u);
  u = cast<void*, int*>(malloc(cast<int, long unsigned int>(8) * sizeof(int)));
  u[1] = u[1] + 2;
}

void demo_stack_array() {
  const int v[2] = {0, 1};
  f(v[1]);
  int w[2] = {0, 1};
  w[1] = w[1] + 2;
  f(w[1]);
  g(&w[1]);
  int* const a = &w[1];
  f(*a);
  g(a);
  int* b = &w[1];
  f(*b);
  g(b);
  b = &w[0];
  int* const r[2] = {w, w};
  gc(r[1]);
  fc(r[1][1]);
  int* s[2] = {w, w};
  g(r[1]);
  f(r[1][1]);
  s[1] = w;
}

void demo_array_access(int i, size_t k, char* buf) {
  const char a[] = "azlkrj";
  buf[k] = a[i];
  buf[i] = a[k];
  buf + i;
  buf + k;
}

void demo_explicit_coercions() {
  bool b = true;
  int i = cast<bool, int>(b);
  bool b2 = cast<int, bool>(i);
  float r = cast<int, float>(i);
  myint i2 = cast<int, int>(i);
}

void demo_implicit_coercions() {
  bool b = true;
  int i = cast<bool, int>(b);
  bool b2 = cast<int, bool>(i);
  size_t s = cast<int, long unsigned int>(i);
  f(cast<long unsigned int, int>(s));
  fc(cast<long unsigned int, int>(s));
  myint j = i;
  f(j);
  fc(j);
}

void demo_arithop_coercions() {
  int i = 1;
  size_t s = cast<int, long unsigned int>(1);
  int r1 = cast<long unsigned int, int>((cast<int, long unsigned int>(i) + s));
  size_t r2 = cast<int, long unsigned int>(i) + s;
  fc(cast<long unsigned int, int>((cast<int, long unsigned int>(i) + s)));
  myint j = 1;
  int r3 = i + j;
}

void demo_pointer_coercions() {
  int i = 1;
  g(&i);
  gc(&i);
}

void demo_strings() {
  const char x[10] = "foo";
  h("foo");
  h(x);
}

typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;

typedef struct vects {
  vect head;
  struct vects* tail;
} vects;

typedef vect* vectptr;

vect vect_mul(int f, const vect v) { return (vect){f * v.x, f * v.y}; }

void demo_struct_init() {
  vect v = {1, 2};
  vect w1 = vect_mul(3, v);
  vect w2 = vect_mul(3, (vect){1, 2});
}
