

// Description of the encoding performed for the AST representation

typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

typedef struct vects { vect head; vects* tail; } vects;

typedef vect vect2;
typedef vect2 vect3;
typedef int int2[2];
typedef int* intstar;

void constants() {
  const int a = 3;
  int const b = a + 3;
  int c = b + 4;
  const vect v = { 0, 1};
  int d = v.x;
}

typedef int* intstar;

void const_pointers() {
  int a = 3;
  const intstar b = &a;
  const int c = *b + 4;
}

// // Function arguments are assumed to be 'const' by default
int f(int n) {
  return n;
}

int immutable_stack_ptr() {
  int x = 3;
  int y = f(x);
  const int * p = &x;
  int * const q = &x;
  int const* const m = &x;
  int const* n = &x;
  const int * const r = &x;

  p = &y;
  *q = 5;
  return *p + *q + *r;
}

int immutable_stack_array() {
  int x = 3;
  int y = 4;
  int* const t[2] = { &x, &y };
  // t[0] = &y; // disallowed
  *(t[0]) = 3; // allowed, because t[0] is not const

  const int* u[2] = { &x, &y };
  u[0] = &y; // allowed, because u itself is not const
  // *(u[0]) = 3; disallowed

  const int* const v[2] = { &x, &y };
  // v[0] = &y; // disallowed
  //*(v[0]) = 3; // disallowed
  return *(t[0]);
}

int immutable_stack_var() {
  int const a = 4;
  const int r = 3;
  int const s = r + 1;
  return r;
}

void access_encoding() {
  const vect a = { 0, 1 };
  // copy as const
  const vect b = a;
  // copy as non-const
  vect c = a;
  // accesses to const
  const int ax = a.x;
 // accesses to non-const
  const int cy = c.y;
}

int foo(vect v) { return v.x; }

int mutable_var_encoding() {
    const vect a = { 0, 1 };
    int ax = foo(a);
    vect c = a;
    int cx = foo(c);
}

typedef struct {
  vect fst;
  vect snd;
} vectpair;

void lvalue_encoding() {
  vect* p;
  p->x = 2;
  (*p).x = 3;

  vectpair* q;
  q->fst.x = 2;
  (*q).fst.x = 3;

  int *v;
  *v = 4;
}
