#include <optitrust_models.h>

typedef struct {

  int x;
  int y;
} vect ;

typedef struct {

  vect pos;
  vect speed;
} particle ;

typedef struct vects {

  vect head;
  vects* tail;
}vects ;

typedef vect vect2;

typedef vect2 vect3;

typedef int int2[2];

typedef int* intstar;

  void addr_array_cell ()  {
  int p[2];
  int* n = &p[0];
}

  void initlist ()  {
  vect v1 =  {1, 2};
  vect3 v2 =  {1, 2};
  int2 p =  {1, 2};
  intstar n = &p[0];
}

  int f (int n)  { return n; }

  void test_loop ()  {
  int a = 0;
   for (int i = 0; i < 10; i++) { a++; }
   for (int i = 10; i >= 0; i--) { a--; }
  const int x = 3;
  const int y = 2;
  for (int x = 2; ; ) {
    int y = 1;
    int r = x + y;
  }
  const int z = x + y;
}

  void stack_var ()  {
  int r = 3;
  r = r + 1 + 2;
  r += 2;
  r++;
  int s = f(r);
}

  void stack_array ()  {
  int t[2] = {5, 6};
  int a = t[0];
  t[1] = a + 2;
}

  void stack_struct ()  {
  vect v =  {5, 6};
  int a = v.x;
  v.y = a + 2;
  vect v2 = v;
  v2 = v;
  particle p1 =  {v, v};
  particle p2 =  {v,  {7, 8}};
}

  void references ()  {
  int a = 3;
  int& b = a;
  b = b + 4;
}

  void constants ()  {
  const int a = 3;
  const int b = a + 3;
  int c = b + 4;
  const vect v =  {0, 1};
  int d = v.x;
}

  void const_pointers ()  {
  int a = 3;
  int* b = &a;
  const int c = *b + 4;
}

  void nonconst_pointers ()  {
  int a = 3;
  int* b = &a;
  *b = *b + 4;
  int c = 3;
  b = &c;
}

  int main ()  {  }

  int h (int x)  {
  int y = x + 1;
  return y;
}

  int immutable_stack_ptr ()  {
  int x = 3;
  int y = f(x);
  int* p = &x;
  int* const q = &x;
  int* const r = &x;
  p = &y;
  *q = 5;
  return *p + *q + *r;
}

  int immutable_stack_array ()  {
  int x = 3;
  int y = 4;
  int* t[2] = {&x, &y};
  *t[0] = 3;
  int* u[2] = {&x, &y};
  u[0] = &y;
  int* v[2] = {&x, &y};
  return *t[0];
}

  int immutable_stack_var ()  {
  const int a = 4;
  const int r = 3;
  const int s = r + 1;
  return r;
}

  int mutable_stack_var ()  {
  int r = 3;
  r = r + 1;
  r++;
  return r;
}

  int mutable_stack_array ()  {
  int x = 3;
  int y = 4;
  int* w[2] = {&x, &y};
  w[0] = &y;
  *w[0] = 3;
  return *w[0];
}

  void access_encoding ()  {
  const vect a =  {0, 1};
  const vect b = a;
  vect c = a;
  const int ax = a.x;
  const int cy = c.y;
}

  int foo (vect v)  { return v.x; }

  int mutable_var_encoding ()  {
  const vect a =  {0, 1};
  int ax = foo(a);
  vect c = a;
  int cx = foo(c);
  return cx;
}

typedef struct {

  vect fst;
  vect snd;
} vectpair ;

  void lvalue_encoding ()  {
  vect* p;
  p->x = 2;
  (*p).x = 3;
  vectpair* q;
  q->fst.x = 2;
  (*q).fst.x = 3;
  int* v;
  *v = 4;
}

  void arrow ()  {
  vect v =  {0, 1};
  vect* p = &v;
  (*p).x = (*p).y;
  p->x = p->y;
}





  void one_fork ()  {
  __pure();
  int x = 0;
  const __ghost_fn fork_out = __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
   for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~~> 0");
     for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}

  void two_forks ()  {
  __pure();
  int x = 0;
  const __ghost_fn fork_out = __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
   for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split3, "f := #_1, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~~> 0, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 3, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join3, "f := #_1, H := &x ~~> 0");
     for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}

  void two_forks_spe_twice ()  {
  __pure();
  int x = 0;
  const __ghost_fn fork_out = __ghost_begin(ro_fork_group, "H := &x ~~> 0, r := 0..5");
   for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("&x ~~> 0");
    __ghost(ro_split2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_split2, "f := #_1 / 2, H := &x ~~> 0");
    __ghost(ro_fork_group, "f := #_1 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_fork_group, "f := #_1 / 2 / 2, H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1 / 2, H := &x ~~> 0");
     for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&x ~~> 0");
      x + 1;
    }
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
    __ghost(ro_allow_join2, "f := #_1, H := &x ~~> 0");
    __ghost(ro_join_group, "H := &x ~~> 0, r := 0..5");
  }
  __ghost_end(fork_out);
}
