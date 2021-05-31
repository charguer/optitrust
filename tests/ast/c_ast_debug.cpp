
int f(int x) {
  x = x + 1;
  return x;
}

int immutable_stack_ptr() {
  int x = 3;
  int y = f(x);
  const int * p = &x;
  int * const q = &x;
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
  const int s = r + 1;
  return r;
}

int mutable_stack_var() {
  int r = 3;
  r = r + 1;
  r++;
  return r;
}

int mutable_stack_array (){
  int x = 3;
  int y = 4;
   int* w[2] = { &x, &y };
   w[0] = &y; // allowed
   *(w[0]) = 3; // allowed
   return *(w[0]);
}
// void advanced_stuff () {
//     int* x;
//     int ** p;
//     int ** const q = &x;
//     int * const * r = &
//     int const ** s;
//     int * const * const u;

// }

/*  ENCODED VERSION
int mutable_stack_var() {
  const int* r = new 3;
  set(r, get(r) + 1);
  operator++(r);
  return get(r);
}
*/




/*
  t[3].f
  t[3]->f
  t->f[3]
  t.f[3]

  both in case t a stack allocated
  or t in a const
  or t is a function argument passed by pointer
  or t is a  function argument passed by value


*/





