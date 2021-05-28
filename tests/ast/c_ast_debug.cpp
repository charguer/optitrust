int immutable_stack_ptr() {
  int x = 3;
  int y = 4;
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
  const int * const t[2] = {&x,&y}; (* TODO: ask Anton *)
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

/*  ENCODED VERSION
int mutable_stack_var() {
  const int* r = new 3;
  set(r, get(r) + 1);
  operator++(r);
  return get(r);
}
*/









