
int immutable_stack_var() {
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









