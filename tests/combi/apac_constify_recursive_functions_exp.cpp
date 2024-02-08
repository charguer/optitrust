void f(int* a, const int b) {
  if (*a > 256) return;
  *a = *a + 1;
  f(a, b);
}
