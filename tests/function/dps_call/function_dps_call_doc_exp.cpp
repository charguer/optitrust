int f(int x);

void f_dps(int x, int* r);

int main() {
  int r;
  int x = 10;
  f_dps(x, &r);
  return 0;
}
