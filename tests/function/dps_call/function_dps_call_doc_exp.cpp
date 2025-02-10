int f(int);

void f_dps(int, int*);

int main() {
  int r;
  int x = 10;
  f_dps(x, &r);
  return 0;
}
