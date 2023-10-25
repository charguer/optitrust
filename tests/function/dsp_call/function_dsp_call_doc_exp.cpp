int f(int x);

void f_dsp(int x, int* r);

int main() {
  int r;
  int x = 10;
  f_dsp(x, &r);
  return 0;
}
