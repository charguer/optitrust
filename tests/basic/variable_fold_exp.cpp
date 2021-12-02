int f(int a, int b) { return (a * b); }

int main() {
  int const x = 1;
  int const y = 2;
  int const s1 = (x * y);
  int const r1 = f(s1, (2 * s1));
}
