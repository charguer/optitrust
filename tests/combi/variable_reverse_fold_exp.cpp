void f(int a) { a = (a + 1); }

int main(int argc, char const *argv[]) {
  int y = 5;
  y = 6;
  y = 7;
  f(y);
  for (int i = 0; (i < 10); i++) {
    y++;
  }
  return 0;
}
