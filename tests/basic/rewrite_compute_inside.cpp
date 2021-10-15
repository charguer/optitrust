int main() {

  block:{
    int x;
    x = 6 + 2;
    x = 6 * 2;
    x = 9 / 2;
    x = 10 % 6;
    x = 2 + 2 + 2 + 2;
    x = 2 + 2 - 2 + 2;
  }
  float y;
  y = 1.0 + 2.5;
  x = 2.5 * 2.5;
  x = 9.0 / 2.1;
  x = 2.5 + 2.5 + 2.5 + 2.5;
  x = 2.5 + 2.5 - 2.5 + 2.5;
  bool a, b, c;
  c = true || a;
  c = false || a;
  c = a || true;
  c = a || false;
  c = true && a;
  c = false && a;
  c = a && true;
  c = a && false;
  c = a || (b && true) || true;
  c = ! true;
  c = ! false;

  a = a (b && true || false);
  return 0;
}