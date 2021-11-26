int f(int x) {
  int a = (x + x);
  return (a + a);
}

int g(int x) {
  if ((x > 0))
    return 1;
  else
    return 2;
}

int h(int x) {
  if ((x > 0))
    return 1;
  return 2;
}

void m(int *p) { (*p)++; }

int k(int a, int b) { return (a + b); }

int main() {
  int x = 3;
  int y;
  /*@bodyf*/ {
    int a = (x + x);
    y = (a + a);
  } /*bodyf@*/
  int z;
  /*@bodyg*/ {
    if ((x > 0))
      z = 1;
    else
      z = 2;
  } /*bodyg@*/
  int u;
  /*@bodyh*/ {
    if ((x > 0)) {
      u = 1;
      goto exit_body;
    }
    u = 2;
  } /*bodyh@*/
exit_body:;
  int *q = new int;
  /*@bodym*/ { (*q)++; } /*bodym@*/
  int result;
  /*@bodyk*/ { result = (result + 4); } /*bodyk@*/
  return 0;
}
