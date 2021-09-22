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

int main() {

bodyf : {
  int a = (x + x);
  y = (a + a);
}
  int y;

bodyg : {
  if ((x > 0))
    z = 1;
  else
    z = 2;
}
  int z;

bodyh : {
  if ((x > 0)) {
    u = 1;
    goto _exit_body;
  }
  u = 2;
}
  int u;
  int x = 3;

bodym : { (*q)++; }
  int *q = new int;
  return 0;
}