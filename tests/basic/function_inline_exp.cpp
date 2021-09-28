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
  int x = 3;
  int y;
bodyf : {
  int a = (x + x);
  y = (a + a);
}

  int z;
bodyg : {
  if ((x > 0))
    z = 1;
  else
    z = 2;
}

  int u;
bodyh : {
  if ((x > 0)) {
    u = 1;
    goto _exit_body;
  }
  u = 2;
}
__exit_body:

  int *q = new int;
bodym : { (*q)++; }

  return 0;
}