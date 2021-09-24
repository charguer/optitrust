int main() {
  int x = 3;
  int y;
  int a_1 = (x + x);
  y = (a_1 + a_1);
  int z;
  if ((x > 0))
    z = 1;
  else
    z = 2;
  int u;
  if ((x > 0)) {
    u = 1;
    goto __exit_body;
  }
  u = 2;
__exit_body:
  int *q = new int;
  (*q)++;
  return 0;
}