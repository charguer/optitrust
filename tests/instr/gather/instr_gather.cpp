#include <optitrust.h>

void foo() {
  __pure();

  int x = 3;
  for (int i = 0; i < 10; i++)
  {
    // FIXME: #unop-inc-dec x++;
    x += 1;
  }

  int y = 1;
  for (int j = 0; j < 10; j++)
  {
    // FIXME: y++;
    y += 1;
  }
  int z = 5;
  for (int k = 0; k < 10; k++)
  {
    // FIXME: z++;
    z += 1;
  }
  int t = 2;
}

void with_deps() {
  __pure();

  int a = 0;
  int b = 0;
  int x = 0;
  int y = 0;
  int u = 0;
  int v = 0;

  a += 1;
  x += a;
  u += x;
  v += 1;
  y += b;
  b += 1;
}
