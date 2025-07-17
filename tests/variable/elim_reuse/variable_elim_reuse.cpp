#include <optitrust.h>

void f() {
  __pure();

  int a = 0;
  int b = 0;
  {
    int x = a;
    x += 2;
    int y = b;
    y += 3;
  }
  b++;

  int c = b;
  c++;
  b = c;
}

// valid transformation with copy-back
void g() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  int c = a;
}

// we should not be allowed to write into y (here b) after the copy-back
void h() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  b = 3;
}

// a very artificial example which shows that we sometimes want to prevent reading from y (in this case b)
// after the transformation (the transformation would be valid if we did not have the `a = 3;` instruction)
void i() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = 3;
  int c = b;
}

void resources_not_available() {
  int a = 0;
  {
    int x = a;
  }
}
