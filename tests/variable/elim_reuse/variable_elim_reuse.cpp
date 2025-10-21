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

  int c = b;
  c++;
  b = c;
}

void copy_back() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  int c = a;
}

void bad_write_after_copy_back() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  b = 3;
}

void bad_read_after_copy_back() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = 3;
  int c = b;
}

// currently, this should fail, but it could be possible to change the transformation to remove each of these copy-back instructions
// the current workaround would be to call Sequence_basic.delete to remove the excess copy-back instructions
void mutltiple_copy_backs() {
  __pure();
  int a = 0;
  int b = a;
  b++;
  a = b;
  a = b;
}

void resources_not_available() {
  int a = 0;
  {
    int x = a;
  }
}
