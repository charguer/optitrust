void test_const_const() {
  const int y = 5;
  int z = y + y;
}

void test_nonconst_const() {
  int y = 5;
  y = 6;
  y = 7;
  int z = y + y;
}

void test_const_nonconst() {
  const int y = 5;
  int z = y + y;
}

void test_nonconst_nonconst() {
  int y = 5;
  y = 6;
  y = 7;
  int z = y + y;
}

typedef struct {
  int x;
  int y;
} vect;

void test_const_const_vect() {
  const vect b = {6, 7};
  int c = b.x;
  int d = b.y;
}

void test_nonconst_const_vect() {
  vect b;
  b.x = 6;
  b.x = 7;
  int c = b.x;
  int d = b.y;
}

void test_const_nonconst_vect() {
  const vect b = {6, 7};
  int c = b.x;
  int d = b.y;
}

void test_nonconst_nonconst_vect() {
  vect b;
  b.x = 6;
  b.x = 7;
  int c = b.x;
  int d = b.y;
}
