void simple(int* t) {
  int a = 2;
  t[0] = a;
  t[1] = a;
  t[2] = a;
}

void name_conflict_no_brace() {
  int a = 0;
  int a1 = 1;
}

void name_conflict_with_braces() {
  { int a = 0; }
  { int a = 1; }
}

void start_add(int a) {
  int b = a + 0;
  int b2 = a + 1;
}

void step() {
  int b = 0;
  int b3 = 2;
  int b4 = 4;
  int c = 0;
  int c5 = 3;
}
