int main() {
  int* t;
  int a = 2;
  {
    int a = 0 * 2;
    t[0] = a;
  }
  {
    int a = 1 * 2;
    t[1] = a;
  }
  {
    int a = 2 * 2;
    t[2] = a;
  }
  return 0;
}

void name_conflict_1() {
  { int a = 0; }
  { int a = 1; }
}

void name_conflict_2() {
  for (int k = 0; k < 2; k++) {
    int a = k;
  }
}
