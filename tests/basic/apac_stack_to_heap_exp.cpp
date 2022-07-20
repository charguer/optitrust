int main() {
  int i;
  int* a = new int(1);
  int* b = new int(i);
  const int* const c = new const int(1);
  const int* const d = new const int(i);
  int* e = new int(i);
  const int* const f = new const int(1);
  const int* const g = new const int(i);
  const int* h = new const int[5]{1, 2, 3, 4, 5};
  int* j = new int[5]{1, 2, 3, 4, 5};
}
