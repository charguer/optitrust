typedef struct {
  int x[5];
} int_5;

int_5 test() {
  const int[5] a = {0, 1, 2, 3, 4};
  return {a};
}

int main() {
  int b[5] = {0, 1, 2, 3, 4};
  int x[5] = b;
  return 0;
}
