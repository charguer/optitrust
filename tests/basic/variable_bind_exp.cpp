typedef struct {
  int x[5];
} int_5;

int_5 test() {
  const int a[5] = {0, 1, 2, 3, 4};
  return (int_5){a};
}

void pointer_arg(int* a, int b) { *a = b; }

int main() {
  {
    int b[5] = {0, 1, 2, 3, 4};
    int arr[5] = b;
    auto a0 = arr[0];
    int r = a0 + 2;
  }
  {
    int* arr;
    auto a0 = arr[0];
    int r = a0 + 2;
  }
  int x;
  int y;
  int* z = &x;
  pointer_arg(z, y);
  return 0;
}
