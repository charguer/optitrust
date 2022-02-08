
typedef struct {
  int t[2];
} foo;

int main() {
  const foo f = { { 0, 1 } };
  const int * const u = f.t;
  int const a = u[0];
  int b = a;
}



/* TODO: move this function to another unit test
void test_loop() {

  int k;
  int a = 0;
  for (k = 0; k < 10; k++) {
    a += k;
  }
  for (int i = 0; i <= 10; i++) {
      i++;
  }
  for (int i = 0; i < 10; i++) {
      i++;
  }
  for (int j = 10; j >= 0; j--) {
      j++;
  }
  for (int j = 10; j > 0; j--) {
      j++;
  }

}
*/