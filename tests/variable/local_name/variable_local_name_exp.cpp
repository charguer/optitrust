typedef int T;

int main() {
  T a;
  for (int k = 6; k < 50; k++) {
    for (int j = 0; j < 10; j++) {
      /*@mymark*/ /*no-brace*/ {
        T x = a;
        for (int i = 0; i < j; i++) {
          x++;
        }
        a = x;
      } /*mymark@*/
    }
  }
  int y = 0;
  return 0;
}
