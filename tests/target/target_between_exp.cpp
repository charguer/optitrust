typedef struct {
  int x;
  int y;
} vect;

int main() {
  for (int i = 0; i < 10; i++) {
    int r1 = 1;
    /*@[__17, __1]@*/
    /*@[__23]@*/
    int r2 = 2;
    if (true) {
      int m1 = 1;
      /*@[__29, __25, __15]@*/
      int m2 = 2;
      /*@[__13, __7]@*/
    } else {
      int s1 = 1;
      int s2 = 2;
    }
    /*@[__11]@*/
  }
}
