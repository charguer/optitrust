/*@12_0*/ typedef struct {
  int x;
  int y;
} vect; /*12_0@*/

/*@14_0, 13_0*/ int f(int n) {
  /*@15_1, 11_0, 8_0, 2_0*/ for (int i = 0; i < 10; i++) {
    /*@15_0, 1_0*/ for (int j = 0; j < 12; j++) {
      i++;
      j++;
    }                 /*15_0, 1_0@*/
  }                   /*15_1, 11_0, 8_0, 2_0@*/
  /*@12_1*/ return 3; /*12_1@*/
} /*14_0, 13_0@*/

/*@14_1*/ int main() {
  /*@15_2, 12_2, 11_1, 10_0, 8_1, 7_0, 4_0, 2_1*/ for (int i = 0; i < 3; i++) {
    vect r = {/*@3_0*/ f(2) /*3_0@*/, /*@3_1*/ f(3) /*3_1@*/};
    const vect s = {/*@5_1*/ 3 /*5_1@*/, /*@5_2, 3_2*/ f(4) /*5_2, 3_2@*/};
  } /*15_2, 12_2, 11_1, 10_0, 8_1, 7_0, 4_0, 2_1@*/
} /*14_1@*/
