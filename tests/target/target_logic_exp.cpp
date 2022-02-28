typedef struct {
  int x;
  int y;
} vect;

int f(int n) {
  /*@0, 1, 0, 1*/ for (int i = 0; i < 10; i++) {
    /*@0*/ for (int j = 0; j < 12; j++) {
      /*@0*/ for (int k = 0; k < 13; k++) {
        /*@0*/ i++; /*0@*/
        /*@1*/ j++; /*1@*/
        k++;
        /*@0*/ i = k + j; /*0@*/
      }                   /*0@*/
    }                     /*0@*/
  }                       /*0, 1, 0, 1@*/
  return 3;
}

int main() {
  /*@1, 2, 1, 2*/ for (int i = 0; i < 3; i++) {
    vect r = {f(2), f(3)};
  } /*1, 2, 1, 2@*/
}
