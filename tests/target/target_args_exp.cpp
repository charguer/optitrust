/*@11_0*/ int f(int x, int y) /*@5_0*/ { return x + y; } /*5_0@*/ /*11_0@*/

/*@11_1*/ float f1(int x, float y) /*@5_1*/ {
  return (float)x + y;
} /*5_1@*/ /*11_1@*/

int main() /*@9_4, 6_4, 5_6, 1_0*/ {
  /*@9_0, 8_0, 7_0, 6_0, 5_2, 3_0, 2_0*/ {
    const int x = 1;
    const int y = 2;
  } /*9_0, 8_0, 7_0, 6_0, 5_2, 3_0, 2_0@*/
  /*@9_1, 8_1, 7_1, 6_1, 5_3, 3_1, 2_1*/ {
    int x = 1;
    int y = 2;
  } /*9_1, 8_1, 7_1, 6_1, 5_3, 3_1, 2_1@*/
  /*@10_0, 9_2, 8_2, 7_2, 6_2, 5_4, 4_0*/ {
    int x = 1;
    int y = 2;
    int z = 3;
  } /*10_0, 9_2, 8_2, 7_2, 6_2, 5_4, 4_0@*/
  /*@9_3, 6_3, 5_5*/ {
    int x = 1;
    int y = 2;
    x++;
  } /*9_3, 6_3, 5_5@*/
  int a;
  int x = 5;
  a = /*@13_0*/ f(1, x) /*13_0@*/;
  int b = (int)/*@13_1, 12_0*/ f1(1, (float)2.) /*13_1, 12_0@*/;
  int c;
  c = f(a, a);
  c = /*@13_2, 12_1*/ f(1, 2) /*13_2, 12_1@*/;
} /*9_4, 6_4, 5_6, 1_0@*/
