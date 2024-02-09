/*@ 4_0__begin @*/

  int main (  )  {
  /*@ 4_1__begin, 1_0__begin @*/
  int a = 0;
  /*@ 1_0__end @*/
  int b = 1;
  for ( int i = 0; i < 100; ++i ) {
    /*@ 4_2__begin, 3_0__begin, 2_1__begin @*/
    a += 1;
    a += 2;
    /*@ 3_1__begin, 3_0__end, 2_2__begin, 2_1__end @*/
    b += 1;
    b += 2;
    /*@ 4_2__end, 3_1__end, 2_2__end @*/
  }
  /*@ 2_0__begin @*/
  a += 1;
  a += 2;
  /*@ 4_1__end, 2_0__end @*/
}

/*@ 4_0__end @*/
