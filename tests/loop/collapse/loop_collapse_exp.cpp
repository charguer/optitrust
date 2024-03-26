#include <optitrust.h>



  void consts (  )  {
  int x;
  for ( int ij = 0; ij < 5 * 5; ij++ ) { x += ij / 5 + ij % 5; }
}

  void from_zero ( int n, int m )  {
  int x;
  for ( int ij = 0; ij < n * m; ij++ ) { x += ij / m + ij % m; }
}

  void incr ( int ai, int bi, int aj, int bj )  {
  int x;
  for ( int ij = 0; ij < ( bi - ai ) * ( bj - aj ); ij++ ) {
    x += ai + ij / ( bj - aj ) + aj + ij % ( bj - aj );
  }
}
