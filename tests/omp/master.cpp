#include <stdio.h>

float average(float,float,float);

void master_example( float* x, float* xold, int n, float tol )
{
  int c, i, toobig;
  float error, y;
  c = 0;
  {
    do{
      for(i = 1; i < n-1; ++i){
        xold[i] = x[i];
      }
      {
        toobig = 0;
      }
      for(i = 1; i < n-1; ++i){
        y = x[i];
        x[i] = average( xold[i-1], x[i], xold[i+1] );
        error = y - x[i];
        if( error > tol || error < -tol ) ++toobig;
      }
      {
        ++c;
        printf( "iteration %d, toobig=%d\n", c, toobig );
      }
    }while( toobig > 0 );
  }
}
