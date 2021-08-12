#include <stdio.h>
#include <stdlib.h>

float read_next( ) {
  float * tmp;
  float return_val;

  {
    tmp = (float *) malloc(sizeof(float));
  }  /* copies the pointer only */

  {
    scanf("%f", tmp);
  }

  return_val = *tmp;

  {
    free(tmp);
  }

  return return_val;
}

