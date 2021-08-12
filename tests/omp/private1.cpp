#include <stdio.h>
#include <assert.h>

int main()
{
  int i, j;
  int *ptr_i, *ptr_j;

  i = 1;
  j = 2;

  ptr_i = &i;
  ptr_j = &j;

  {
    i = 3;
    j = j + 2;
    // assert (*ptr_i == 1 && *ptr_j == 2); // Gives an error when parsing with clang
  }

  // assert(i == 1 && j == 2); // Same error here

  return 0;
}