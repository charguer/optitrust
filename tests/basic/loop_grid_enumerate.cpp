#include <stdio.h>

const int X = 5, Y = 6, Z = 7;
const int nbCells = X * Y * Z;

int main() {
  int total = 0;
  for (int idCell = 0; idCell < nbCells; idCell++) {
    total += idCell;
  }
  printf("%d\n", total);
  return 0;
}

