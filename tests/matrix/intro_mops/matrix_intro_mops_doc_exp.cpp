#include <stdlib.h>

int main() {
  const int nbCells = 10;
  double* deposit = (double*)MALLOC1((long unsigned int)nbCells, sizeof(int));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[MINDEX1(nbCells, idCell)] += 1.;
  }
  return 0;
}
