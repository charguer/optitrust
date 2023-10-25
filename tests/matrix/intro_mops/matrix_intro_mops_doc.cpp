
#include <stdlib.h>
int main() {
  const int nbCells = 10;
  double* deposit = (double*) malloc (nbCells * sizeof(int));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[idCell] += 1.0;
  }
  return 0;
}
