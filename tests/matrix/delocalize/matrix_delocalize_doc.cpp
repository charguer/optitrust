#include <stdlib.h>
#include <optitrust.h>

int nbParticlesInCell(int idCell);
int computeParticleDestination(int idCell, int i);

typedef struct { int v[8]; } res;
res indicesOfCorners(int idCell);

const int nbCorners = 8;
const double particleCharge = 1.0;
const int nbCells = 10;
const int nbSteps = 100;

int demo() {
  double* deposit = (double*) MALLOC1(nbCells, sizeof(int));
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    for (int idCell = 0; idCell < nbCells; idCell++) {
      const int nb = nbParticlesInCell(idCell);
      for (int i = 0; i < nb; i++) {
        const int idCell2 = computeParticleDestination(idCell, i);
        for (int k = 0; k < 8; k++) {
          deposit[MINDEX1(nbCells, indicesOfCorners(idCell2).v[k])] += 1.0;
        }
      }
    }
  }
  return 0;
}
