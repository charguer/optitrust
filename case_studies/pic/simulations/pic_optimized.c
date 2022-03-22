#include <omp.h> // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num

#include <stdlib.h>

#include <stdio.h>

#include <math.h>

#include "mymacros.h"

#include "mymacros.h"

#include "optitrust.h"

#include <stdio.h>

#include "pic_demo_aux.h"

typedef struct {
  double x;
  double y;
  double z;
} vect;

typedef struct {
  double posX;
  double posY;
  double posZ;
  double speedX;
  double speedY;
  double speedZ;
  int id;
} particle;

vect vect_add(vect v1, vect v2);

vect vect_mul(double d, vect v);

vect vect_add(vect v1, vect v2) {
  return (vect){v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

vect vect_mul(double d, vect v) { return (vect){d * v.x, d * v.y, d * v.z}; }



typedef struct chunk {
  struct chunk *next;
  int size;
  double itemsPosX[CHUNK_SIZE];
  double itemsPosY[CHUNK_SIZE];
  double itemsPosZ[CHUNK_SIZE];
  double itemsSpeedX[CHUNK_SIZE];
  double itemsSpeedY[CHUNK_SIZE];
  double itemsSpeedZ[CHUNK_SIZE];
  int itemsId[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;

typedef struct bag_iter {
  bool destructive;
  chunk *iter_chunk;
  int size;
  int index;
} bag_iter;

void bag_init(bag *b);

void bag_append(bag *b, bag *other);

void bag_nullify(bag *b);

int bag_size(bag *b);

void bag_add_front_chunk(bag *b);

void bag_push_concurrent(bag *b, particle p);

void bag_push_serial(bag *b, particle p);

void bag_push(bag *b, particle p);

void bag_swap(bag *b1, bag *b2);

void bag_push_initial(bag *b, particle p);

void bag_init_initial(bag *b);

void bag_free_initial(bag *b);

chunk *chunk_next(chunk *c, bool destructive);

chunk *atomic_read_chunk(chunk **p);

void atomic_write_chunk(chunk **p, chunk *v);

int atomic_increment(int *size);

chunk *chunk_alloc() {
  chunk *c;
  if (posix_memalign((void **)&c, 64, sizeof(chunk))) {
    fprintf(stderr, "chunk_alloc: posix_memalign.\n");
    exit(1);
  }
  return c;
}

void chunk_free(chunk *c) { free(c); }

void bag_init(bag *b) {
  chunk *c = chunk_alloc();
  c->size = 0;
  c->next = NULL;
  b->front = c;
  b->back = c;
}

void bag_append(bag *b, bag *other) {
  if (other->front) {
    b->back->next = other->front;
    b->back = other->back;
    bag_init(other);
  }
}

void bag_nullify(bag *b) {
  b->front = NULL;
  b->back = NULL;
}

int bag_size(bag *b) {
  chunk *c = b->front;
  int size = 0;
  while (c) {
    size += c->size;
    c = c->next;
  }
  return size;
}

void bag_add_front_chunk_serial(bag *b) {
  chunk *c = chunk_alloc();
  c->size = 0;
  c->next = b->front;
  b->front = c;
}

void bag_add_front_chunk_concurrent(bag *b) {
  chunk *c = chunk_alloc();
  c->size = 0;
  c->next = b->front;
  atomic_write_chunk(&b->front, c);
}

void bag_push_concurrent(bag *b, particle p) {
  chunk *c;
  int index;
  while (true) {
    c = b->front;
    index = atomic_increment(&c->size);
    if (index < CHUNK_SIZE) {
      c->itemsPosX[index] = p.posX;
      c->itemsPosY[index] = p.posY;
      c->itemsPosZ[index] = p.posZ;
      c->itemsSpeedX[index] = p.speedX;
      c->itemsSpeedY[index] = p.speedY;
      c->itemsSpeedZ[index] = p.speedZ;
      c->itemsId[index] = p.id;
      if (index == CHUNK_SIZE - 1) {
        bag_add_front_chunk_concurrent(b);
      }
      return;
    } else {
      c->size = CHUNK_SIZE;
      while (atomic_read_chunk(&b->front) == c) {
      }
    }
  }
}

void bag_push_serial(bag *b, particle p) {
  chunk *c = b->front;
  int index = c->size;
  c->size++;
  c->itemsPosX[index] = p.posX;
  c->itemsPosY[index] = p.posY;
  c->itemsPosZ[index] = p.posZ;
  c->itemsSpeedX[index] = p.speedX;
  c->itemsSpeedY[index] = p.speedY;
  c->itemsSpeedZ[index] = p.speedZ;
  c->itemsId[index] = p.id;
  if (index == CHUNK_SIZE - 1) {
    bag_add_front_chunk_serial(b);
  }
}

void bag_push(bag *b, particle p) { bag_push_serial(b, p); }

void bag_swap(bag *b1, bag *b2) {
  bag temp = *b1;
  *b1 = *b2;
  *b2 = temp;
}

chunk *chunk_next(chunk *c, bool destructive) {
  chunk *cnext = c->next;
  if (destructive) {
    chunk_free(c);
  }
  return cnext;
}

void bag_push_initial(bag *b, particle p) { bag_push_serial(b, p); }

void bag_init_initial(bag *b) { bag_init(b); }

void bag_free_initial(bag *b) {
  chunk *c = b->front;
  while (c != NULL) {
    c = chunk_next(c, true);
  }
}

double areaX;

double areaY;

double areaZ;

int gridX;

int gridY;

int gridZ;

int nbCells;

double cellX;

double cellY;

double cellZ;

int nbSteps;

double stepDuration;

double averageChargeDensity;

double averageMassDensity;

double particleCharge;

double particleMass;

int nbParticles;

char sim_distrib;

double *params;

double *speed_params;

int seed;

double ***rho;

double ***Ex;

double ***Ey;

double ***Ez;

vect *field;

double *deposit;

bag *bagsCur;

bag *bagsNext;

void addParticle(int idParticle, double x, double y, double z, double vx,
                 double vy, double vz);

int cellOfCoord(int i, int j, int k);

void allocateStructures();

void allocateStructuresForPoissonSolver();

void deallocateStructures();

void deallocateStructuresForPoissonSolver();

int int_of_double(double a) { return (int)a - (a < 0.); }

int wrap(int gridSize, int a) { return (a % gridSize + gridSize) % gridSize; }

int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX, gridY, gridZ, i, j, k);
}

int idCellOfPos(vect pos) {
  const int iX = int_of_double(pos.x / cellX);
  const int iY = int_of_double(pos.y / cellY);
  const int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
}

double fwrap(double gridWidth, double x) {
  const double r = fmod(x, gridWidth);
  if (r >= 0) {
    return r;
  } else {
    return r + gridWidth;
  }
}

vect wrapArea(vect pos) {
  const double x = fwrap(areaX, pos.x);
  const double y = fwrap(areaY, pos.y);
  const double z = fwrap(areaZ, pos.z);
  return (vect){x, y, z};
}

double relativePosX(double x) {
  const int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}

double relativePosY(double y) {
  const int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}

double relativePosZ(double z) {
  const int iZ = int_of_double(z / cellZ);
  return (z - iZ * cellZ) / cellZ;
}

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

coord coordOfCell(int idCell) {
  const int iZ = idCell % gridZ;
  const int iXY = idCell / gridZ;
  const int iY = iXY % gridY;
  const int iX = iXY / gridY;
  return (coord){iX, iY, iZ};
}

typedef struct { int v[8]; } int_nbCorners;

typedef struct { double v[8]; } double_nbCorners;

typedef struct { vect v[8]; } vect_nbCorners;

int_nbCorners indicesOfCorners(int idCell) {
  const coord coord = coordOfCell(idCell);
  const int x = coord.iX;
  const int y = coord.iY;
  const int z = coord.iZ;
  const int x2 = wrap(gridX, x + 1);
  const int y2 = wrap(gridY, y + 1);
  const int z2 = wrap(gridZ, z + 1);
  return (int_nbCorners){{cellOfCoord(x, y, z), cellOfCoord(x, y, z2),
                          cellOfCoord(x, y2, z), cellOfCoord(x, y2, z2),
                          cellOfCoord(x2, y, z), cellOfCoord(x2, y, z2),
                          cellOfCoord(x2, y2, z), cellOfCoord(x2, y2, z2)}};
}

vect_nbCorners getFieldAtCorners(int idCell, vect *field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int k = 0; k < 8; k++) {
    res.v[k] = field[indices.v[k]];
  }
  return res;
}

void accumulateChargeAtCorners(double *deposit, int idCell,
                               double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < 8; k++) {
    deposit[indices.v[k]] += charges.v[k];
  }
}

const double coefX[8] = {1., 1., 1., 1., 0., 0., 0., 0.};

const double signX[8] = {-1., -1., -1., -1., 1., 1., 1., 1.};

const double coefY[8] = {1., 1., 0., 0., 1., 1., 0., 0.};

const double signY[8] = {-1., -1., 1., 1., -1., -1., 1., 1.};

const double coefZ[8] = {1., 0., 1., 0., 1., 0., 1., 0.};

const double signZ[8] = {-1., 1., -1., 1., -1., 1., -1., 1.};

double_nbCorners cornerInterpolationCoeff(vect pos) {
  const int iX = int_of_double(pos.x / cellX);
  const double rX = (pos.x - iX * cellX) / cellX;
  const int iY = int_of_double(pos.y / cellY);
  const double rY = (pos.y - iY * cellY) / cellY;
  const int iZ = int_of_double(pos.z / cellZ);
  const double rZ = (pos.z - iZ * cellZ) / cellZ;
  double_nbCorners r;
  for (int k = 0; k < 8; k++) {
    r.v[k] = (coefX[k] + signX[k] * rX) * (coefY[k] + signY[k] * rY) *
             (coefZ[k] + signZ[k] * rZ);
  }
  return r;
}

void computeRhoFromDeposit() {
  const double factor = averageChargeDensity * nbCells / nbParticles;
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        rho[i][j][k] = factor * deposit[cellOfCoord(i, j, k)];
      }
    }
  }
}

void resetDeposit() {
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[idCell] = 0;
  }
}

void updateFieldUsingDeposit() {
  computeRhoFromDeposit();
  computeFieldFromRho();
  resetDeposit();
}

void allocateStructures() {
  allocateStructuresForPoissonSolver();
  deposit = (double *)malloc(nbCells * sizeof(double));
  field = (vect *)malloc(nbCells * sizeof(vect));
  bagsCur = (bag *)malloc(nbCells * sizeof(bag));
  bagsNext = (bag *)malloc(nbCells * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init_initial(&bagsCur[idCell]);
    bag_init_initial(&bagsNext[idCell]);
  }
}

void deallocateStructures() {
  deallocateStructuresForPoissonSolver();
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free_initial(&bagsCur[idCell]);
    bag_free_initial(&bagsNext[idCell]);
  }
  free(bagsCur);
  free(bagsNext);
  free(field);
}

void computeConstants() {
  nbCells = gridX * gridY * gridZ;
  cellX = areaX / gridX;
  cellY = areaY / gridY;
  cellZ = areaZ / gridZ;
  const double cellVolume = cellX * cellY * cellZ;
  const double totalVolume = nbCells * cellVolume;
  const double totalCharge = averageChargeDensity * totalVolume;
  const double totalMass = averageMassDensity * totalVolume;
  particleCharge = totalCharge / nbParticles;
  particleMass = totalMass / nbParticles;
}

void addParticle(int idParticle, double x, double y, double z, double vx,
                 double vy, double vz) {
  vect pos = {x, y, z};
  vect speed = {vx, vy, vz};
  const int idCell = idCellOfPos(pos);
  const coord co = coordOfCell(idCell);
  particle p;
  p.posX = pos.x / cellX - co.iX;
  p.posY = pos.y / cellY - co.iY;
  p.posZ = pos.z / cellZ - co.iZ;
  p.speedX = speed.x / (cellX / stepDuration);
  p.speedY = speed.y / (cellY / stepDuration);
  p.speedZ = speed.z / (cellZ / stepDuration);
  p.id = idParticle;
  bag_push_initial(&bagsCur[idCell], p);
  double_nbCorners contribs = cornerInterpolationCoeff(pos);
  accumulateChargeAtCorners(deposit, idCell, contribs);
}

void stepLeapFrog() {
  updateFieldUsingDeposit();
  const double negHalfStepDuration = -0.5 * stepDuration;
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag *b = &bagsCur[idCell];
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    for (chunk *c = b->front; c != NULL; c = chunk_next(c, false)) {
      const int nb = c->size;
      for (int i = 0; i < nb; i++) {
        const double rX0 = c->itemsPosX[i];
        const double rY0 = c->itemsPosY[i];
        const double rZ0 = c->itemsPosZ[i];
        double_nbCorners coeffs;
        for (int k = 0; k < 8; k++) {
          coeffs.v[k] = (coefX[k] + signX[k] * rX0) *
                        (coefY[k] + signY[k] * rY0) *
                        (coefZ[k] + signZ[k] * rZ0);
        }
        vect fieldAtPos = {0., 0., 0.};
        fieldAtPos.x = fieldAtPos.x + coeffs.v[0] * field_at_corners.v[0].x +
                       coeffs.v[1] * field_at_corners.v[1].x +
                       coeffs.v[2] * field_at_corners.v[2].x +
                       coeffs.v[3] * field_at_corners.v[3].x +
                       coeffs.v[4] * field_at_corners.v[4].x +
                       coeffs.v[5] * field_at_corners.v[5].x +
                       coeffs.v[6] * field_at_corners.v[6].x +
                       coeffs.v[7] * field_at_corners.v[7].x;
        fieldAtPos.y = fieldAtPos.y + coeffs.v[0] * field_at_corners.v[0].y +
                       coeffs.v[1] * field_at_corners.v[1].y +
                       coeffs.v[2] * field_at_corners.v[2].y +
                       coeffs.v[3] * field_at_corners.v[3].y +
                       coeffs.v[4] * field_at_corners.v[4].y +
                       coeffs.v[5] * field_at_corners.v[5].y +
                       coeffs.v[6] * field_at_corners.v[6].y +
                       coeffs.v[7] * field_at_corners.v[7].y;
        fieldAtPos.z = fieldAtPos.z + coeffs.v[0] * field_at_corners.v[0].z +
                       coeffs.v[1] * field_at_corners.v[1].z +
                       coeffs.v[2] * field_at_corners.v[2].z +
                       coeffs.v[3] * field_at_corners.v[3].z +
                       coeffs.v[4] * field_at_corners.v[4].z +
                       coeffs.v[5] * field_at_corners.v[5].z +
                       coeffs.v[6] * field_at_corners.v[6].z +
                       coeffs.v[7] * field_at_corners.v[7].z;
        c->itemsSpeedX[i] = c->itemsSpeedX[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass * fieldAtPos.x * stepDuration /
                                cellX;
        c->itemsSpeedY[i] = c->itemsSpeedY[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass * fieldAtPos.y * stepDuration /
                                cellY;
        c->itemsSpeedZ[i] = c->itemsSpeedZ[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass * fieldAtPos.z * stepDuration /
                                cellZ;
      }
    }
  }
}

void step() {
  const double factorC =
      particleCharge * (stepDuration * stepDuration) / particleMass;
  const double factorX =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellX;
  const double factorY =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellY;
  const double factorZ =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellZ;
<<<<<<< HEAD
  double *depositCorners = (double *)MMALLOC2(nbCells, 8, sizeof(double));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int k = 0; k < 8; k++) {
      depositCorners[mybij(nbCells, 8, idCell, k)] = 0.;
    }
  }
  double *depositThreadCorners =
      (double *)MMALLOC3(nbCells, 8, nbThreads, sizeof(double));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      for (int idThread = 0; idThread < nbThreads; idThread++) {
        depositThreadCorners[MINDEX3(nbCells, 8, nbThreads, idCell, idCorner,
                                     idThread)] = 0.;
      }
    }
  }
core:
  for (int cX = 0; cX < block; cX++) {
    for (int cY = 0; cY < block; cY++) {
      for (int cZ = 0; cZ < block; cZ++) {
        for (int bX = cX * 2; bX < gridX; bX += block * 2) {
          for (int bY = cY * 2; bY < gridY; bY += block * 2) {
            for (int bZ = cZ * 2; bZ < gridZ; bZ += block * 2) {
              const int idThread = omp_get_thread_num();
              for (int iX = bX; iX < bX + 2; iX++) {
                for (int iY = bY; iY < bY + 2; iY++) {
                  for (int iZ = bZ; iZ < bZ + 2; iZ++) {
                    const int idCell = (iX * gridY + iY) * gridZ + iZ;
                    const int_nbCorners indices = indicesOfCorners(idCell);
                    vect_nbCorners res;
                    for (int k = 0; k < 8; k++) {
                      res.v[k].x = field[indices.v[k]].x * factorX;
                      res.v[k].y = field[indices.v[k]].y * factorY;
                      res.v[k].z = field[indices.v[k]].z * factorZ;
                    }
                    bag *b = &bagsCur[idCell];
                    bag_iter bag_it;
                    for (chunk *c = b->front; c != NULL;
                         c = chunk_next(c, false)) {
                      const int nb = c->size;
                      for (int i = 0; i < nb; i++) {
                        const double rX0 = c->itemsPosX[i];
                        const double rY0 = c->itemsPosY[i];
                        const double rZ0 = c->itemsPosZ[i];
                        double_nbCorners coeffs;
                        for (int k = 0; k < 8; k++) {
                          coeffs.v[k] = (coefX[k] + signX[k] * rX0) *
                                        (coefY[k] + signY[k] * rY0) *
                                        (coefZ[k] + signZ[k] * rZ0);
                        }
                        double fieldAtPosX = 0.;
                        double fieldAtPosY = 0.;
                        double fieldAtPosZ = 0.;
                        fieldAtPosX = fieldAtPosX + coeffs.v[0] * res.v[0].x +
                                      coeffs.v[1] * res.v[1].x +
                                      coeffs.v[2] * res.v[2].x +
                                      coeffs.v[3] * res.v[3].x +
                                      coeffs.v[4] * res.v[4].x +
                                      coeffs.v[5] * res.v[5].x +
                                      coeffs.v[6] * res.v[6].x +
                                      coeffs.v[7] * res.v[7].x;
                        fieldAtPosY = fieldAtPosY + coeffs.v[0] * res.v[0].y +
                                      coeffs.v[1] * res.v[1].y +
                                      coeffs.v[2] * res.v[2].y +
                                      coeffs.v[3] * res.v[3].y +
                                      coeffs.v[4] * res.v[4].y +
                                      coeffs.v[5] * res.v[5].y +
                                      coeffs.v[6] * res.v[6].y +
                                      coeffs.v[7] * res.v[7].y;
                        fieldAtPosZ = fieldAtPosZ + coeffs.v[0] * res.v[0].z +
                                      coeffs.v[1] * res.v[1].z +
                                      coeffs.v[2] * res.v[2].z +
                                      coeffs.v[3] * res.v[3].z +
                                      coeffs.v[4] * res.v[4].z +
                                      coeffs.v[5] * res.v[5].z +
                                      coeffs.v[6] * res.v[6].z +
                                      coeffs.v[7] * res.v[7].z;
                        c->itemsSpeedX[i] = c->itemsSpeedX[i] + fieldAtPosX;
                        c->itemsSpeedY[i] = c->itemsSpeedY[i] + fieldAtPosY;
                        c->itemsSpeedZ[i] = c->itemsSpeedZ[i] + fieldAtPosZ;
                        const double pX =
                            c->itemsPosX[i] + iX + c->itemsSpeedX[i];
                        const double pY =
                            c->itemsPosY[i] + iY + c->itemsSpeedY[i];
                        const double pZ =
                            c->itemsPosZ[i] + iZ + c->itemsSpeedZ[i];
                        const double pX2 = fwrap(areaX, pX * cellX) / cellX;
                        const double pY2 = fwrap(areaY, pY * cellY) / cellY;
                        const double pZ2 = fwrap(areaZ, pZ * cellZ) / cellZ;
                        const int iX2 = int_of_double(pX2);
                        const int iY2 = int_of_double(pY2);
                        const int iZ2 = int_of_double(pZ2);
                        const int idCell2 = cellOfCoord(iX2, iY2, iZ2);
                        c->itemsPosX[i] = pX2 - iX2;
                        c->itemsPosY[i] = pY2 - iY2;
                        c->itemsPosZ[i] = pZ2 - iZ2;
                        const double rX1 = c->itemsPosX[i];
                        const double rY1 = c->itemsPosY[i];
                        const double rZ1 = c->itemsPosZ[i];
                        particle p2;
                        p2.posX = rX1;
                        p2.posY = rY1;
                        p2.posZ = rZ1;
                        p2.speedX = c->itemsSpeedX[i];
                        p2.speedY = c->itemsSpeedY[i];
                        p2.speedZ = c->itemsSpeedZ[i];
                        p2.id = c->itemsId[i];
                        bag_push(&bagsNext[MINDEX1(nbCells, idCell2)], p2);
                        double_nbCorners contribs;
                      charge:
                        for (int k = 0; k < 8; k++) {
                          depositThreadCorners[MINDEX3(nbCells, 8, nbThreads,
                                                       idCell2, k, idThread)] +=
                              (coefX[k] + signX[k] * rX1) *
                              (coefY[k] + signY[k] * rY1) *
                              (coefZ[k] + signZ[k] * rZ1);
                        }
                      }
                      bag_init_initial(b);
                    }
                  }
                }
              }
=======
  for (int iX = 0; iX < gridX; iX++) {
    for (int iY = 0; iY < gridY; iY++) {
      for (int iZ = 0; iZ < gridZ; iZ++) {
        const int idCell = (iX * gridY + iY) * gridZ + iZ;
        const int_nbCorners indices = indicesOfCorners(idCell);
        vect_nbCorners res;
        for (int k = 0; k < 8; k++) {
          res.v[k].x = field[indices.v[k]].x * factorX;
          res.v[k].y = field[indices.v[k]].y * factorY;
          res.v[k].z = field[indices.v[k]].z * factorZ;
        }
        bag *b = &bagsCur[idCell];
        for (chunk *c = b->front; c != NULL; c = chunk_next(c, true)) {
          const int nb = c->size;
          for (int i = 0; i < nb; i++) {
            const double rX0 = c->itemsPosX[i];
            const double rY0 = c->itemsPosY[i];
            const double rZ0 = c->itemsPosZ[i];
            double_nbCorners coeffs;
            for (int k = 0; k < 8; k++) {
              coeffs.v[k] = (coefX[k] + signX[k] * rX0) *
                            (coefY[k] + signY[k] * rY0) *
                            (coefZ[k] + signZ[k] * rZ0);
            }
            double fieldAtPosX = 0.;
            double fieldAtPosY = 0.;
            double fieldAtPosZ = 0.;
            fieldAtPosX = fieldAtPosX + coeffs.v[0] * res.v[0].x +
                          coeffs.v[1] * res.v[1].x + coeffs.v[2] * res.v[2].x +
                          coeffs.v[3] * res.v[3].x + coeffs.v[4] * res.v[4].x +
                          coeffs.v[5] * res.v[5].x + coeffs.v[6] * res.v[6].x +
                          coeffs.v[7] * res.v[7].x;
            fieldAtPosY = fieldAtPosY + coeffs.v[0] * res.v[0].y +
                          coeffs.v[1] * res.v[1].y + coeffs.v[2] * res.v[2].y +
                          coeffs.v[3] * res.v[3].y + coeffs.v[4] * res.v[4].y +
                          coeffs.v[5] * res.v[5].y + coeffs.v[6] * res.v[6].y +
                          coeffs.v[7] * res.v[7].y;
            fieldAtPosZ = fieldAtPosZ + coeffs.v[0] * res.v[0].z +
                          coeffs.v[1] * res.v[1].z + coeffs.v[2] * res.v[2].z +
                          coeffs.v[3] * res.v[3].z + coeffs.v[4] * res.v[4].z +
                          coeffs.v[5] * res.v[5].z + coeffs.v[6] * res.v[6].z +
                          coeffs.v[7] * res.v[7].z;
            c->itemsSpeedX[i] = c->itemsSpeedX[i] + fieldAtPosX;
            c->itemsSpeedY[i] = c->itemsSpeedY[i] + fieldAtPosY;
            c->itemsSpeedZ[i] = c->itemsSpeedZ[i] + fieldAtPosZ;
            const double pX = c->itemsPosX[i] + iX + c->itemsSpeedX[i];
            const double pY = c->itemsPosY[i] + iY + c->itemsSpeedY[i];
            const double pZ = c->itemsPosZ[i] + iZ + c->itemsSpeedZ[i];
            const double pX2 = fwrap(areaX, pX * cellX) / cellX;
            const double pY2 = fwrap(areaY, pY * cellY) / cellY;
            const double pZ2 = fwrap(areaZ, pZ * cellZ) / cellZ;
            const int iX2 = int_of_double(pX2);
            const int iY2 = int_of_double(pY2);
            const int iZ2 = int_of_double(pZ2);
            const int idCell2 = cellOfCoord(iX2, iY2, iZ2);
            c->itemsPosX[i] = pX2 - iX2;
            c->itemsPosY[i] = pY2 - iY2;
            c->itemsPosZ[i] = pZ2 - iZ2;
            const double rX1 = c->itemsPosX[i];
            const double rY1 = c->itemsPosY[i];
            const double rZ1 = c->itemsPosZ[i];
            particle p2;
            p2.posX = rX1;
            p2.posY = rY1;
            p2.posZ = rZ1;
            p2.speedX = c->itemsSpeedX[i];
            p2.speedY = c->itemsSpeedY[i];
            p2.speedZ = c->itemsSpeedZ[i];
            p2.id = c->itemsId[i];
            bag_push(&bagsNext[idCell2], p2);
            double_nbCorners contribs;
            const int_nbCorners indices = indicesOfCorners(idCell2);
            for (int k = 0; k < 8; k++) {
              deposit[indices.v[k]] += (coefX[k] + signX[k] * rX1) *
                                       (coefY[k] + signY[k] * rY1) *
                                       (coefZ[k] + signZ[k] * rZ1);
>>>>>>> 7b9ee395ff82ef98e922e2de5d8ba273a84b2303
            }
          }
        }
        bag_init_initial(b);
      }
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
<<<<<<< HEAD
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      double sum = 0.;
      for (int idThread = 0; idThread < nbThreads; idThread++) {
        sum += depositThreadCorners[MINDEX3(nbCells, 8, nbThreads, idCell,
                                            idCorner, idThread)];
      }
      depositCorners[MINDEX2(nbCells, 8, idCell, idCorner)] = sum;
    }
  }
  MFREE(depositThreadCorners);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    double sum = 0.;
    for (int k = 0; k < 8; k++) {
      sum += depositCorners[mybij(nbCells, 8, idCell, k)];
    }
    deposit[MINDEX1(nbCells, idCell)] = sum;
  }
  MFREE(depositCorners);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagsCur[idCell], &bagsNext[MINDEX1(nbCells, idCell)]);
=======
    bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
>>>>>>> 7b9ee395ff82ef98e922e2de5d8ba273a84b2303
  }
  updateFieldUsingDeposit();
}

void reportParticlesState() {
  FILE *f = fopen("particles.res", "wb");
  fwrite(&nbParticles, sizeof(int), 1, f);
  fwrite(&areaX, sizeof(double), 1, f);
  fwrite(&areaY, sizeof(double), 1, f);
  fwrite(&areaZ, sizeof(double), 1, f);
  int count = 0;
  for (int idCell = 0; idCell < nbCells; idCell++) {
    const coord co = coordOfCell(idCell);
    bag *b = &bagsCur[idCell];
    for (chunk *c = b->front; c != NULL; c = chunk_next(c, false)) {
      const int nb = c->size;
      for (int i = 0; i < nb; i++) {
        count++;
        int id = c->itemsId[i];
        double posX = (c->itemsPosX[i] + co.iX) * cellX;
        double posY = (c->itemsPosY[i] + co.iY) * cellY;
        double posZ = (c->itemsPosZ[i] + co.iZ) * cellZ;
        double speedX = c->itemsSpeedX[i] * cellX / stepDuration;
        double speedY = c->itemsSpeedY[i] * cellY / stepDuration;
        double speedZ = c->itemsSpeedZ[i] * cellZ / stepDuration;
        fwrite(&id, sizeof(int), 1, f);
        fwrite(&posX, sizeof(double), 1, f);
        fwrite(&posY, sizeof(double), 1, f);
        fwrite(&posZ, sizeof(double), 1, f);
        fwrite(&speedX, sizeof(double), 1, f);
        fwrite(&speedY, sizeof(double), 1, f);
        fwrite(&speedZ, sizeof(double), 1, f);
      }
    }
  }
  if (count != nbParticles) {
    printf("!!! ERROR: reportParticlesState: particles lost, remaining %d\n",
           count);
  }
  fclose(f);
}

int main(int argc, char **argv) {
  loadParameters(argc, argv);
  computeConstants();
  allocateStructures();
  resetDeposit();
  createParticles();
  stepLeapFrog();
  double timeStart = omp_get_wtime();
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    step();
  }
  reportPerformance(timeStart);
  reportParticlesState();
  deallocateStructures();
  free(deposit);
}
