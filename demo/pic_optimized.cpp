#include <omp.h> // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num

#include <stdlib.h>

#include <stdio.h>

#include <math.h>

#include "mymacros.h"

#include "mymacros.h"

#include "optitrust.h"

#include <stdio.h>

#include "pic_demo_aux.h"

#include "stdalign.h"

int nbThreads;

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
} particle;

vect vect_add(vect v1, vect v2);

vect vect_mul(double d, vect v);

vect vect_add(vect v1, vect v2) {
  return (vect){v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

vect vect_mul(double d, vect v) { return (vect){d * v.x, d * v.y, d * v.z}; }

const int CHUNK_SIZE = 128;

typedef struct chunk {
  struct chunk *next;
  int size;
  float itemsPosX[CHUNK_SIZE];
  float itemsPosY[CHUNK_SIZE];
  float itemsPosZ[CHUNK_SIZE];
  double itemsSpeedX[CHUNK_SIZE];
  double itemsSpeedY[CHUNK_SIZE];
  double itemsSpeedZ[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;

typedef struct bag_iter {
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

chunk *chunk_alloc() { return (chunk *)malloc(sizeof(chunk)); }

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

void bag_add_front_chunk(bag *b) {
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
      if (index == CHUNK_SIZE - 1) {
        bag_add_front_chunk(b);
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
  if (index == CHUNK_SIZE - 1) {
    bag_add_front_chunk(b);
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

const int gridX;

const int gridY;

const int gridZ;

int nbThreads;

const int block = 2;

const int halfBlock = block / 2;

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

bag *bagsNexts;

double *depositThreadCorners;

double *depositCorners;

bag *bagsCur;

void addParticle(double x, double y, double z, double vx, double vy, double vz);

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

typedef struct {
  int v[8];
} int_nbCorners;

typedef struct {
  double v[8];
} double_nbCorners;

typedef struct {
  vect v[8];
} vect_nbCorners;

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

alignas(64) const double coefX[8] = {1., 1., 1., 1., 0., 0., 0., 0.};

alignas(64) const double signX[8] = {-1., -1., -1., -1., 1., 1., 1., 1.};

alignas(64) const double coefY[8] = {1., 1., 0., 0., 1., 1., 0., 0.};

alignas(64) const double signY[8] = {-1., -1., 1., 1., -1., -1., 1., 1.};

alignas(64) const double coefZ[8] = {1., 0., 1., 0., 1., 0., 1., 0.};

alignas(64) const double signZ[8] = {-1., 1., -1., 1., -1., 1., -1., 1.};

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
  deposit = (double *)MMALLOC1(nbCells, sizeof(double));
  bagsNexts = (bag *)MMALLOC2(2, nbCells, sizeof(bag));
  depositThreadCorners =
      (double *)MMALLOC3(nbThreads, nbCells, 8, sizeof(double));
  depositCorners = (double *)MMALLOC2(nbCells, 8, sizeof(double));
  field = (vect *)MMALLOC1(nbCells, sizeof(vect));
  bagsCur = (bag *)MMALLOC1(nbCells, sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init_initial(&bagsCur[idCell]);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagsKind = 0; bagsKind < 2; bagsKind++) {
      bag_init_initial(&bagsNexts[MINDEX2(2, nbCells, bagsKind, idCell)]);
    }
  }
}

void deallocateStructures() {
  deallocateStructuresForPoissonSolver();
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free_initial(&bagsCur[idCell]);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagsKind = 0; bagsKind < 2; bagsKind++) {
      bag_free_initial(&bagsNexts[MINDEX2(2, nbCells, bagsKind, idCell)]);
    }
  }
  free(bagsCur);
  free(field);
  MFREE(depositCorners);
  MFREE(depositThreadCorners);
  MFREE(bagsNexts);
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

void addParticle(double x, double y, double z, double vx, double vy,
                 double vz) {
  const vect pos = {x, y, z};
  const vect speed = {vx, vy, vz};
  const particle particle = {pos.x, pos.y, pos.z, speed.x, speed.y, speed.z};
  const int idCell = idCellOfPos(pos);
  bag_push_initial(&bagsCur[idCell], particle);
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
        const int iX0 = int_of_double(c->itemsPosX[i] / cellX);
        const double rX0 = (c->itemsPosX[i] - iX0 * cellX) / cellX;
        const int iY0 = int_of_double(c->itemsPosY[i] / cellY);
        const double rY0 = (c->itemsPosY[i] - iY0 * cellY) / cellY;
        const int iZ0 = int_of_double(c->itemsPosZ[i] / cellZ);
        const double rZ0 = (c->itemsPosZ[i] - iZ0 * cellZ) / cellZ;
        double_nbCorners coeffs;
        for (int k = 0; k < 8; k++) {
          coeffs.v[k] = (coefX[k] + signX[k] * rX0) *
                        (coefY[k] + signY[k] * rY0) *
                        (coefZ[k] + signZ[k] * rZ0);
        }
        vect fieldAtPos = {0., 0., 0.};
        fieldAtPos.x = fieldAtPos.x + (coeffs.v[0] * field_at_corners.v[0].x +
                                       coeffs.v[1] * field_at_corners.v[1].x +
                                       coeffs.v[2] * field_at_corners.v[2].x +
                                       coeffs.v[3] * field_at_corners.v[3].x +
                                       coeffs.v[4] * field_at_corners.v[4].x +
                                       coeffs.v[5] * field_at_corners.v[5].x +
                                       coeffs.v[6] * field_at_corners.v[6].x +
                                       coeffs.v[7] * field_at_corners.v[7].x);
        fieldAtPos.y = fieldAtPos.y + (coeffs.v[0] * field_at_corners.v[0].y +
                                       coeffs.v[1] * field_at_corners.v[1].y +
                                       coeffs.v[2] * field_at_corners.v[2].y +
                                       coeffs.v[3] * field_at_corners.v[3].y +
                                       coeffs.v[4] * field_at_corners.v[4].y +
                                       coeffs.v[5] * field_at_corners.v[5].y +
                                       coeffs.v[6] * field_at_corners.v[6].y +
                                       coeffs.v[7] * field_at_corners.v[7].y);
        fieldAtPos.z = fieldAtPos.z + (coeffs.v[0] * field_at_corners.v[0].z +
                                       coeffs.v[1] * field_at_corners.v[1].z +
                                       coeffs.v[2] * field_at_corners.v[2].z +
                                       coeffs.v[3] * field_at_corners.v[3].z +
                                       coeffs.v[4] * field_at_corners.v[4].z +
                                       coeffs.v[5] * field_at_corners.v[5].z +
                                       coeffs.v[6] * field_at_corners.v[6].z +
                                       coeffs.v[7] * field_at_corners.v[7].z);
        vect accel = {particleCharge / particleMass * fieldAtPos.x,
                      particleCharge / particleMass * fieldAtPos.y,
                      particleCharge / particleMass * fieldAtPos.z};
        c->itemsSpeedX[i] = c->itemsSpeedX[i] + negHalfStepDuration * accel.x;
        c->itemsSpeedY[i] = c->itemsSpeedY[i] + negHalfStepDuration * accel.y;
        c->itemsSpeedZ[i] = c->itemsSpeedZ[i] + negHalfStepDuration * accel.z;
      }
    }
  }
}

int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
  coord coord = coordOfCell(idCell);
  int iX = coord.iX;
  int iY = coord.iY;
  int iZ = coord.iZ;
  int res[8] = {cellOfCoord(iX, iY, iZ),
                cellOfCoord(iX, iY, wrap(gridZ, iZ - 1)),
                cellOfCoord(iX, wrap(gridY, iY - 1), iZ),
                cellOfCoord(iX, wrap(gridY, iY - 1), wrap(gridZ, iZ - 1)),
                cellOfCoord(wrap(gridX, iX - 1), iY, iZ),
                cellOfCoord(wrap(gridX, iX - 1), iY, wrap(gridZ, iZ - 1)),
                cellOfCoord(wrap(gridX, iX - 1), wrap(gridY, iY - 1), iZ),
                cellOfCoord(wrap(gridX, iX - 1), wrap(gridY, iY - 1),
                            wrap(gridZ, iZ - 1))};
  return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
}

const int SHARED = 1;

const int PRIVATE = 0;

void step() {
  nbThreads = omp_get_num_threads();
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      for (int k = 0; k < nbThreads; k++) {
        depositThreadCorners[MINDEX3(nbThreads, nbCells, 8, k, idCell,
                                     idCorner)] = 0.;
      }
    }
  }
core:
  for (int cX = 0; cX < 2; cX++) {
    for (int cY = 0; cY < 2; cY++) {
      for (int cZ = 0; cZ < 2; cZ++) {
#pragma omp parallel for collapse(3)
        for (int bX = cX * block; bX < gridX; bX += 2 * block) {
          for (int bY = cY * block; bY < gridY; bY += 2 * block) {
            for (int bZ = cZ * block; bZ < gridZ; bZ += 2 * block) {
              int idThread = omp_get_thread_num();
              for (int iX = bX; iX < bX + block; iX++) {
                for (int iY = bY; iY < bY + block; iY++) {
                  for (int iZ = bZ; iZ < bZ + block; iZ++) {
                    const int idCell = (iX * gridY + iY) * gridZ + iZ;
                    const int_nbCorners indices = indicesOfCorners(idCell);
                    vect_nbCorners res;
                    for (int k = 0; k < 8; k++) {
                      res.v[k].x = field[indices.v[k]].x *
                                   (particleCharge * stepDuration *
                                    stepDuration / particleMass / cellX);
                      res.v[k].y = field[indices.v[k]].y *
                                   (particleCharge * stepDuration *
                                    stepDuration / particleMass / cellY);
                      res.v[k].z = field[indices.v[k]].z *
                                   (particleCharge * stepDuration *
                                    stepDuration / particleMass / cellZ);
                    }
                    bag *b = &bagsCur[idCell];
                    for (chunk *c = b->front; c != NULL;
                         c = chunk_next(c, true)) {
                      const int nb = c->size;
                      int idCell2_step[CHUNK_SIZE];
#pragma omp simd
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
                        fieldAtPosX += coeffs.v[0] * res.v[0].x +
                                       coeffs.v[1] * res.v[1].x +
                                       coeffs.v[2] * res.v[2].x +
                                       coeffs.v[3] * res.v[3].x +
                                       coeffs.v[4] * res.v[4].x +
                                       coeffs.v[5] * res.v[5].x +
                                       coeffs.v[6] * res.v[6].x +
                                       coeffs.v[7] * res.v[7].x;
                        fieldAtPosY += coeffs.v[0] * res.v[0].y +
                                       coeffs.v[1] * res.v[1].y +
                                       coeffs.v[2] * res.v[2].y +
                                       coeffs.v[3] * res.v[3].y +
                                       coeffs.v[4] * res.v[4].y +
                                       coeffs.v[5] * res.v[5].y +
                                       coeffs.v[6] * res.v[6].y +
                                       coeffs.v[7] * res.v[7].y;
                        fieldAtPosZ += coeffs.v[0] * res.v[0].z +
                                       coeffs.v[1] * res.v[1].z +
                                       coeffs.v[2] * res.v[2].z +
                                       coeffs.v[3] * res.v[3].z +
                                       coeffs.v[4] * res.v[4].z +
                                       coeffs.v[5] * res.v[5].z +
                                       coeffs.v[6] * res.v[6].z +
                                       coeffs.v[7] * res.v[7].z;
                        c->itemsSpeedX[i] += fieldAtPosX;
                        c->itemsSpeedY[i] += fieldAtPosY;
                        c->itemsSpeedZ[i] += fieldAtPosZ;
                      }
#pragma omp simd
                      for (int i = 0; i < nb; i++) {
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
                        int *idCell2 = &idCell2_step[i];
                        *idCell2 = cellOfCoord(iX2, iY2, iZ2);
                        c->itemsPosX[i] = (float)(pX2 - iX2);
                        c->itemsPosY[i] = (float)(pY2 - iY2);
                        c->itemsPosZ[i] = (float)(pZ2 - iZ2);
                      }
#pragma omp simd
                      for (int i = 0; i < nb; i++) {
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
                        int *idCell2 = &idCell2_step[i];
                        const coord co = coordOfCell(*idCell2);
                        const bool isDistFromBlockLessThanHalfABlock =
                            co.iX - bX >= -halfBlock &&
                            co.iX - bX < block + halfBlock &&
                            co.iY - bY >= -halfBlock &&
                            co.iY - bY < block + halfBlock &&
                            co.iZ - bZ >= -halfBlock &&
                            co.iZ - bZ < block + halfBlock;
                        if (isDistFromBlockLessThanHalfABlock) {
                          bag_push_serial(&bagsNexts[MINDEX2(
                                              2, nbCells, PRIVATE, *idCell2)],
                                          p2);
                        } else {
                          bag_push_concurrent(
                              &bagsNexts[MINDEX2(2, nbCells, SHARED, *idCell2)],
                              p2);
                        }
                        double_nbCorners contribs;
#pragma omp simd aligned(coefX, coefY, coefZ, signX, signY, signZ : 64)
                        for (int k = 0; k < 8; k++) {
                          depositThreadCorners[MINDEX3(
                              nbThreads, nbCells, 8, idThread, *idCell2, k)] +=
                              (coefX[k] + signX[k] * rX1) *
                              (coefY[k] + signY[k] * rY1) *
                              (coefZ[k] + signZ[k] * rZ1);
                        }
                      }
                    }
                    bag_init_initial(b);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagsKind = 0; bagsKind < 2; bagsKind++) {
      bag_append(&bagsCur[MINDEX1(nbCells, idCell)],
                 &bagsNexts[MINDEX2(2, nbCells, bagsKind, idCell)]);
    }
  }
#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      int sum = 0;
      for (int k = 0; k < nbThreads; k++) {
        sum += depositThreadCorners[MINDEX3(nbThreads, nbCells, 8, k, idCell,
                                            idCorner)];
      }
      depositCorners[MINDEX2(nbCells, 8, idCell, idCorner)] = sum;
    }
  }
#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    int sum = 0;
    for (int k = 0; k < 8; k++) {
      sum += depositCorners[mybij(nbCells, 8, idCell, k)];
    }
    deposit[MINDEX1(nbCells, idCell)] = sum;
  }
  updateFieldUsingDeposit();
}

int main(int argc, char **argv) {
  loadParameters(argc, argv);
  computeConstants();
  allocateStructures();
  resetDeposit();
  createParticles();
  stepLeapFrog();
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    step();
  }
  deallocateStructures();
  free(deposit);
}
