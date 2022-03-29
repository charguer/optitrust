#include <omp.h>  // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num

#include <stdlib.h>

#include <stdio.h>

#include <math.h>

#include "mymacros.h"

#include "mymacros.h"

#include <stdlib.h>

#include <stdbool.h>

#include <stdio.h>

#include <stdio.h>

#include "pic_demo_aux.h"

#include "stdalign.h"

inline int MINDEX1(int N1, int i1) { return i1; }

inline int MINDEX2(int N1, int N2, int i1, int i2) { return i1 * N2 + i2; }

inline int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N3 + i3;
}

inline int MINDEX4(int N1, int N2, int N3, int N4, int i1, int i2, int i3,
                   int i4) {
  return i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4;
}

void* CALLOC1(int N1, size_t bytes_per_item);

void* CALLOC2(int N1, int N2, size_t bytes_per_item);

void* CALLOC3(int N1, int N2, int N3, size_t bytes_per_item);

void* CALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item);

void* MALLOC1(int N1, size_t bytes_per_item);

void* MALLOC2(int N1, int N2, size_t bytes_per_item);

void* MALLOC3(int N1, int N2, int N3, size_t bytes_per_item);

void* MALLOC4(int N1, int N2, int N3, int N4, size_t bytes_per_item);

void* MALLOC_ALIGNED1(size_t N1, size_t bytes_per_item, size_t alignment);

void* MALLOC_ALIGNED2(size_t N1, size_t N2, size_t bytes_per_item,
                      size_t alignment);

void* MALLOC_ALIGNED3(size_t N1, size_t N2, size_t N3, size_t bytes_per_item,
                      size_t alignment);

void* MALLOC_ALIGNED4(size_t N1, size_t N2, size_t N3, size_t N4,
                      size_t bytes_per_item, size_t alignment);

void MFREE(void* p);

void MFREE1(int N1, void* p);

void MFREE2(int N1, int N2, void* p);

bool ANY_BOOL();

int ANY(int maxValue);

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



typedef struct chunk {
  struct chunk* next;
  int size;
  alignas(64) double itemsPosX[CHUNK_SIZE];
  alignas(64) double itemsPosY[CHUNK_SIZE];
  alignas(64) double itemsPosZ[CHUNK_SIZE];
  alignas(64) double itemsSpeedX[CHUNK_SIZE];
  alignas(64) double itemsSpeedY[CHUNK_SIZE];
  alignas(64) double itemsSpeedZ[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk* front;
  chunk* back;
} bag;

typedef struct bag_iter {
  bool destructive;
  chunk* iter_chunk;
  int size;
  int index;
} bag_iter;

void bag_init(bag* b);

void bag_append(bag* b, bag* other);

void bag_nullify(bag* b);

int bag_size(bag* b);

void bag_add_front_chunk(bag* b);

void bag_push_concurrent(bag* b, particle p);

void bag_push_serial(bag* b, particle p);

void bag_push(bag* b, particle p);

void bag_swap(bag* b1, bag* b2);

void bag_push_initial(bag* b, particle p);

void bag_init_initial(bag* b);

void bag_free_initial(bag* b);

chunk* chunk_next(chunk* c, bool destructive);

chunk* atomic_read_chunk(chunk** p);

void atomic_write_chunk(chunk** p, chunk* v);

int atomic_increment(int* size);

chunk* chunk_alloc() {
  chunk* c;
  if (posix_memalign((void**)&c, 64, sizeof(chunk))) {
    fprintf(stderr, "chunk_alloc: posix_memalign.\n");
    exit(1);
  }
  return c;
}

void chunk_free(chunk* c) { free(c); }

void bag_init(bag* b) {
  chunk* c = chunk_alloc();
  c->size = 0;
  c->next = NULL;
  b->front = c;
  b->back = c;
}

void bag_append(bag* b, bag* other) {
  if (other->front) {
    b->back->next = other->front;
    b->back = other->back;
    bag_init(other);
  }
}

void bag_nullify(bag* b) {
  b->front = NULL;
  b->back = NULL;
}

int bag_size(bag* b) {
  chunk* c = b->front;
  int size = 0;
  while (c) {
    size += c->size;
    c = c->next;
  }
  return size;
}

void bag_add_front_chunk_serial(bag* b) {
  chunk* c = chunk_alloc();
  c->size = 0;
  c->next = b->front;
  b->front = c;
}

void bag_add_front_chunk_concurrent(bag* b) {
  chunk* c = chunk_alloc();
  c->size = 0;
  c->next = b->front;
  atomic_write_chunk(&b->front, c);
}

void bag_push_concurrent(bag* b, particle p) {
  chunk* c;
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

void bag_push_serial(bag* b, particle p) {
  chunk* c = b->front;
  int index = c->size;
  c->size++;
  c->itemsPosX[index] = p.posX;
  c->itemsPosY[index] = p.posY;
  c->itemsPosZ[index] = p.posZ;
  c->itemsSpeedX[index] = p.speedX;
  c->itemsSpeedY[index] = p.speedY;
  c->itemsSpeedZ[index] = p.speedZ;
  if (index == CHUNK_SIZE - 1) {
    bag_add_front_chunk_serial(b);
  }
}

void bag_push(bag* b, particle p) { bag_push_serial(b, p); }

void bag_swap(bag* b1, bag* b2) {
  bag temp = *b1;
  *b1 = *b2;
  *b2 = temp;
}

chunk* chunk_next(chunk* c, bool destructive) {
  chunk* cnext = c->next;
  if (destructive) {
    chunk_free(c);
  }
  return cnext;
}

void bag_push_initial(bag* b, particle p) { bag_push_serial(b, p); }

void bag_init_initial(bag* b) { bag_init(b); }

void bag_free_initial(bag* b) {
  chunk* c = b->front;
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

const int block = 2;

const int halfBlock = 1;

int nbThreads = 4;

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

double* params;

double* speed_params;

int seed;

double*** rho;

double*** Ex;

double*** Ey;

double*** Ez;

vect* field;

alignas(64) double* deposit;

bag* bagsNexts;

alignas(64) double* depositThreadCorners;

alignas(64) double* depositCorners;

bag* bagsCur;

void addParticle(double x, double y, double z, double vx, double vy, double vz);

int cellOfCoord(int i, int j, int k);

void allocateStructures();

void allocateStructuresForPoissonSolver();

void deallocateStructures();

void deallocateStructuresForPoissonSolver();

inline int int_of_double(double a) { return (int)a - (a < 0.); }

inline int wrap(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

inline int cellOfCoord(int i, int j, int k) {
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

inline double relativePosX(double x) {
  const int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}

inline double relativePosY(double y) {
  const int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}

inline double relativePosZ(double z) {
  const int iZ = int_of_double(z / cellZ);
  return (z - iZ * cellZ) / cellZ;
}

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

inline coord coordOfCell(int idCell) {
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

void accumulateChargeAtCorners(double* deposit, int idCell,
                               double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < 8; k++) {
    deposit[MINDEX1(nbCells, indices.v[k])] += charges.v[k];
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
        rho[i][j][k] = factor * deposit[MINDEX1(nbCells, cellOfCoord(i, j, k))];
      }
    }
  }
}

void resetDeposit() {
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[MINDEX1(nbCells, idCell)] = 0;
  }
}

void updateFieldUsingDeposit() {
  computeRhoFromDeposit();
  computeFieldFromRho();
  resetDeposit();
}

void allocateStructures() {
  allocateStructuresForPoissonSolver();
  deposit = (double*)MALLOC_ALIGNED1(nbCells, sizeof(double), 64);
  bagsNexts = (bag*)MALLOC_ALIGNED2(nbCells, 2, sizeof(bag), 64);
  depositThreadCorners =
      (double*)MALLOC_ALIGNED3(nbThreads, nbCells, 8, sizeof(double), 64);
  depositCorners = (double*)MALLOC_ALIGNED2(nbCells, 8, sizeof(double), 64);
  field = (vect*)malloc(nbCells * sizeof(vect));
  bagsCur = (bag*)malloc(nbCells * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init_initial(&bagsCur[idCell]);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagsKind = 0; bagsKind < 2; bagsKind++) {
      bag_init_initial(&bagsNexts[MINDEX2(nbCells, 2, idCell, bagsKind)]);
    }
  }
}

void deallocateStructures() {
  deallocateStructuresForPoissonSolver();
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagsKind = 0; bagsKind < 2; bagsKind++) {
      bag_free_initial(&bagsNexts[MINDEX2(nbCells, 2, idCell, bagsKind)]);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free_initial(&bagsCur[idCell]);
  }
  free(bagsCur);
  free(field);
  MFREE(bagsNexts);
  MFREE(depositCorners);
  MFREE(depositThreadCorners);
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
  bag_push_initial(&bagsCur[idCell], p);
  double_nbCorners contribs = cornerInterpolationCoeff(pos);
  accumulateChargeAtCorners(deposit, idCell, contribs);
}

void stepLeapFrog() {
  updateFieldUsingDeposit();
  const double negHalfStepDuration = -0.5 * stepDuration;
  const double factorC =
      particleCharge * (stepDuration * stepDuration) / particleMass;
  const double factorX = factorC / cellX;
  const double factorY = factorC / cellY;
  const double factorZ = factorC / cellZ;
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag* b = &bagsCur[idCell];
    const int_nbCorners indices = indicesOfCorners(idCell);
    vect_nbCorners field_at_corners;
    for (int k = 0; k < 8; k++) {
      field_at_corners.v[k].x = field[indices.v[k]].x * factorX;
      field_at_corners.v[k].y = field[indices.v[k]].y * factorY;
      field_at_corners.v[k].z = field[indices.v[k]].z * factorZ;
    }
    for (chunk* c = b->front; c != NULL; c = chunk_next(c, false)) {
      const int nb = c->size;
#pragma omp simd
      for (int i = 0; i < nb; i++) {
        double_nbCorners coeffs;
        c->itemsSpeedX[i] = c->itemsSpeedX[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass *
                                ((coefX[0] + signX[0] * c->itemsPosX[i]) *
                                     (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                     (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                     field_at_corners.v[0].x +
                                 (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                     (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                     (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                     field_at_corners.v[1].x +
                                 (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                     (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                     (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                     field_at_corners.v[2].x +
                                 (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                     (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                     (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                     field_at_corners.v[3].x +
                                 (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                     (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                     (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                     field_at_corners.v[4].x +
                                 (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                     (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                     (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                     field_at_corners.v[5].x +
                                 (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                     (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                     (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                     field_at_corners.v[6].x +
                                 (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                     (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                     (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                     field_at_corners.v[7].x) /
                                factorX * stepDuration / cellX;
        c->itemsSpeedY[i] = c->itemsSpeedY[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass *
                                ((coefX[0] + signX[0] * c->itemsPosX[i]) *
                                     (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                     (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                     field_at_corners.v[0].y +
                                 (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                     (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                     (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                     field_at_corners.v[1].y +
                                 (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                     (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                     (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                     field_at_corners.v[2].y +
                                 (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                     (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                     (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                     field_at_corners.v[3].y +
                                 (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                     (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                     (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                     field_at_corners.v[4].y +
                                 (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                     (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                     (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                     field_at_corners.v[5].y +
                                 (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                     (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                     (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                     field_at_corners.v[6].y +
                                 (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                     (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                     (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                     field_at_corners.v[7].y) /
                                factorY * stepDuration / cellY;
        c->itemsSpeedZ[i] = c->itemsSpeedZ[i] +
                            negHalfStepDuration * particleCharge /
                                particleMass *
                                ((coefX[0] + signX[0] * c->itemsPosX[i]) *
                                     (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                     (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                     field_at_corners.v[0].z +
                                 (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                     (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                     (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                     field_at_corners.v[1].z +
                                 (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                     (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                     (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                     field_at_corners.v[2].z +
                                 (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                     (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                     (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                     field_at_corners.v[3].z +
                                 (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                     (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                     (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                     field_at_corners.v[4].z +
                                 (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                     (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                     (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                     field_at_corners.v[5].z +
                                 (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                     (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                     (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                     field_at_corners.v[6].z +
                                 (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                     (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                     (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                     field_at_corners.v[7].z) /
                                factorZ * stepDuration / cellZ;
      }
    }
  }
}

double fwrapInt(int m, double v) {
  const int q = int_of_double(v);
  const double r = v - q;
  const int j = wrap(m, q);
  return j + r;
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
  const double factorC =
      particleCharge * (stepDuration * stepDuration) / particleMass;
  const double factorX =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellX;
  const double factorY =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellY;
  const double factorZ =
      particleCharge * (stepDuration * stepDuration) / particleMass / cellZ;
#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      for (int idThread = 0; idThread < nbThreads; idThread++) {
        depositThreadCorners[MINDEX3(nbThreads, nbCells, 8, idThread, idCell,
                                     idCorner)] = 0.;
      }
    }
  }
core:
  for (int cX = 0; cX < block; cX++) {
    for (int cY = 0; cY < block; cY++) {
      for (int cZ = 0; cZ < block; cZ++) {
#pragma omp parallel for collapse(3)
        for (int bX = cX * 2; bX < gridX; bX += block * 2) {
          for (int bY = cY * 2; bY < gridY; bY += block * 2) {
            for (int bZ = cZ * 2; bZ < gridZ; bZ += block * 2) {
              const int idThread = omp_get_thread_num();
              for (int iX = bX; iX < bX + 2; iX++) {
                for (int iY = bY; iY < bY + 2; iY++) {
                  for (int iZ = bZ; iZ < bZ + 2; iZ++) {
                    const int idCell = (iX * gridY + iY) * gridZ + iZ;
                    const int_nbCorners indices = indicesOfCorners(idCell);
                    vect_nbCorners field_at_corners;
                    for (int k = 0; k < 8; k++) {
                      field_at_corners.v[k].x = field[indices.v[k]].x * factorX;
                      field_at_corners.v[k].y = field[indices.v[k]].y * factorY;
                      field_at_corners.v[k].z = field[indices.v[k]].z * factorZ;
                    }
                    bag* b = &bagsCur[idCell];
                    for (chunk* c = b->front; c != NULL;
                         c = chunk_next(c, true)) {
                      const int nb = c->size;
                      alignas(64) int idCell2_step[CHUNK_SIZE];
#pragma omp simd
                      for (int i = 0; i < nb; i++) {
                        double_nbCorners coeffs;
                        c->itemsSpeedX[i] +=
                            (coefX[0] + signX[0] * c->itemsPosX[i]) *
                                (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                field_at_corners.v[0].x +
                            (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                field_at_corners.v[1].x +
                            (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                field_at_corners.v[2].x +
                            (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                field_at_corners.v[3].x +
                            (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                field_at_corners.v[4].x +
                            (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                field_at_corners.v[5].x +
                            (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                field_at_corners.v[6].x +
                            (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                field_at_corners.v[7].x;
                        c->itemsSpeedY[i] +=
                            (coefX[0] + signX[0] * c->itemsPosX[i]) *
                                (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                field_at_corners.v[0].y +
                            (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                field_at_corners.v[1].y +
                            (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                field_at_corners.v[2].y +
                            (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                field_at_corners.v[3].y +
                            (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                field_at_corners.v[4].y +
                            (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                field_at_corners.v[5].y +
                            (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                field_at_corners.v[6].y +
                            (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                field_at_corners.v[7].y;
                        c->itemsSpeedZ[i] +=
                            (coefX[0] + signX[0] * c->itemsPosX[i]) *
                                (coefY[0] + signY[0] * c->itemsPosY[i]) *
                                (coefZ[0] + signZ[0] * c->itemsPosZ[i]) *
                                field_at_corners.v[0].z +
                            (coefX[1] + signX[1] * c->itemsPosX[i]) *
                                (coefY[1] + signY[1] * c->itemsPosY[i]) *
                                (coefZ[1] + signZ[1] * c->itemsPosZ[i]) *
                                field_at_corners.v[1].z +
                            (coefX[2] + signX[2] * c->itemsPosX[i]) *
                                (coefY[2] + signY[2] * c->itemsPosY[i]) *
                                (coefZ[2] + signZ[2] * c->itemsPosZ[i]) *
                                field_at_corners.v[2].z +
                            (coefX[3] + signX[3] * c->itemsPosX[i]) *
                                (coefY[3] + signY[3] * c->itemsPosY[i]) *
                                (coefZ[3] + signZ[3] * c->itemsPosZ[i]) *
                                field_at_corners.v[3].z +
                            (coefX[4] + signX[4] * c->itemsPosX[i]) *
                                (coefY[4] + signY[4] * c->itemsPosY[i]) *
                                (coefZ[4] + signZ[4] * c->itemsPosZ[i]) *
                                field_at_corners.v[4].z +
                            (coefX[5] + signX[5] * c->itemsPosX[i]) *
                                (coefY[5] + signY[5] * c->itemsPosY[i]) *
                                (coefZ[5] + signZ[5] * c->itemsPosZ[i]) *
                                field_at_corners.v[5].z +
                            (coefX[6] + signX[6] * c->itemsPosX[i]) *
                                (coefY[6] + signY[6] * c->itemsPosY[i]) *
                                (coefZ[6] + signZ[6] * c->itemsPosZ[i]) *
                                field_at_corners.v[6].z +
                            (coefX[7] + signX[7] * c->itemsPosX[i]) *
                                (coefY[7] + signY[7] * c->itemsPosY[i]) *
                                (coefZ[7] + signZ[7] * c->itemsPosZ[i]) *
                                field_at_corners.v[7].z;
                      }
#pragma omp simd
                      for (int i = 0; i < nb; i++) {
                        const double pX =
                            c->itemsPosX[i] + iX + c->itemsSpeedX[i];
                        const double pY =
                            c->itemsPosY[i] + iY + c->itemsSpeedY[i];
                        const double pZ =
                            c->itemsPosZ[i] + iZ + c->itemsSpeedZ[i];
                        const int qX = int_of_double(pX);
                        const double rX = pX - qX;
                        const int iX2 = qX & gridX + -1;
                        const int qY = int_of_double(pY);
                        const double rY = pY - qY;
                        const int iY2 = qY & gridY + -1;
                        const int qZ = int_of_double(pZ);
                        const double rZ = pZ - qZ;
                        const int iZ2 = qZ & gridZ + -1;
                        idCell2_step[i] =
                            MINDEX3(gridX, gridY, gridZ, iX2, iY2, iZ2);
                        c->itemsPosX[i] = rX;
                        c->itemsPosY[i] = rY;
                        c->itemsPosZ[i] = rZ;
                      }
                      for (int i = 0; i < nb; i++) {
                        particle p2;
                        p2.posX = c->itemsPosX[i];
                        p2.posY = c->itemsPosY[i];
                        p2.posZ = c->itemsPosZ[i];
                        p2.speedX = c->itemsSpeedX[i];
                        p2.speedY = c->itemsSpeedY[i];
                        p2.speedZ = c->itemsSpeedZ[i];
                        const coord co = coordOfCell(idCell2_step[i]);
                        const bool isDistFromBlockLessThanHalfABlock =
                            co.iX >= bX - halfBlock &&
                                co.iX < bX + block + halfBlock ||
                            bX == 0 && co.iX >= gridX - halfBlock ||
                            bX == gridX - block && co.iX < halfBlock &&
                                (co.iY >= bY - halfBlock &&
                                 co.iY < bY + block + halfBlock) ||
                            bY == 0 && co.iY >= gridY - halfBlock ||
                            bY == gridY - block && co.iY < halfBlock &&
                                (co.iZ >= bZ - halfBlock &&
                                 co.iZ < bZ + block + halfBlock) ||
                            bZ == 0 && co.iZ >= gridZ - halfBlock ||
                            bZ == gridZ - block && co.iZ < halfBlock;
                        if (isDistFromBlockLessThanHalfABlock) {
                          bag_push_serial(
                              &bagsNexts[MINDEX2(nbCells, 2, idCell2_step[i],
                                                 PRIVATE)],
                              p2);
                        } else {
                          bag_push_concurrent(
                              &bagsNexts[MINDEX2(nbCells, 2, idCell2_step[i],
                                                 SHARED)],
                              p2);
                        }
#pragma omp simd aligned(coefX, coefY, coefZ, signX, signY, signZ : 64)
                        for (int k = 0; k < 8; k++) {
                          depositThreadCorners[MINDEX3(
                              nbThreads, nbCells, 8, idThread, idCell2_step[i],
                              k)] += (coefX[k] + signX[k] * c->itemsPosX[i]) *
                                     (coefY[k] + signY[k] * c->itemsPosY[i]) *
                                     (coefZ[k] + signZ[k] * c->itemsPosZ[i]);
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
                 &bagsNexts[MINDEX2(nbCells, 2, idCell, bagsKind)]);
    }
#pragma omp simd
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      depositCorners[idCell * 8 + idCorner] =
          depositThreadCorners[0 * nbCells * 8 + idCell * 8 + idCorner];
    }
    for (int idThread = 1; idThread < nbThreads; idThread++) {
#pragma omp simd
      for (int idCorner = 0; idCorner < 8; idCorner++)
        depositCorners[idCell * 8 + idCorner] +=
            depositThreadCorners[idThread * nbCells * 8 + idCell * 8 +
                                 idCorner];
    }
  }
#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    double sum = 0.;
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      sum += depositCorners[mybij(nbCells, 8, idCell, idCorner)];
    }
    deposit[MINDEX1(nbCells, idCell)] = sum;
  }
  updateFieldUsingDeposit();
}

int main(int argc, char** argv) {
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
  deallocateStructures();
  free(deposit);
}
