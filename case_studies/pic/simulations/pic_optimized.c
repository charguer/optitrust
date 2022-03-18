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
  vect pos;
  vect speed;
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
  particle items[CHUNK_SIZE];
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

void bag_iter_load_chunk(bag_iter *it, chunk *c);

void bag_iter_init(bag_iter *it, bag *b);

particle *bag_iter_get(bag_iter *it);

chunk *bag_iter_get_chunk(bag_iter *it);

particle *bag_iter_begin(bag_iter *it, bag *b);

chunk *chunk_next(chunk *c, bool destructive);

particle *bag_iter_next_common(bag_iter *it, bool destructive);

particle *bag_iter_next(bag_iter *it);

particle *bag_iter_next_destructive(bag_iter *it);

void bag_iter_ho_basic(bag *b, void body(particle *), bool destructive);

void bag_iter_ho_chunk(bag *b, void body(particle *), bool destructive);

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
      c->items[index] = p;
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
  c->items[index] = p;
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

void bag_iter_load_chunk(bag_iter *it, chunk *c) {
  it->iter_chunk = c;
  it->size = c->size;
  it->index = 0;
}

void bag_iter_init(bag_iter *it, bag *b) { bag_iter_load_chunk(it, b->front); }

particle *bag_iter_get(bag_iter *it) {
  if (it->size == 0) {
    return NULL;
  } else {
    return &it->iter_chunk->items[it->index];
  }
}

chunk *bag_iter_get_chunk(bag_iter *it) { return it->iter_chunk; }

particle *bag_iter_begin(bag_iter *it, bag *b) {
  bag_iter_init(it, b);
  return bag_iter_get(it);
}

chunk *chunk_next(chunk *c, bool destructive) {
  chunk *cnext = c->next;
  if (destructive) {
    chunk_free(c);
  }
  return cnext;
}

particle *bag_iter_next_common(bag_iter *it, bool destructive) {
  it->index++;
  if (it->index == it->size) {
    chunk *c = it->iter_chunk;
    chunk *cnext = chunk_next(c, destructive);
    if (cnext == NULL) {
      return NULL;
    }
    bag_iter_load_chunk(it, cnext);
  }
  return bag_iter_get(it);
}

particle *bag_iter_next(bag_iter *it) {
  return bag_iter_next_common(it, false);
}

particle *bag_iter_next_destructive(bag_iter *it) {
  return bag_iter_next_common(it, true);
}

void bag_iter_ho_basic(bag *b, void body(particle *), bool destructive) {
  bag_iter it;
  for (particle *p = bag_iter_begin(&it, b); p != NULL;
       p = bag_iter_next_common(&it, destructive)) {
    body(p);
  }
}

void bag_iter_ho_chunk(bag *b, void body(particle *), bool destructive) {
  for (chunk *c = b->front; c != NULL; c = chunk_next(c, destructive)) {
    const int nb = c->size;
    for (int i = 0; i < nb; i++) {
      particle *p = &c->items[i];
      body(p);
    }
  }
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

double_nbCorners cornerInterpolationCoeff(vect pos) {
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  double_nbCorners r;
  const double coefX[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  const double signX[8] = {-1., -1., -1., -1., 1., 1., 1., 1.};
  const double coefY[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
  const double signY[8] = {-1., -1., 1., 1., -1., -1., 1., 1.};
  const double coefZ[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
  const double signZ[8] = {-1., 1., -1., 1., -1., 1., -1., 1.};
  r.v[0] = (coefX[0] + signX[0] * rX) * (coefY[0] + signY[0] * rY) *
           (coefZ[0] + signZ[0] * rZ);
  r.v[1] = (coefX[1] + signX[1] * rX) * (coefY[1] + signY[1] * rY) *
           (coefZ[1] + signZ[1] * rZ);
  r.v[2] = (coefX[2] + signX[2] * rX) * (coefY[2] + signY[2] * rY) *
           (coefZ[2] + signZ[2] * rZ);
  r.v[3] = (coefX[3] + signX[3] * rX) * (coefY[3] + signY[3] * rY) *
           (coefZ[3] + signZ[3] * rZ);
  r.v[4] = (coefX[4] + signX[4] * rX) * (coefY[4] + signY[4] * rY) *
           (coefZ[4] + signZ[4] * rZ);
  r.v[5] = (coefX[5] + signX[5] * rX) * (coefY[5] + signY[5] * rY) *
           (coefZ[5] + signZ[5] * rZ);
  r.v[6] = (coefX[6] + signX[6] * rX) * (coefY[6] + signY[6] * rY) *
           (coefZ[6] + signZ[6] * rZ);
  r.v[7] = (coefX[7] + signX[7] * rX) * (coefY[7] + signY[7] * rY) *
           (coefZ[7] + signZ[7] * rZ);
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
  const vect pos = {x, y, z};
  const vect speed = {vx, vy, vz};
  const particle particle = {pos, speed, idParticle};
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
    bag_iter bag_it;
    for (particle *p = bag_iter_begin(&bag_it, b); p != NULL;
         p = bag_iter_next_common(&bag_it, false)) {
      double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
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
      vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
      p->speed = vect_add(p->speed, vect_mul(negHalfStepDuration, accel));
    }
  }
}

void step() {
  for (int idCell = 0; idCell < nbCells; idCell++) {
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    bag *b = &bagsCur[idCell];
    bag_iter bag_it;
    for (particle *p = bag_iter_begin(&bag_it, b); p != NULL;
         p = bag_iter_next_common(&bag_it, true)) {
      double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
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
      vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
      vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
      vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
      pos2 = wrapArea(pos2);
      particle p2 = {pos2, speed2, p->id};
      const int idCell2 = idCellOfPos(pos2);
      bag_push(&bagsNext[idCell2], p2);
      double_nbCorners contribs = cornerInterpolationCoeff(pos2);
      accumulateChargeAtCorners(deposit, idCell2, contribs);
    }
    bag_init_initial(b);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
  }
  updateFieldUsingDeposit();
}

void reportParticlesState() {
  FILE *f = fopen("particles.res", "wb");
  fwrite(&nbParticles, sizeof(int), 1, f);
  fwrite(&areaX, sizeof(double), 1, f);
  fwrite(&areaY, sizeof(double), 1, f);
  fwrite(&areaZ, sizeof(double), 1, f);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag *b = &bagsCur[idCell];
    bag_iter bag_it;
    for (particle *p = bag_iter_begin(&bag_it, b); p != NULL;
         p = bag_iter_next_common(&bag_it, false)) {
      int id = p->id;
      double posX = p->pos.x;
      double posY = p->pos.y;
      double posZ = p->pos.z;
      double speedX = p->speed.x;
      double speedY = p->speed.y;
      double speedZ = p->speed.z;
      fwrite(&id, sizeof(int), 1, f);
      fwrite(&posX, sizeof(double), 1, f);
      fwrite(&posY, sizeof(double), 1, f);
      fwrite(&posZ, sizeof(double), 1, f);
      fwrite(&speedX, sizeof(double), 1, f);
      fwrite(&speedY, sizeof(double), 1, f);
      fwrite(&speedZ, sizeof(double), 1, f);
    }
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
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    step();
  }
  reportParticlesState();
  deallocateStructures();
  free(deposit);
}
