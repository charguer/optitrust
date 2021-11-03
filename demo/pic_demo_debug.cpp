#include <stdlib.h>

#include <stdio.h>

#include "optitrust.h"

typedef struct {
  double x;
  double y;
  double z;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;

vect vect_add(vect v1, vect v2) {
  return {(v1.x + v2.x), (v1.y + v2.y), (v1.z + v2.z)};
}

vect vect_mul(double d, vect v) { return {(d * v.x), (d * v.y), (d * v.z)}; }

int const CHUNK_SIZE = 128;

typedef struct chunk {
  chunk *next;
  int size;
  particle items[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;

chunk *atomic_read(chunk **p) {
  chunk *value;
  value = (*p);
  return value;
}

chunk *chunk_alloc() { return (chunk *)malloc(sizeof(chunk)); }

void chunk_free(chunk *c) { free(c); }

void bag_init(bag *b, int id_bag, int id_cell) {
  chunk *c = chunk_alloc();
  (c->size) = 0;
  (c->next) = NULL;
  (b->front) = c;
  (b->back) = c;
}

void bag_append(bag *b, bag *other, int id_bag, int id_cell) {
  if ((other->front)) {
    ((b->back)->next) = (other->front);
    (b->back) = (other->back);
    bag_init(other, id_bag, id_cell);
  }
}

void bag_nullify(bag *b) {
  (b->front) = NULL;
  (b->back) = NULL;
}

int bag_size(bag *b) {
  chunk *c = (b->front);
  int size = 0;
  while (c) {
    size += (c->size);
    c = (c->next);
  }
  return size;
}

void bag_add_front_chunk(bag *b) {
  chunk *c = chunk_alloc();
  (c->size) = 0;
  (c->next) = (b->front);
  (b->front) = c;
}

void bag_push_concurrent(bag *b, particle p) {
  chunk *c;
  int index;
  while (true) {
    c = (b->front);
    index = (c->size)++;
    if ((index < CHUNK_SIZE)) {
      (c->items)[index] = p;
      if ((index == (CHUNK_SIZE - 1))) {
        bag_add_front_chunk(b);
      }
      return;
    } else {
      (c->size) = CHUNK_SIZE;
      while ((atomic_read((&(b->front))) == c)) {
      }
    }
  }
}

void bag_push_serial(bag *b, particle p) {
  chunk *c = (b->front);
  int index = (c->size)++;
  (c->items)[index] = p;
  if ((index == (CHUNK_SIZE - 1))) {
    bag_add_front_chunk(b);
  }
}

void bag_push(bag *b, particle p) { return bag_push_serial(b, p); }

void bag_swap(bag *b1, bag *b2) {
  bag temp = (*b1);
  (*b1) = (*b2);
  (*b2) = temp;
}

typedef struct bag_iter {
  chunk *iter_chunk;
  int size;
  int index;
} bag_iter;

void bag_iter_load_chunk(bag_iter *it, chunk *c) {
  (it->iter_chunk) = c;
  if ((c != NULL)) {
    (it->size) = (c->size);
    (it->index) = 0;
  }
}

particle *bag_iter_current(bag_iter *it) {
  return (&((it->iter_chunk)->items)[(it->index)]);
}

void bag_iter_init(bag_iter *it, bag *b) {
  bag_iter_load_chunk(it, (b->front));
}

bool bag_iter_finished(bag_iter *it) { return ((it->iter_chunk) == NULL); }

particle *bag_iter_next_destructive(bag_iter *it) {
  int i = (it->index);
  (it->index)++;
  if (((it->index) == (it->size))) {
    chunk *c = (it->iter_chunk);
    bag_iter_load_chunk(it, (c->next));
    chunk_free(c);
    if (bag_iter_finished(it)) {
      return NULL;
    }
  }
  return bag_iter_current(it);
}

void bag_push_initial(bag *b, particle p) { bag_push_serial(b, p); }

void bag_init_initial(bag *b) { bag_init(b, (-1), (-1)); }

unsigned int FREELIST_SIZE;

int **free_index;

chunk ***free_chunks;

chunk **all_free_chunks;

int number_of_spare_chunks_per_parity;

int **spare_chunks_ids;

int num_threads;

int *cumulative_free_indexes;

int bag_last_spare_chunk_to_be_used;

chunk *manual_chunk_alloc(int thread_id) {
  if ((free_index[thread_id][0] > 0)) {
    return free_chunks[thread_id][--free_index[thread_id][0]];
  } else {
    return (chunk *)malloc(sizeof(chunk));
  }
}

void manual_chunk_free(chunk *c, int thread_id) {
  if ((free_index[thread_id][0] < FREELIST_SIZE)) {
    free_chunks[thread_id][free_index[thread_id][0]++] = c;
  } else {
    free(c);
  }
}

int const THREAD_INITIAL = (-1);

int const THREAD_ZERO = 0;

chunk *manual_obtain_chunk_initial() {
  if ((free_index[THREAD_ZERO][0] < 1)) {
    fprintf(stderr,
            "Not enough chunks in all_free_chunks. Check its allocation.\n");
    exit(1);
  }
  return all_free_chunks[--free_index[THREAD_ZERO][0]];
}

chunk *manual_obtain_chunk(int id_bag, int id_cell, int thread_id) {
  if ((thread_id == THREAD_INITIAL)) {
    return manual_obtain_chunk_initial();
  }
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (int k = (num_threads - 1); (k > 0); k--)
    if ((cumulative_free_indexes[k] <= id_chunk))
      break;
  int offset = (id_chunk - cumulative_free_indexes[k]);
  if (((offset < 0) || (offset >= free_index[k][0]))) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf("Maybe did you forgot to call compute_cumulative_free_list_sizes "
           "and/or update_free_list_sizes ?\n");
    exit(1);
  }
  chunk *c = free_chunks[k][((free_index[k][0] - 1) - offset)];
  return c;
}

void compute_cumulative_free_list_sizes() {
  int k;
  cumulative_free_indexes[0] = 0;
  for (int k = 1; (k < num_threads); k++)
    cumulative_free_indexes[k] =
        (cumulative_free_indexes[(k - 1)] + free_index[(k - 1)][0]);
  int nb_free_chunks = (cumulative_free_indexes[(num_threads - 1)] +
                        free_index[(num_threads - 1)][0]);
  if ((nb_free_chunks < number_of_spare_chunks_per_parity)) {
    int nb_chunks_to_allocate =
        (number_of_spare_chunks_per_parity - nb_free_chunks);
    printf("Not enough free chunks in the free lists ! We must malloc %d "
           "chunks.\n",
           nb_chunks_to_allocate);
    int nb_allocated_chunks = 0;
    while ((nb_allocated_chunks < nb_chunks_to_allocate)) {
      free_chunks[THREAD_ZERO][free_index[THREAD_ZERO][0]++] =
          (chunk *)malloc(sizeof(chunk));
      nb_allocated_chunks++;
    }
    compute_cumulative_free_list_sizes();
  }
  for (int k = (num_threads - 1); (k > 0); k--)
    if ((cumulative_free_indexes[k] <= number_of_spare_chunks_per_parity)) {
      bag_last_spare_chunk_to_be_used = k;
      return;
    }
}

void update_free_list_sizes() {
  for (int i = 0; (i < bag_last_spare_chunk_to_be_used); i++)
    free_index[i][0] = 0;
  free_index[bag_last_spare_chunk_to_be_used][0] -=
      (number_of_spare_chunks_per_parity -
       cumulative_free_indexes[bag_last_spare_chunk_to_be_used]);
}

bag *CHOOSE(int nb, bag *b1, bag *b2) { return b1; }

double const areaX = 10.;

double const areaY = 10.;

double const areaZ = 10.;

double const stepDuration = 0.2;

double const particleCharge = 10.;

double const particleMass = 5.;

int const gridSize = 64;

int const gridX = 64;

int const gridY = 64;

int const gridZ = 64;

int const nbCells = ((gridX * gridY) * gridZ);

double const cellX = (areaX / gridX);

double const cellY = (areaY / gridY);

double const cellZ = (areaZ / gridZ);

int const nbSteps = 100;

int int_of_double(double a) { return ((int)a - (a < 0.)); }

int wrapX(int gridSize, int a) {
  return (((a % gridSize) + gridSize) % gridSize);
}

int const nbCorners = 8;

vect *fields = (vect *)malloc((nbCells * sizeof(vect)));

int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridSize, gridSize, gridSize, i, j, k);
}

int idCellOfPos(vect pos) {
  int ix = int_of_double((pos.x / cellX));
  int iy = int_of_double((pos.y / cellY));
  int iz = int_of_double((pos.z / cellZ));
  return cellOfCoord(ix, iy, iz);
}

double relativePosX(double x) {
  int ix = int_of_double((x / cellX));
  return ((x - (ix * cellX)) / cellX);
}

double relativePosY(double y) {
  int iy = int_of_double((y / cellY));
  return ((y - (iy * cellY)) / cellY);
}

double relativePosZ(double z) {
  int iz = int_of_double((z / cellZ));
  return ((z - (iz * cellZ)) / cellZ);
}

typedef struct {
  int ix;
  int iy;
  int iz;
} coord;

coord coordOfCell(int idCell) {
  int iz = (idCell % gridZ);
  int ixy = (idCell / gridZ);
  int iy = (ixy % gridY);
  int ix = (ixy / gridY);
  return {ix, iy, iz};
}

typedef struct {
  int values[nbCorners];
} int_nbCorners;

typedef struct {
  double values[nbCorners];
} double_nbCorners;

typedef struct {
  vect values[nbCorners];
} vect_nbCorners;

int_nbCorners indicesOfCorners(int idCell) {
  coord coord = coordOfCell(idCell);
  int x = coord.ix;
  int y = coord.iy;
  int z = coord.iz;
  int x2 = wrapX(gridSize, (x + 1));
  int y2 = wrapX(gridSize, (y + 1));
  int z2 = wrapX(gridSize, (z + 1));
  return {cellOfCoord(x, y, z),   cellOfCoord(x, y, z2),
          cellOfCoord(x, y2, z),  cellOfCoord(x, y2, z2),
          cellOfCoord(x2, y, z),  cellOfCoord(x2, y, z2),
          cellOfCoord(x2, y2, z), cellOfCoord(x2, y2, z2)};
}

vect_nbCorners getFieldAtCorners(int idCell) {
  int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners result;
  for (int k = 0; (k < nbCorners); k++) {
    result.values[k] = fields[indices.values[k]];
  }
  return result;
}

void accumulateChargeAtCorners(double *nextCharge, int idCell,
                               double_nbCorners charges) {
  int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; (k < nbCorners); k++) {
    nextCharge[indices.values[k]] += charges.values[k];
  }
}

double_nbCorners cornerInterpolationCoeff(vect pos) {
  double coef_x[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  double sign_x[8] = {(-1.), (-1.), (-1.), (-1.), 1., 1., 1., 1.};
  double coef_y[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
  double sign_y[8] = {(-1.), (-1.), 1., 1., (-1.), (-1.), 1., 1.};
  double coef_z[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
  double sign_z[8] = {(-1.), 1., (-1.), 1., (-1.), 1., (-1.), 1.};
  int ix1 = int_of_double((pos.x / cellX));
  double rx = ((pos.x - (ix1 * cellX)) / cellX);
  int iy1 = int_of_double((pos.y / cellY));
  double ry = ((pos.y - (iy1 * cellY)) / cellY);
  int iz1 = int_of_double((pos.z / cellZ));
  double rz = ((pos.z - (iz1 * cellZ)) / cellZ);
  double_nbCorners r;
  for (int k = 0; (k < nbCorners); k++) {
    r.values[k] =
        (((coef_x[k] + (sign_x[k] * rx)) * (coef_y[k] + (sign_y[k] * ry))) *
         (coef_z[k] + (sign_z[k] * rz)));
  }
  return r;
}

vect vect_matrix_mul(const double_nbCorners coeffs,
                     const vect_nbCorners matrix) {
  vect result = {0., 0., 0.};
  result.x = ((((((((result.x + (coeffs.values[0] * matrix.values[0].x)) +
                    (coeffs.values[1] * matrix.values[1].x)) +
                   (coeffs.values[2] * matrix.values[2].x)) +
                  (coeffs.values[3] * matrix.values[3].x)) +
                 (coeffs.values[4] * matrix.values[4].x)) +
                (coeffs.values[5] * matrix.values[5].x)) +
               (coeffs.values[6] * matrix.values[6].x)) +
              (coeffs.values[7] * matrix.values[7].x));
  result.y = ((((((((result.y + (coeffs.values[0] * matrix.values[0].y)) +
                    (coeffs.values[1] * matrix.values[1].y)) +
                   (coeffs.values[2] * matrix.values[2].y)) +
                  (coeffs.values[3] * matrix.values[3].y)) +
                 (coeffs.values[4] * matrix.values[4].y)) +
                (coeffs.values[5] * matrix.values[5].y)) +
               (coeffs.values[6] * matrix.values[6].y)) +
              (coeffs.values[7] * matrix.values[7].y));
  result.z = ((((((((result.z + (coeffs.values[0] * matrix.values[0].z)) +
                    (coeffs.values[1] * matrix.values[1].z)) +
                   (coeffs.values[2] * matrix.values[2].z)) +
                  (coeffs.values[3] * matrix.values[3].z)) +
                 (coeffs.values[4] * matrix.values[4].z)) +
                (coeffs.values[5] * matrix.values[5].z)) +
               (coeffs.values[6] * matrix.values[6].z)) +
              (coeffs.values[7] * matrix.values[7].z));
  return result;
}

double_nbCorners vect8_mul(double const a, const double_nbCorners v) {
  double_nbCorners result;
  for (int k = 0; (k < nbCorners); k++) {
    result.values[k] = (a * v.values[k]);
  }
  return result;
}

void init(bag *bagsCur, bag *bagsNext, vect *field) {}

void updateFieldUsingNextCharge(double *nextCharge, vect *field) {}

int main() {
  bag *bagsCur = (bag *)malloc((nbCells * sizeof(bag)));
  bag *bagsNext = (bag *)malloc((nbCells * sizeof(bag)));
  double *nextCharge = (double *)malloc((nbCells * sizeof(double)));
  vect *field = (vect *)malloc((nbCells * sizeof(vect)));
  init(bagsCur, bagsNext, field);
  for (int step = 0; (step < nbSteps); step++) {
    updateFieldUsingNextCharge(nextCharge, field);
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      nextCharge[idCell] = 0.;
    }
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      vect_nbCorners field_at_corners = getFieldAtCorners(idCell);
      bag *b = (&bagsCur[idCell]);
      chunk *c = (b->front);
      while (true) {
        int nb = (c->size);
        for (int i = 0; (i < nb); i++) {
          double coef_x[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
          double sign_x[8] = {(-1.), (-1.), (-1.), (-1.), 1., 1., 1., 1.};
          double coef_y[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
          double sign_y[8] = {(-1.), (-1.), 1., 1., (-1.), (-1.), 1., 1.};
          double coef_z[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
          double sign_z[8] = {(-1.), 1., (-1.), 1., (-1.), 1., (-1.), 1.};
          int ix11 = int_of_double(((c->items)[i].pos.x / cellX));
          int iy11 = int_of_double(((c->items)[i].pos.y / cellY));
          int iz11 = int_of_double(((c->items)[i].pos.z / cellZ));
          double rx1 = (((c->items)[i].pos.x - (ix11 * cellX)) / cellX);
          double ry1 = (((c->items)[i].pos.y - (iy11 * cellY)) / cellY);
          double rz1 = (((c->items)[i].pos.z - (iz11 * cellZ)) / cellZ);
          double_nbCorners coeffs;
          for (int k = 0; (k < nbCorners); k++) {
            coeffs.values[k] = (((coef_x[k] + (sign_x[k] * rx1)) *
                                 (coef_y[k] + (sign_y[k] * ry1))) *
                                (coef_z[k] + (sign_z[k] * rz1)));
          }
          double fieldAtPos_x = 0.;
          double fieldAtPos_y = 0.;
          double fieldAtPos_z = 0.;
          fieldAtPos_x =
              ((((((((fieldAtPos_x +
                      (coeffs.values[0] * field_at_corners.values[0].x)) +
                     (coeffs.values[1] * field_at_corners.values[1].x)) +
                    (coeffs.values[2] * field_at_corners.values[2].x)) +
                   (coeffs.values[3] * field_at_corners.values[3].x)) +
                  (coeffs.values[4] * field_at_corners.values[4].x)) +
                 (coeffs.values[5] * field_at_corners.values[5].x)) +
                (coeffs.values[6] * field_at_corners.values[6].x)) +
               (coeffs.values[7] * field_at_corners.values[7].x));
          fieldAtPos_y =
              ((((((((fieldAtPos_y +
                      (coeffs.values[0] * field_at_corners.values[0].y)) +
                     (coeffs.values[1] * field_at_corners.values[1].y)) +
                    (coeffs.values[2] * field_at_corners.values[2].y)) +
                   (coeffs.values[3] * field_at_corners.values[3].y)) +
                  (coeffs.values[4] * field_at_corners.values[4].y)) +
                 (coeffs.values[5] * field_at_corners.values[5].y)) +
                (coeffs.values[6] * field_at_corners.values[6].y)) +
               (coeffs.values[7] * field_at_corners.values[7].y));
          fieldAtPos_z =
              ((((((((fieldAtPos_z +
                      (coeffs.values[0] * field_at_corners.values[0].z)) +
                     (coeffs.values[1] * field_at_corners.values[1].z)) +
                    (coeffs.values[2] * field_at_corners.values[2].z)) +
                   (coeffs.values[3] * field_at_corners.values[3].z)) +
                  (coeffs.values[4] * field_at_corners.values[4].z)) +
                 (coeffs.values[5] * field_at_corners.values[5].z)) +
                (coeffs.values[6] * field_at_corners.values[6].z)) +
               (coeffs.values[7] * field_at_corners.values[7].z));
          vect accel = {((particleCharge / particleMass) * fieldAtPos_x),
                        ((particleCharge / particleMass) * fieldAtPos_y),
                        ((particleCharge / particleMass) * fieldAtPos_z)};
          (c->items)[i].speed.x =
              ((c->items)[i].speed.x + (stepDuration * accel.x));
          (c->items)[i].speed.y =
              ((c->items)[i].speed.y + (stepDuration * accel.y));
          (c->items)[i].speed.z =
              ((c->items)[i].speed.z + (stepDuration * accel.z));
          (c->items)[i].pos.x =
              ((c->items)[i].pos.x + (stepDuration * (c->items)[i].speed.x));
          (c->items)[i].pos.y =
              ((c->items)[i].pos.y + (stepDuration * (c->items)[i].speed.y));
          (c->items)[i].pos.z =
              ((c->items)[i].pos.z + (stepDuration * (c->items)[i].speed.z));
          int ix2 = int_of_double(((c->items)[i].pos.x / cellX));
          int iy2 = int_of_double(((c->items)[i].pos.y / cellY));
          int iz2 = int_of_double(((c->items)[i].pos.z / cellZ));
          int idCell2 = cellOfCoord(ix2, iy2, iz2);
          if (ANY_BOOL()) {
            chunk *c1 = ((&bagsNext[idCell2])->front);
            int index1 = (c1->size)++;
            (c1->items)[index1].pos.x = (c->items)[i].pos.x;
            (c1->items)[index1].pos.y = (c->items)[i].pos.y;
            (c1->items)[index1].pos.z = (c->items)[i].pos.z;
            (c1->items)[index1].speed.x = (c->items)[i].speed.x;
            (c1->items)[index1].speed.y = (c->items)[i].speed.y;
            (c1->items)[index1].speed.z = (c->items)[i].speed.z;
            if ((index1 == (CHUNK_SIZE - 1))) {
              bag_add_front_chunk((&bagsNext[idCell2]));
            }
          } else {
            chunk *c1;
            int index1;
            while (true) {
              c1 = ((&bagsNext[idCell2])->front);
              index1 = (c1->size)++;
              if ((index1 < CHUNK_SIZE)) {
                (c1->items)[index1].pos.x = (c->items)[i].pos.x;
                (c1->items)[index1].pos.y = (c->items)[i].pos.y;
                (c1->items)[index1].pos.z = (c->items)[i].pos.z;
                (c1->items)[index1].speed.x = (c->items)[i].speed.x;
                (c1->items)[index1].speed.y = (c->items)[i].speed.y;
                (c1->items)[index1].speed.z = (c->items)[i].speed.z;
                if ((index1 == (CHUNK_SIZE - 1))) {
                  bag_add_front_chunk((&bagsNext[idCell2]));
                }
                goto exit_body;
              } else {
                (c1->size) = CHUNK_SIZE;
                while ((atomic_read((&((&bagsNext[idCell2])->front))) == c1)) {
                }
              }
            }
          exit_body:;
          }
          int ix12 = int_of_double(((c->items)[i].pos.x / cellX));
          int iy12 = int_of_double(((c->items)[i].pos.y / cellY));
          int iz12 = int_of_double(((c->items)[i].pos.z / cellZ));
          double rx2 = (((c->items)[i].pos.x - (ix12 * cellX)) / cellX);
          double ry2 = (((c->items)[i].pos.y - (iy12 * cellY)) / cellY);
          double rz2 = (((c->items)[i].pos.z - (iz12 * cellZ)) / cellZ);
          double_nbCorners coeffs2;
          double_nbCorners result1;
          int_nbCorners indices1 = indicesOfCorners(idCell2);
          for (int k = 0; (k < nbCorners); k++) {
            nextCharge[indices1.values[k]] +=
                (particleCharge * (((coef_x[k] + (sign_x[k] * rx2)) *
                                    (coef_y[k] + (sign_y[k] * ry2))) *
                                   (coef_z[k] + (sign_z[k] * rz2))));
          }
        }
        chunk *cnext = (c->next);
        if ((cnext != NULL)) {
          chunk_free(c);
          c = cnext;
        } else {
          (c->size) = 0;
          (b->front) = c;
          (b->back) = c;
          (c->next) = NULL;
          break;
        }
      }
    }
    for (int idCell = 0; (idCell < nbCells); idCell++) {
      bag_swap((&bagsCur[idCell]), (&bagsNext[idCell]));
    }
  }
}