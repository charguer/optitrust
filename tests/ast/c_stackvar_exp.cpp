#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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
  return (vect){v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

vect vect_mul(double d, vect v) { return (vect){d * v.x, d * v.y, d * v.z}; }

const int CHUNK_SIZE = 128;

typedef struct chunk {
  struct chunk* next;
  int size;
  particle items[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk* front;
  chunk* back;
} bag;

chunk* atomic_read(chunk** p) {
  chunk* value;
  set(value, get(p));
  return value;
}

chunk* chunk_alloc() { return (chunk*)malloc(sizeof(chunk)); }

void chunk_free(chunk* c) { free(c); }

void bag_init(bag* b, int id_bag, int id_cell) {
  chunk* c = chunk_alloc();
  set(c->size, 0);
  set(c->next, NULL);
  set(b->front, c);
  set(b->back, c);
}

void bag_append(bag* b, bag* other, int id_bag, int id_cell) {
  if (other->front) {
    set(b->back->next, other->front);
    set(b->back, other->back);
    bag_init(other, id_bag, id_cell);
  }
}

void bag_nullify(bag* b) {
  set(b->front, NULL);
  set(b->back, NULL);
}

int bag_size(bag* b) {
  chunk* c = b->front;
  int size = 0;
  while (c) {
    += (size, c->size);
    set(c, c->next);
  }
  return size;
}

void bag_add_front_chunk(bag* b) {
  chunk* c = chunk_alloc();
  set(c->size, 0);
  set(c->next, b->front);
  set(b->front, c);
}

void bag_push_concurrent(bag* b, particle p) {
  chunk* c;
  int index;
  while (true) {
    set(c, b->front);
    set(index, c->size++);
    if (index < CHUNK_SIZE) {
      set(c->items[index], p);
      if (index == CHUNK_SIZE - 1) {
        bag_add_front_chunk(b);
      }
      return;
    } else {
      set(c->size, CHUNK_SIZE);
      while (atomic_read(&b->front) == c) {
      }
    }
  }
}

void bag_push_serial(bag* b, particle p) {
  chunk* c = b->front;
  int index = c->size++;
  set(c->items[index], p);
  if (index == CHUNK_SIZE - 1) {
    bag_add_front_chunk(b);
  }
}

void bag_push(bag* b, particle p) { bag_push_serial(b, p); }

void bag_swap(bag* b1, bag* b2) {
  bag temp = get(b1);
  set(get(b1), get(b2));
  set(get(b2), temp);
}

typedef struct bag_iter {
  chunk* iter_chunk;
  int size;
  int index;
} bag_iter;

void bag_iter_load_chunk(bag_iter* it, chunk* c) {
  set(it->iter_chunk, c);
  set(it->size, c->size);
  set(it->index, 0);
}

void bag_iter_init(bag_iter* it, bag* b) { bag_iter_load_chunk(it, b->front); }

particle* bag_iter_get(bag_iter* it) {
  return &it->iter_chunk->items[it->index];
}

chunk* bag_iter_get_chunk(bag_iter* it) { return it->iter_chunk; }

particle* bag_iter_begin(bag_iter* it, bag* b) {
  bag_iter_init(it, b);
  return bag_iter_get(it);
}

chunk* chunk_next(chunk* c, bool destructive) {
  chunk* cnext = c->next;
  if (destructive) {
    chunk_free(c);
  }
  return cnext;
}

particle* bag_iter_next(bag_iter* it, bool destructive) {
  it->index++;
  if (it->index == it->size) {
    chunk* c = it->iter_chunk;
    chunk* cnext = chunk_next(c, destructive);
    if (cnext == NULL) {
      return NULL;
    }
    bag_iter_load_chunk(it, cnext);
  }
  return bag_iter_get(it);
}

void bag_ho_iter_basic(bag* b, void body(particle*)) {
  bag_iter it;
  for (particle* p = bag_iter_begin(&it, b); p != NULL;
       set(p, bag_iter_next(&it, true))) {
    body(p);
  }
}

void bag_ho_iter_chunk(bag* b, void body(particle*)) {
  for (chunk* c = b->front; c != NULL; set(c, chunk_next(c, true))) {
    int nb = c->size;
    for (int i = 0; i < nb; i++) {
      particle* p = &c->items[i];
      body(p);
    }
  }
}

void bag_push_initial(bag* b, particle p) { bag_push_serial(b, p); }

void bag_init_initial(bag* b) { bag_init(b, -1, -1); }

unsigned int FREELIST_SIZE;

int** free_index;

chunk*** free_chunks;

chunk** all_free_chunks;

int number_of_spare_chunks_per_parity;

int** spare_chunks_ids;

int num_threads;

int* cumulative_free_indexes;

int bag_last_spare_chunk_to_be_used;

chunk* manual_chunk_alloc(int thread_id) {
  if (free_index[thread_id][0] > 0) {
    return free_chunks[thread_id][--free_index[thread_id][0]];
  } else {
    return (chunk*)malloc(sizeof(chunk));
  }
}

void manual_chunk_free(chunk* c, int thread_id) {
  if (free_index[thread_id][0] < FREELIST_SIZE) {
    set(free_chunks[thread_id][free_index[thread_id][0]++], c);
  } else {
    free(c);
  }
}

const int THREAD_INITIAL = -1;

const int THREAD_ZERO = 0;

chunk* manual_obtain_chunk_initial() {
  if (free_index[THREAD_ZERO][0] < 1) {
    fprintf(stderr,
            "Not enough chunks in all_free_chunks. Check its allocation.\n");
    exit(1);
  }
  return all_free_chunks[--free_index[THREAD_ZERO][0]];
}

chunk* manual_obtain_chunk(int id_bag, int id_cell, int thread_id) {
  if (thread_id == THREAD_INITIAL) {
    return manual_obtain_chunk_initial();
  }
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (set(k, num_threads - 1); k >= 0; k--)
    if (cumulative_free_indexes[k] <= id_chunk) break;
  int offset = id_chunk - cumulative_free_indexes[k];
  if (offset < 0 || offset >= free_index[k][0]) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf(
        "Maybe did you forgot to call compute_cumulative_free_list_sizes "
        "and/or update_free_list_sizes ?\n");
    exit(1);
  }
  chunk* c = free_chunks[k][free_index[k][0] - 1 - offset];
  return c;
}

void compute_cumulative_free_list_sizes() {
  int k;
  set(cumulative_free_indexes[0], 0);
  for (set(k, 1); k < num_threads; k++)
    set(cumulative_free_indexes[k],
        cumulative_free_indexes[k - 1] + free_index[k - 1][0]);
  int nb_free_chunks =
      cumulative_free_indexes[num_threads - 1] + free_index[num_threads - 1][0];
  if (nb_free_chunks < number_of_spare_chunks_per_parity) {
    int nb_chunks_to_allocate =
        number_of_spare_chunks_per_parity - nb_free_chunks;
    printf(
        "Not enough free chunks in the free lists ! We must malloc %d "
        "chunks.\n",
        nb_chunks_to_allocate);
    int nb_allocated_chunks = 0;
    while (nb_allocated_chunks < nb_chunks_to_allocate) {
      set(free_chunks[THREAD_ZERO][free_index[THREAD_ZERO][0]++],
          (chunk*)malloc(sizeof(chunk)));
      nb_allocated_chunks++;
    }
    compute_cumulative_free_list_sizes();
  }
  for (set(k, num_threads - 1); k >= 0; k--)
    if (cumulative_free_indexes[k] <= number_of_spare_chunks_per_parity) {
      set(bag_last_spare_chunk_to_be_used, k);
      return;
    }
}

void update_free_list_sizes() {
  for (int i = 0; i < bag_last_spare_chunk_to_be_used; i++)
    set(free_index[i][0], 0);
  -= (free_index[bag_last_spare_chunk_to_be_used][0],
      number_of_spare_chunks_per_parity -
          cumulative_free_indexes[bag_last_spare_chunk_to_be_used]);
}

bag* CHOOSE(int nb, bag* b1, bag* b2) { return b1; }

const double areaX = 10.;

const double areaY = 10.;

const double areaZ = 10.;

const double stepDuration = 0.2;

const double particleCharge = 10.;

const double particleMass = 5.;

const int gridX = 64;

const int gridY = 64;

const int gridZ = 64;

const int nbCells = gridX * gridY * gridZ;

const double cellX = areaX / gridX;

const double cellY = areaY / gridY;

const double cellZ = areaZ / gridZ;

const int nbSteps = 100;

int int_of_double(double a) { return (int)a - (a < 0.); }

int wrap(int gridSize, int a) { return (a % gridSize + gridSize) % gridSize; }

const int nbCorners = 8;

vect* fields = (vect*)malloc(nbCells * sizeof(vect));

int MINDEX3(int N1, int N2, int N3, int i1, int i2, int i3) {
  return i1 * N2 * N3 + i2 * N2 + i3;
}

int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX, gridY, gridZ, i, j, k);
}

int idCellOfPos(vect pos) {
  int iX = int_of_double(pos.x / cellX);
  int iY = int_of_double(pos.y / cellY);
  int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
}

double relativePosX(double x) {
  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}

double relativePosY(double y) {
  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}

double relativePosZ(double z) {
  int iZ = int_of_double(z / cellZ);
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
  int v[nbCorners];
} int_nbCorners;

typedef struct {
  double v[nbCorners];
} double_nbCorners;

typedef struct {
  vect v[nbCorners];
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

vect_nbCorners getFieldAtCorners(int idCell, vect* field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    set(res.v[k], field[indices.v[k]]);
  }
  return res;
}

void accumulateChargeAtCorners(double* nextCharge, int idCell,
                               double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < nbCorners; k++) {
    += (nextCharge[indices.v[k]], charges.v[k]);
  }
}

double_nbCorners cornerInterpolationCoeff(vect pos) {
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  double_nbCorners r;
  set(r.v[0], cX * cY * cZ);
  set(r.v[1], cX * cY * rZ);
  set(r.v[2], cX * rY * cZ);
  set(r.v[3], cX * rY * rZ);
  set(r.v[4], rX * cY * cZ);
  set(r.v[5], rX * cY * rZ);
  set(r.v[6], rX * rY * cZ);
  set(r.v[7], rX * rY * rZ);
  return r;
}

vect matrix_vect_mul(const double_nbCorners coeffs,
                     const vect_nbCorners matrix) {
  vect res = {0., 0., 0.};
  for (int k = 0; k < nbCorners; k++) {
    set(res, vect_add(res, vect_mul(coeffs.v[k], matrix.v[k])));
  }
  return res;
}

double_nbCorners vect8_mul(const double a, const double_nbCorners data) {
  double_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    set(res.v[k], a * data.v[k]);
  }
  return res;
}

void init(bag* bagsCur, bag* bagsNext, vect* field) {}

void updateFieldUsingNextCharge(double* nextCharge, vect* field) {}

int main() {
  bag* bagsCur = (bag*)malloc(nbCells * sizeof(bag));
  bag* bagsNext = (bag*)malloc(nbCells * sizeof(bag));
  double* nextCharge = (double*)malloc(nbCells * sizeof(double));
  vect* field = (vect*)malloc(nbCells * sizeof(vect));
  init(bagsCur, bagsNext, field);
  for (int step = 0; step < nbSteps; step++) {
    updateFieldUsingNextCharge(nextCharge, field);
    for (int idCell = 0; idCell < nbCells; idCell++) {
      set(nextCharge[idCell], 0.);
    }
    for (int idCell = 0; idCell < nbCells; idCell++) {
      vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
      bag* b = &bagsCur[idCell];
      bag_iter bag_it;
      for (particle* p = bag_iter_begin(&bag_it, b); p != NULL;
           set(p, bag_iter_next(&bag_it, true))) {
        double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
        vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);
        vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
        vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
        vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
        particle p2 = {pos2, speed2};
        int idCell2 = idCellOfPos(pos2);
        bag_push(&bagsNext[idCell2], p2);
        double_nbCorners coeffs2 = cornerInterpolationCoeff(pos2);
        double_nbCorners deltaChargeOnCorners =
            vect8_mul(particleCharge, coeffs2);
        accumulateChargeAtCorners(nextCharge, idCell2, deltaChargeOnCorners);
      }
      bag_init_initial(b);
    }
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
    }
  }
}
