#include "common.h"

const int BLOCK_SIZE = (1 << 10);

const int NB_BLOCKS = (NUM_PARTICLES / BLOCK_SIZE);

typedef struct {
  float x;
  float y;
  float z;
  float vx;
  float vy;
  float vz;
  float c;
  float m;
  float v;
} particle;

typedef struct {
  float x[1024];
  float y[1024];
  float z[1024];
  float vx[1024];
  float vy[1024];
  float vz[1024];
  float c[1024];
  float m[1024];
  float v[1024];
} particle_block;

particle_block *data;

void *my_alloc(int nb_elts, int size_elt) {
  return calloc_aligned(ALIGNMENT, (nb_elts * size_elt));
}

int main(int argc, char **argv) {
  if ((argc < 2)) {
    return 1;
  }
  char *mode = argv[1];
  data = (particle_block *)my_alloc(NB_BLOCKS, sizeof(particle_block));
  start_clock();
  if ((strcmp(mode, "updates") == 0)) {
    for (int i1 = 0; (i1 < NB_BLOCKS); i1++) {
      for (int i2 = 0; (i2 < BLOCK_SIZE); i2++) {
        data[i1].x[i2] += data[i1].vx[i2];
        data[i1].y[i2] += (data[i1].vy[i2] + (1.33 * data[i1].c[i2]));
      }
    }
  } else if ((strcmp(mode, "populate") == 0)) {
    srand(time(NULL));
    for (int k = 0; (k < NB_POPULATE); k++) {
      int i = (rand() % NUM_PARTICLES);
      int i1 = (i / BLOCK_SIZE);
      int i2 = (i % BLOCK_SIZE);
      data[i1].x[i2] = (float)rand();
      data[i1].y[i2] = (float)rand();
      data[i1].z[i2] = (float)rand();
      data[i1].vx[i2] = 0.;
      data[i1].vy[i2] = 0.;
      data[i1].vz[i2] = 0.;
      data[i1].c[i2] = 1.;
      data[i1].m[i2] = 1.;
      data[i1].v[i2] = 1.;
    }
  }
  stop_clock_and_report();
  free(data);
  return 0;
}
