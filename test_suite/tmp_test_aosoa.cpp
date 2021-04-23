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

typedef particle particle_block[1024];

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
        data[i1][i2].x += data[i1][i2].vx;
        data[i1][i2].y += (data[i1][i2].vy + (1.33 * data[i1][i2].c));
      }
    }
  } else if ((strcmp(mode, "populate") == 0)) {
    srand(time(NULL));
    for (int k = 0; (k < NB_POPULATE); k++) {
      int i = (rand() % NUM_PARTICLES);
      int i1 = (i / BLOCK_SIZE);
      int i2 = (i % BLOCK_SIZE);
      data[i1][i2].x = (float)rand();
      data[i1][i2].y = (float)rand();
      data[i1][i2].z = (float)rand();
      data[i1][i2].vx = 0.;
      data[i1][i2].vy = 0.;
      data[i1][i2].vz = 0.;
      data[i1][i2].c = 1.;
      data[i1][i2].m = 1.;
      data[i1][i2].v = 1.;
    }
  }
  stop_clock_and_report();
  free(data);
  return 0;
}
