#include "common.h"

{

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

  { const particle_block **data = new particle_block *; }

  void *my_alloc(int nb_elts, int size_elt) {
    {
      delete data;
      delete time_start;
      return calloc_aligned(ALIGNMENT, (nb_elts * size_elt));
    }
  }

  int main(int argc, char **argv) {
    if ((argc < 2)) {
      {
        delete data;
        delete time_start;
        return 1;
      }
    }
    {
      const char **mode = new char *;
      set(mode, argv[1]);
    }
    set(data, (particle_block *)my_alloc(NB_BLOCKS, sizeof(particle_block)));
    start_clock();
    if ((strcmp((*mode), "updates") == 0)) {
      {
        for ({
               const int *i1 = new int;
               set(i1, 0);
             };
             ((*i1) < NB_BLOCKS); operator++(i1)) {
          {
            for ({
                   const int *i2 = new int;
                   set(i2, 0);
                 };
                 ((*i2) < BLOCK_SIZE); operator++(i2)) {
              set(array_access(struct_access(array_access(data, (*i1)), x),
                               (*i2)),
                  ((*array_access(struct_access(array_access(data, (*i1)), x),
                                  (*i2))) +
                   (*array_access(struct_access(array_access(data, (*i1)), vx),
                                  (*i2)))));
              set(array_access(struct_access(array_access(data, (*i1)), y),
                               (*i2)),
                  ((*array_access(struct_access(array_access(data, (*i1)), y),
                                  (*i2))) +
                   ((*array_access(struct_access(array_access(data, (*i1)), vy),
                                   (*i2))) +
                    (1.33 *
                     (*array_access(struct_access(array_access(data, (*i1)), c),
                                    (*i2)))))));
            }
            delete i2;
          }
        }
        delete i1;
      }
    } else if ((strcmp((*mode), "populate") == 0)) {
      srand(time(NULL));
      {
        for ({
               const int *k = new int;
               set(k, 0);
             };
             ((*k) < NB_POPULATE); operator++(k)) {
          {
            {
              const int *i = new int;
              set(i, (rand() % NUM_PARTICLES));
            }
            {
              const int *i1 = new int;
              set(i1, ((*i) / BLOCK_SIZE));
            }
            {
              const int *i2 = new int;
              set(i2, ((*i) % BLOCK_SIZE));
            }
            set(array_access(struct_access(array_access(data, (*i1)), x),
                             (*i2)),
                (float)rand());
            set(array_access(struct_access(array_access(data, (*i1)), y),
                             (*i2)),
                (float)rand());
            set(array_access(struct_access(array_access(data, (*i1)), z),
                             (*i2)),
                (float)rand());
            set(array_access(struct_access(array_access(data, (*i1)), vx),
                             (*i2)),
                0.);
            set(array_access(struct_access(array_access(data, (*i1)), vy),
                             (*i2)),
                0.);
            set(array_access(struct_access(array_access(data, (*i1)), vz),
                             (*i2)),
                0.);
            set(array_access(struct_access(array_access(data, (*i1)), c),
                             (*i2)),
                1.);
            set(array_access(struct_access(array_access(data, (*i1)), m),
                             (*i2)),
                1.);
            set(array_access(struct_access(array_access(data, (*i1)), v),
                             (*i2)),
                1.);
          }
          delete i2;
          delete i1;
          delete i;
        }
        delete k;
      }
    }
    stop_clock_and_report();
    free((*data));
    {
      delete mode;
      delete data;
      delete time_start;
      return 0;
    }
  }
  delete data;
  delete time_start;
}
