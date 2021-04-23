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

  typedef particle particle_block[1024];

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
              set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                                x),
                  ((*struct_access(
                       array_access(array_access(data, (*i1)), (*i2)), x)) +
                   (*struct_access(
                       array_access(array_access(data, (*i1)), (*i2)), vx))));
              set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                                y),
                  ((*struct_access(
                       array_access(array_access(data, (*i1)), (*i2)), y)) +
                   ((*struct_access(
                        array_access(array_access(data, (*i1)), (*i2)), vy)) +
                    (1.33 * (*struct_access(
                                array_access(array_access(data, (*i1)), (*i2)),
                                c))))));
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
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              x),
                (float)rand());
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              y),
                (float)rand());
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              z),
                (float)rand());
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              vx),
                0.);
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              vy),
                0.);
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              vz),
                0.);
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              c),
                1.);
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              m),
                1.);
            set(struct_access(array_access(array_access(data, (*i1)), (*i2)),
                              v),
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
