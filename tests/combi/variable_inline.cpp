#include "../../include/optitrust.h"

const int CHUNK_SIZE = 10;
typedef struct {
  int x;
  int y;
  int z;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;
typedef struct chunk {
  chunk *next;
  int size;
  particle items[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;




int main() {

  bag *b = (bag*) malloc (100 * sizeof (bag));
  chunk* c = b-> front;

  int nb = (c ->size);
  for (int i = 0; i < nb; i++){
     particle* const p = &(c ->items[i]);

    vect f = {0,0,0};
    (p->pos) = f;   (*p).pos
    (p->speed) = f;

    (p -> pos).x = 0;
    (p -> pos).y = 0;
    (p -> pos).z = 0;

    ((*p).speed).x = 0;
    ((*p).speed).y = 0;
    ((*p).speed).z = 0;
  }


  // vect v = {0,0};
  // particle p = {{0,0},{0,0}};

  // vect u = p.pos;
  // int x = p.pos.x;

  // particle p2 = p;



  return 0;
}

