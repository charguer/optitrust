#include <stdlib.h>

void test_ref() {
  int a = 3;
  int r = a + a;
}

void test_nonconst() { int r = 3 + 3; }

void test_nonconst_fail() {
  int a = 3;
  a = 3;
  int r = a + a;
}

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
  chunk* next;
  int size;
  particle items[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk* front;
  chunk* back;
} bag;

int main() {
  bag* b = (bag*)malloc((long unsigned int)100 * sizeof(bag));
  chunk* c = b->front;
  int nb = c->size;
  for (int i = 0; i < nb; i++) {
    vect f = {0, 0, 0};
    c->items[i].pos = f;
    c->items[i].speed = f;
    c->items[i].pos.x = 0;
    c->items[i].pos.y = 0;
    c->items[i].pos.z = 0;
    c->items[i].speed.x = 0;
    c->items[i].speed.y = 0;
    c->items[i].speed.z = 0;
  }
  return 0;
}
