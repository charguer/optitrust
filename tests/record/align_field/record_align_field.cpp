#include <stdalign.h>
typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int posX;
  int posY;
  int speedX;
  int speedY;
} particle;

typedef struct chunk {
  struct chunk *next;
  int size;
  int itemsWeight[2];
  int itemsPosX[2];
  int itemsPosY[2];
  int itemsSpeedX[2];
  int itemsSpeedY[2];
} chunk;
