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
  alignas(16) int itemsWeight[2];
  alignas(16) int itemsPosX[2];
  alignas(16) int itemsPosY[2];
  alignas(16) int itemsSpeedX[2];
  alignas(16) int itemsSpeedY[2];
} chunk;
