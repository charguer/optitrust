#include "../../include/optitrust.h"

typedef struct {
  
  double itemsPosX[10];
  double itemsPosY[10];
  double itemsPosZ[10];
  double itemsSpeedX[10];
  double itemsSpeedY[10];
  double itemsSpeedZ[10];
} chunk;


int main() {
  chunk* c;
  c->itemsPosX[0] = 0;
  c->itemsPosY[0] = 0;
  c->itemsPosZ[0] = 0;
  
  c->itemsSpeedX[0] = c->itemsPosX[0];
  c->itemsSpeedY[0] = c->itemsPosY[0];
  c->itemsSpeedZ[0] = c->itemsPosZ[0];

  
  c->itemsPosX[0] = c->itemsSpeedX[0];
  c->itemsPosY[0] = c->itemsSpeedY[0];
  c->itemsPosZ[0] = c->itemsSpeedZ[0];

  return 0;
  
}
