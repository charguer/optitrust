#ifndef PARTICLE_H
#define PARTICLE_H

#include "mymacros.h"
#include "optitrust.h"

typedef struct {
  double x, y, z;
} vect;

typedef struct {
  vect pos;
  vect speed;
  CHECKER_ONLY(int id;)
} particle;

// --------- Vector operations

vect vect_add(vect v1, vect v2);
vect vect_mul(double d, vect v);

#endif
