#include "particle.h"

vect vect_add(vect v1, vect v2) {
  return (vect) { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(double d, vect v) {
  return (vect) { d * v.x, d * v.y, d * v.z };
}
