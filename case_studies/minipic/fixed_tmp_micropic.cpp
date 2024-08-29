#include <optitrust.h>



typedef struct {

  double x;
  double y;
  double z;
} vect ;

template <typename T, typename U>  U* __struct_access_x (T* v)  {
  __pure();
  __admitted();
  return &v->x;
}

template <typename T, typename U>  U __struct_get_x (T v)  {
  __pure();
  __admitted();
  return v.x;
}

template <typename T, typename U>  U* __struct_access_y (T* v)  {
  __pure();
  __admitted();
  return &v->y;
}

template <typename T, typename U>  U __struct_get_y (T v)  {
  __pure();
  __admitted();
  return v.y;
}

template <typename T, typename U>  U* __struct_access_z (T* v)  {
  __pure();
  __admitted();
  return &v->z;
}

template <typename T, typename U>  U __struct_get_z (T v)  {
  __pure();
  __admitted();
  return v.z;
}

  vect vect_add (vect v1, vect v2)  {
  __pure();
  __admitted();
  return ((vect) {v1.x + v2.x, v1.y + v2.y, v1.z + v2.z});
}

  vect vect_mul (double d, vect v)  {
  __pure();
  __admitted();
  return ((vect) {d * v.x, d * v.y, d * v.z});
}

typedef struct {

  vect pos;
  vect speed;
} particle ;

template <typename T, typename U>  U* __struct_access_pos (T* v)  {
  __pure();
  __admitted();
  return &v->pos;
}

template <typename T, typename U>  U __struct_get_pos (T v)  {
  __pure();
  __admitted();
  return v.pos;
}

template <typename T, typename U>  U* __struct_access_speed (T* v)  {
  __pure();
  __admitted();
  return &v->speed;
}

template <typename T, typename U>  U __struct_get_speed (T v)  {
  __pure();
  __admitted();
  return v.speed;
}

template <typename T, typename U>  U* __struct_access_charge (T* v)  {
  __pure();
  __admitted();
  return &v->charge;
}

template <typename T, typename U>  U __struct_get_charge (T v)  {
  __pure();
  __admitted();
  return v.charge;
}

template <typename T, typename U>  U* __struct_access_mass (T* v)  {
  __pure();
  __admitted();
  return &v->mass;
}

template <typename T, typename U>  U __struct_get_mass (T v)  {
  __pure();
  __admitted();
  return v.mass;
}

const double areaX = 10.;

const double areaY = 10.;

const double areaZ = 10.;

const int gridX = 64;

const int gridY = 64;

const int gridZ = 64;

const int nbCells = gridX * gridY * gridZ;

const double cellX = areaX / gridX;

const double cellY = areaY / gridY;

const double cellZ = areaZ / gridZ;

  int int_of_double (double a)  {
  __pure();
  __admitted();
  return (int) a - (a < 0.);
}

  double relativePosX (double x)  {
  __pure();
  __admitted();
  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}

  double relativePosY (double y)  {
  __pure();
  __admitted();
  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}

  double relativePosZ (double z)  {
  __pure();
  __admitted();
  int iZ = int_of_double(z / cellZ);
  return (z - iZ * cellZ) / cellZ;
}

const int nbCorners = 8;

  double* cornerInterpolationCoeff (vect pos)  {
  __produces("_Res ~> Matrix1(nbCorners)");
  __admitted();
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + - 1. * rX;
  const double cY = 1. + - 1. * rY;
  const double cZ = 1. + - 1. * rZ;
  double* const r = (double* const) MALLOC1(nbCorners, sizeof(double ));
  r[0] = cX * cY * cZ;
  r[1] = cX * cY * rZ;
  r[2] = cX * rY * cZ;
  r[3] = cX * rY * rZ;
  r[4] = rX * cY * cZ;
  r[5] = rX * cY * rZ;
  r[6] = rX * rY * cZ;
  r[7] = rX * rY * rZ;
  return r;
}

  vect matrix_vect_mul (double* coeffs, vect* matrix)  {
  __requires("#_1: _Fraction");
  __requires("#_2: _Fraction");
  __consumes("_RO(#_1, coeffs ~> Matrix1(nbCorners))");
  __consumes("_RO(#_2, matrix ~> Matrix1(nbCorners))");
  __produces("_RO(#_1, coeffs ~> Matrix1(nbCorners))");
  __produces("_RO(#_2, matrix ~> Matrix1(nbCorners))");
  vect res = ((vect) {0., 0., 0.});
  for (int k = 0; k < nbCorners; k++) {
    __strict();
    __requires("#_3: _Fraction");
    __requires("#_4: _Fraction");
    __smodifies("&res ~> Cell");
    __xconsumes("_RO(#_3, &coeffs[MINDEX1(nbCorners, k)] ~> Cell)");
    __xconsumes("_RO(#_4, &matrix[MINDEX1(nbCorners, k)] ~> Cell)");
    __xproduces("_RO(#_3, &coeffs[MINDEX1(nbCorners, k)] ~> Cell)");
    __xproduces("_RO(#_4, &matrix[MINDEX1(nbCorners, k)] ~> Cell)");
    res = vect_add(res, vect_mul(coeffs[MINDEX1(nbCorners, k)], matrix[MINDEX1(nbCorners, k)]));
  }
  __admitted();
  return res;
}

  void simulate_single_cell (double stepDuration, particle* particles, int nbParticles, vect* fieldAtCorners, int nbSteps, double particleCharge, double particleMass
)  {
  __requires("#_1: _Fraction");
  __consumes("particles ~> Matrix1(nbParticles)");
  __consumes("_RO(#_1, fieldAtCorners ~> Matrix1(nbCorners))");
  __produces("particles ~> Matrix1(nbParticles)");
  __produces("_RO(#_1, fieldAtCorners ~> Matrix1(nbCorners))");
  vect* const lFieldAtCorners = (vect*) MALLOC1(nbCorners, sizeof(vect));
  __ghost([&] ()   {
    __consumes("_Uninit(lFieldAtCorners ~> Matrix1(nbCorners))");
    __produces("lFieldAtCorners ~> Matrix1(nbCorners)");
    __admitted();
    __with("justif := shift_groups");
  }, "");
  for (int i1 = 0; i1 < nbCorners; i1++) {
    __strict();
    __requires("#_2: _Fraction");
    __xconsumes("_Uninit(&lFieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
    __xconsumes("_RO(#_2, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
    __xproduces("&lFieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell");
    __xproduces("_RO(#_2, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
    __ghost([&] ()   {
      __consumes("_Uninit(&lFieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __produces("_Uninit(&lFieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell)");
      __produces("_Uninit(&lFieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell)");
      __produces("_Uninit(&lFieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell)");
      __admitted();
    }, "");
    lFieldAtCorners[MINDEX1(nbCorners, i1)].x = fieldAtCorners[MINDEX1(nbCorners, i1)].x * (
      particleCharge / particleMass * (stepDuration * stepDuration));
    lFieldAtCorners[MINDEX1(nbCorners, i1)].y = fieldAtCorners[MINDEX1(nbCorners, i1)].y * (
      particleCharge / particleMass * (stepDuration * stepDuration));
    lFieldAtCorners[MINDEX1(nbCorners, i1)].z = fieldAtCorners[MINDEX1(nbCorners, i1)].z * (
      particleCharge / particleMass * (stepDuration * stepDuration));
    __ghost([&] ()   {
      __consumes("&lFieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell");
      __consumes("&lFieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell");
      __consumes("&lFieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell");
      __produces("&lFieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].x ~> Cell)");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].y ~> Cell)");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell), _RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell))");
      __consumes("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)].z ~> Cell)");
      __produces("_RO(#_3, &fieldAtCorners[MINDEX1(nbCorners, i1)] ~> Cell)");
      __admitted();
    }, "");
  }
  particle* const lParticles = (particle*) MALLOC1(nbParticles, sizeof(particle));
  __ghost([&] ()   {
    __consumes("_Uninit(lParticles ~> Matrix1(nbParticles))");
    __produces("lParticles ~> Matrix1(nbParticles)");
    __admitted();
    __with("justif := shift_groups");
  }, "");
  for (int i1 = 0; i1 < nbParticles; i1++) {
    __strict();
    __requires("#_2: _Fraction");
    __xconsumes("_Uninit(&lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __xconsumes("_RO(#_2, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __xproduces("&lParticles[MINDEX1(nbParticles, i1)] ~> Cell");
    __xproduces("_RO(#_2, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __ghost([&] ()   {
      __consumes("_Uninit(&lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __admitted();
    }, "");
    lParticles[MINDEX1(nbParticles, i1)].pos.x = particles[MINDEX1(nbParticles, i1)].pos.x;
    lParticles[MINDEX1(nbParticles, i1)].pos.y = particles[MINDEX1(nbParticles, i1)].pos.y;
    lParticles[MINDEX1(nbParticles, i1)].pos.z = particles[MINDEX1(nbParticles, i1)].pos.z;
    __ghost([&] ()   {
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell");
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell");
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell");
      __produces("&lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __produces("_Uninit(&lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __admitted();
    }, "");
    lParticles[MINDEX1(nbParticles, i1)].speed.x = particles[MINDEX1(nbParticles, i1)].speed.x;
    lParticles[MINDEX1(nbParticles, i1)].speed.y = particles[MINDEX1(nbParticles, i1)].speed.y;
    lParticles[MINDEX1(nbParticles, i1)].speed.z = particles[MINDEX1(nbParticles, i1)].speed.z;
    __ghost([&] ()   {
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell");
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell");
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell");
      __produces("&lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell");
      __consumes("&lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell");
      __produces("&lParticles[MINDEX1(nbParticles, i1)] ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell), _RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __consumes("_RO(#_3, &particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("_RO(#_3, &particles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __admitted();
    }, "");
  }
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    __strict();
    __smodifies("lParticles ~> Matrix1(nbParticles)");
    __sreads("lFieldAtCorners ~> Matrix1(8)");
    for (int idPart = 0; idPart < nbParticles; idPart++) {
      __strict();
      __sreads("lFieldAtCorners ~> Matrix1(8)");
      __xconsumes("&lParticles[MINDEX1(nbParticles, idPart)] ~> Cell");
      __xproduces("&lParticles[MINDEX1(nbParticles, idPart)] ~> Cell");
      __ghost([&] ()   {
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)] ~> Cell");
        __produces("&lParticles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __produces("&lParticles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __admitted();
      }, "");
      double* const coeffs = cornerInterpolationCoeff(lParticles[MINDEX1(nbParticles, idPart)].pos);
      double fieldAtPosX = 0.;
      double fieldAtPosY = 0.;
      double fieldAtPosZ = 0.;
      for (int k = 0; k < nbCorners; k++) {
        __strict();
        __requires("#_6: _Fraction");
        __requires("#_7: _Fraction");
        __smodifies("&fieldAtPosX ~> Cell");
        __smodifies("&fieldAtPosY ~> Cell");
        __smodifies("&fieldAtPosZ ~> Cell");
        __xconsumes("_RO(#_6, &coeffs[MINDEX1(nbCorners, k)] ~> Cell)");
        __xconsumes("_RO(#_7, &lFieldAtCorners[MINDEX1(nbCorners, k)] ~> Cell)");
        __xproduces("_RO(#_6, &coeffs[MINDEX1(nbCorners, k)] ~> Cell)");
        __xproduces("_RO(#_7, &lFieldAtCorners[MINDEX1(nbCorners, k)] ~> Cell)");
        const double fieldAtPosTmpX = ( (vect) {fieldAtPosX / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
              stepDuration * stepDuration))}).x + coeffs[MINDEX1(nbCorners, k)] * lFieldAtCorners[MINDEX1(nbCorners, k)].x;
        const double fieldAtPosTmpY = ( (vect) {fieldAtPosX / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
              stepDuration * stepDuration))}).y + coeffs[MINDEX1(nbCorners, k)] * lFieldAtCorners[MINDEX1(nbCorners, k)].y;
        const double fieldAtPosTmpZ = ( (vect) {fieldAtPosX / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
              stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
              stepDuration * stepDuration))}).z + coeffs[MINDEX1(nbCorners, k)] * lFieldAtCorners[MINDEX1(nbCorners, k)].z;
        __ghost([&] ()   {
          __consumes("_Uninit(&fieldAtPosX ~> Cell)");
          __consumes("_Uninit(&fieldAtPosY ~> Cell)");
          __consumes("_Uninit(&fieldAtPosZ ~> Cell)");
          __produces("_Uninit(&fieldAtPosX ~> Cell)");
          __produces("_Uninit(&fieldAtPosY ~> Cell)");
          __produces("_Uninit(&fieldAtPosZ ~> Cell)");
          __admitted();
        }, "");
        fieldAtPosX = fieldAtPosTmpX * (particleCharge / particleMass * (stepDuration * stepDuration
          ));
        fieldAtPosY = fieldAtPosTmpY * (particleCharge / particleMass * (stepDuration * stepDuration
          ));
        fieldAtPosZ = fieldAtPosTmpZ * (particleCharge / particleMass * (stepDuration * stepDuration
          ));
        __ghost([&] ()   {
          __consumes("&fieldAtPosX ~> Cell");
          __consumes("&fieldAtPosY ~> Cell");
          __consumes("&fieldAtPosZ ~> Cell");
          __produces("&fieldAtPosX ~> Cell");
          __produces("&fieldAtPosY ~> Cell");
          __produces("&fieldAtPosZ ~> Cell");
          __admitted();
        }, "");
      }
      MFREE1(nbCorners, coeffs);
      const double accelX = particleCharge / particleMass * (  (vect) {fieldAtPosX / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
            stepDuration * stepDuration))}).x;
      const double accelY = particleCharge / particleMass * ( (vect) {fieldAtPosX / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
            stepDuration * stepDuration))}).y;
      const double accelZ = particleCharge / particleMass * ( (vect) {fieldAtPosX / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosY / (particleCharge / particleMass * (
            stepDuration * stepDuration)), fieldAtPosZ / (particleCharge / particleMass * (
            stepDuration * stepDuration))}).z;
      const double speed2X = (lParticles[MINDEX1(nbParticles, idPart)].speed.x + stepDuration * accelX
      ) * stepDuration;
      const double speed2Y = (lParticles[MINDEX1(nbParticles, idPart)].speed.y + stepDuration * accelY
      ) * stepDuration;
      const double speed2Z = (lParticles[MINDEX1(nbParticles, idPart)].speed.z + stepDuration * accelZ
      ) * stepDuration;
      const double pos2X = lParticles[MINDEX1(nbParticles, idPart)].pos.x + stepDuration * (
        speed2X / stepDuration);
      const double pos2Y = lParticles[MINDEX1(nbParticles, idPart)].pos.y + stepDuration * (
        speed2Y / stepDuration);
      const double pos2Z = lParticles[MINDEX1(nbParticles, idPart)].pos.z + stepDuration * (
        speed2Z / stepDuration);
      __ghost([&] ()   {
        __consumes("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].pos ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].pos.x ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].pos.y ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].pos.z ~> Cell)");
        __admitted();
      }, "");
      lParticles[MINDEX1(nbParticles, idPart)].pos.x = pos2X;
      lParticles[MINDEX1(nbParticles, idPart)].pos.y = pos2Y;
      lParticles[MINDEX1(nbParticles, idPart)].pos.z = pos2Z;
      __ghost([&] ()   {
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].pos.x ~> Cell");
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].pos.y ~> Cell");
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].pos.z ~> Cell");
        __produces("&lParticles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __admitted();
      }, "");
      __ghost([&] ()   {
        __consumes("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].speed ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].speed.x ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].speed.y ~> Cell)");
        __produces("_Uninit(&lParticles[MINDEX1(nbParticles, idPart)].speed.z ~> Cell)");
        __admitted();
      }, "");
      lParticles[MINDEX1(nbParticles, idPart)].speed.x = speed2X / stepDuration;
      lParticles[MINDEX1(nbParticles, idPart)].speed.y = speed2Y / stepDuration;
      lParticles[MINDEX1(nbParticles, idPart)].speed.z = speed2Z / stepDuration;
      __ghost([&] ()   {
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].speed.x ~> Cell");
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].speed.y ~> Cell");
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].speed.z ~> Cell");
        __produces("&lParticles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __admitted();
      }, "");
      __ghost([&] ()   {
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __consumes("&lParticles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __produces("&lParticles[MINDEX1(nbParticles, idPart)] ~> Cell");
        __admitted();
      }, "");
    }
  }
  for (int i1 = 0; i1 < nbParticles; i1++) {
    __strict();
    __requires("#_2: _Fraction");
    __xconsumes("_Uninit(&particles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __xconsumes("_RO(#_2, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __xproduces("&particles[MINDEX1(nbParticles, i1)] ~> Cell");
    __xproduces("_RO(#_2, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
    __ghost([&] ()   {
      __consumes("_Uninit(&particles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("_Uninit(&particles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __admitted();
    }, "");
    particles[MINDEX1(nbParticles, i1)].pos.x = lParticles[MINDEX1(nbParticles, i1)].pos.x;
    particles[MINDEX1(nbParticles, i1)].pos.y = lParticles[MINDEX1(nbParticles, i1)].pos.y;
    particles[MINDEX1(nbParticles, i1)].pos.z = lParticles[MINDEX1(nbParticles, i1)].pos.z;
    __ghost([&] ()   {
      __consumes("&particles[MINDEX1(nbParticles, i1)].pos.x ~> Cell");
      __consumes("&particles[MINDEX1(nbParticles, i1)].pos.y ~> Cell");
      __consumes("&particles[MINDEX1(nbParticles, i1)].pos.z ~> Cell");
      __produces("&particles[MINDEX1(nbParticles, i1)].pos ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.x ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.y ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos.z ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("_Uninit(&particles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __produces("_Uninit(&particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __admitted();
    }, "");
    particles[MINDEX1(nbParticles, i1)].speed.x = lParticles[MINDEX1(nbParticles, i1)].speed.x;
    particles[MINDEX1(nbParticles, i1)].speed.y = lParticles[MINDEX1(nbParticles, i1)].speed.y;
    particles[MINDEX1(nbParticles, i1)].speed.z = lParticles[MINDEX1(nbParticles, i1)].speed.z;
    __ghost([&] ()   {
      __consumes("&particles[MINDEX1(nbParticles, i1)].speed.x ~> Cell");
      __consumes("&particles[MINDEX1(nbParticles, i1)].speed.y ~> Cell");
      __consumes("&particles[MINDEX1(nbParticles, i1)].speed.z ~> Cell");
      __produces("&particles[MINDEX1(nbParticles, i1)].speed ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.x ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.y ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed.z ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __consumes("&particles[MINDEX1(nbParticles, i1)].pos ~> Cell");
      __consumes("&particles[MINDEX1(nbParticles, i1)].speed ~> Cell");
      __produces("&particles[MINDEX1(nbParticles, i1)] ~> Cell");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].pos ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __admitted();
    }, "");
    __ghost([&] ()   {
      __requires("#_3: _Fraction");
      __consumes("Wand(_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell), _RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell))");
      __consumes("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)].speed ~> Cell)");
      __produces("_RO(#_3, &lParticles[MINDEX1(nbParticles, i1)] ~> Cell)");
      __admitted();
    }, "");
  }
  __ghost([&] ()   {
    __consumes("lParticles ~> Matrix1(nbParticles)");
    __produces("_Uninit(lParticles ~> Matrix1(nbParticles))");
    __admitted();
    __with("justif := shift_groups");
  }, "");
  MFREE1(nbParticles, lParticles);
  __ghost([&] ()   {
    __consumes("_Uninit(lFieldAtCorners ~> Matrix1(nbCorners))");
    __produces("_Uninit(lFieldAtCorners ~> Matrix1(nbCorners))");
    __admitted();
    __with("justif := shift_groups");
  }, "");
  MFREE1(nbCorners, lFieldAtCorners);
}
