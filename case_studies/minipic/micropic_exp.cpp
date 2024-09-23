#include <optitrust.h>

typedef struct {
  double x;
  double y;
  double z;
} vect;

template <typename T, typename U>
U* __struct_access_x(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_x(T v) {
  __admitted();
}

template <typename T, typename U>
U* __struct_access_y(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_y(T v) {
  __admitted();
}

template <typename T, typename U>
U* __struct_access_z(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_z(T v) {
  __admitted();
}

vect vect_add(vect v1, vect v2) {
  __admitted();
  return (vect){v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

vect vect_mul(double d, vect v) {
  __admitted();
  return (vect){d * v.x, d * v.y, d * v.z};
}

typedef struct {
  vect pos;
  vect speed;
} particle;

template <typename T, typename U>
U* __struct_access_pos(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_pos(T v) {
  __admitted();
}

template <typename T, typename U>
U* __struct_access_speed(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_speed(T v) {
  __admitted();
}

template <typename T, typename U>
U* __struct_access_charge(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_charge(T v) {
  __admitted();
}

template <typename T, typename U>
U* __struct_access_mass(T* v) {
  __admitted();
}

template <typename T, typename U>
U __struct_get_mass(T v) {
  __admitted();
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

int int_of_double(double a) {
  __admitted();
  return (int)a - (a < 0.);
}

double relativePosX(double x) {
  __admitted();
  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}

double relativePosY(double y) {
  __admitted();
  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}

double relativePosZ(double z) {
  __admitted();
  int iZ = int_of_double(z / cellZ);
  return (z - iZ * cellZ) / cellZ;
}

const int nbCorners = 8;

void cornerInterpolationCoeff(vect pos, double* r) {
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  r[0] = cX * cY * cZ;
  r[1] = cX * cY * rZ;
  r[2] = cX * rY * cZ;
  r[3] = cX * rY * rZ;
  r[4] = rX * cY * cZ;
  r[5] = rX * cY * rZ;
  r[6] = rX * rY * cZ;
  r[7] = rX * rY * rZ;
}

vect matrix_vect_mul(double* coeffs, vect* matrix) {
  vect res = {0., 0., 0.};
  for (int k = 0; k < nbCorners; k++) {
    res = vect_add(res, vect_mul(coeffs[k], matrix[k]));
  }
  __admitted();
  return res;
}

void simulate_single_cell(double deltaT, particle* particles, int nbParticles,
                          vect* fieldAtCorners, int nbSteps, double pCharge,
                          double pMass) {
  vect* const lFieldAtCorners = (vect*)MALLOC1(nbCorners, sizeof(vect));
  for (int i1 = 0; i1 < nbCorners; i1++) {
    lFieldAtCorners[i1].x =
        fieldAtCorners[i1].x * deltaT * deltaT * pCharge / pMass;
    lFieldAtCorners[i1].y =
        fieldAtCorners[i1].y * deltaT * deltaT * pCharge / pMass;
    lFieldAtCorners[i1].z =
        fieldAtCorners[i1].z * deltaT * deltaT * pCharge / pMass;
  }
  for (int i1 = 0; i1 < nbParticles; i1 += 1) {
    particles[i1].speed.x *= deltaT;
    particles[i1].speed.y *= deltaT;
    particles[i1].speed.z *= deltaT;
  }
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    for (int idPart = 0; idPart < nbParticles; idPart++) {
      double* const coeffs = (double*)MALLOC1(nbCorners, sizeof(double));
      const double rX = relativePosX(particles[idPart].pos.x);
      const double rY = relativePosY(particles[idPart].pos.y);
      const double rZ = relativePosZ(particles[idPart].pos.z);
      const double cX = 1. + -1. * rX;
      const double cY = 1. + -1. * rY;
      const double cZ = 1. + -1. * rZ;
      coeffs[0] = cX * cY * cZ;
      coeffs[1] = cX * cY * rZ;
      coeffs[2] = cX * rY * cZ;
      coeffs[3] = cX * rY * rZ;
      coeffs[4] = rX * cY * cZ;
      coeffs[5] = rX * cY * rZ;
      coeffs[6] = rX * rY * cZ;
      coeffs[7] = rX * rY * rZ;
      double fieldAtPosX = 0.;
      double fieldAtPosY = 0.;
      double fieldAtPosZ = 0.;
      for (int k = 0; k < nbCorners; k++) {
        fieldAtPosX += coeffs[k] * lFieldAtCorners[k].x;
        fieldAtPosY += coeffs[k] * lFieldAtCorners[k].y;
        fieldAtPosZ += coeffs[k] * lFieldAtCorners[k].z;
      }
      MFREE1(nbCorners, coeffs);
      const double speed2X = particles[idPart].speed.x + fieldAtPosX;
      const double speed2Y = particles[idPart].speed.y + fieldAtPosY;
      const double speed2Z = particles[idPart].speed.z + fieldAtPosZ;
      particles[idPart].pos.x += speed2X;
      particles[idPart].pos.y += speed2Y;
      particles[idPart].pos.z += speed2Z;
      particles[idPart].speed.x = speed2X;
      particles[idPart].speed.y = speed2Y;
      particles[idPart].speed.z = speed2Z;
    }
  }
  for (int i1 = 0; i1 < nbParticles; i1 += 1) {
    particles[i1].speed.z /= deltaT;
    particles[i1].speed.y /= deltaT;
    particles[i1].speed.x /= deltaT;
  }
  MFREE1(nbCorners, lFieldAtCorners);
}
