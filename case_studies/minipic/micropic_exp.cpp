#include <optitrust.h>

typedef struct {
  double x;
  double y;
  double z;
} vect;

template <typename T>
T* __struct_access_x(T* v) {
  __admitted();
  return &v->x;
}

template <typename T>
T __struct_get_x(T v) {
  __admitted();
  return v.x;
}

template <typename T>
T* __struct_access_y(T* v) {
  __admitted();
  return &v->y;
}

template <typename T>
T __struct_get_y(T v) {
  __admitted();
  return v.y;
}

template <typename T>
T* __struct_access_z(T* v) {
  __admitted();
  return &v->z;
}

template <typename T>
T __struct_get_z(T v) {
  __admitted();
  return v.z;
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

template <typename T>
T* __struct_access_pos(T* v) {
  __admitted();
  return &v->pos;
}

template <typename T>
T __struct_get_pos(T v) {
  __admitted();
  return v.pos;
}

template <typename T>
T* __struct_access_speed(T* v) {
  __admitted();
  return &v->speed;
}

template <typename T>
T __struct_get_speed(T v) {
  __admitted();
  return v.speed;
}

template <typename T>
T* __struct_access_charge(T* v) {
  __admitted();
  return &v->charge;
}

template <typename T>
T __struct_get_charge(T v) {
  __admitted();
  return v.charge;
}

template <typename T>
T* __struct_access_mass(T* v) {
  __admitted();
  return &v->mass;
}

template <typename T>
T __struct_get_mass(T v) {
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

double* cornerInterpolationCoeff(vect pos) {
  __admitted();
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  double* const r = (double* const)MALLOC1(nbCorners, sizeof(double));
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

vect matrix_vect_mul(double* coeffs, vect* matrix) {
  vect res = {0., 0., 0.};
  for (int k = 0; k < nbCorners; k++) {
    res = vect_add(res, vect_mul(coeffs[k], matrix[k]));
  }
  __admitted();
  return res;
}

void simulate_single_cell(double stepDuration, particle* particles,
                          int nbParticles, vect* fieldAtCorners, int nbSteps,
                          double particleCharge, double particleMass) {
  vect* const lFieldAtCorners = (vect*)MALLOC1(nbCorners, sizeof(vect));
  for (int i1 = 0; i1 < nbCorners; i1++) {
    lFieldAtCorners[i1].x = fieldAtCorners[i1].x * particleCharge *
                            stepDuration * stepDuration / particleMass;
    lFieldAtCorners[i1].y = fieldAtCorners[i1].y * particleCharge *
                            stepDuration * stepDuration / particleMass;
    lFieldAtCorners[i1].z = fieldAtCorners[i1].z * particleCharge *
                            stepDuration * stepDuration / particleMass;
  }
  particle* const lParticles =
      (particle*)MALLOC1(nbParticles, sizeof(particle));
  for (int i1 = 0; i1 < nbParticles; i1++) {
    lParticles[i1].pos.x = particles[i1].pos.x;
    lParticles[i1].pos.y = particles[i1].pos.y;
    lParticles[i1].pos.z = particles[i1].pos.z;
    lParticles[i1].speed.x = particles[i1].speed.x * stepDuration;
    lParticles[i1].speed.y = particles[i1].speed.y * stepDuration;
    lParticles[i1].speed.z = particles[i1].speed.z * stepDuration;
  }
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    for (int idPart = 0; idPart < nbParticles; idPart++) {
      double* const coeffs = cornerInterpolationCoeff(lParticles[idPart].pos);
      double fieldAtPosX = 0.;
      double fieldAtPosY = 0.;
      double fieldAtPosZ = 0.;
      for (int k = 0; k < nbCorners; k++) {
        fieldAtPosX += coeffs[k] * lFieldAtCorners[k].x;
        fieldAtPosY += coeffs[k] * lFieldAtCorners[k].y;
        fieldAtPosZ += coeffs[k] * lFieldAtCorners[k].z;
      }
      MFREE1(nbCorners, coeffs);
      const double speed2X = lParticles[idPart].speed.x + fieldAtPosX;
      const double speed2Y = lParticles[idPart].speed.y + fieldAtPosY;
      const double speed2Z = lParticles[idPart].speed.z + fieldAtPosZ;
      lParticles[idPart].pos.x += speed2X;
      lParticles[idPart].pos.y += speed2Y;
      lParticles[idPart].pos.z += speed2Z;
      lParticles[idPart].speed.x = speed2X;
      lParticles[idPart].speed.y = speed2Y;
      lParticles[idPart].speed.z = speed2Z;
    }
  }
  for (int i1 = 0; i1 < nbParticles; i1++) {
    particles[i1].pos.x = lParticles[i1].pos.x;
    particles[i1].pos.y = lParticles[i1].pos.y;
    particles[i1].pos.z = lParticles[i1].pos.z;
    particles[i1].speed.x = lParticles[i1].speed.x / stepDuration;
    particles[i1].speed.y = lParticles[i1].speed.y / stepDuration;
    particles[i1].speed.z = lParticles[i1].speed.z / stepDuration;
  }
  MFREE1(nbParticles, lParticles);
  for (int i1 = 0; i1 < nbCorners; i1++) {
    fieldAtCorners[i1].x = lFieldAtCorners[i1].x * particleMass /
                           (particleCharge * stepDuration * stepDuration);
    fieldAtCorners[i1].y = lFieldAtCorners[i1].y * particleMass /
                           (particleCharge * stepDuration * stepDuration);
    fieldAtCorners[i1].z = lFieldAtCorners[i1].z * particleMass /
                           (particleCharge * stepDuration * stepDuration);
  }
  MFREE1(nbCorners, lFieldAtCorners);
}
