#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double max (double a, double b) {
  return (a > b) ? a : b;
}

double min (double a, double b) {
  return (a < b) ? a : b;
}

typedef struct {
  double x;
  double y;
  double z;
} vect;


typedef struct {
  vect pos;
  vect speed;
} particle;

int nbParticles = -1;

double areaX;
double areaY;
double areaZ;

particle* readfile(char* filename) {

  FILE* f = fopen(filename, "rb");

  if (f == NULL){
    printf("Couldn't read from file!\n");
    exit(0);
  }

  int nb;
  fread(&nb, sizeof(int), 1, f);
  if (nbParticles != -1 && nb != nbParticles) {
    printf("Not matching number of particles: first=%d second=%d\n", nbParticles, nb);
    exit(1);
  }
  nbParticles = nb;

  // LATER: we could check that area values are equal, but does not seem essential
  fread(&areaX, sizeof(double), 1, f);
  fread(&areaY, sizeof(double), 1, f);
  fread(&areaZ, sizeof(double), 1, f);

  particle* ps = (particle*) malloc(nbParticles * sizeof(particle));

  for (int i = 0; i < nbParticles; i++) {
    if (i > 0 && i % 100000 == 0)
       printf("reading particle: %d\n", i);
    int id;
    fread(&id, sizeof(int), 1, f);
    fread(&(ps[id].pos.x), sizeof(double), 1, f);
    fread(&(ps[id].pos.y), sizeof(double), 1, f);
    fread(&(ps[id].pos.z), sizeof(double), 1, f);
    fread(&(ps[id].speed.x), sizeof(double), 1, f);
    fread(&(ps[id].speed.y), sizeof(double), 1, f);
    fread(&(ps[id].speed.z), sizeof(double), 1, f);
  }
  fclose(f);
  return ps;
}

double squareNorm(vect v) {
  return v.x * v.x + v.y * v.y + v.z * v.z;
}

double distWrapAround(double xWidth, double x1, double x2) {
  return min(abs(x1-x2),
         min(abs((x1+xWidth)-x2),
             abs(x1-(x2+xWidth))));
}

double squareDist(vect v1, vect v2) {
  vect d = { distWrapAround(areaX, v1.x, v2.x),
             distWrapAround(areaY, v1.y, v2.y),
             distWrapAround(areaZ, v1.z, v2.z) };
  return squareNorm(d);
}

int main(int argc, char* argv[]) {

  if (argc < 3) {
    printf("Expected two command line arguments.\n");
    exit(1);
  }

  particle* ps1 = readfile(argv[1]);
  particle* ps2 = readfile(argv[2]);

  double max_sqdist_pos = -1;
  double max_sqdist_speed = -1;
  for (int i = 0; i < nbParticles; i++){
    double d = squareDist(ps1[i].pos, ps2[i].pos);
    max_sqdist_pos = max(max_sqdist_pos, d);
    vect p1 = ps1[i].pos;
    vect p2 = ps2[i].pos;
    if (d > 1. || p1.x < 0 || p1.x > areaX
        || p1.y < 0 || p1.y > areaY
        || p1.z < 0 || p1.z > areaZ
        || p2.x < 0 || p2.x > areaX
        || p2.y < 0 || p2.y > areaY
        || p2.z < 0 || p2.z > areaZ) {

      printf("%g %g %g %g %g %g\n", p1.x, p2.x, p1.y, p2.y, p1.z, p2.z);
    }
    max_sqdist_speed = max(max_sqdist_speed, squareDist(ps1[i].speed, ps2[i].speed));
  }

  printf("Compared %d particles\n", nbParticles);
  printf("Maximal dist pos  : %f\n", sqrt(max_sqdist_pos));
  printf("Maximal dist speed: %f\n", sqrt(max_sqdist_speed));

  free(ps1);
  free(ps2);

  return 0;
}
