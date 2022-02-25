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
    printf("Not matching number of particles\n");
    exit(1);
  }
  
  fread(&areaX, sizeof(double), 1, f);
  fread(&areaY, sizeof(double), 1, f);
  fread(&areaZ, sizeof(double), 1, f);

  nbParticles = nb;
  printf("nb : %d\n", nb);
  particle* ps = (particle*) malloc(nbParticles * sizeof(particle));

  for (int i = 0; i < nbParticles; i++) {
    if (i % 100000 == 0)
       printf("i : %d\n", i);
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

double sqdiff(double x, double y) {
  double r = x - y;
  return r * r;
}


// Euclidean distance
double ec_dist(vect v1, vect v2) {
  return sqdiff(v1.x, v2.x)
       + sqdiff(v1.y, v2.y)
       + sqdiff(v1.z, v2.z);
}

double dist (vect v1, vect v2){
  double d = ec_dist(v1, v2);
  double m = min(d, abs(areaX - (v2.x - v1.x)));
  m = min(m, abs(areaY - (v2.y - v1.y)));
  m = min(m, abs(areaZ - (v2.z - v1.z)));

  return m;
}

int main(int argc, char* argv[]) {

  if (argc < 3) {
    printf("Expected two command line arguments.\n");
    exit(1);
  }

  particle* ps1 = readfile(argv[1]);
  particle* ps2 = readfile(argv[2]);

  double max_dist_pos = -1;
  double max_dist_speed = -1;
  for (int i = 0; i < nbParticles; i++){
    max_dist_pos = max(max_dist_pos, dist(ps1[i].pos, ps2[i].pos));
    max_dist_speed = max (max_dist_speed, dist(ps1[i].speed, ps2[i].speed));
  }

  printf ("Maximal dist pos  : %f\n", max_dist_pos);
  printf ("Maximal dist speed: %f\n", max_dist_speed);

  free(ps1);
  free(ps2);

  return 0;
}
