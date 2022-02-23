#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double max (double a, double b){
  return a > b ? a : b;
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


void checker (char* filename, particle* ps){

  FILE* f = fopen(filename, "r");

  if(f = NULL){
    printf("Couldn't read from file!\n");
    exit(0);
  }
  int prev_nbParticles = nbParticles;
  fread(&nbParticles, sizeof(int), 1, f);
  for(int i = 0; i < nbParticles; i++){
    int id;
    fread(&id, sizeof(int), 1, f);
    fread(&(ps[i].pos.x), sizeof(double), 1, f);
    fread(&(ps[i].pos.y), sizeof(double), 1, f);
    fread(&(ps[i].pos.z), sizeof(double), 1, f);
    fread(&(ps[i].speed.x), sizeof(double), 1, f);
    fread(&(ps[i].speed.y), sizeof(double), 1, f);
    fread(&(ps[i].speed.z), sizeof(double), 1, f);
  }
  fclose(f);
}

double square( double x, double y){
  return (x - y) * (x - y);
}

double distance (vect cmp1, vect cmp2){

  double dist;

  dist += square(cmp1.x, cmp2.x);
  dist += square(cmp1.y, cmp2.y);
  dist += square(cmp1.z, cmp2.z);

  return dist;
}



int main(int argc, char* argv[]){

  char* f1;
  char* f2;

  if (argc == 3){
    f1 = argv[1];
    f2 = argv[2];
  
    particle* particles1 = (particle*) malloc(nbParticles * sizeof(particle));
    particle* particles2 = (particle*) malloc(nbParticles * sizeof(particle));

    checker(f1, particles1);
    checker(f2, particles2);

    double max_dist_pos = -1;
    double max_dist_speed = -1;
    for (int i = 0; i < nbParticles; i++){
     max_dist_pos = max (max_dist_pos, distance (particles1[i].pos, particles2[i].pos));
     max_dist_speed = max (max_dist_pos, distance ((particles1[i]).speed, (particles2[i]).speed));
    }

    printf ("Maximal position distance between particles: %f", max_dist_pos);
    printf ("Maximal speed distance between particles: %f", max_dist_pos);
  
    free(particles1);
    free(particles2);
  }
  else {
    printf("Expected two commandline arguments");
  }
  return 0;
}
