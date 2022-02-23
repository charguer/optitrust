#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double max (double a, double b){
  return a > b ? a : b;
}


double* checker (char* file){

  FILE* file1 = fopen (file, "r");


  if(file1 == NULL){
    // couldn't read from file1
    return NULL;
  }
  char chunk[128];
  
  int NB_PARTICLES;

  fread(&NB_PARTICLES, sizeof(int), 1, file1);
  

  double* particle = (double*) malloc( NB_PARTICLES * 6 * sizeof (double));

  for(int i = 0; i < NB_PARTICLES; i++){
    int id;
    fread(&id, sizeof(int), 1, file1);
    for(int j = 0; j < 6; j++){
      fread(&particle[i * 6 + j], sizeof(double), 1, file1);
    }
    id++;
  }
  return particle;

}

double distance (double* particle1, double* particle2){
  double dist = 0.;

  for(int i = 0; i < 6; i++){
    double temp = particle1[i] - particle2[i];
    dist += temp * temp;
  }
  return sqrt(dist);
}


int main(){

  char* file1 = "yans_file"; // Dummy value
  char* file2 = "our_file"; // Dummy value
  
  FILE* f1 = fopen(file1,"r");
  FILE* f2 = fopen(file1, "r");
  

  int size = 10000; // Dummy value

  double* tmp_particle1 = (double*) malloc (6 * sizeof(double));
  double* tmp_particle2 = (double*) malloc (6 * sizeof(double));

  

  double* particles1 = checker(file1);
  double* particles2 = checker(file2);


  double max_dist = -1.;
  for (int i = 0; i < 10000; i++){
   for(int j = 0; j < 6; j++){
     tmp_particle1[j] = particles1[i * 6 + j];
     tmp_particle2[j] = particles2[i * 6 + j];
     max_dist = max(max_dist, distance(tmp_particle1, tmp_particle2));

   }
  }

  if (max_dist > 1.)
    printf ("Wrong results\n");
  else 
    printf ("Correct results\n");


  free(tmp_particle1);
  free(tmp_particle2);

  return 0;
}
