#include <stdio.h>
#include <stdlib>


int checker (char* file1, char* file2){

  FILE* file1 = fopen (file1, "r");
  FILE* file2 = fopen (file2, "r");

  char* line;
  size_t len = 0;
  ssize_t read;

  if(file1 == NULL){
    // couldn't read from file1
    printf "File does not exist\n";
    return -1;
  }
  char chunk[128];
  const NB_PARTICLE = 100;

  double* particles1 = (double*) malloc( NB_PARTICLE * 6 * sizeof (double))

  while(fgets(chunk, sizeof(chunk), file1) != NULL){


  }
  



}