#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void partition(int * out_pivot, int * arr, int right_limit) {
  int pivot = arr[right_limit - 1];
  int idx_left = -1;
  int idx_iter, tmp;

  for(idx_iter = 0; idx_iter < right_limit - 1; idx_iter++) {
    if(arr[idx_iter] < pivot) {
      idx_left++;
      tmp = arr[idx_left];
      arr[idx_left] = arr[idx_iter];
      arr[idx_iter] = tmp;
    }
  }

  tmp = arr[idx_left + 1];
  arr[idx_left + 1] = arr[right_limit - 1];
  arr[right_limit - 1] = tmp;
  *out_pivot = (idx_left + 1);
}

void insertion_sort(int * arr, int right_limit) {
  for(int idx = 0; idx < right_limit - 1; ++idx) {
    int idx_min = idx;
    int idx_iter;
    for(idx_iter = idx_min + 1; idx_iter < right_limit; ++idx_iter) {
      if(arr[idx_min] > arr[idx_iter]) {
        idx_min = idx_iter;
      }
    }
    int tmp = arr[idx];
    arr[idx] = arr[idx_min];
    arr[idx_min] = tmp;
  }
}
  
void sort_core(int * in_out_data, int right_limit) {
  if (0 >= right_limit) {
    return;
  }
  
  if(right_limit <= 256) {
    insertion_sort(in_out_data, right_limit);
  } else {
    int pivot;
    partition(&pivot, in_out_data, right_limit);
    sort_core(&in_out_data[0], pivot);
    sort_core(&in_out_data[pivot + 1], right_limit - (pivot + 1));
  }
}
  
void sort(int * in_out_data, int in_size) {
  sort_core(in_out_data, in_size);
}

int main(int argc, char ** argv){
  int size = 1000000;
  if(argc > 1) {
    size = atoi(argv[1]);
  }
  
  int * data = (int *) malloc(size * sizeof(int));

  if(!data) {
    perror("Array allocation failure");
    return 1;
  }
  
  srand(time(NULL));
  
  for(int idx = 0; idx < size; idx++) {
    data[idx] = rand();
  }

  sort(data, size);
  
  int idx;
  for(idx = 1; idx < size; idx++) {
    if(data[idx - 1] > data[idx]){
      fprintf(stderr, "Error: array is not sorted\n");
      free(data);
      return 1;
    }
  }

  free(data);
  return 0;
}
