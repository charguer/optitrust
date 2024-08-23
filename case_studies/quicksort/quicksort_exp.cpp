#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void partition(int* out_pivot, int* arr, int right_limit) {
  int pivot = arr[right_limit - 1];
  int idx_left = -1;
  int idx_iter, tmp;
  for (idx_iter = 0; idx_iter < right_limit - 1; idx_iter++) {
    if (arr[idx_iter] < pivot) {
      idx_left++;
      tmp = arr[idx_left];
      arr[idx_left] = arr[idx_iter];
      arr[idx_iter] = tmp;
    }
  }
  tmp = arr[idx_left + 1];
  arr[idx_left + 1] = arr[right_limit - 1];
  arr[right_limit - 1] = tmp;
  *out_pivot = idx_left + 1;
}

void insertion_sort(int* arr, int right_limit) {
  for (int idx = 0; idx < right_limit - 1; ++idx) {
    int idx_min = idx;
    int idx_iter;
    for (idx_iter = idx_min + 1; idx_iter < right_limit; ++idx_iter) {
      if (arr[idx_min] > arr[idx_iter]) {
        idx_min = idx_iter;
      }
    }
    int tmp = arr[idx];
    arr[idx] = arr[idx_min];
    arr[idx_min] = tmp;
  }
}

void sort_core(int* in_out_data, int right_limit) {
#pragma omp taskgroup
  {
    if (0 >= right_limit) {
      goto __apac_exit;
    }
    if (right_limit <= 256) {
#pragma omp task default(shared) depend(in : in_out_data, right_limit) depend(inout : in_out_data[0])
      insertion_sort(in_out_data, right_limit);
    } else {
      int* pivot = new int();
#pragma omp task default(shared) depend(in : in_out_data, right_limit, pivot) depend(inout : in_out_data[0], pivot[0])
      partition(pivot, in_out_data, right_limit);
#pragma omp taskwait depend(in : pivot[0], pivot)
#pragma omp task default(shared) depend(in : in_out_data, pivot[0], pivot) depend(inout : in_out_data[0])
      sort_core(&in_out_data[0], *pivot);
#pragma omp task default(shared) depend(in : in_out_data, pivot[0], right_limit, pivot) depend(inout : in_out_data[*pivot + 1])
      sort_core(&in_out_data[*pivot + 1], right_limit - (*pivot + 1));
#pragma omp task default(shared) depend(inout : pivot)
      delete pivot;
    }
#pragma omp taskwait
  __apac_exit:;
  }
}

void sort(int* in_out_data, int in_size) {
#pragma omp taskgroup
  {
#pragma omp task default(shared) depend(in : in_out_data, in_size) depend(inout : in_out_data[0])
    sort_core(in_out_data, in_size);
#pragma omp taskwait
  __apac_exit:;
  }
}

int main(int argc, char** argv) {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    int size = 1000000;
    if (argc > 1) {
      size = atoi(argv[1]);
    }
    int* data = (int*)malloc(size * sizeof(int));
    if (!data) {
      perror("Array allocation failure");
      __apac_result = 1;
      goto __apac_exit;
    }
    srand(time(NULL));
    for (int idx = 0; idx < size; idx++) {
      data[idx] = rand();
    }
#pragma omp task default(shared) depend(in : data, size) depend(inout : data[0])
    sort(data, size);
#pragma omp taskwait
    int idx;
    for (idx = 1; idx < size; idx++) {
      if (data[idx - 1] > data[idx]) {
        fprintf(stderr, "Error: array is not sorted\n");
        free(data);
        __apac_result = 1;
        goto __apac_exit;
      }
    }
    free(data);
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
