#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
const static int __apac_count_infinite = getenv("APAC_TASK_COUNT_INFINITE") ? 1 : 0;

const static int __apac_depth_infinite = getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;

const static int __apac_count_max = getenv("APAC_TASK_COUNT_MAX") ? atoi(getenv("APAC_TASK_COUNT_MAX")) : omp_get_max_threads() * 10;

const static int __apac_depth_max = getenv("APAC_TASK_DEPTH_MAX") ? atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

int __apac_count = 0;

int __apac_depth = 0;

#pragma omp threadprivate(__apac_depth)

void partition(int* out_pivot, int* arr, const int right_limit) {
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

void insertion_sort(int* arr, const int right_limit) {
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

void sort_core(int* in_out_data, const int right_limit) {
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
#pragma omp taskwait depend(in : right_limit)
    if (right_limit <= 256) {
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp task default(shared) depend(in : right_limit) depend(inout : in_out_data[0]) if (__apac_count_ok || __apac_depth_ok)
      {
        if (__apac_count_ok || __apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        insertion_sort(in_out_data, right_limit);
        if (__apac_count_ok) {
#pragma omp atomic
          __apac_count--;
        }
      }
    } else {
      int* pivot = new int();
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp task default(shared) depend(in : right_limit) depend(inout : in_out_data[0], pivot[0]) if (__apac_count_ok || __apac_depth_ok)
      {
        if (__apac_count_ok || __apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        partition(pivot, in_out_data, right_limit);
        if (__apac_count_ok) {
#pragma omp atomic
          __apac_count--;
        }
#pragma omp task default(shared) depend(inout : pivot[0])
        delete pivot;
      }
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp taskwait depend(in : pivot[0])
#pragma omp task default(shared) depend(in : pivot[0], right_limit) depend(inout : in_out_data[*pivot + 1]) if (__apac_count_ok || __apac_depth_ok)
      {
        if (__apac_count_ok || __apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        sort_core(&in_out_data[*pivot + 1], right_limit - (*pivot + 1));
        if (__apac_count_ok) {
#pragma omp atomic
          __apac_count--;
        }
#pragma omp task default(shared) depend(inout : pivot[0])
        delete pivot;
      }
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count++;
      }
#pragma omp task default(shared) depend(in : pivot[0]) depend(inout : in_out_data[0]) if (__apac_count_ok || __apac_depth_ok)
      {
        if (__apac_count_ok || __apac_depth_ok) {
          __apac_depth = __apac_depth_local + 1;
        }
        sort_core(&in_out_data[0], *pivot);
        if (__apac_count_ok) {
#pragma omp atomic
          __apac_count--;
        }
#pragma omp task default(shared) depend(inout : pivot[0])
        delete pivot;
      }
#pragma omp task default(shared) depend(inout : pivot[0])
      delete pivot;
    }
#pragma omp taskwait depend(in : right_limit)
    if (0 >= right_limit) {
#pragma omp taskwait
      goto __apac_exit;
    }
  __apac_exit:;
  }
}

void sort(int* in_out_data, const int in_size) {
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    int __apac_count_ok = __apac_count_infinite || __apac_count < __apac_count_max;
    int __apac_depth_local = __apac_depth;
    int __apac_depth_ok = __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    if (__apac_count_ok) {
#pragma omp atomic
      __apac_count++;
    }
#pragma omp task default(shared) depend(in : in_size) depend(inout : in_out_data[0]) if (__apac_count_ok || __apac_depth_ok)
    {
      if (__apac_count_ok || __apac_depth_ok) {
        __apac_depth = __apac_depth_local + 1;
      }
      sort_core(in_out_data, in_size);
      if (__apac_count_ok) {
#pragma omp atomic
        __apac_count--;
      }
    }
  __apac_exit:;
  }
}

int main(int argc, char** argv) {
  int size = 1000000;
  if (argc > 1) {
    size = atoi(argv[1]);
  }
  int* data = (int*)malloc(size * sizeof(int));
  if (!data) {
    perror("Array allocation failure");
    return 1;
  }
  srand(time(NULL));
  for (int idx = 0; idx < size; idx++) {
    data[idx] = rand();
  }
  sort(data, size);
  int idx;
  for (idx = 1; idx < size; idx++) {
    if (data[idx - 1] > data[idx]) {
      fprintf(stderr, "Error: array is not sorted\n");
      free(data);
      return 1;
    }
  }
  free(data);
  return 0;
}
