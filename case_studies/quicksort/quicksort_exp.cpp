#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void swap(int* a, int* b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

void init(int* arr) {
  for (int idx = 0; idx < 100000000; idx++) {
    arr[idx] = rand();
  }
}

void Partition(int* outPivot, int* arr, const int rightLimit) {
  int pivot = arr[rightLimit - 1];
  int idxLeft = -1;
  int idxIter;
  for (idxIter = 0; idxIter < rightLimit - 1; idxIter++) {
    if (arr[idxIter] < pivot) {
      idxLeft++;
      swap(&arr[idxLeft], &arr[idxIter]);
    }
  }
  swap(&arr[idxLeft + 1], &arr[rightLimit - 1]);
  *outPivot = idxLeft + 1;
}

void InsertionSort(int* arr, const int rightLimit) {
  for (int idx = 0; idx < rightLimit - 1; ++idx) {
    int idxMin = idx;
    int idxIter;
    for (idxIter = idxMin + 1; idxIter < rightLimit; ++idxIter) {
      if (arr[idxMin] > arr[idxIter]) {
        idxMin = idxIter;
      }
    }
    swap(&arr[idx], &arr[idxMin]);
  }
}

void SortCore(int* inOutData, const int rightLimit) {
  if (0 >= rightLimit) {
    return;
  }
  if (rightLimit <= 256) {
    InsertionSort(inOutData, rightLimit);
  } else {
    int pivot;
    Partition(&pivot, inOutData, rightLimit);
    SortCore(&inOutData[0], pivot);
    SortCore(&inOutData[pivot + 1], rightLimit - (pivot + 1));
  }
}

void Sort(int* inOutData, const int inSize) { SortCore(inOutData, inSize); }

int main() {
  int* data = (int*)malloc(100000000 * sizeof(int));
  if (!data) {
    printf("Error while allocating data array!\n");
    return 1;
  }
  srand(time(NULL));
  init(data);
  Sort(data, 100000000);
  int idx;
  for (idx = 1; idx < 100000000; idx++) {
    if (data[idx - 1] > data[idx]) {
      printf("Array is not sorted!\n");
      free(data);
      return 1;
    }
  }
  free(data);
  return 0;
}
