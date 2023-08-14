#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define SIZE 100000000

void swap(int * a, int * b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

void init(int * arr) {
  for(int idx = 0; idx < SIZE; idx++) {
    arr[idx] = rand();
  }
}

void Partition(int * outPivot, int * arr, int rightLimit) {
  int pivot = arr[rightLimit - 1];
  int idxLeft = -1;
  int idxIter;

  for(idxIter = 0; idxIter < rightLimit - 1; idxIter++) {
    if(arr[idxIter] < pivot) {
      idxLeft++;
      swap(&arr[idxLeft], &arr[idxIter]);
    }
  }
  
  swap(&arr[idxLeft + 1], &arr[rightLimit - 1]);
  *outPivot = (idxLeft + 1);
}

void InsertionSort(int * arr, int rightLimit) {
  for(int idx = 0; idx < rightLimit - 1; ++idx) {
    int idxMin = idx;
    int idxIter;
    for(idxIter = idxMin + 1; idxIter < rightLimit; ++idxIter) {
      if(arr[idxMin] > arr[idxIter]) {
        idxMin = idxIter;
      }
    }
    swap(&arr[idx], &arr[idxMin]);
  }
}
  
void SortCore(int * inOutData, int rightLimit) {
  if (0 >= rightLimit) {
    return;
  }
  
  if(rightLimit <= 256) {
    InsertionSort(inOutData, rightLimit);
  } else {
    int pivot;
    Partition(&pivot, inOutData, rightLimit);
    SortCore(&inOutData[0], pivot);
    SortCore(&inOutData[pivot + 1], rightLimit - (pivot + 1));
  }
}
  
void Sort(int * inOutData, int inSize) {
  SortCore(inOutData, inSize);
}

int main(){
  int * data = (int *) malloc(SIZE * sizeof(int));

  if(!data) {
    printf("Error while allocating data array!\n");
    return 1;
  }
  
  srand(time(NULL));
  
  init(data);

  Sort(data, SIZE);
  
  int idx;
  for(idx = 1; idx < SIZE; idx++) {
    if(data[idx - 1] > data[idx]){
      printf("Array is not sorted!\n");
      free(data);
      return 1;
    }
  }

  free(data);
  return 0;
}
