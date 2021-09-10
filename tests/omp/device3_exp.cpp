#include <omp.h>

#include <stdio.h>

void foo() {
  int default_device = omp_get_default_device();
  printf("Default device = %d\n", default_device);
  omp_set_default_device(default_device + 1);
  if ((omp_get_default_device() != (default_device + 1)))
    printf("Default device is still = %d\n", default_device);
}