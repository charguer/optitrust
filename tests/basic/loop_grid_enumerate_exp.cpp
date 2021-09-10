#include <stdio.h>

int const X = 5;

int const Y = 6;

int const Z = 7;

int const nbCells = ((X * Y) * Z);

int main() {
  int total = 0;
  for (int x = 0; (x < X); x++) {
    for (int y = 0; (y < Y); y++) {
      for (int z = 0; (z < Z); z++) {
        int idCell = ((((x * Y) + y) * Z) + z);
        total += idCell;
      }
    }
  }
  printf("%d\n", total);
  return 0;
}