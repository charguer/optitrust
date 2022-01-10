#include <stdio.h>

const int X = 5;

const int Y = 6;

const int Z = 7;

const int nbCells = ((X * Y) * Z);

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
