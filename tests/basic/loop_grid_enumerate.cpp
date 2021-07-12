const int gridSize = 64;
const int nbCells = gridSize * gridSize * gridSize;

int total;

int main() {
  int X = gridSize, Y = gridSize, Z = gridSize;
  for (int idCell = 0; idCell < nbCells; idCell++) {
        total += idCell;
  }
}

