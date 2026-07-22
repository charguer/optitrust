// Difficulty: Level 3
// Request: target both loops i and j

void rows(int);
void cols(int);

void matmat(int n) {
  for (int i = 0; i < n; i++) {
    rows(i);
  }
  for (int j = 0; j < n; j++) {
    cols(j);
  }
}
