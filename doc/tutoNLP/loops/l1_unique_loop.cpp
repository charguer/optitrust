// Difficulty: Level 1
// Request: target the loop i

void work(int);

void kernel(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
}
