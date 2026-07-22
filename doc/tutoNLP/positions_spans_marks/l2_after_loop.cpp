// Difficulty: Level 2
// Request: target the position after the loop i

void work(int);
void finish();

void f(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
  finish();
}
