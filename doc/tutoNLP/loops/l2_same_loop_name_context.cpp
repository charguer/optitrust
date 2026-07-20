// Difficulty: Level 2
// Request: target the loop i inside main_loop

void clear(int);
void update(int);

void init(int n) {
  for (int i = 0; i < n; i++) {
    clear(i);
  }
}

void main_loop(int n) {
  for (int i = 0; i < n; i++) {
    update(i);
  }
}
