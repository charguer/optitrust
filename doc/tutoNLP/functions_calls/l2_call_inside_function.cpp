// Difficulty: Level 2
// Request: target the call to foo inside main

void foo();

void helper() {
  foo();
}

int main() {
  foo();
  return 0;
}
