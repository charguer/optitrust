typedef struct {
  int x;
  int y;
} vect;

int main() {
  int x = 10;
test1:
  x += 2 + 3 + 4 + 5 + 6 + 7;
  vect result = {0, 0};
test2:
  result.x += 2 + 3 + 4 + 5 + 6 + 7;
  return 0;
}
