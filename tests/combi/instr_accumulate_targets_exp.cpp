typedef struct {
  int x;
  int y;
} vect;

int main() {
  int x = 10;
  x = (x + (((((2 + 3) + 4) + 5) + 6) + 7));
  vect result = {0, 0};
  result.x += (((((2 + 3) + 4) + 5) + 6) + 7);
  return 0;
}
