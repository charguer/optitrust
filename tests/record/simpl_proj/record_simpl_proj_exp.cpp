typedef struct {
  int x;
  int y;
  int z;
} vect;

int main() {
  vect a = {0, 1, 2};
  int x = 1;
  vect b = {3, 4, 5};
  vect c = {b.x + x * a.x, b.y + x * a.y, b.z + x * a.z};
  return 0;
}
