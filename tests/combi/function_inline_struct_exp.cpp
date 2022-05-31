typedef struct {
  int x;
  int y;
} vect;

vect f(int a) { return (vect){a - 1, a}; }

int g(vect v) { return v.x + v.y; }

int main() {
  int s = g((vect){2 - 1, 2});

  int p = 3 - 1;
}
