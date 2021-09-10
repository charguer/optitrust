typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() { return {1, 1}; }

int main() {
  vect p = {0, 0};
  vect b;
  b = p;
  vect d;
  d = {1, 2};
}