typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

typedef struct {
  obj items[10];
} bag;

int main() {
  const vect p = {0, 0};
  const vect q = {0, 0};
}
