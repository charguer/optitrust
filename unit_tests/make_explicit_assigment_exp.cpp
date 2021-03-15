typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

int main() {
  vect p = {0, 0};
  vect s = {0, 0};
  obj a = {0, {0, 0}, s};
  obj b;
  overloaded = (b, a);
  int nx = (((a.pos).x) + ((a.speed).x));
  int ny = (((a.pos).y) + ((a.speed).y));
}
