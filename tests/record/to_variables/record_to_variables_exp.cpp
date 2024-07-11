typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() { return (vect){1, 1}; }

int main() {
  vect p = {0, 0};
  int sX;
  int sY;
  sX = p.x;
  sY = p.y;
  int aWeight;
  vect aPos;
  vect aSpeed;
  aWeight = 0;
  aPos = p;
  aSpeed = {sX, sY};
  const int bWeight = 0;
  const vect bPos = p;
  const vect bSpeed = {sX, sY};
  return 0;
}
