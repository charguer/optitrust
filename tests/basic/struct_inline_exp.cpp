typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int posX;
  int posY;
  int speedX;
  int speedY;
} obj;

typedef struct { vect foo; } dummy;

int main() {
  vect p = {0, 0};
  vect s = {0, 0};
  dummy d = {p};
  vect dfoo = d.foo;
  obj a = {0, 0, 0, s};
  obj b = {0, p.x, p.y, s};
  int nx = a.posX + a.speedX;
  int ny = a.posY + a.speedY;
  a.posX = 5;
  p.x = 5;
  vect t = {1, 0};
  int z = t.x;
  vect u = {0, 0};
}
