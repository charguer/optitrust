typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int pos_x;
  int pos_y;
  vect speed;
} obj;

int main() {
  vect p = {0, 0};
  vect s = {0, 0};
  obj a = {0, 0, 0, s};
  obj b = {0, p.y, p.x, s};
  int nx = (a.pos_x + a.speed.x);
  int ny = (a.pos_y + a.speed.y);
  a.pos_x = 5;
  p.x = 5;
  vect t = {1, 0};
  int z = 1;
  vect u;
  u = {0, 0};
}
