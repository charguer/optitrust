typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int pos_x;
  int pos_y;
  int speed_x;
  int speed_y;
} obj;

int main() {
  vect p = {0, 0};
  vect s = {0, 0};
  obj a = {0, 0, 0, s.x, s.y};
  obj b = {0, p.x, p.y, s.x, s.y};
  int nx = (a.pos_x + a.speed_x);
  int ny = (a.pos_y + a.speed_y);
  a.pos_x = 5;
  p.x = 5;
  vect t = {1, 0};
  int z = t.x;
  vect u;
  u = {0, 0};
}