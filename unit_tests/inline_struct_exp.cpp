
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int pos_x;
    int pos_y;
    vect speed;
  } obj;

int main() {
  obj a = { 1, 2 , { 3, 4 } };
  int nx = a.pos_x + a.speed.x;
  int ny = a.pos_y + a.speed.y;
}

