
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    vect pos; // vect to be inlined here
    vect speed;
  } obj;

int main() {
  obj a = {{0,0},{0,0}};
  int nx = a.pos.x + a.speed.x;
  int ny = a.pos.y + a.speed.y;
  // reading of 'a.pos' without a '.x' or '.y' at the end is not accepted by this transformation
}

