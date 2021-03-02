
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
  // a.pos.x   similar to damien's code a.pos[x] to a.x[pos]  from aos-to-soa
  /* LATER (much harder)
  vect p = a.pos;
  becomes
  vect p = { a.pos_x, a.pos_y};
  */
}

// TODO: this one is not yet implemented
