typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    vect pos; // vect to be inlined here
    vect speed;
  } obj;



int main() {
  vect p = {0,0};
  vect s = {0,0};
  
  obj a = {0,{0,0},s};
  obj b = {0, p, s};
  // obj c = {0, p.x, p.y, s};
  int nx = a.pos.x + a.speed.x;
  int ny = a.pos.y + a.speed.y;

  a.pos.x = 5;
  p.x = 5; // no change

  vect t = {1,0};
  int z = t.x;
  vect u;
  u = {0,0};
 

}
