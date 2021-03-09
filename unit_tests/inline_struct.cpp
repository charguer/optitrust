
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
  
  obj a = {0,p,s};
  int nx = a.pos.x + a.speed.x;
  int ny = a.pos.y + a.speed.y;
  // reading of 'a.pos' without a '.x' or '.y' at the end is not accepted by this transformation
}

