typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;

int main() {
  
  vect v = {0,0};
  particle p = {{0,0},{0,0}};

  vect u = p.pos;


  particle p2 = p;
  particle p3[2];

  return 0;
}

