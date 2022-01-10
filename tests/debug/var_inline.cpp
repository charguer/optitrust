typedef struct {
  int x;
  int y; 
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;

vect f() {
  return {1,1};
}

int main() {
  
  vect v = {0, 1};
  
  particle p = {{0,0},{0,0}};
  

  
  particle p2 = {p.pos, p.speed};

  int a, b;

  a = v.x;
  b = v.y;

  a = p2.pos.x;
  b = p2.pos.y; 
  


  // vect b;
  // b = p;
  // vect u;
  // obj a = {0,{0,0},{0,0}};
  // u = a.pos;
  // vect t[2];
  // vect p2 = p;
  // t[0] = p2;
  // obj c;
  // c = a;
  return 0;
}
