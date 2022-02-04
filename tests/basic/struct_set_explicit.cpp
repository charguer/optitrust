
typedef struct {
  int x;
  int y; }
vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() {
  return {1,1};
}

int main() {
  int x = 1;
  int y;
  x = y;
  vect p = {0,0};
  vect b;
  b = p;
  vect u;
  obj a = {0,{0,0},{0,0}};
  u = a.pos;
  vect t[2];
  vect p2 = p;
  t[0] = p2;
  obj c;
  c = a;
  return 0;
}
