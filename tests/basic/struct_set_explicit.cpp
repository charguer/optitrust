
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
  vect p = {0,0};
  vect b; 
  b = p;
  obj a = {0,{0,0},{0,0}};
  vect u;
  u = a.pos;
  vect t[2];
  vect p2 = p;
  t[0] = p2;
  return 0;  
}
