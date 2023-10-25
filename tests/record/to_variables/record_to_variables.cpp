
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
  vect s;
  s.x = p.x;
  s.y = p.y;
  
  obj a;
  a.weight = 0;
  a.pos = p;
  a.speed = s;
  
  return 0;
}
