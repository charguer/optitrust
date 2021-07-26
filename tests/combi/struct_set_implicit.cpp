
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
  b.x = p.x;
  b.y = p.y;
  
  vect d;
  d.x = 1;
  d.y = 2;
}
