
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
  vect u = a.pos;

  const obj c = {0,{0,0},{0,0}};
  obj *const v = &a;
  *v = c;
}
