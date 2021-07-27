
typedef struct { int x, y; } vect;

int f(vect h) {    
  return h.x; 
}

void test1() {
  vect v = {1, 2};
  int x;
  x = f(v);
}

vect g(int a) { 
  vect r = { a, a };
  return r;
}

void test2() {
  vect r;
  r = g(1);
}

int glob = 1;


int h(vect a, vect b){
  int x = a.x + b.x + glob;
  return x;
}

void test3() {
  int glob = 2;
  vect x = { 0, 1 };
  vect a = { 2, 3 };
  int r;
  r = h(x, a);
  int b;
  b = h(x, g(4));
}


vect add(vect v1, vect v2) {
  return {v1.x + v2.x, v1.y + v2.y};
}

int main() {
  vect v1 = { 0, 1 };
  vect res;
  res = add(v1, { 2, 3 });
  return 0;
}
