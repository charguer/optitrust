typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

typedef int* intstar;

int f(int n) {
  return n;
}

void g(int t[2], vect* varg) {
  int b = t[0];
  int a = varg->x;
}

int main() {
  // loops
  for (int i = 0; i < 10; i++) {
     for (int j = 0; j < 5; j += 1) {
       i++;
       break;
     }
     continue;
  }
  // local variables
  int val = 13;
  int r = 3;
  r = r + 1;
  r += 2;
  r++;
  int s = f(r);
  // pointers
  int n = 3;
  int* m = &n;
  *m = *m + 4;
  // arrays
  int t[2] = { 5, 6 };
  int u = t[0];
  t[1] = u + 2;
  // structs
  vect v = { 5, 6 };
  int a = v.x;
  v.y = a + 2;
  vect v2 = v;
  v2 = v;
  particle p1 = { v, v };
  particle p2 = { v, { 7,8 } };
  // sequences
  {
    int r1 = 1;
    int r2 = 2;
    int r3 = r2;
  }
  // calls
  int y = f(2);
  lbl2: g(t, &v);

  int k;
  for(k = 0; k < 10; k++){
    y = k;
  }
}
