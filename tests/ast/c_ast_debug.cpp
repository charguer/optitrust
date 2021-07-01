
/* disallowed: typedef struct foo { int x; int y; } vect; */

typedef struct { int x; int y; } vect;

// TODO: report this test in c_ast
// todo: when printing this type, the first "vects" is missing
typedef struct vects { vect head; vects* tail; } vects;

typedef vect myvect;

int ref_on_mutable_int() {
  int x = 3;
  int& rx = x;
  rx = 4;
  return rx;
}

int main() {
    vect v = { 1, 2 };
    myvect w = { 3, 4 };
    int vx = v.x;
    int wy = w.y;
}
