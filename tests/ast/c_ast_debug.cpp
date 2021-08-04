
/* disallowed: typedef struct foo { int x; int y; } vect; */

typedef struct { int x; int y; } vect;


typedef vect myvect;

int ref_on_mutable_int() {
  int x = 3;
  int& rx = x;
  rx = 4;
  return rx;
}

int ref_on_immutable_int() {
  const int x = 3;  // same as [int const x = 3]
  const int& rx = x; // same as [int const& rx = x]
  return rx;
}

int ref_on_field() {
  vect v = { 2, 3 };
  int& vx = v.x;
  vx = 5;
  return vx;
}

int ref_on_mutable_int_cell() {
  int t[2] = { 4, 5 };
  int* const p0 = &t[0];
  int& r0 = t[0];
  // *p0 = 6;
  r0 = 7;
  return *p0 + r0;
}

int ref_on_immutable_int_cell() {
  int const t[2] = { 4, 5 };
  int const& r0 = t[0];
  vect const v[2] = { { 2, 3 }, { 4, 5 } };
  vect const& w = v[0];
  return r0 + w.x;
}

int ref_argument(int& x, int const& y, int const& z) {
  x = x + y;
  int& u = x;
  const int& v = y;
  return u + v + z;
}

int main() {
    vect v = { 1, 2 };
    myvect w = { 3, 4 };
    int vx = v.x;
    int wy = w.y;
    int a = 3;
    int b = 4;
    const int c = 5;
    ref_argument(a, b, c);
}
