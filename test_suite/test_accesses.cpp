typedef struct {
  int x, y;
} point;

typedef struct {
  int t[3];
} vect;

// struct as fun arg
int get_x (point p) {
  return p.x;
}

// array as fun arg
int get_1 (int t[3]) {
  return (t[1] + *(t + 1)) / 2;
}

int main () {
  // pointer to struct
  point *p = new point;
  p->x = 2;
  p->y = p->x;
  delete p;

  // struct
  point q;
  q.x = 2;
  q.y = q.x;

  // const struct
  const point r = {1, 2};
  q.y = r.x;

  // array
  int t[3];
  t[1] = 2;
  t[0] = t[1];
  *(t + 2) = *t;

  // array as pointer
  int *s = new int[3];
  s[1] = 2;
  s[0] = s[1];
  *(s + 2) = *s;
  delete[] s;

  // array of struct
  point a[3];
  a[1].x = 2;
  a[1].y = a[1].x;
  (*(a + 1)).x = (*(a + 1)).y;

  // struct of array
  vect v;
  v.t[1] = 2;
  v.t[0] = v.t[1];
  *(v.t + 2) = *(v.t);

  return 0;
}
