typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int pos_x;
  int pos_y;
  vect speed;
} obj;

int main() {
  {
    {
      const vect *p = new vect;
      set(p, {0, 0});
    }
    {
      const vect *s = new vect;
      set(s, {0, 0});
    }
    {
      const obj *a = new obj;
      set(a, {0, 0, 0, (*s)});
    }
    {
      const obj *b = new obj;
      set(b, {0, p.y, p.x, (*s)});
    }
    {
      const int *nx = new int;
      set(nx, ((*struct_access(a, pos_x)) +
               (*struct_access(struct_access(a, speed), x))));
    }
    {
      const int *ny = new int;
      set(ny, ((*struct_access(a, pos_y)) +
               (*struct_access(struct_access(a, speed), y))));
    }
    set(struct_access(a, pos_x), 5);
    set(struct_access(p, x), 5);
    {
      const vect *t = new vect;
      set(t, {1, 0});
    }
    {
      const int *z = new int;
      set(z, 1);
    }
    { const vect *u = new vect; }
    set(u, {0, 0});
  }
  delete u;
  delete z;
  delete t;
  delete ny;
  delete nx;
  delete b;
  delete a;
  delete s;
  delete p;
}
