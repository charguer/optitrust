typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() { return {1, 1}; }

int main() {
  {
    {
      const vect *p = new vect;
      set(p, {0, 0});
    }
    {
      const vect *b = new vect;
      set(b, (*p));
    }
    { const vect *d = new vect; }
    set(struct_access(d, x), 1);
    set(struct_access(d, y), 2);
    { const vect *e = new vect; }
    set(e, f());
    {
      const obj *a = new obj;
      set(a, {0, {0, 0}, 0});
    }
    set(struct_access(a, pos), (*p));
  }
  delete a;
  delete e;
  delete d;
  delete b;
  delete p;
}
