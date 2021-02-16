typedef struct {
  int y;
  int x;
} vect;

int f(int n) { return n; }

void stack_var() {
  {
    {
      const int *r = new int;
      r = 3;
    }
    r = ((*r) + 1);
    r++;
    {
      const int *s = new int;
      s = f((*r));
    }
  }
  delete s;
  delete r;
}

void stack_array() {
  {
    {
      const int[2] *t = new int[2];
      t = {5, 6};
    }
    {
      const int *a = new int;
      a = (*t[0]);
    }
    t[1] = ((*a) + 2);
  }
  delete a;
  delete t;
}

void stack_struct() {
  {
    {
      const vect *v = new vect;
      v = {5, 6};
    }
    {
      const int *a = new int;
      a = (*(&(v.x)));
    }
    (&(v.y)) = ((*a) + 2);
    {
      const vect *v2 = new vect;
      v2 = (*v);
    }
  }
  delete v2;
  delete a;
  delete v;
}

void by_value(int t[2], vect v) {
  {
    {
      const int *b = new int;
      b = t[0];
    }
    {
      const int *a = new int;
      a = (&(v.x));
    }
    {
      const vect *v2 = new vect;
      v2 = v;
    }
  }
  delete v2;
  delete a;
  delete b;
}

int main() {}
