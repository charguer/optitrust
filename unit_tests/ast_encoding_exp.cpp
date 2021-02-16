typedef struct {
  int y;
  int x;
} vect;

int f(int n) { return n; }

void stack_var() {
  {
    {
      const int *r = new int;
      set(r, 3);
    }
    set(r, ((*r) + 1));
    set(r, ((*r) + 2));
    operator++(r);
    {
      const int *s = new int;
      set(s, f((*r)));
    }
  }
  delete s;
  delete r;
}

void stack_array() {
  {
    {
      const int[2] *t = new int[2];
      set(t, {5, 6});
    }
    {
      const int *a = new int;
      set(a, (*array_access(t, 0)));
    }
    set(array_access(t, 1), ((*a) + 2));
  }
  delete a;
  delete t;
}

void stack_struct() {
  {
    {
      const vect *v = new vect;
      set(v, {5, 6});
    }
    {
      const int *a = new int;
      set(a, (*struct_access(v, x)));
    }
    set(struct_access(v, y), ((*a) + 2));
    {
      const vect *v2 = new vect;
      set(v2, (*v));
    }
  }
  delete v2;
  delete a;
  delete v;
}

void constants() {
  {
    const int a = 3;
    const int b = (a + 3);
    {
      const int *c = new int;
      set(c, (b + 4));
    }
  }
  delete c;
}

typedef int *intstar;

void const_pointers() {
  {
    {
      const int *a = new int;
      set(a, 3);
    }
    const intstar b = a;
    const int c = ((*b) + 4);
  }
  delete a;
}

void nonconst_pointers() {
  {
    {
      const int *a = new int;
      set(a, 3);
    }
    {
      const int **b = new int *;
      set(b, a);
    }
    set((*b), ((*(*b)) + 4));
  }
  delete b;
  delete a;
}

void by_value(int t[2], vect v) {
  {
    {
      const int *b = new int;
      set(b, t[0]);
    }
    {
      const int *a = new int;
      set(a, (v.x));
    }
    {
      const vect *v2 = new vect;
      set(v2, v);
    }
  }
  delete v2;
  delete a;
  delete b;
}

int main() {}
