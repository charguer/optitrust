
typedef int* T;  // remove T
typedef int* U;

int x; // remove x // TODO: does not work at the moment
int y;

void f(int x) { } // remove f

// void f(T t) { } : it this line was present, should raise an error on removal of f

void g(U y) { }

int main() {
  int z = 3; // remove z

  // int x = 2;   : it this line is present, there are conflicting occurences of x
  // f(2)  : if this line is present, should raise an error on removal of f
}
