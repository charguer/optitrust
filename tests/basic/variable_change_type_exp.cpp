int const B = 10;

int const N = 100;

typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int x;
  int y;
} vect2;

vect2 w[N];

vect u[N];

void f(vect r[N]) { r[0].x++; }

int main() {
  int i;
  int c = w[i].x;
  f(w);
  int d = u[i].x;
}