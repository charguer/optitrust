const int B = 10;

typedef struct {
  int y;
  int x;
} vect;

typedef struct {
  int y[10];
  int x[10];
} vects;

vects t;

vects *u;

vects w[100];

int main() {
  int i;
  int a = (t.x)[i];
  int b = (u[99].x)[i];
  int c = (w[99].x)[i];
}
