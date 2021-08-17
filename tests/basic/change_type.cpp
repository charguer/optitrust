
const int B = 10;

const int N = 100;

typedef struct { int x; int y; } vect;

// Below is the strange C syntax for saying that T is a shorthand for vect[B]
// typedef vect vects[B];

// Case of a fixed-sized array of groups of B vectors

vect w[N];
vect u[N];

void f(vect r[N]) {
  r[0].x++;
}

int main() {
  int i;
  int c = w[i].x;
  // vect w2[N];
  // w2[0] = w[0];
  f(w);
  int d = u[i].x; 

}

