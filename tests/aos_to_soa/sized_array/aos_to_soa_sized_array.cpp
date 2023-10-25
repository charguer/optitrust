
const int B = 10;

const int N = 100;

typedef struct { int x; int y; } vect;

// Below is the strange C syntax for saying that T is a shorthand for vect[B]
// typedef vect vects[B];

// Case of a fixed-sized array of groups of B vectors

typedef struct { int x; int y; } vect2;

vect2 w[N];
vect u[N];


int main() {
  int i;
  int c = w[i].x;
  int d = u[i].x; // this should not be changed

}

