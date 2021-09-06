
const int B = 10;

typedef struct { int x; int y; } vect;

// Below is the strange C syntax for saying that T is a shorthand for vect[B]

typedef vect vects[B];
// Case of a global group (array) of B vectors
vects t;

// Case of a pointer on a global group of B vectors
vects* u;

// Case of a fixed-sized array of groups of B vectors
const int N = 100;
vects w[N];

int main() {
  int i;
  int a = t[i].x;
  int b = u[9][i].x;
  int c = w[99][i].x;

  // LATER: this one is not properly translated:
  // int d = (*u)[i].x;
  // LATER: stranger error
  // vects uget = *u;
  // int d = uget[i].x;

}


