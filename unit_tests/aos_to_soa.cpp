
const int B = 10;

typedef struct { int x; int y; } vect;

// Below is the strange C syntax for saying that T is a shorthand for vect[B]
typedef vect vects[B];

// Case of a global group (array) of B vectors
vects t;

// Case of a pointer on a global group of B vectors
vects* u;

// Case of a fixed-sized array of groups of B vectors
vects w[100];

int main() {
  int i;
  int a = t[i].x;
  int b = u[99][i].x;
  // TODO: this one is not properly translated:
  // int b2 = (*u)[i].x;
  int c = w[99][i].x;
}

// TODO: it would be nice to keep track in the AST of the order of the fields, in the form of a list of names, even though we use (and want to continue) using a map for describing the contents of the fields. This would allow printing fields in the same order as they were specified.

/* TODO: the output uses 10 as opposed to B. I think Damien said this was a limitation of Clang which does the expansion too aggressively. We had some ideas for work-arounds but I don't recall which one at the moment.

typedef struct {
  int y[10];
  int x[10];
} vects;
*/
