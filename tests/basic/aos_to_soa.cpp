
const int B = 10;

const int N = 100;

typedef struct { int x; int y; } vect;

// Below is the strange C syntax for saying that T is a shorthand for vect[B]
// typedef vect vects[B];



// Case of a fixed-sized array of groups of B vectors
vect w[N];

int main() {
  int i;
  int c = w[i].x;

  // LATER: this one is not properly translated:
  // int d = (*u)[i].x;
  // LATER: stranger error
  // vects uget = *u;
  // int d = uget[i].x;

}

/* TOOD:
this file rename to  aos_to_sao_sized_array

update current file to using:
 typedef vect* vects;

*/
