
// Description of the encoding performed for the AST representation

typedef struct { int x; int y; } vect;

// Function arguments are assumed to be 'const' by default
int f(int n) {
  return n;
}

// Stack allocated variables are turned into heap allocated variables
void stack_var() {
  int r = 3;
  r = r + 1;
  r += 2;
  r++;
  int s = f(r);
}

// Field and arrays accessed are composed in two steps:
// first, compute the address of the memory cell, then derefence it.

// For example,  t[i]  is decomposed as the operation that computes &t[i],
// then applying the star operator to it, obtaining  "*(&t[i])".
// The star is the standard dereferencing operator.
// In the AST, the operation &t[i] is the application of the primitive
// binary operation "Binop_array_access" to the argument t and i.

// Likewise,  t.x  is decomposed as *(&t.x), which involves the primitive
// unary operator "Unop_struct_access x" applied to the argument t.

void stack_array() {
  int t[2] = { 5, 6 };
  int a = t[0];
  t[1] = a + 2;
}

void stack_struct() {
  vect v = { 5, 6 };
  int a = v.x;
  v.y = a + 2;
  vect v2 = v;
}

// When arrays are passed by values, they are not heap allocated.
// They are values, out of which it is possible to directly read a value
// using the operator Binop_array_get.
// Likewise a struct passed by value is read using Unop_struct_get.
void by_value(int t[2], vect v) {
  int b = t[0];
  int a = v.x;
  // if trying to writing into t or v, should get an error, because they are assumed to be const
  // TODO: check whether we indeed see an error
  vect v2 = v; // this should make a copy of the data
}

int main() {


}