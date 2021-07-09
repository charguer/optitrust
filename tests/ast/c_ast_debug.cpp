
/* disallowed: typedef struct foo { int x; int y; } vect; */

// typedef struct { int x; int y; } vect;

// TODO: report this test in c_ast
// todo: when printing this type, the first "vects" is missing
// typedef struct vects { vect head; vects* tail; } vects;

// typedef vect myvect;


int ref_on_mutable_int_cell() {
  int t[2] = { 4, 5 };
  const int*  p0 = &t[0];
  // int& r0 = t[0];
  // *p0 = 6;
  // r0 = 7;
  return *p0;/*  + r0; */
}

