
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    int pos_x;
    int pos_y;
    vect speed;
  } obj;

int main() {
  obj a = { 1, 2 , { 3, 4 } };
  int nx = a.pos_x + a.speed.x;
  int ny = a.pos_y + a.speed.y;
  // a.pos.x   similar to damien's code a.pos[x] to a.x[pos]  from aos-to-soa
  /* LATER (obtained as make_explicit_assignment + inline_decl_struct + make_implicit_assignment
  where make_implicit_assignmanet is a combination of isolate sub-sequence + make_implicit_assignment_core
  where make_implicit_assignment_core is {vect v;v.x = t1; v.y = t2} -> vect v = {t1;t2}
  where isolate sub-sequence it takes the path on the first elment to be puted on the first subsequence
  the number of elements in the sub-sequence(LATER :a constrain to determine which operations are going to be part of it)
  
  )
  vect p = a.pos;
  becomes
  vect p = { a.pos_x, a.pos_y};
  */
}

// TODO: this one is not yet implemented
