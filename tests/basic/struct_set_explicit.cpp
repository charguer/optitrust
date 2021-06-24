
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() { // TODO: same issue as function inlining
  return {1,1};
}
int main() {
  vect p = {0,0};
  vect b = p;

  vect d;
  d = p;

  // d = { 1, 2 }; // TODO !

  // vect e;
  // e = f();
  // TODO LATER: demo of  insert_decl ~name:"x" ~body:"f()"
  //  what would be nice is to  insert_decl ~name:"x" ~body_path:[cVarDef "b"; sExpr ]
  obj a = {0,{0,0},{0,0}};
  vect u;
  u = a.pos;
  vect t[2];
  vect p2 = p;
  t[0] = p2;
  /* TODO; unrestricted
  LFS = p;
  ->
  LFS.x = p.x;
  */
//  for (int i = 0; i < 10; i++){
//    int x = 0;
//    x += i;
//  }


}
