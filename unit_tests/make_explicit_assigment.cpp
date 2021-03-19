
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
  vect b =p;
  
  vect d;
  //d =  f(); //TODO this should fail with a nice error mesage 
  // saying that field assignement can be made explicit only when
  // the right hand side is a value (or variable) 
  // TODO LATER: demo of  insert_decl ~name:"x" ~body:"f()"
  //  what would be nice is to  insert_decl ~name:"x" ~body_path:[cVarDef "b"; cExpr ]
  obj a = {0,{0,0},0};

  a.pos = p;
 
  //vect c = p;
  // encoded as   { const vect* c = new vect;  assign(c, p)@Initialisation_instruction }@Heap_allocated
  // { { const vect c;  }@Heap_allocated;   assign(c, p)  }@No_braces
  // vect c; c = p

  //   cSeq [  ]

  // https://www.youtube.com/watch?v=1ArVtCQqQRE

  //vect c ; 
   //c = p // encoded as overloaded=(c,get(p))
   // todo:  c.x = p.x    encoded as overloaded=(access_field c "x", get(access_field p x))
}
