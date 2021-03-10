
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    vect pos; // vect to be inlined here
    vect speed;
  } obj;

int main() {
  vect p = {0,0};
  vect s = {0,0};
  
  obj a = {0,p,s};
  int nx = a.pos.x + a.speed.x;
  int ny = a.pos.y + a.speed.y;
  // reading of 'a.pos' without a '.x' or '.y' at the end is not accepted by this transformation
 
  // a.pos = { 5, 6 }
  // -> a.pos_x = 5; a.pos_y = 6

  // operation_set(&a.pos, p)

  // implement only this one:
  // a.pos.x = v
  // -> a.pos_x = v


  //---
  // operation_get(&a.pos)


  
  //  ANOTHER TRANSFORMATION: struct_assignement_make_explicit
  // vect v2; v2 = v1

  

  // equivalent to
  // v2.x = v1.x;
  // v2.y = v1.y

  // NOW the inline struct would be ABLE TO HANDLE:
  // a.pos = p

   // USER RESPONSABILITY to invoke struct_assignement_make_explicit
   // a.pos.x = p.x
   // a.pos.y = p.y

   // at this point the inline_struct transformation applies:

  // a.pos_x = p.x ; 
  // a.pos_y = p.y;

  // IMPORTANT: if inline_struct  finds  "a.pos" without ".x" or ".y"
  // it should fail
  

  // v = v2  transformation should apply even if not part of a variable def

  // IN SHORT:  remove decoration and assign each field, and inline seq in surounding seq
  // TODO LET'S POSTPONE: test   inline seq in surounding seq

  // vect v = v2
  // { vect v;    v = v2 }@"this is actually an initialization"

  // EASIEST: user to explicitly request "detach_initialization v"
  // before :  vect v = v2
  // after :   vect v ;   v = v2   // without the decoration

  // before:  cSeq ~body [ cSeq ~body:[cSeq [cVarDef "v" ; cAssign ~name:"v"] ] ] ]
  // after:   ..

  // target assignement on "v":
  //  { vect v;    vx = v2.x;  vy = v2.y }@"this is actually an initialization"
}

