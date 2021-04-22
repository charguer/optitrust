typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    vect pos; // vect to be inlined here
    vect speed;
  } obj;

typedef struct {
   obj items[10];
} bag; 


int main() {
  vect p = {0,0};
  const vect q = {0,0};
  // vect s = {0,0};
  
  // obj a = {0,{0,0},s};
  // obj b = {0, p, s};
  
  // int nx = a.pos.x + a.speed.x;
  // int ny = a.pos.y + a.speed.y;

  // a.pos.x = 5;
  // p.x = 5; // no change

  // vect t = {1,0};
  // int z = t.x;
  // vect u;
  // u = {0,0};
 
  // reading of 'a.pos' without a '.x' or '.y' at the end is not accepted by this transformation
 
  // a.pos = { 5, 6 }
  // -> a.pos_x = 5; a.pos_y = 6

  

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

  // after:   ..

  // target assignement on "v":
  //  { vect v;    vx = v2.x;  vy = v2.y }@"this is actually an initialization"


}
