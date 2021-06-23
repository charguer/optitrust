
typedef struct { int x, y; } vect;

int f(vect h) {    
  return h.x; 
}

void test1() {
  vect v = {1, 2};
  int x;
  x = f(v);
}

vect g(int a) { 
  vect r = { a, a };
  return r;
}

void test2() {
  vect r;
  r = g(1);
}

int glob = 1;


int h(vect a, vect b){
  int x = a.x + b.x + glob;
  return x;
}

void test3() {
  int glob = 2;
  vect x = { 0, 1 };
  vect a = { 2, 3 };
  int r;
  r = h(x, a);
  int b;
  b = h(x, g(4));
}


vect add(vect v1, vect v2) {
  return {v1.x + v2.x, v1.y + v2.y};
}

int main() {
  vect v1 = { 0, 1 };
  vect res;
  res = add(v1, { 2, 3 });
  return 0;
}
/*
void test3() {
  int glob = 2;
  vect x = {0, 1};
  vect a = {2, 3};
  int r;
  {
    inlinedArg1: vect a_0 = x; 
    inlinedArg2: vect b_0 = a; 
    inlinedRes: int res_1;
    int x = (((a_0.x) + (b_0.x)) + glob);
    res_1 = x;
    r = res_1;
  }
Beware about "capture" => need at least to check that this does not happen.
=> the free varaibles of inlined function  should not be bound in the target place.
=> question: how could we "compare" variables to see that "glob" is from the
local context and not the global one as before. 

Strategy for simplifying inlining:
- for each argument, if it's a variable, always inline
- if it's a value, by default inline, unless user says not to
- if it's a term, never inline
- r = res_1   in this case elimiate res1

loop1: for (i..)
loop2: for (i..)

consider now

 add(add(v1,V2), v3)

res = { v1.x + v2.x, ... };
 add(res, v3)
 
   inline_fun_core

  simplify_inlining: it can use inline_decl for args or res
  and remove the braces and the label at the end.

  inline_fun = iniline_fun_core + simplify_inlining

  */