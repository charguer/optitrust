
int f(int x) {
  return x + x;
}

int g(int x) {
  if (x < 0) {
    return 0;
  }
  int t = x + 1;
  return t;
}
typedef struct {
  double x, y, z;
} vect;


// inline_fun_call // inline_call  path_to_the_call
// this path could denote several function calls
vect v_add(vect v1, vect v2) {
  vect r = { v1.x + v2.x,
             v1.y + v2.y,
             v1.z + v2.z };
  return r;
}
// function def is preserved, and other calls to it as well
int main() {

  int x = 3;
  int y = f(g(x));


  int y;
  vect v1,v2;
  // vect v3 = v_add(v1,v2);
  vect v3;
  v3 = v_add(v1,v2);
  // y = f(3);
  // y = g(-1);
  // y = g(2);
   /* 
   int res;
   def_arg1: int x= -1;
   if (x < 0) {
     res = 0;
     goto exit;
   }
   int t = x + 1;
   res = t;
   exit: y = res;
  */
  int z = 3 + 3 + 3;
  return 0;
}
