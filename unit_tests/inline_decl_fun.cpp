
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


// inline_fun_call // inline_call  path_to_the_call
// this path could denote several function calls

// function def is preserved, and other calls to it as well
int main() {
  int y;
  y = f(3);
  y = g(-1);
  y = g(2);
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
