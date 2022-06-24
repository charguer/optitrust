
int g(int a, int b) {
  return a + b + a;
}

int f(int x) {
  int y = 5;
  int z = 3;
  return y + z;
}

void test_invariant1 (int x){
  int a;
  a = f(x);
  f(x);
}

void test_invariant2(int x){
  
  const int c = f(x);
  

}

void test_detach (int x){ 
  int d = f(x);
}


void test_expression (int x){

  int b = f(x) + g(0, 1);

}

void test_nested_call(int x){
  int e = g(f(x), x);

}


void test_indepth(int x){

  int z = g(f(x),x);

}



int main() {}
