

void test_equality (){

  int x = 1;
  
  int y = 2;

  y = x ;
}

int f (int a, int b){


  return a + b;
}


void test_function_call (){
  int x = 1;
  int y = 10;
  int z;
  z = f(x, y);
}



void test_array () {

  int x[3] = {1, 2, 3};
  int y[3] = {1, 2, 3};
  int z;
  z = f(x[0], y[0]);
}

