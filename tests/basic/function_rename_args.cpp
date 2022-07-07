

void test_no_args(){
  int z = 10;
  z = 20;
}


void test_one_arg(int x){
  int z = x;
  z = 10;
}


void test_two_args(int x, int y){
  int z = x;
  for(int x = 0; x < 4; x++){
    z += x + y;
    int y = 1;
    int r = y + x;
  }
  int a = x + y;
}

class Test_method_rename_args {
  private:
    int u;
  
  public:
    int test_method_args(int x){
      int a = u + u;
      return a + a;
    }
};
