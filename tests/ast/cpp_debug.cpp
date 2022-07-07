class Test_method_inline {
  
  public:
    int u;
    int f1(){
      int a = u + u;
      return a + a;
    }

};


int main(){
  
  Test_method_inline c;
  int z = c.u;
  int y1 = c.f1();

  return 0;
}
