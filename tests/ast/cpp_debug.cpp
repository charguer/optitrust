class Test_method_inline {
  private:
    int u;
  
  public:
    int f1(){
      int a = u + u;
      return a + a;
    }

};


int main(){
  
  Test_method_inline c;
  int y1 = c.f1();

  return 0;
}
