class Method_test {

  public:
    int x;
    Method_test (int val) {
      x = val;}
    
    int get1(){return x;}

    const int& get2(){return x;}

};


void test_method_const (){
  Method_test foo(10); 
  
  int y;
  y = foo.get1();
  const int z = foo.get2();

}


int main(){}
