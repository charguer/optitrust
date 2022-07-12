class Method_const {

  public:
    int x;
    Method_const(int val) {
      x = val;}
    
    int get_x(){return x;}

};


void test_method_const (){
  Method_const foo(10); 
  
  int y;
  y = foo.get_x();

}


int main(){}