class Test_method_inline {

  private:
    int x;
  
  public:
    int f(int y){
      return y + x;
    }
    int g(int y) { 
      return this->x + y;
    }

};

void test_class_members_rename (){
  Test_method_inline t;
   
  int a;
  a = t.f(10);
  // encoded as 
  // int* a = new int (f(*t, 10))
  int b;
  b = t.g(a);
}

int main() {}