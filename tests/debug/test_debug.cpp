class TestFieldRename {

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
  TestFieldRename t;
   
  int a = t.f(10);
  // encoded as 
  // int* a = new int (f(*t, 10))
  int b = t.g(a);
}

int main() {}