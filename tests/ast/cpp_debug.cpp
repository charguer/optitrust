class Test {
    int x;
  public:
    Test();
    int get();
    void set(int y);


};


Test :: Test(){
   x = 0;
}

int Test :: get(){
  return x;
}

void Test :: set(int y){
  x = y;
}


int main(){
  Test t;
  t.set(10);
  int y = t.get();
  
  return 0;
}