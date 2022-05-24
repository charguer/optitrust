
void test_decl (){

   {
    int a;
    int b = 2;
    int c = 5;
    int d = 6;
    int e = 7;
   }


}

void test_loop(){
  int a = 0; 
  {
    int g = 10;
    for(int i = 0; i < 10; i++){
      i = i + 1;
    }
  }
}

int main() {
  return 0;
}