
int test(int a){
  return a + 1;

};

int main(){
  
  for (int i = test(10); i < 10 ; i++){
    i += 10;
  }
  return 0;
  
}

