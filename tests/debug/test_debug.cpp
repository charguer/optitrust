int f (int x){

  return x + 1;
}


int main(){
  
  int b = 0;
  for (int a = f(b); a != 10; a++){
    a = a + 1;
  }
}