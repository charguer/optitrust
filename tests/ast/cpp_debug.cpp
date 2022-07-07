int main(){
  
  int* x = new int;
  
  int* y = new int[2];

  delete x;
  delete[] y;

  return 0;
}