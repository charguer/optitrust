int f(int x){
  return x + 1;
}


int main(){

  int p = 10;
  int q = p;
  q = p;

  int &x = p;
  x = x + 1;
  int y = f(x) + 1;


  return 0;
}