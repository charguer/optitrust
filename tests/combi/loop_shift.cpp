int main(){
  
  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 2; i < 10; i++){
    x += i + 2;
  }
  
  for (int i2 = 0; i2 < 10; i2++){
    x += i2;
  }

  int w = 10 + 2;
  for (int j = st; j < st+N; j++){
    x += j;
  }
 
  int shift = 5;
  for (int k = 0; k < N; k++){
    x += k;
  }
}