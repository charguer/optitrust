int main(){
  
  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 0; i < 10; i++){
    x += i;
  }
  
  for (int j = st; j < N; j++){
    x += j;
  }
 
  int cut = 5;
  for (int k = 0; k < N; k++){
    x += k;
  }
  
  for (int l = st; l < N; l++){
    x += l;
  }

}