int main(){

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 2; i < 10; i++){
    x += i;
  }

  for (int j = st; j < 15; j++){
    x += j;
  }

  int ld = 2;
  int u = N + 5;
  for (int k = 0; k < N; k++){
    x += k;
  }

  for (int l = 3; l < 3; l++) {
    x += l;
  }
}