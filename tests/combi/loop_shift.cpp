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

  float* input;
  float* output;
  for (int bi = 0; bi < N; bi += 32) {
    for (int i = 0; i < 32; i++) {
      float sum = 0.;
      for (int k2 = 0; k2 < N; k2++) {
        sum += input[k2 + N * /*@__19*/ (i - -bi) /*__19@*/];
      }
      output[N * /*@__19*/ (i - -bi) /*__19@*/] = sum;
    }
  }
}