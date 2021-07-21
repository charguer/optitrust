int main(){
  int i, w;
  int  x[1000];
  int  y[1000];
  for (i = 0; i < 1000; i++) {
    x[i] += y[i];
    if (true)
      y[i] = 0;
  }
  return 0;
}