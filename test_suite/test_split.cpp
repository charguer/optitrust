int main () {
  int t[10] = {0,0,0,0,0,0,0,0,0,0};
  
  #pragma omp parallel
  for (int i = 0; i < 10; i++) {
    int x = i % 2;
    int y = i / 2;
    int z = i * 3;
    t[i] += i * (1 - x) + y - z;
    x = (1 - x);
    t[9 - i] += i * (1 - x) - z;
  }

  return 0;
}
