void work(int n);

void sub3(int n) {
  work(n);
#pragma omp barrier
  work(n);
}

void sub2(int k) {
#pragma omp parallel shared(k)
  sub3(k);
}

void sub1(int n) {
#pragma omp parallel private(i) shared(n)
  {
#pragma omp for
    for (int i = 0; (i < n); i++)
      sub2(i);
  }
}

int main() {
  sub1(2);
  sub2(2);
  sub3(2);
  return 0;
}