int ANY(int maxValue) { return 0; }

int const N = 2;

typedef int T;

int main() {
  T a;
  /*@section_of_interest*/ T x[N];
  x[0] = a;
  for (int k = 1; (k < N); k++) {
    x[k] = 0;
  }
  for (int i = 0; (i < N); i++) {
    x[ANY(0)]++;
  }
  a = x[0];
  for (int k = 1; (k < N); k++) {
    a += x[k];
  } /*section_of_interest@*/
  int y = 0;
  return 0;
}