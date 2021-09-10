int const LARGE_NUMBER = 10000000;

double item[LARGE_NUMBER];

void process(double);

int main() {
#pragma omp parallel
  {
#pragma omp single
    {
      for (int i = 0; (i < LARGE_NUMBER); i++) {
        process(item[i]);
#pragma omp task
      }
    }
  }
}