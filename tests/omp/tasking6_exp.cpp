int const LARGE_NUMBER = 10000000;

double item[LARGE_NUMBER];

void process(double);

int main() {
#pragma omp parallel
  {
#pragma omp single
    {
#pragma omp task untied
      {
        for (int i = 0; (i < LARGE_NUMBER); i++) {
#pragma omp task
          process(item[i]);
        }
      }
    }
  }
  return 0;
}