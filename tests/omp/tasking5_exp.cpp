const int LARGE_NUMBER = 10000000;

double item[LARGE_NUMBER];

void process(double);

int main() {
  int i;
#pragma omp parallel
  {
#pragma omp single
    {
      for (i = 0; i < LARGE_NUMBER; i++) {
#pragma omp task
        process(item[i]);
      }
    }
  }
}
