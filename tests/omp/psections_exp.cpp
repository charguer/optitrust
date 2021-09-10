void XAXIS();

void YAXIS();

void ZAXIS();

void sect_example() {
#pragma omp parallel sections
  {
#pragma omp section
    XAXIS();
#pragma omp section
    YAXIS();
#pragma omp section
    ZAXIS();
  }
}