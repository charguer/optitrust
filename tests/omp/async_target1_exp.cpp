#pragma omp declare target

float F(float);

#pragma omp end declare target

int const N = 1000000000;

int const CHUNKSZ = 1000000;

void init(float *, int);

float Z[N];

void pipedF() {
  int C;
  init(Z, N);
  for (int C = 0; (C < N); C += CHUNKSZ) {
#pragma omp task shared(z)
#pragma omp target teams map(from : Z [C:CHUNKSZ])
#pragma omp parallel for
    for (int i = 0; (i < CHUNKSZ); i++)
      Z[i] = F(Z[i]);
#pragma omp taskwait
  }
}