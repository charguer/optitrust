int const N = 1000;

#pragma omp declare target

float p[N];

#pragma omp end declare target

float v1[N];

float v2[N];

void init(float *, float *, int);

void output(float *, int);

void vec_mult() {
  init(v1, v2, N);
#pragma omp target update to(v1, v2)
#pragma omp target
#pragma omp parallel for
  for (int i = 0; (i < N); i++)
    p[i] = (v1[i] * v2[i]);
#pragma omp target update from(p)
  output(p, N);
}