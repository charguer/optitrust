const int THRESHOLD1 = 1000000;

const int THRESHOLD2 = 1000;

void init(float*, float*, int);

void output(float*, int);

void vec_mult(float* p, float* v1, float* v2, int N) {
  int i;
  init(v1, v2, N);
#pragma omp target parallel for if (target                         \
                                    : N > THRESHOLD1) if (parallel \
                                                          : N > THRESHOLD2 \
  ) map( to: v1[0:N], v2[:N] ) map( from: p[0:N] )
  for ( i = 0; i < N; i++ ) p[i] = v1[i] * v2[i];
  output(p, N);
}
