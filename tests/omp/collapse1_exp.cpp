void bar(float *a, int i, int j, int k);

int kl;

int ku;

int ks;

int jl;

int ju;

int js;

int il;

int iu;

int is;

void sub(float *a) {
#pragma omp for collapse(2) private(i, k, j)
  for (int k = kl; (k < ku); k += ks)
    for (int j = jl; (j < ju); j += js)
      for (int i = il; (i < iu); i += is)
        bar(a, i, j, k);
}