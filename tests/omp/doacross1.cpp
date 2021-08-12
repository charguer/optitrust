float foo(int i);
float bar(float a, float b);
float baz(float b);

void work( int N, float *A, float *B, float *C )
{
  for (int i=1; i<N; i++)
  {
    A[i] = foo(i);
    B[i] = bar(A[i], B[i-1]);
    C[i] = baz(B[i]);
  }
}
