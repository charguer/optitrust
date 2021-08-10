int foo(int *p){
  *p = *p + 10;
  return *p;
}
 
int myaddint(int *a, int *b, int n)
{
  for (int i=0; i<n; i++){
      a[i]  = foo(&b[i]);  /* foo is not called under a condition */
  }
  return a[n-1];
}
 
float goo(float *p){
  *p = *p + 18.5f;
  return *p;
}
 
int myaddfloat(float *x, float *y, int n)
{
  for (int i=0; i<n; i++){
     x[i] = (x[i] > y[i]) ? goo(&y[i]) : y[i];
       /* goo is called under the condition (or within a branch) */
  }
  return x[n-1];
}