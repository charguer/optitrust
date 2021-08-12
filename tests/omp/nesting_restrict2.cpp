void work(int i, int j) {}
void work1(int i, int n)
{
  /* incorrect nesting of loop regions */
  for (int j=0; j<n; j++)
    work(i, j);
}

void wrong2(int n)
{
  for (int i=0; i<n; i++)
     work1(i, n);
}

