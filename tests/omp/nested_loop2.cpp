void work(int i, int j) {}


void work1(int i, int n)
{
    for (int j=0; j<n; j++)
      work(i, j);
}


void good_nesting2(int n)
{
    for (int i=0; i<n; i++)
      work1(i, n);
}

