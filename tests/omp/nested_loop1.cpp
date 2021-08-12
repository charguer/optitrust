void work(int i, int j) {}

void good_nesting(int n)
{
    for (int i=0; i<n; i++) {
      {
        for (int j=0; j < n; j++)
          work(i, j);
      }
    }
}
