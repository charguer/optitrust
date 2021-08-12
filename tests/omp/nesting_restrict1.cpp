void work(int i, int j) {}

void wrong1(int n)
{
  for (int i=0; i<n; i++) {
     /* incorrect nesting of loop regions */
       for (int j=0; j<n; j++)
         work(i, j);
  }
}
