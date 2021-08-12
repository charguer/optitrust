void work(int i, int j) {}
void wrong3(int n)
{
  for (int i=0; i<n; i++) {
  /* incorrect nesting of regions */
    work(i, 0);
  }
}