void work(int i, int j) {}
void wrong5(int n)
{
  work(n, 0);
  /* incorrect nesting of barrier region in a critical region */
  work(n, 1);
}
