void work(int i, int j) {}
void wrong4(int n)
{
  for (int i=0; i<n; i++) {
    work(i, 0); 
  /* incorrect nesting of barrier region in a loop region */
    work(i, 1);
  }
}
