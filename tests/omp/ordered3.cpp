void work(int i) {}
void ordered_good(int n)
{
  for (int i=0; i<n; i++) {
    if (i <= 10) {
         work(i);
    }
    if (i > 10) {
        work(i+1);
    }
  }
}

