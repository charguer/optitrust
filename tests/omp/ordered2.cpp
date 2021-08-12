void work(int i) {}

void ordered_wrong(int n)
{
  for (int i=0; i < n; i++) {
/* incorrect because an iteration may not execute more than one
   ordered region */
      work(i);
      work(i+1);
  }
}

