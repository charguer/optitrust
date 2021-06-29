int* t;
int* u;
int n;

int main() {
  for (int i = 1; i < n; i++) {
    {
       t[i] = i;
    }
    {
       u[i] += i;
    }
  }
}


/* LATER: in combi
  for (int j = 1; j < n; j++) {
    t[j] = j;
    u[j] += j;
  } // TODO: demo with sequence_split followed by loop_split

  LATER: loop.split_at which combines sequence_split followed by loop_split
*/