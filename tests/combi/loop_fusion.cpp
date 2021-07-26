int* t;
int* u;
int* v;
int n;

int main() {
  float p = 5.0;
  for (int i = 0; i < n; i++) {
    t[i] = i;
  }
  for (int i = 0; i < n; i++) {
    u[i] += i;
  }
  for (int i = 0; i < n; i++) {
    v[i] += i;
  }
}
/*
  t1;
  t2;
  tofusion: {
     for () { c1 }
     for () { c2 }
  };
  t3

-->

  t1;
  t2;
  for () { c1; c2 }
  t3




  LATER in combi

  t1;
  t2;
  for () { }
  for () { }
  t3

  call sequence_sub ~label:"tofusion" [cFor "i"] 2

  LATER fusion_at [cFor "i"]
    => automatically computes the index of that loop in the surrounding sequence
     and automatically calls sequence_sub on that index and 2
     just checking that the instruction at index+1  is another for loop
     with same counters
*/
